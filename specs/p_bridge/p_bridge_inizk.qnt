module p_bridge_inizk{
    
    import basicSpells.* from "../../spells/basicSpells"
    import common.* from "../common"

    
    import f_hash_types.* from "../f_hash/f_hash_types"    
    import f_hash.* from "../f_hash/f_hash"    

    import g_ledger_types.* from "../g_ledger/g_ledger_types"
    import g_ledger.* from "../g_ledger/g_ledger"    

    import p_bridge_types.* from "./p_bridge_types"    
    import p_bridge.* from "./p_bridge"    

    import f_inizk_types.* from "../f_inizk/f_inizk_types"  

    pure def brd_certify_1(sys: BRD_CSystem) : Set[BRD_CTransition] = {
        sys.brd.input.brd_get_certify_request_messages().map(p => {
            if(brd_access_check(p.cfg, sys.brd.params)){
                {...sys.brd_id_ctrans(),
                    brd: {
                        post_state: sys.brd.state,
                        effects: Set(
                            BRD_LogMessage(CertifyRequest(p)),
                            BRD_UpdateInput(Set()),
                            BRD_AppendHandle(CertifyReadWait(p))  
                        )
                    },
                    ledger1: {
                        post_state: sys.ledger1.state,
                        effects: Set(
                            LED_UpdateInput(Set(ReadRequest({cfg: p.cfg})))
                        )
                    }
                }
            }else{
                {...sys.brd_id_ctrans(),
                    brd: sys.brd.brd_skip_trans()
                }
            }
        })
    }

    pure def brd_certify_2(sys : BRD_CSystem) : Set[BRD_CTransition] = {
        sys.ledger1.input.filterMap(z => match z {| ReadResponse(p) => Some(p) | _ => None }).map(p => {
            sys.brd.handles.filterMap(z => match z {
                | CertifyReadWait(q) => Some(q)
                | _ => None
            }).filter(q => p.cfg == q.cfg).map(q => {
                val brd_trans = {
                    post_state: sys.brd.state,
                    effects: Set(
                        BRD_UpdateInput(Set()),
                        BRD_RemoveHandle(CertifyReadWait(q)),
                        BRD_AppendHandle(CertifyAPLWait({cfg: q.cfg, rot: q.rot, x: q.x, L1: p.result}))
                    )
                }

                val ledger1_trans = {
                    post_state: sys.ledger1.state,
                    effects: Set(
                        LED_UpdateInput(Set(APLRequest({cfg: p.cfg})))
                    )
                }

                Set({...sys.brd_id_ctrans(),
                    brd: brd_trans,
                    ledger1: ledger1_trans
                })
            }).flatten()
        }).flatten()
    }

    pure def brd_certify_3(sys : BRD_CSystem) : Set[BRD_CTransition] = {
        sys.ledger1.input.filterMap(z => match z { | APLResponse(p) => Some(p) | _ => None }).map(p => {
            sys.brd.handles.filterMap(z => match z {| CertifyAPLWait(q) => Some(q) | _ => None
            }).filter(q => p.cfg == q.cfg).map(q => {
                val bstP = sys.brd.state.bst.get(q.cfg.party)
                if (bstP.h == None or bstP.apl == None) {
                    // Bridge hasn't committed a hash yet; certify returns no proof
                    Set({...sys.brd_id_ctrans(),
                        brd: {
                            post_state: sys.brd.state,
                            effects: Set(
                                BRD_UpdateInput(Set(CertifyResponse({cfg: q.cfg, rot: q.rot, x: q.x, L1: q.L1, apl: p.p, h: 0, res: None, r: sys.clock.state.TIME}))),
                                BRD_RemoveHandle(CertifyAPLWait(q))
                            )
                        },
                        ledger1: sys.ledger1.led_skip_trans()
                    })
                } else {
                    val h = bstP.h.unwrap()
                    val apl = bstP.apl.unwrap()
                    val L = q.L1.slice(0, apl)

                    val brd_trans = {
                        post_state: sys.brd.state,
                        effects: Set(
                            BRD_UpdateInput(Set()),
                            BRD_RemoveHandle(CertifyAPLWait(q)),
                            BRD_AppendHandle(CertifyProveWait({cfg: q.cfg, rot: q.rot, x: q.x, L1: q.L1, apl: apl, h: h}))
                        )
                    }

                    val nizk_trans = {
                        post_state: sys.nizk.state,
                        effects: Set(
                            NIZK_UpdateInput(Set(NIZK_ProveRequest({cfg: p.cfg, x: {x: q.x, h: h}, w: L})))
                        )
                    }

                    Set({...sys.brd_id_ctrans(),
                        brd: brd_trans,
                        ledger1: sys.ledger1.led_skip_trans(),
                        nizk: nizk_trans
                    })
                }
            }).flatten()
        }).flatten()
    }

    pure def brd_certify_4(sys : BRD_CSystem) : Set[BRD_CTransition] = {
        sys.hash.input.filterMap(z => match z { | HashResponse(p) => Some(p) | _ => None }).map(p => {
            sys.brd.handles.filterMap(z => match z {
                | CertifyHashWait(q) => Some(q)
                | _ => None
            }).filter(q => p.cfg == q.cfg).map(q => {
                val brd_trans = {
                    post_state: sys.brd.state,
                    effects: Set(
                        BRD_UpdateInput(Set()),
                        BRD_RemoveHandle(CertifyHashWait(q)),
                        BRD_AppendHandle(CertifyProveWait({cfg: q.cfg, rot: q.rot, x: q.x, L1: q.L1, apl: q.apl, h: p.h}))
                    )
                }

                val L = q.L1.slice(0,q.apl)

                val nizk_trans = {
                    post_state: sys.nizk.state,
                    effects: Set(
                        NIZK_UpdateInput(Set(NIZK_ProveRequest({cfg: p.cfg, x: {x: q.x, h: p.h}, w: L})))
                    )
                }

                Set({...sys.brd_id_ctrans(),
                    brd: brd_trans,
                    hash: sys.hash.hash_skip_trans(),
                    nizk: nizk_trans
                })
            }).flatten()
        }).flatten()
    }

    pure def brd_certify_5(sys : BRD_CSystem) : Set[BRD_CTransition] = {
        sys.nizk.input.filterMap(z => match z { | NIZK_ProveResponse(p) => Some(p) | _ => None }).map(p => {
            sys.brd.handles.filterMap(z => match z {
                | CertifyProveWait(q) => Some(q)
                | _ => None
            }).filter(q => p.cfg == q.cfg).map(q => {
                val res = if (p.pi == None) None else Some({pi: p.pi.unwrap(), h: q.h})

                val brd_trans = {
                    post_state: sys.brd.state,
                    effects: Set(
                        BRD_UpdateInput(Set(CertifyResponse({cfg: q.cfg, rot: q.rot, x: q.x, L1: q.L1, apl: q.apl, h: q.h, res: res, r: sys.clock.state.TIME}))),
                        BRD_RemoveHandle(CertifyProveWait(q))
                    )
                }

                Set({...sys.brd_id_ctrans(),
                    brd: brd_trans,
                    nizk: sys.nizk.nizk_skip_trans()
                })
            }).flatten()
        }).flatten()
    }

    pure def brd_verify_1(sys : BRD_CSystem) : Set[BRD_CTransition] = {
        sys.brd.input.brd_get_verify_request_messages().map(p => {
            if(brd_access_check(p.cfg, sys.brd.params)) {
                if(sys.ledger2.state.mem.has(p.rot) == false) {
                    Set({...sys.brd_id_ctrans(),
                        brd: sys.brd.brd_skip_trans()
                    })
                }  else {
                    val mem = sys.ledger2.state.mem.get(p.rot)
                    val brd_trans = {
                        post_state: sys.brd.state,
                        effects: Set(
                            BRD_LogMessage(BRD_VerifyRequest(p)),
                            BRD_UpdateInput(Set()),
                            BRD_AppendHandle(BRD_VerifyNIZKVerifyWait({cfg: p.cfg, rot: p.rot, x: p.x, proof: p.proof, mem: mem}))
                        )
                    }

                    val nizk_trans = {
                        post_state: sys.nizk.state,
                        effects: Set(
                            //NIZK_VerifyRequest({cfg: MsgConfig, x: NIZK_Statement, pi: NIZK_Proof}),
                            NIZK_UpdateInput(Set(NIZK_VerifyRequest({cfg: p.cfg, x: {x: p.x, h: p.proof.h}, pi: p.proof.pi})))
                        )
                    }

                    Set({...sys.brd_id_ctrans(),
                        brd: brd_trans,
                        nizk: nizk_trans
                    })
                }
            }else{
                Set({...sys.brd_id_ctrans(),
                    brd: sys.brd.brd_skip_trans()
                })
            }
        }).flatten()
    }

    pure def brd_verify_2(sys : BRD_CSystem) : Set[BRD_CTransition] = {
        sys.nizk.input.filterMap(z => match z { | NIZK_VerifyResponse(p) => Some(p) | _ => None }).map(p => {
            sys.brd.handles.filterMap(z => match z {
                | BRD_VerifyNIZKVerifyWait(q) => Some(q)
                | _ => None
            }).filter(q => p.cfg == q.cfg).map(q => {
                val res = p.b and q.mem.hs.listHas(q.proof.h)
                
                val brd_trans = {
                    post_state: sys.brd.state,
                    effects: Set(
                        BRD_UpdateInput(Set(BRD_VerifyResponse({cfg: q.cfg, rot: q.rot, x: q.x, proof: q.proof, mem: q.mem, res: res, r: sys.clock.state.TIME}))),
                        BRD_RemoveHandle(BRD_VerifyNIZKVerifyWait(q))
                    )
                }

                Set({...sys.brd_id_ctrans(),
                    brd: brd_trans,
                    nizk: sys.nizk.nizk_skip_trans()
                })
            }).flatten()
        }).flatten()
    }

    pure def brd_inizk_main_listener(sys: BRD_CSystem) : Set[BRD_CTransition] = {
        brd_certify_1(sys)
        .union(brd_certify_2(sys))
        .union(brd_certify_3(sys))
        .union(brd_certify_4(sys))
        .union(brd_certify_5(sys))
        .union(brd_verify_1(sys))
        .union(brd_verify_2(sys))
    }
                
    
}