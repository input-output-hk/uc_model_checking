module p_bridge_br{


    import basicSpells.* from "../../spells/basicSpells"
    import common.* from "../common"

    import f_pki_types.* from "../f_pki/f_pki_types"    
    import f_pki.* from "../f_pki/f_pki"    

    import f_atms_types.* from "../f_atms/f_atms_types"    
    import f_atms.* from "../f_atms/f_atms"    

    import f_dif_types.* from "../f_dif/f_dif_types"    
    import f_dif.* from "../f_dif/f_dif"        
    
    import g_clock_types.* from "../g_clock/g_clock_types"    
    import g_clock.* from "../g_clock/g_clock"

    import g_ledger_types.* from "../g_ledger/g_ledger_types"
    import g_ledger.* from "../g_ledger/g_ledger"    

    import p_bridge_types.* from "./p_bridge_types"    
    import p_bridge.* from "./p_bridge"     

    import f_hash_types.* from "../f_hash/f_hash_types"    
    import f_hash.* from "../f_hash/f_hash"        
    

    pure def brd_br_1(sys: BRD_CSystem) : Set[BRD_CTransition] = {
        sys.brd.input.filterMap(z => match z {
            | BridgeRequest(p) => Some(p)
            | _ => None
        }).map(p => {
            if(brd_access_check(p.cfg, sys.brd.params)){
                val r = sys.clock.state.TIME
                if(r < sys.brd.params.r0 + 2 or sys.brd.state.bridge_flag.contains((p.cfg.party, r))){

                    Set({...sys.brd_id_ctrans(),
                         brd: sys.brd.brd_skip_trans()})

                }else{

                    val brd_trans = {
                        post_state: {...sys.brd.state,
                            bridge_flag: sys.brd.state.bridge_flag.union(Set((p.cfg.party, r)))
                        },

                        effects: Set(
                            BRD_LogMessage(BridgeRequest(p)),
                            BRD_UpdateInput(Set()),
                            BRD_AppendHandle(BridgeFetchWait({cfg: p.cfg, r: r}))
                        )
                    }

                    val dif_trans = {
                        post_state: sys.dif.state,
                        effects: Set(
                            DIF_UpdateInput(Set(DIF_FetchRequest({cfg: p.cfg})))
                        )   
                    }
                    
                    Set({...sys.brd_id_ctrans(),
                        brd: brd_trans,
                        dif: dif_trans})
                }
            }else{
                Set({
                    ...sys.brd_id_ctrans(),
                    brd: sys.brd.brd_skip_trans()
                })
            }
        }).flatten()
    }

    pure def brd_br_2(sys : BRD_CSystem) : Set[BRD_CTransition] = {
        sys.dif.input.filterMap(z => match z {
            | DIF_FetchResponse(p) => Some(p)
            | _ => None
        }).map(p => {
            sys.brd.handles.filterMap(z => match z {
                | BridgeFetchWait(q) => Some(q)
                | _ => None
            }).filter(q => p.cfg == q.cfg).map(q => {

                val brd_trans = {
                    post_state: sys.brd.state,
                    effects: Set(
                        BRD_RemoveHandle(BridgeFetchWait(q)),
                        BRD_AppendHandle(BridgeL1ReadWait({cfg: p.cfg, r: q.r, ms: p.data}))
                    )
                }

                val ledger1_trans = {
                    post_state: sys.ledger1.state,
                    effects: Set(
                        LED_UpdateInput(Set(ReadRequest({cfg: p.cfg})))
                    )
                }

                Set({...sys.brd_id_ctrans(),
                    brd: brd_trans,
                    dif: sys.dif.dif_skip_trans(),
                    ledger1: ledger1_trans
                })
            }).flatten()
        }).flatten()
    }

    pure def brd_br_3(sys : BRD_CSystem) : Set[BRD_CTransition] = {
        sys.ledger1.input.filterMap(z => match z {
            | ReadResponse(p) => Some(p)
            | _ => None
        }).map(p => {
            sys.brd.handles.filterMap(z => match z {
                | BridgeL1ReadWait(q) => Some(q)
                | _ => None
            }).filter(q => p.cfg == q.cfg).map(q => {

                val brd_trans = {
                    post_state: sys.brd.state,
                    effects: Set(
                        BRD_RemoveHandle(BridgeL1ReadWait(q)),
                        BRD_AppendHandle(BridgeL2ReadWait({cfg: p.cfg, r: q.r, ms: q.ms, L1: p.result}))
                    )
                }

                val ledger2_trans = {
                    post_state: sys.ledger2.state,
                    effects: Set(
                        LED_UpdateInput(Set(ReadRequest({cfg: p.cfg})))
                    )
                }

                Set({...sys.brd_id_ctrans(),
                    brd: brd_trans,
                    ledger1: sys.ledger1.led_skip_trans(),
                    ledger2: ledger2_trans
                })
            }).flatten()
        }).flatten()
    }

    pure def brd_br_4(sys : BRD_CSystem) : Set[BRD_CTransition] = {
        sys.ledger2.input.filterMap(z => match z {
            | ReadResponse(p) => Some(p)
            | _ => None
        }).map(p => {
            sys.brd.handles.filterMap(z => match z {
                | BridgeL2ReadWait(q) => Some(q)
                | _ => None
            }).filter(q => p.cfg == q.cfg).map(q => {

                val brd_trans = {
                    post_state: sys.brd.state,
                    effects: Set(
                        BRD_RemoveHandle(BridgeL2ReadWait(q)),
                        BRD_UpdateInput(Set(BridgeBRAlgorithmRequest({cfg: p.cfg, args: {r: q.r, ms: q.ms, L1: q.L1, L2: p.result}}))),
                    )
                }

                Set({...sys.brd_id_ctrans(),
                    brd: brd_trans,
                    ledger2: sys.ledger2.led_skip_trans(),
                })
            }).flatten()
        }).flatten()
    }

    pure def brd_br_5(sys : BRD_CSystem) : Set[BRD_CTransition] = {
        sys.brd.input.filterMap(z => match z {
            | BridgeBRAlgorithmRequest(p) => Some(p)
            | _ => None
        }).map(p => {
            val r = p.args.r
            val r0 = sys.brd.params.r0
            val R = sys.brd.params.R
            val delta = sys.brd.params.delta
            val bstP = sys.brd.state.bst.get(p.cfg.party)
            if(r == r0 + 2){
                brd_br_5_case1(sys, p, bstP)
            }else if(r0 + 3 <= r and r <= r0 + 3 + (R - ((r0 + 3) % R)) - 1){
                brd_br_5_case2(sys, p, bstP)
            }else if(r % R == 0){
                brd_br_5_case3(sys, p, bstP)
            }else if(1 <= r % R and r % R <= delta + 1){
                brd_br_5_case4(sys, p, bstP)
            }else if(r % R == delta + 2){
                brd_br_5_case5(sys, p, bstP)
            }else{
                // Inactive phase (r % R in [delta+3, R-1]): preserve state, no transaction
                brd_br_5_case6(sys, p, bstP)
            }
        }).flatten()
    }    

    pure def brd_br_5_case1(sys : BRD_CSystem, p: {cfg: MsgConfig, args: BRD_BR_Args}, bst: BRD_BST_Entry) : Set[BRD_CTransition] = {
        if (bst.rot == None) {
            // RoT not yet completed; skip this round without installing
            Set({...sys.brd_id_ctrans(),
                brd: {
                    post_state: sys.brd.state,
                    effects: Set(
                        BRD_UpdateInput(Set(BridgeBRAlgorithmResponse({
                            cfg: p.cfg,
                            args: p.args,
                            resp: {btx2: None, m: None, bst: Some(bst)}
                        })))
                    )
                }
            })
        } else {
            val rot = bst.rot.unwrap()
            val code = rot
            val cID = rot
            val btx2 = Install({cID: cID, code: code})

            val brd_trans = {
                post_state: sys.brd.state,
                effects: Set(
                    BRD_UpdateInput(Set(BridgeBRAlgorithmResponse({
                        cfg: p.cfg,
                        args: p.args,
                        resp: ({
                            btx2: Some(btx2),
                            m: None,
                            bst: Some({...bst, cID: Some(cID)})
                        })})))
                )
            }

            Set({...sys.brd_id_ctrans(),
                brd: brd_trans,
            })
        }
    }

    pure def brd_br_5_case2(sys : BRD_CSystem, p: {cfg: MsgConfig, args: BRD_BR_Args}, bst: BRD_BST_Entry) : Set[BRD_CTransition] = {

        val brd_trans = {
            post_state: sys.brd.state,
            effects: Set(
                BRD_UpdateInput(Set(BridgeBRAlgorithmResponse({
                    cfg: p.cfg, 
                    args: p.args, 
                    resp: ({
                        //btx1: None,
                        btx2: None,
                        m: None,
                        bst: Some(bst)
                    })})))
            )
        }

        Set({...sys.brd_id_ctrans(),
            brd: brd_trans,
        })
    }

    pure def brd_br_5_case4(sys : BRD_CSystem, p: {cfg: MsgConfig, args: BRD_BR_Args}, bst: BRD_BST_Entry) : Set[BRD_CTransition] = {

        val brd_trans = {
            post_state: sys.brd.state,
            effects: Set(
                BRD_UpdateInput(Set(BridgeBRAlgorithmResponse({
                    cfg: p.cfg, 
                    args: p.args, 
                    resp: ({
                        //btx1: None,
                        btx2: None,
                        m: None,
                        bst: Some({...bst, m: bst.m.union(p.args.ms)})
                    })})))
            )
        }

        Set({...sys.brd_id_ctrans(),
            brd: brd_trans,
        })
    }        

    pure def brd_br_5_case5(sys : BRD_CSystem, p: {cfg: MsgConfig, args: BRD_BR_Args}, bst: BRD_BST_Entry) : Set[BRD_CTransition] = {
        if (bst.aux == None or bst.h == None or bst.avk_prim == None or bst.cID == None) {
            // Prerequisite state not yet established (case3 must run first); skip
            Set({...sys.brd_id_ctrans(),
                brd: {
                    post_state: sys.brd.state,
                    effects: Set(
                        BRD_UpdateInput(Set(BridgeBRAlgorithmResponse({
                            cfg: p.cfg,
                            args: p.args,
                            resp: {btx2: None, m: None, bst: Some(bst)}
                        })))
                    )
                }
            })
        } else {
            val bvks = bst.aux.unwrap().bvks
            val com = bst.aux.unwrap().com
            val cID = bst.cID.unwrap()
            val msg = {h: bst.h.unwrap(), avk: bst.avk_prim.unwrap()}

            val S = com.foldl(List(), (acc, party) => {
                if (bvks.has(party)){
                    val bvk = bvks.get(party)
                    val e = bst.m.find(x => x.party == party and x.avk == bvk)
                    if (e != None){
                        acc.append({vk: bvk, sig: e.unwrap().sig})
                    }else{
                        acc
                    }
                }else{
                    acc
                }
            })

            val vks = com.listFilterMap(party => if (bvks.has(party) and com.listHas(party)) Some(bvks.get(party)) else None)

            val br_trans = {
                post_state: sys.brd.state,
                effects: Set(
                    BRD_UpdateInput(Set()),
                    BRD_AppendHandle((BridgeASigWait({
                        cfg: p.cfg,
                        args: p.args,
                        bst: bst,
                        msg: msg,
                        cID: cID
                    }))),
                )
            }

            val atms_trans = {
                post_state: sys.atms.state,
                effects: Set(
                    ATMS_UpdateInput(Set(ASignRequest({
                        cfg: p.cfg,
                        m: msg,
                        vks: vks,
                        S: S
                    })))
                )
            }

            Set({...sys.brd_id_ctrans(),
                brd: br_trans,
                atms: atms_trans,
            })
        }
    }

    pure def brd_br_5_case5_2(sys : BRD_CSystem) : Set[BRD_CTransition] = {          
        sys.atms.input.filterMap(z => match z {
            | ASignResponse(p) => Some(p)
            | _ => None
        }).map(p => {
            sys.brd.handles.filterMap(z => match z {
                | BridgeASigWait(q) => Some(q)
                | _ => None
            }).filter(q => p.cfg == q.cfg).map(q => {
                if(p.tau == None){
                    // Threshold signature failed; skip update, clear accumulated messages
                    val brd_trans_fail = {
                        post_state: sys.brd.state,
                        effects: Set(
                            BRD_RemoveHandle(BridgeASigWait(q)),
                            BRD_UpdateInput(Set(BridgeBRAlgorithmResponse({
                                cfg: p.cfg,
                                args: q.args,
                                resp: ({
                                    btx2: None,
                                    m: None,
                                    bst: Some({...q.bst, m: Set()})
                                })})))
                        )
                    }
                    Set({...sys.brd_id_ctrans(),
                        atms: sys.atms.atms_skip_trans(),
                        brd: brd_trans_fail
                    })
                }else{
                    val btx2 = Update({msg: q.msg, sigma: p.tau.unwrap()})
                    val brd_trans = {
                        post_state: sys.brd.state,
                        effects: Set(
                            BRD_RemoveHandle(BridgeASigWait(q)),
                            BRD_UpdateInput(Set(BridgeBRAlgorithmResponse({
                                cfg: p.cfg, 
                                args: q.args, 
                                resp: ({
                                    //btx1: None,
                                    btx2: Some(btx2),
                                    m: None,
                                    bst: Some({...q.bst, m : Set(), aux: q.bst.aux_prim})
                                })})))
                        )
                    }

                    Set({...sys.brd_id_ctrans(),
                        atms: sys.atms.atms_skip_trans(),
                        brd: brd_trans
                    })
                }
            }).flatten()
        }).flatten()
    }


    pure def brd_br_5_case6(sys : BRD_CSystem, p: {cfg: MsgConfig, args: BRD_BR_Args}, bst: BRD_BST_Entry) : Set[BRD_CTransition] = {

        val brd_trans = {
            post_state: sys.brd.state,
            effects: Set(
                BRD_UpdateInput(Set(BridgeBRAlgorithmResponse({
                    cfg: p.cfg, 
                    args: p.args, 
                    resp: ({
                        //btx1: None,
                        btx2: None,
                        m: None,
                        bst: Some(bst)
                    })})))
            )
        }

        Set({...sys.brd_id_ctrans(),
            brd: brd_trans,
        })
    }

    pure def brd_br_5_case3(sys : BRD_CSystem, p: {cfg: MsgConfig, args: BRD_BR_Args}, bst : BRD_BST_Entry) : Set[BRD_CTransition] = {
        if (bst.aux == None) {
            // RoT not yet completed; aux (bvks + committee) not available
            Set({...sys.brd_id_ctrans(),
                brd: {
                    post_state: sys.brd.state,
                    effects: Set(
                        BRD_UpdateInput(Set(BridgeBRAlgorithmResponse({
                            cfg: p.cfg,
                            args: p.args,
                            resp: {btx2: None, m: None, bst: Some(bst)}
                        })))
                    )
                }
            })
        } else {
            val brd_trans = {
                post_state: sys.brd.state,
                effects: Set(
                    BRD_UpdateInput(Set()),
                    BRD_AppendHandle(BridgeComSelWait({cfg: p.cfg, args: p.args, bst: {...bst, m: bst.m.union(p.args.ms)} }))
                )
            }

            val ledger1_trans = {
                post_state: sys.ledger1.state,
                effects: Set(
                    LED_UpdateInput(Set(ComSelRequest({cfg: p.cfg})))
                )
            }

            Set({...sys.brd_id_ctrans(),
                brd: brd_trans,
                ledger1: ledger1_trans,
            })
        }
    }

    pure def brd_br_5_case3_2(sys : BRD_CSystem) : Set[BRD_CTransition] = {    
        sys.ledger1.input.filterMap(z => match z {
            | ComSelResponse(p) => Some(p)
            | _ => None
        }).map(p => {
            sys.brd.handles.filterMap(z => match z {
                | BridgeComSelWait(q) => Some(q)
                | _ => None
            }).filter(q => p.cfg == q.cfg).map(q => {
                val brd_trans = {
                    post_state: sys.brd.state,
                    effects: Set(
                        BRD_RemoveHandle(BridgeComSelWait(q)),
                        BRD_AppendHandle(BridgeAKeyWait({cfg: p.cfg, args: q.args, bst: q.bst, com_prim: p.com}))
                    )
                }

                val vks = p.com.listFilterMap(party => {
                    val aux = q.bst.aux.unwrap()
                    if (aux.bvks.has(party) and p.com.listHas(party)){
                        Some(aux.bvks.get(party))
                    }else{
                        None
                    }
                })

                val atms_trans = {
                    post_state: sys.atms.state,
                    effects: Set(
                        ATMS_UpdateInput(Set(AKeyRequest({cfg: p.cfg, vks: vks})))
                    )
                }

                Set({...sys.brd_id_ctrans(),
                    brd: brd_trans,
                    ledger1: sys.ledger1.led_skip_trans(),
                    atms: atms_trans,
                })
            })
        }).flatten().flatten()
    }

    pure def brd_br_5_case3_3(sys : BRD_CSystem) : Set[BRD_CTransition] = {    
        sys.atms.input.filterMap(z => match z {
            | AKeyResponse(p) => Some(p)
            | _ => None
        }).map(p => {
            sys.brd.handles.filterMap(z => match z {
                | BridgeAKeyWait(q) => Some(q)
                | _ => None
            }).filter(q => p.cfg == q.cfg).map(q => {

                if(p.avk == None){
                    val brd_trans = {
                        post_state: sys.brd.state,
                        effects: Set(
                            BRD_RemoveHandle(BridgeAKeyWait(q)),
                            BRD_UpdateInput(Set(BridgeBRAlgorithmResponse({
                                cfg: q.cfg,
                                args: q.args,
                                resp: ({
                                    btx2: None,
                                    m: None,
                                    bst: None
                                })})))
                        )
                    }
                    Set({...sys.brd_id_ctrans(),
                        atms: sys.atms.atms_skip_trans(),
                        brd: brd_trans
                    })
                }else{

                    val brd_trans = {
                        post_state: sys.brd.state,
                        effects: Set(
                            BRD_RemoveHandle(BridgeAKeyWait(q)),
                            BRD_AppendHandle(BridgeAPLWait({
                                cfg: q.cfg,
                                args: q.args,
                                bst: q.bst,
                                com_prim: q.com_prim,
                                avk_prim: p.avk.unwrap()
                            }))
                        )
                    }

                    val ledger1_trans = {
                        post_state: sys.ledger1.state,
                        effects: Set(
                            LED_UpdateInput(Set(APLRequest({
                                cfg: q.cfg,
                            })))
                        )
                    }

                    Set({...sys.brd_id_ctrans(),
                        atms: sys.atms.atms_skip_trans(),
                        brd: brd_trans,
                        ledger1: ledger1_trans
                    })
                }
            })
        }).flatten().flatten()
    }

    pure def brd_br_5_case3_4(sys : BRD_CSystem) : Set[BRD_CTransition] = {  
        sys.ledger1.input.filterMap(z => match z {
            | APLResponse(p) => Some(p)
            | _ => None
        }).map(p => {
            sys.brd.handles.filterMap(z => match z {
                | BridgeAPLWait(q) => Some(q)
                | _ => None
            }).filter(q => p.cfg == q.cfg).map(q => {

                val brd_trans = {
                    post_state: sys.brd.state,
                    effects: Set(
                        BRD_RemoveHandle(BridgeAPLWait(q)),
                        BRD_AppendHandle(BridgeHashWait({
                            cfg: q.cfg,
                            args: q.args,
                            bst: q.bst,
                            com_prim: q.com_prim,
                            avk_prim: q.avk_prim,
                            apl: p.p
                        }))
                    )
                }

                val L = q.args.L1.slice(0, p.p)

                val hash_trans = {
                    post_state: sys.hash.state,
                    effects: Set(
                        HASH_UpdateInput(Set(HashRequest({
                            cfg: q.cfg,
                            x: L
                        })))
                    )
                }

                Set({...sys.brd_id_ctrans(),
                    brd: brd_trans,
                    ledger1: sys.ledger1.led_skip_trans(),
                    hash: hash_trans
                })
            }).flatten()
        }).flatten()

    }

    pure def brd_br_5_case3_5(sys : BRD_CSystem) : Set[BRD_CTransition] = {    
        sys.hash.input.filterMap(z => match z {
            | HashResponse(p) => Some(p)
            | _ => None
        }).map(p => {
            sys.brd.handles.filterMap(z => match z {
                | BridgeHashWait(q) => Some(q)
                | _ => None
            }).filter(q => p.cfg == q.cfg).map(q => {
                    
                val brd_trans = {
                    post_state: sys.brd.state,
                    effects: Set(
                        BRD_RemoveHandle(BridgeHashWait(q)),
                        BRD_AppendHandle(BridgeSigWait({
                            cfg: q.cfg,
                            args: q.args,
                            bst: q.bst,
                            com_prim: q.com_prim,
                            avk_prim: q.avk_prim,
                            apl: q.apl,
                            h: p.h
                        }))
                    )
                }

                val atms_trans = {
                    post_state: sys.atms.state,
                    effects: Set(
                        ATMS_UpdateInput(Set(SignRequest({
                            cfg: q.cfg,
                            m: {h: p.h, avk: q.avk_prim}
                        })))
                    )
                }


                Set({...sys.brd_id_ctrans(),
                    brd: brd_trans,
                    hash: sys.hash.hash_skip_trans(),
                    atms: atms_trans
                })
            })
        }).flatten().flatten()
    }

    pure def brd_br_5_case3_6(sys : BRD_CSystem) : Set[BRD_CTransition] = {  
        sys.atms.input.filterMap(z => match z {
            | SignResponse(p) => Some(p)
            | _ => None
        }).map(p => {
            sys.brd.handles.filterMap(z => match z {
                | BridgeSigWait(q) => Some(q)
                | _ => None
            }).filter(q => p.cfg == q.cfg).map(q => {

                val bst = {...q.bst, 
                    aux_prim: Some({bvks: q.bst.aux.unwrap().bvks, com: q.com_prim}),    
                    h: Some(q.h),
                    avk_prim: Some(q.avk_prim)
                }

                if(p.sig == None or q.bst.aux.unwrap().com.listHas(p.cfg.party) == false){

                    val brd_trans = {
                        post_state: sys.brd.state,
                        effects: Set(
                            BRD_UpdateInput(Set(BridgeBRAlgorithmResponse({
                                cfg: q.cfg, 
                                args: q.args, 
                                resp: ({
                                    //btx1: None,
                                    btx2: None,
                                    m: None,
                                    bst: Some(bst) 
                                })}))),
                            BRD_RemoveHandle(BridgeSigWait(q))
                        )
                    }

                    Set({...sys.brd_id_ctrans(),
                        atms: sys.atms.atms_skip_trans(),
                        brd: brd_trans,
                    })
                }else{

                    val brd_trans = {
                        post_state: sys.brd.state,
                        effects: Set(
                            BRD_UpdateInput(Set(BridgeBRAlgorithmResponse({
                                cfg: q.cfg, 
                                args: q.args, 
                                resp: ({
                                    //btx1: None,
                                    btx2: None,
                                    m: Some({party: p.cfg.party, avk: bst.aux.unwrap().bvks.get(p.cfg.party), sig: p.sig.unwrap()}),
                                    bst: Some(bst) 
                                })}))),
                            BRD_RemoveHandle(BridgeSigWait(q))
                        )
                    }

                    Set({...sys.brd_id_ctrans(),
                        atms: sys.atms.atms_skip_trans(),
                        brd: brd_trans,
                    })
                }

            })
        }).flatten().flatten()
    }

    pure def brd_br_6(sys : BRD_CSystem) : Set[BRD_CTransition] = {      
        sys.brd.input.filterMap(z => match z {
            | BridgeBRAlgorithmResponse(p) => Some(p)
            | _ => None
        }).map(p => {

            if(p.resp.bst == None){
                // ATMS key aggregation failed; preserve current bst, no transaction
                val current_bst = sys.brd.state.bst.get(p.cfg.party)
                Set({...sys.brd_id_ctrans(),
                    brd: {
                        post_state: sys.brd.state,
                        effects: Set(
                            BRD_UpdateInput(Set(BridgeDiffuse({
                                cfg: p.cfg, args: p.args,
                                resp: {btx2: None, m: None, bst: Some(current_bst)}
                            }))),
                            BRD_LogMessage(BridgeBRAlgorithmResponse(p))
                        )
                    }
                })
            }else{

                Set({...sys.brd_id_ctrans(),
                    brd: {
                        post_state: {...sys.brd.state,
                            bst: sys.brd.state.bst.put(p.cfg.party, p.resp.bst.unwrap())
                        },
                        effects: Set(
                            BRD_UpdateInput(Set(BridgeDiffuse({cfg: p.cfg, args: p.args, resp: p.resp}))),
                            BRD_LogMessage(BridgeBRAlgorithmResponse(p))
                        )
                    },
                })
            }
        }).flatten()
    }

    pure def brd_br_7(sys : BRD_CSystem) : Set[BRD_CTransition] = {   
        sys.brd.input.filterMap(z => match z {
            | BridgeDiffuse(p) => Some(p)
            | _ => None
        }).map(p => {
            if(p.resp.m == None){
                Set({...sys.brd_id_ctrans(),
                    brd: {
                        post_state: sys.brd.state,
                        effects: Set(
                            BRD_UpdateInput(Set(BridgeL2Submit(p)))
                        )
                    }
                })
            }else{

                val brd_trans = {
                        post_state: sys.brd.state,
                        effects: Set(
                            BRD_UpdateInput(Set()),
                            BRD_AppendHandle(BridgeDiffuseWait({cfg: p.cfg, args: p.args, resp: p.resp}))
                        )}

                val dif_trans = {
                    post_state: sys.dif.state,
                    effects: Set(
                        DIF_UpdateInput(Set(DIF_DiffuseRequest({cfg: p.cfg, m: p.resp.m.unwrap()})))
                    )
                }

                Set({...sys.brd_id_ctrans(),
                    brd: brd_trans,
                    dif : dif_trans
                })
            }
        }).flatten()
    }

    pure def brd_br_8(sys : BRD_CSystem) : Set[BRD_CTransition] = {   
        sys.dif.input.filterMap(z => match z {
            | DIF_DiffuseResponse(p) => Some(p)
            | _ => None
        }).map(p => {
            sys.brd.handles.filterMap(z => match z {
                | BridgeDiffuseWait(q) => Some(q)
                | _ => None
            }).filter(q => p.cfg == q.cfg).map(q => {
                Set({...sys.brd_id_ctrans(),
                    dif: sys.dif.dif_skip_trans(),
                    brd: {
                        post_state: sys.brd.state,
                        effects: Set(
                            BRD_UpdateInput(Set(BridgeL2Submit({cfg: p.cfg, args: q.args, resp: q.resp}))),
                            BRD_RemoveHandle(BridgeDiffuseWait(q))
                        )
                    }
                })
            })
        }).flatten().flatten()
    }


    // pure def brd_br_9(sys : BRD_CSystem) : Set[BRD_CTransition] = {   
    //     sys.brd.input.brd_get_bridge_l1_submit_messages().map(p => {
    //         Set({...sys.brd_id_ctrans(),
    //             brd: {
    //                 post_state: sys.brd.state,
    //                 effects: Set(
    //                     BRD_UpdateInput(Set(BridgeL2Submit(p)))
    //                 )
    //             }
    //         })
    //     }).flatten()
    // }

    // pure def brd_br_10(sys : BRD_CSystem) : Set[BRD_CTransition] = {  
    //     sys.ledger1.input.led_get_submit_response_messages().map(p => {
    //         sys.brd.handles.brd_get_bridge_l1_submit_wait_messages().filter(q => p.cfg == q.cfg).map(q => {
    //             Set({...sys.brd_id_ctrans(),
    //                 ledger1: sys.ledger1.led_skip_trans(),
    //                 brd: {
    //                     post_state: sys.brd.state,
    //                     effects: Set(
    //                         BRD_UpdateInput(Set(BridgeL2Submit({cfg: p.cfg, args: q.args, resp: q.resp}))),
    //                         BRD_RemoveHandle(BridgeL1SubmitWait(q))
    //                     )
    //                 }
    //             })
    //         }).flatten()
    //     }).flatten()
    // }

    pure def brd_br_11(sys : BRD_CSystem) : Set[BRD_CTransition] = {   
        sys.brd.input.filterMap(z => match z {
            | BridgeL2Submit(p) => Some(p)
            | _ => None
        }).map(p => {
            if(p.resp.btx2 == None){
                Set({...sys.brd_id_ctrans(),
                    brd: {
                        post_state: sys.brd.state,
                        effects: Set(
                            BRD_UpdateInput(Set()),
                            BRD_AppendHandle(BridgeOKWait({cfg: p.cfg, args: p.args, resp: p.resp}))
                        )
                    },
                    clock: {
                        post_state: sys.clock.state,
                        effects: Set(
                            GC_UpdateInput(Set(OkayRequest({cfg: p.cfg})))
                        )
                    }
                })
            } else {
                val brd_trans = {
                        post_state: sys.brd.state,
                        effects: Set(
                            BRD_AppendHandle(BridgeL2SubmitWait({cfg: p.cfg, args: p.args, resp: p.resp})),
                            BRD_UpdateInput(Set())
                        )
                }

                val ledger2_trans = {
                    post_state: sys.ledger2.state,
                    effects: Set(
                        LED_UpdateInput(Set(SubmitRequest({cfg: p.cfg, tx: p.resp.btx2.unwrap()})))
                    )
                }

                Set({...sys.brd_id_ctrans(),
                    brd: brd_trans,
                    ledger2: ledger2_trans
                })
            }
        }).flatten()
    }    

    pure def brd_br_12(sys : BRD_CSystem) : Set[BRD_CTransition] = {  
        sys.ledger2.input.filterMap(z => match z {
            | SubmitResponse(p) => Some(p)
            | _ => None
        }).map(p => {
            sys.brd.handles.filterMap(z => match z {
                | BridgeL2SubmitWait(q) => Some(q)
                | _ => None
            }).filter(q => p.cfg == q.cfg).map(q => {

                Set({...sys.brd_id_ctrans(),
                    ledger2: sys.ledger2.led_skip_trans(),
                    brd: {
                        post_state: sys.brd.state,
                        effects: Set(
                            BRD_UpdateInput(Set()),
                            BRD_AppendHandle(BridgeOKWait({cfg: p.cfg, args: q.args, resp: q.resp})),
                            BRD_RemoveHandle(BridgeL2SubmitWait(q))
                        )
                    },
                    clock: {
                        post_state: sys.clock.state,
                        effects: Set(
                            GC_UpdateInput(Set(OkayRequest({cfg: p.cfg})))
                        )
                    }
                })
            }).flatten()
        }).flatten()
    }    

    pure def brd_br_13(sys : BRD_CSystem) : Set[BRD_CTransition] = {  
        sys.clock.input.filterMap(z => match z {
            | OkayResponse(p) => Some(p)
            |  _ => None
        }).map(p => {
            sys.brd.handles.filterMap(z => match z {
                | BridgeOKWait(q) => Some(q)
                | _ => None
            }).filter(q => p.cfg == q.cfg).map(q => {
                Set({...sys.brd_id_ctrans(),
                    clock: sys.clock.gc_skip_trans(),
                    brd: {
                        post_state: sys.brd.state, 
                        effects: Set(
                            BRD_RemoveHandle(BridgeOKWait(q)),
                            BRD_UpdateInput(Set(BridgeResponse({cfg: p.cfg, args: q.args, resp: q.resp})))
                        )
                    },
                })
            }).flatten()
        }).flatten()
    }

    pure def brd_br_main_listener(sys: BRD_CSystem) : Set[BRD_CTransition] = {
        Set(
            brd_br_1(sys),
            brd_br_2(sys),
            brd_br_3(sys),
            brd_br_4(sys),
            brd_br_5(sys),
            brd_br_5_case5_2(sys),
            brd_br_6(sys),
            brd_br_5_case3_6(sys),
            brd_br_5_case3_5(sys),
            brd_br_5_case3_4(sys),
            brd_br_5_case3_3(sys),
            brd_br_5_case3_2(sys),
            brd_br_7(sys),
            brd_br_8(sys),
            //brd_br_9(sys), // no submission to L1
            //brd_br_10(sys), // no submission to L1
            brd_br_11(sys),
            brd_br_12(sys),
            brd_br_13(sys)
        ).flatten()
    }


}