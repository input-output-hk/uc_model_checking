module f_inizk{

    import common.* from "../common"
    import params.* from "../params"
    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"
    import f_inizk_types.* from "./f_inizk_types"
    import f_hash_types.* from "../f_hash/f_hash_types"
    import f_hash.* from "../f_hash/f_hash"
    import g_ledger_types.* from "../g_ledger/g_ledger_types"

    var f_nizk_s : NIZK_System

    val ALL_PROOFS = 1.to(UNIVERSE_SIZE)

    pure def nizk_sim_loses_ctrl(nizk: NIZK_System) : Set[NIZK_Transition] = {
        nizk.input.filterMap(m => match m {
            | NIZK_SIMControl => Some(m)
            | _ => None
        }).map(c =>  
            {
                post_state : nizk.state,
                effects : Set(
                    NIZK_UpdateInput(Set()),
                )
            })
    }    

    pure def nizk_rel_1(nizk: NIZK_System, hash: HASH_System) : Set[(NIZK_Transition, HASH_Transition)] = {
        nizk.input.filterMap(m => match m {
            | NIZK_RelRequest(m) => Some(m)
            | _ => None
        }).map(m => {
           // if(nizk_access_check(m.cfg, nizk.params)) {
                val sid = m.cfg.sid
                val party = m.cfg.party
                val sid_state = nizk.state.get(sid)
                //val in_relation = nizk_Rel_sat(m.x, m.w)

                val nizk_transition = {
                    post_state : nizk.state,
                    effects : Set( 
                        NIZK_LogMessage(NIZK_RelRequest(m)),
                        NIZK_AppendHandle(NIZK_RelRequestWait(m)),
                        NIZK_UpdateInput(Set())
                    )
                }

                val hash_transition = {
                    post_state : hash.state,
                    effects : Set( 
                        HASH_UpdateInput(Set(HashRequest({cfg: m.cfg, x: m.w})))
                    )
                }

                Set((nizk_transition, hash_transition))                          
           // }else{
           //     Set() // TODO:? 
           // }
        }).flatten()
    }

    pure def nizk_rel_2(nizk: NIZK_System, hash: HASH_System) : Set[(NIZK_Transition, HASH_Transition)] = {
        hash.input.filterMap(z => match z { | HashResponse(p) => Some(p) | _ => None }).map(r => {
            nizk.handles.filterMap(h => match h {
                | NIZK_RelRequestWait(x) => Some(x)
                | _ => None
            }).filter(x => r.cfg == x.cfg).map(p => {
                val sid = p.cfg.sid
                val party = p.cfg.party
                val sid_state = nizk.state.get(sid)

                val in_relation = nizk_Rel_sat(p.x, p.w) and r.h == p.x.h

                val nizk_transition = {
                    post_state : nizk.state,
                    effects : Set( 
                        //NIZK_LogMessage(NIZK_RelRequestWait(p)),
                        NIZK_UpdateInput(Set(NIZK_RelResponse({cfg: p.cfg, x: p.x, w: p.w, b: in_relation}))),
                        NIZK_RemoveHandle(NIZK_RelRequestWait(p))
                    )
                }

                val hash_transition = {
                    post_state : hash.state,
                    effects : Set(
                        HASH_UpdateInput(Set())
                    ) 
                }

                (nizk_transition, hash_transition)                          
            })
        }).flatten()
    }

    pure def nizk_prove_0(nizk: NIZK_System) : Set[NIZK_Transition] = {
        nizk.input.filterMap(m => match m {
            | NIZK_ProveRequest(m) => Some(m)
            | _ => None
        }).map(m => {
            if(nizk_access_check(m.cfg, nizk.params)) {
                val sid = m.cfg.sid
                val party = m.cfg.party
                val sid_state = nizk.state.get(sid)

                {
                    post_state : nizk.state,
                    effects : Set( 
                        NIZK_LogMessage(NIZK_ProveRequest(m)),
                        NIZK_UpdateInput(Set(NIZK_RelRequest({cfg: m.cfg, x: m.x, w: m.w}))),
                        NIZK_AppendHandle(NIZK_ProveRelWait({cfg: m.cfg, x: m.x, w: m.w}))
                    )
                }            
                
            }else{
                {
                    post_state : nizk.state,
                    effects : Set( 
                        NIZK_UpdateInput(Set()),
                    )
                }
            }
        })
    }

    pure def nizk_prove_1(nizk: NIZK_System) : Set[NIZK_Transition] = {
        nizk.handles.filterMap(h => match h {
            | NIZK_ProveRelWait(m) => Some(m)
            | _ => None
        }).map(m => {
            nizk.input.filterMap(i => match i {
                | NIZK_RelResponse(r) => Some(r)
                | _ => None
            }).filter(x => x.cfg.nonce == m.cfg.nonce).map(r => {
             
                val sid = m.cfg.sid
                val party = m.cfg.party
                val sid_state = nizk.state.get(sid)
            
                if(not (r.b)) { 
                    {
                        post_state : nizk.state,
                        effects : Set( 
                            NIZK_UpdateInput(Set(NIZK_ProveResponse({cfg: m.cfg, x: m.x, w: m.w, pi: None}))),
                            NIZK_RemoveHandle(NIZK_ProveRelWait(m)),
                        )
                    }
                }else{
                    {
                        post_state: nizk.state,
                        effects: Set(
                            NIZK_UpdateInput(Set(NIZK_SIMControl)),
                            NIZK_AppendHandle(NIZK_SIMProveHold({cfg:m.cfg, x:m.x, w:m.w})),
                            NIZK_RemoveHandle(NIZK_ProveRelWait(m)),
                        )
                    }
                }
            })
        }).flatten()
    }

    pure def nizk_prove_2(nizk: NIZK_System, sim_pi: NIZK_Proof) : Set[NIZK_Transition] = {
        nizk.input.filterMap(m => match m {
            | NIZK_SIMControl => Some(m)
            | _ => None
        }).map(p => {
            nizk.handles.filterMap(h => match h {
                | NIZK_SIMProveHold(q) => Some(q)
                | _ => None
            }).map(q => {
                {
                    post_state : nizk.state,
                    effects : Set( 
                        NIZK_UpdateInput(Set(NIZK_SIMProveRelease({cfg: q.cfg, x: q.x, w: q.w, pi: sim_pi}))),
                        NIZK_RemoveHandle(NIZK_SIMProveHold(q))
                    )
                }
            })
        }).flatten()
    }

    action nizk_prove_2_act : bool = all {
        f_nizk_s.input.filterMap(m => match m { | NIZK_SIMControl => Some(m) | _ => None }).size() > 0,
        f_nizk_s.handles.filterMap(h => match h { | NIZK_SIMProveHold(q) => Some(q) | _ => None }).size() > 0,
        nondet sim_pi = ALL_PROOFS.oneOf()
        nondet t = nizk_prove_2(f_nizk_s, sim_pi).oneOf()
        f_nizk_s' =  nizk_process_transitions(f_nizk_s, t)
    } 

    pure def nizk_prove_3(nizk: NIZK_System) : Set[NIZK_Transition] = {
        nizk.input.filterMap(m => match m {
            | NIZK_SIMProveRelease(p) => Some(p)
            | _ => None
        }).map(p => {
            val party = p.cfg.party
            val sid = p.cfg.sid
            val sid_state = nizk.state.get(sid)

            def san_predicate = (pi) => {
                or{
                    sid_state.Ver.exists(e => e.x == p.x and e.pi == pi and e.b == false)
                }
            }

            val maybe_new_pi = if(san_predicate(p.pi)){
                ALL_PROOFS.find(x => not (san_predicate(x)))
            }else{
                Some(p.pi)
            }

            if(maybe_new_pi == None){ // we exhausted all possible proofs
                {
                    post_state: nizk.state,
                    effects: Set(
                        NIZK_UpdateInput(Set())
                    )
                }
            }else{
                val new_pi = maybe_new_pi.unwrap()
                {
                    post_state: nizk.state.set(sid, {...sid_state,
                        Ver: sid_state.Ver.setAdd({x: p.x, pi: new_pi, b: true})
                    }),
                    effects: Set(
                        NIZK_UpdateInput(Set(NIZK_ProveResponse({cfg: p.cfg, x: p.x, w: p.w, pi: Some(new_pi)})))
                    )
                }
            }
        })
    }

    pure def nizk_verify_1(nizk: NIZK_System) : Set[NIZK_Transition] = {
        nizk.input.filterMap(m => match m {
            | NIZK_VerifyRequest(m) => Some(m)
            | _ => None
        }).map(m => {
            if(nizk_access_check(m.cfg, nizk.params)) {
                val sid = m.cfg.sid
                val party = m.cfg.party
                val sid_state = nizk.state.get(sid)
                {
                    post_state: nizk.state,
                    effects: Set(
                        NIZK_LogMessage(NIZK_VerifyRequest(m)),
                        NIZK_UpdateInput(Set(NIZK_SIMControl)),
                        NIZK_AppendHandle(NIZK_SIMVerifyHold(m))
                    )
                }
            }else{
                {
                    post_state : nizk.state,
                    effects : Set( 
                        NIZK_UpdateInput(Set()),
                    )
                }
            }
        })
    }    

    pure def nizk_verify_2(nizk: NIZK_System, sim_w: NIZK_Witness) : Set[NIZK_Transition] = {
        nizk.input.filterMap(m => match m {
            | NIZK_SIMControl => Some(m)
            | _ => None
        }).map(p => {
            nizk.handles.filterMap(h => match h {
                | NIZK_SIMVerifyHold(q) => Some(q)
                | _ => None
            }).map(q => {
                {
                    post_state : nizk.state,
                    effects : Set( 
                        NIZK_UpdateInput(Set(NIZK_SIMVerifyRelease({cfg: q.cfg, x: q.x, pi: q.pi, w: sim_w}))),
                        NIZK_RemoveHandle(NIZK_SIMVerifyHold(q))
                    )
                }
            })
        }).flatten()
    }    

    action nizk_verify_2_act : bool = all {
        f_nizk_s.input.filterMap(m => match m { | NIZK_SIMControl => Some(m) | _ => None }).size() > 0,
        f_nizk_s.handles.filterMap(h => match h { | NIZK_SIMVerifyHold(q) => Some(q) | _ => None }).size() > 0,
        val all_witnesses = 1.to((UNIVERSE_SIZE + 1) / 2).map(x => List(Normal({b: true, tx: x})))
        nondet sim_w = all_witnesses.union(Set(List())).oneOf()
        nondet t = nizk_verify_2(f_nizk_s, sim_w).oneOf()
        f_nizk_s' =  nizk_process_transitions(f_nizk_s, t)
    }      

    pure def nizk_verify_3(nizk: NIZK_System) : Set[NIZK_Transition] = {
        nizk.input.filterMap(m => match m {
            | NIZK_SIMVerifyRelease(p) => Some(p)
            | _ => None
        }).map(p => {
            val sid = p.cfg.sid
            val party = p.cfg.party
            val sid_state = nizk.state.get(sid)

            if(sid_state.Ver.exists(e => e.x == p.x and e.pi == p.pi)){
                val old_b = sid_state.Ver.find(e => e.x == p.x and e.pi == p.pi).unwrap().b
                {
                    post_state: nizk.state,
                    effects: Set( 
                        NIZK_UpdateInput(Set(NIZK_VerifyResponse({cfg:p.cfg, x:p.x, pi: p.pi, b: old_b})))
                    )
                }
            }else{
                // this is the moment
                {
                    post_state: nizk.state,
                    effects: Set(
                        NIZK_UpdateInput(Set(NIZK_RelRequest({cfg:p.cfg, x:p.x, w: p.w}))),
                        NIZK_AppendHandle(NIZK_VerifyRelWait({cfg:p.cfg, x:p.x, w: p.w, pi: p.pi}))
                    )
                }
            }
        })
    }    

    pure def nizk_verify_4(nizk: NIZK_System) : Set[NIZK_Transition] = {
        nizk.input.filterMap(m => match m {
            | NIZK_RelResponse(r) => Some(r)
            | _ => None
        }).map(r => {
            nizk.handles.filterMap(h => match h {
                | NIZK_VerifyRelWait(x) => Some(x)
                | _ => None
            }).filter(x => r.cfg == x.cfg).map(p => {
                val sid = p.cfg.sid
                val party = p.cfg.party
                val sid_state = nizk.state.get(sid)
                val new_b = r.b

                    {
                        post_state: nizk.state.set(sid, {...sid_state,
                            Ver: sid_state.Ver.setAdd({x: p.x, pi: p.pi, b: new_b})
                        }),
                        effects: Set(
                            NIZK_UpdateInput(Set(NIZK_VerifyResponse({cfg:p.cfg, x:p.x, pi: p.pi, b: new_b}))),
                            NIZK_RemoveHandle(NIZK_VerifyRelWait(p))
                        )
                    }

            })
        }).flatten()
    }


    def nizk_process_transitions(sys: NIZK_System, transition: NIZK_Transition) : NIZK_System = {
        val new_sys = {...sys, state: transition.post_state}
        transition.effects.fold(new_sys, ((a1,a2) => nizk_apply_effect(a1,a2)))
    } 

   pure def nizk_main_listener(nizk: NIZK_System, hash: HASH_System) : (Set[(NIZK_Transition, HASH_Transition)]) = {
        Set(
           nizk_prove_0(nizk),
           nizk_prove_1(nizk),
           nizk_prove_3(nizk),

           nizk_verify_1(nizk),
           nizk_verify_3(nizk),
           nizk_verify_4(nizk),
           if(nizk.params.responsive_simulator) Set() else nizk_sim_loses_ctrl(nizk)

            
        ).flatten().map(x => (x, hash_id_trans(hash))).union(Set(
            nizk_rel_1(nizk, hash),
            nizk_rel_2(nizk, hash),
        ).flatten())
    }            

    action nizk_step_with(q: (NIZK_System, HASH_System) => Set[(NIZK_Transition, HASH_Transition)]): bool = {
        all{
            val ts = q(f_nizk_s, f_hash_s)
            all {
                nondet transition = ts.oneOf()
                all {
                    f_nizk_s' = nizk_process_transitions(f_nizk_s, transition._1),
                    f_hash_s' = hash_process_transitions(f_hash_s, transition._2)
                }
            }
        }
    }  

    action nizk_step : bool =  any {
        nizk_step_with(nizk_main_listener),
        all {
            nizk_verify_2_act,
            f_hash_s' = f_hash_s
        },
        all {
            nizk_prove_2_act,
            f_hash_s' = f_hash_s
        }
    }  

   action nizk_init(x : {P: Set[Party], C: Set[Party],SIDs: Set[SID], enable_logging: bool, responsive_simulator: bool}) : bool =  all {
        f_nizk_s' = {
            params : {
                P: x.P,
                C: x.C,
                SIDs: x.SIDs,

                enable_logging: x.enable_logging,
                responsive_simulator: x.responsive_simulator
            },
            state : x.SIDs.mapBy(sid => {
                Ver: Set(),
            }),
            input : Set(),
            nonce : 0,
            log : Set(),
            handles : Set(),
        }
    }
}