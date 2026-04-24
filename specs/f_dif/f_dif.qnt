module f_dif{
    
    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"
    
    import common.* from "../common"
    import f_dif_types.* from "./f_dif_types"
    import g_clock.* from "../g_clock/g_clock"
    import g_clock_types.* from "../g_clock/g_clock_types"

    var f_dif_s : DIF_System

    def dif_diffuse_1(sys: DIF_System) : Set[DIF_Transition] = { 
        sys.input.filterMap(m => match m {
            | DIF_DiffuseRequest(msg) => Some(msg)
            | _ => None
        }).map(msg => 
            if (dif_access_check(msg.cfg, sys.params)) {

                val S = msg.cfg.party
                val m = msg.m
                val sid = msg.cfg.sid
                val t = g_clock_s.state.TIME
                val sid_state = sys.state.get(sid)

                if(not ((S, t).in(sid_state.D_flag))){

                    val new_D_flag = sid_state.D_flag.union(Set((S, t)))
                    val new_ctr = sid_state.ctr + 1

                    // computing the new_L
                    // TODO: abstract this into a function for joining maps
                    val new_L_vals = sys.params.P.map(party =>{
                        ({to: party, ctr: new_ctr}, {data:m, time: t})
                    })
                    val old_L_vals = sid_state.L.keys().map(k => (k, sid_state.L.get(k)))
                    val new_L = new_L_vals.union(old_L_vals).setToMap()
                    

                    // return the transition
                    {
                        post_state : sys.state.set(sid, {...sid_state,
                            L: new_L,
                            ctr: new_ctr,
                            D_flag: new_D_flag,
                        }),
                        effects : Set(
                            DIF_LogMessage(DIF_DiffuseRequest(msg)),
                            DIF_UpdateInput(Set(DIF_SIMControl)),
                            DIF_AppendHandle(DIF_SIMDiffuseHold({ 
                                cfg: msg.cfg,
                                ctr: new_ctr,
                                t: t,
                                m: m
                            })),
                            ),
                    }    

                }else{
                    // already diffused this time
                    {
                        post_state: sys.state,
                        effects: Set(
                            DIF_UpdateInput(Set())
                        ),
                    }

                }              
            }else{
               {
                    post_state: sys.state,
                    effects: Set(
                        DIF_UpdateInput(Set())
                    ),
                }
            }
        )
    }    

    pure def dif_diffuse_2(sys: DIF_System, clock: GC_System) : Set[(DIF_Transition, GC_Transition)] = {
        sys.input.filterMap(m => match m {
            | DIF_SIMControl => Some(m)
            | _ => None
        }).map(c => { // SIM must have a control 
            sys.handles.filterMap(h => match h {
                | DIF_SIMDiffuseHold(msg) => Some(msg)
                | _ => None
            }).map(msg => {
                {
                    post_state : sys.state,
                    effects : Set(
                        DIF_UpdateInput(Set(DIF_SIMDiffuseRelease(msg))),
                        DIF_RemoveHandle(DIF_SIMDiffuseHold(msg)),
                    )
                }
            })
        }).flatten().map(x => (x, gc_id_trans(clock)))
    }

    action dif_diffuse_2_act : bool = all {
        // f_dif_s.input.dif_get_sim_control_messages().size() > 0,
        // f_dif_s.handles.dif_get_sim_diffuse_hold_messages().size() > 0,
        nondet t = dif_diffuse_2(f_dif_s, g_clock_s).oneOf()
        dif_gc_step_with((dif, clock) => dif_diffuse_2(dif, clock))
    }


    pure def dif_diffuse_3_prim(dif: DIF_System, clock: GC_System) : Set[(DIF_Transition, GC_Transition)] = {
        dif.input.filterMap(m => match m {
            | DIF_SIMDiffuseRelease(msg) => Some(msg)
            | _ => None
        }).map(msg => {
            {
            post_state: dif.state,
            effects: Set(
                DIF_UpdateInput(Set(DIF_DiffuseResponse(msg))),
            )
            }
        }).map(x => (x, gc_id_trans(clock)))
    }


    // pure def dif_diffuse_3(dif: DIF_System, clock: GC_System) : Set[(DIF_Transition, GC_Transition)] = {
    //     dif.input.filterMap(m => match m {
    //         | DIF_SIMDiffuseRelease(msg) => Some(msg)
    //         | _ => None
    //     }).map(msg => {
    //         val dif_transition = {
    //             post_state: dif.state,
    //             effects: Set(
    //                 DIF_UpdateInput(Set()),
    //                 DIF_AppendHandle(DIF_ClockRequestWait(msg))
    //             )
    //         }

    //         val clock_transition = {
    //             post_state: clock.state,
    //             effects: Set(
    //                 GC_UpdateInput(Set(OkayRequest({cfg: msg.cfg})))
    //             )
    //         }

    //         (dif_transition, clock_transition)
    //     })
    // }
 
    // pure def dif_diffuse_4(dif: DIF_System, clock: GC_System) : Set[(DIF_Transition, GC_Transition)] = {
    //     clock.input.filterMap(z => match z {
    //         | OkayResponse(q) => Some(q)
    //         | _ => None
    //     }).map(msg1 => 
    //         dif.handles.filterMap(h => match h {
    //             | DIF_ClockRequestWait(msg2) => Some(msg2)
    //             | _ => None
    //         }).map(msg2 => {
    //             if (msg1.cfg.nonce == msg2.cfg.nonce){
    //                 val dif_transition = {
    //                     post_state: dif.state,
    //                     effects: Set(
    //                         DIF_UpdateInput(Set(DIF_DiffuseResponse(msg2))),
    //                         DIF_RemoveHandle(DIF_ClockRequestWait(msg2))
    //                     )
    //                 }

    //                 val clock_transition = {
    //                     post_state: clock.state,
    //                     effects: Set(
    //                         GC_UpdateInput(Set())
    //                     )
    //                 }

    //                 Set((dif_transition, clock_transition))
    //             }else{
    //                 Set()
    //             }
    //         })
    //     ).flatten().flatten()
    // }

    def dif_inject(dif:DIF_System) : Set[DIF_Transition] = {
        dif.input.filterMap(m => match m {
            | DIF_InjectRequest(p) => Some(p)
            | _ => None
        }).map(p => {
            val P = p.cfg.party
            val sid = p.cfg.sid
            val Q = p.Q
            if(P.in(dif.params.C) and sid.in(dif.params.SIDs) and Q.in(dif.params.P)){
                val sid_state = dif.state.get(sid)
                val t = g_clock_s.state.TIME
                {
                    post_state: dif.state.set(sid, {...sid_state,
                        ctr: sid_state.ctr + 1,
                        L: sid_state.L.put({to:Q, ctr: sid_state.ctr + 1}, {data: p.m, time: t})
                    }),
                    effects: Set(
                        DIF_UpdateInput(Set(DIF_InjectResponse(p))),
                        DIF_LogMessage(DIF_InjectRequest(p))
                    )
                }
            }else{
               {
                    post_state: dif.state,
                    effects: Set(
                        DIF_UpdateInput(Set())
                    )
                }
            }
        })
    }    

    def dif_fetch_1(dif: DIF_System) : Set[DIF_Transition] = {
        dif.input.filterMap(m => match m {
            | DIF_FetchRequest(msg) => Some(msg)
            | _ => None
        }).map(msg => {
            if (dif_access_check(msg.cfg, dif.params)){

                val tp = g_clock_s.state.TIME // to make traces shorter
                val sid = msg.cfg.sid
                val P = msg.cfg.party
                val delta = dif.params.delta
                val sid_state = dif.state.get(sid)
                val L = sid_state.L        

                if((P, tp).in(sid_state.F_flag)){
                    {
                        post_state : dif.state,
                        effects : Set(
                            DIF_UpdateInput(Set())
                        )
                    }
                }else{
                
                    val i_late = L.keys().filterMap(k => {
                        val v = L.get(k)
                        if(k.to == P and (tp - v.time) >= delta){
                            Some(k.ctr)
                        }else{
                            None
                        }
                    })

                    {
                        post_state : dif.state.set(sid, {...sid_state,
                            F_flag: sid_state.F_flag.union(Set((P, tp))) // adding to F_flag
                        }),
                        effects : Set(
                            DIF_LogMessage(DIF_FetchRequest(msg)),
                            DIF_UpdateInput(Set(DIF_SIMControl)),
                            DIF_AppendHandle(DIF_SIMFetchHold({
                                cfg: msg.cfg,
                                t_prim: tp,
                                i_late: i_late,
                            }))
                        ),
                    }
                }
            }else{
                {
                    post_state : dif.state,
                    effects : Set(
                        DIF_UpdateInput(Set()),
                    )
                }
            }
        })
    }

    pure def dif_fetch_2(dif: DIF_System, sim_i_early: Set[DIF_Index]) : Set[DIF_Transition] = {
        dif.input.filterMap(m => match m {
            | DIF_SIMControl => Some(m)
            | _ => None
        }).map(c => { // sim must have control
            dif.handles.filterMap(h => match h {
                | DIF_SIMFetchHold(msg) => Some(msg)
                | _ => None
            }).map(msg => {
                val sid = msg.cfg.sid
                val L = dif.state.get(sid).L
                //dif.state.get(sid).L.keys().powerset().map(i_early => 
                //Set(Set()).map(i_early =>
                    {   
                        post_state : dif.state,
                        effects : Set( 
                            DIF_RemoveHandle(DIF_SIMFetchHold(msg)),
                            DIF_UpdateInput(Set(DIF_SIMFetchRelease({
                                cfg: msg.cfg,
                                t_prim: msg.t_prim,
                                i_late: msg.i_late,
                                i_early: sim_i_early
                            }))),
                            )  
                    }
            })
        }).flatten()
    }        

    action dif_fetch_2_act : bool = all {
        // f_dif_s.input.dif_get_sim_control_messages().size() > 0,
        // f_dif_s.handles.dif_get_sim_fetch_hold_messages().size() > 0,
        val all_ctrs = f_dif_s.params.SIDs.map(sid => f_dif_s.state.get(sid).L.keys().map(k => k.ctr)).flatten()
        nondet sim_i_early = all_ctrs.powerset().oneOf()
        nondet t = dif_fetch_2(f_dif_s, sim_i_early).oneOf()
        dif_gc_step_with((dif, clock) => dif_fetch_2(dif, sim_i_early).map(x => (x, gc_id_trans(clock))))
    }     

    pure def dif_fetch_3(dif : DIF_System) : Set[DIF_Transition] = {
        dif.input.filterMap(m => match m {
            | DIF_SIMFetchRelease(msg) => Some(msg)
            | _ => None
        }).map(msg => 
            val sid = msg.cfg.sid
            val P = msg.cfg.party
            val t_prim = msg.t_prim
            val i_late = msg.i_late
            val sid_state = dif.state.get(sid)
            val L = sid_state.L

            val i_early = msg.i_early.filterMap({i =>
                if(L.has({to: P, ctr: i})){
                    Some(i)
                }else{
                    None
                }
            })

            val msgs = i_early.union(i_late).filter(k => L.has({to: P, ctr: k})).map(k => L.get({to: P, ctr: k}).data) // reported broken indexing
            val new_L = L.keys().exclude(i_early.union(i_late).map(k => {to: P, ctr: k})).mapBy(k => L.get(k))

            {
                post_state: dif.state.set(sid, {...sid_state, 
                                L: new_L
                            }),
                effects: Set(
                    DIF_UpdateInput(Set(DIF_FetchResponse({cfg:msg.cfg, t_prim: msg.t_prim, data: msgs})))
                )
            }
        )
    }

   pure def dif_ok_1(dif : DIF_System, gc: GC_System) : Set[(DIF_Transition, GC_Transition)] = {
        dif.input.filterMap(m => match m {
            | DIF_OKRequest(msg) => Some(msg)
            | _ => None
        }).map(msg =>
        {
            val sid = msg.cfg.sid
            val P   = msg.cfg.party
            val nonce = msg.cfg.nonce

            val dif_transition = {
                post_state: dif.state,
                effects: Set(
                    DIF_UpdateInput(Set()),
                    DIF_LogMessage(DIF_OKRequest(msg)),
                    DIF_AppendHandle(DIF_OKRequestWait(msg))
                )
            }

            val gc_transition = {
                post_state: gc.state,
                effects: Set(
                    GC_UpdateInput(Set(OkayRequest({cfg: msg.cfg})))
                )
            }

            (dif_transition, gc_transition)
        })
    }     


    pure def dif_ok_2(dif : DIF_System, gc: GC_System) : Set[(DIF_Transition, GC_Transition)] = {
        gc.input.filterMap(z => match z {
            | OkayResponse(q) => Some(q)
            | _ => None
        }).map(msg2 => {
            dif.handles.filterMap(h => match h {
                | DIF_OKRequestWait(msg) => Some(msg)
                | _ => None
            }).map(msg => {
                if(msg2.cfg.nonce == msg.cfg.nonce){
                    val dif_transition = {
                        post_state: dif.state,
                        effects: Set(
                            DIF_UpdateInput(Set(DIF_OKResponse(msg))),
                            DIF_RemoveHandle(DIF_OKRequestWait(msg))
                        )
                    }

                    val gc_transition = {
                        post_state: gc.state,
                        effects: Set(
                            GC_UpdateInput(Set())
                        )
                    }                    
                    Set((dif_transition, gc_transition))
                }else{
                    Set()
                }
            }).flatten()
        }).flatten()
    }       

    // sim might want to give control back to the environment
    pure def dif_sim_loses_ctrl(dif: DIF_System) : Set[DIF_Transition] = {
        dif.input.filterMap(m => match m {
            | DIF_SIMControl => Some(m)
            | _ => None
        }).map(c =>  // sim must have control
            {   
                post_state : dif.state,
                effects : Set( 
                    DIF_UpdateInput(Set()),
                )  
            })
    }

    def dif_main_listener(dif: DIF_System, clock: GC_System) : (Set[(DIF_Transition, GC_Transition)]) = {
        val id : GC_Transition = {post_state: clock.state, effects: Set()}

        Set(
            dif_inject(dif),
            dif_diffuse_1(dif),
            dif_fetch_1(dif),
            dif_fetch_3(dif),
            if (dif.params.responsive_simulator) Set() else dif_sim_loses_ctrl(dif)
        ).flatten().map(x => (x,id)).union(
            Set(
                //dif_diffuse_3(dif, clock),
                //dif_diffuse_4(dif, clock),
                dif_diffuse_3_prim(dif, clock),

                dif_ok_1(dif, clock),
                dif_ok_2(dif, clock),
            ).flatten()
        )
    }

    pure def dif_process_transitions(sys: DIF_System, clock: GC_System, transition: DIF_Transition) : DIF_System = {
        val new_sys = {...sys, state: transition.post_state}
        transition.effects.fold(new_sys, ((a1,a2) => dif_apply_effect(a1, clock, a2)))
    } 

    action dif_gc_step_with(q: (DIF_System, GC_System) => Set[(DIF_Transition, GC_Transition)]): bool = {
        all{
            val ts = q(f_dif_s, g_clock_s)
            all {
                nondet transition = ts.oneOf()
                all {
                    f_dif_s' = dif_process_transitions(f_dif_s, g_clock_s, transition._1),
                    g_clock_s' = gc_process_transitions(g_clock_s, transition._2)
                }
            }
        }
    }    

    action dif_step : bool =  any {
        dif_gc_step_with(dif_main_listener),
        dif_fetch_2_act,
        dif_diffuse_2_act
    }

    pure def dif_initial(x : {P: Set[Party], C: Set[Party], SIDs: Set[SID], delta: Time, enable_logging: bool, responsive_simulator: bool}) : DIF_System = {
        {
            params : {
                P : x.P,
                C : x.C,
                delta: x.delta,
                SIDs: x.SIDs,
                enable_logging: x.enable_logging,
                responsive_simulator: x.responsive_simulator,
            },
            state : x.SIDs.mapBy(sid => {
                {
                    L : Map(),
                    ctr : 0,
                    F_flag : Set(),
                    D_flag : Set()
                }
            }),
            input : Set(),
            nonce : 0,        // to be able to match requests and responses in the trace
            log : Set(),
            handles : Set()
        }        
    }

    action dif_init(x : {P: Set[Party], C: Set[Party], SIDs: Set[SID], delta: Time, enable_logging: bool, responsive_simulator: bool}) : bool =  all {
        x.C.subseteq(x.P),
        x.delta >= 0,
        f_dif_s' = dif_initial({P: x.P, C: x.C, SIDs: x.SIDs, delta: x.delta, enable_logging: x.enable_logging, responsive_simulator: x.responsive_simulator})
    }      
    
}