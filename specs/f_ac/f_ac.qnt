module f_ac{

    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"
    
    import common.* from "../common"
    import f_ac_types.* from "./f_ac_types"
    import g_clock.* from "../g_clock/g_clock"
    import g_clock_types.* from "../g_clock/g_clock_types"

 
    // mutable variable represents the system state
    var f_ac_s : FAC_System


    pure def fac_send_1(ac: FAC_System, clock: GC_System) :  Set[(FAC_Transition, GC_Transition)] = { 
        ac.input.fac_get_send_request_messages().map(msg => 
            val Q = msg.Q
            val x = msg.x
            val sid = msg.cfg.sid
            val P = msg.cfg.party
            val t = clock.state.TIME
            val sid_state = ac.state.get(sid)

            if (and {
                fac_access_check(msg.cfg, ac.params),

                Q.in(ac.params.P),
                not ((P, t).in(sid_state.S_flag)),

            }) {
                val new_ctr = sid_state.ctr + 1

                val new_L = sid_state.L.put({to: Q, ctr: new_ctr}, {from: P, time: t, data: x})
                val new_S_flag = sid_state.S_flag.setAdd((P, t))

                {
                    post_state : ac.state.set(sid, {
                        L: new_L, 
                        ctr: new_ctr,
                        S_flag: new_S_flag,
                        F_flag: sid_state.F_flag
                    }),
                    effects : Set(
                        FAC_LogMessage(SendRequest(msg)),
                        FAC_UpdateInput(Set(SIMControl)),
                        FAC_AppendHandle(SIMSendHold({
                            cfg: msg.cfg,
                            ctr: new_ctr,
                            Q: Q,
                            t: t,
                            x: x
                        })),
                    )
                }                    
                
            }else{
                {
                    post_state: ac.state,
                    effects: Set(
                        FAC_UpdateInput(Set())
                    ),
                }
            }
        ).map(x => (x, gc_id_trans(clock)))
    }

    pure def fac_send_2(ac: FAC_System, clock: GC_System) : Set[(FAC_Transition, GC_Transition)] = {
        ac.input.fac_get_sim_control_messages().map(c => { // SIM must have a control
            ac.handles.fac_get_sim_send_hold_messages().map(msg => {
                {
                    post_state : ac.state,
                    effects : Set(
                        FAC_UpdateInput(Set(SIMSendRelease(msg))),
                        FAC_RemoveHandle(SIMSendHold(msg)),
                    )
                }
            })
        }).flatten().map(x => (x, gc_id_trans(clock)))
    }    

    action fac_send_2_act : bool = all {
        fac_gc_step_with((fac, gc) => fac_send_2(fac, gc))
    }     


    pure def fac_send_3(ac: FAC_System, clock: GC_System) : Set[(FAC_Transition, GC_Transition)] = {
        ac.input.fac_get_sim_send_release_messages().map(msg => {
            val sid = msg.cfg.sid
            val sid_state = ac.state.get(sid)
                {
                    post_state : ac.state,
                    effects : Set(
                        FAC_UpdateInput(Set(ClockRequest(msg))),
                    )
                }                                  
        }).map(x => (x, gc_id_trans(clock)))
    }
    

    def fac_send_4(ac: FAC_System, clock: GC_System) : Set[(FAC_Transition, GC_Transition)] = {
        ac.input.fac_get_clock_request_messages().map(msg => {

            val fac_transition = {
                post_state: ac.state,
                effects: Set(
                    FAC_UpdateInput(Set()),
                    FAC_AppendHandle(ClockRequestWait(msg))
                )
            }

            val clock_transition = {
                post_state: clock.state,
                effects: Set(
                    GC_UpdateInput(Set(OkayRequest({cfg: msg.cfg})))
                )
            }

            (fac_transition, clock_transition)
        })
    }

    def fac_send_5(ac: FAC_System, clock: GC_System) : Set[(FAC_Transition, GC_Transition)] = {
        clock.input.get_okay_response_messages().map(msg2 => {
            ac.handles.fac_get_clock_request_wait_messages().map(msg => {
                val sid = msg.cfg.sid
                val P   = msg.cfg.party
            
                if(msg2.cfg.nonce == msg.cfg.nonce){
                    val fac_transition = {
                        post_state: ac.state,
                        effects: Set(
                            FAC_UpdateInput(Set(ClockResponse(msg))),
                            FAC_RemoveHandle(ClockRequestWait(msg))
                        )
                    }

                    val clock_transition = {
                        post_state: clock.state,
                        effects: Set(
                            GC_UpdateInput(Set())
                        )
                    }                    
                    Set((fac_transition, clock_transition))
                }else{
                    Set()
                }
            }).flatten()
        }).flatten()
    }        

    def fac_send_6(ac : FAC_System, clock: GC_System) : Set[(FAC_Transition, GC_Transition)] = {
        ac.input.fac_get_clock_response_messages().map(msg => {
            {
                post_state: ac.state,
                effects: Set(
                    FAC_UpdateInput(Set(SendResponse(msg))),
                )
            }

        }).map(x => (x, gc_id_trans(clock)))
    }



    def fac_fetch_1(ac: FAC_System, clock: GC_System) : Set[(FAC_Transition, GC_Transition)] = {
        ac.input.fac_get_fetch_request_messages().map(msg => {
            val tp = clock.state.TIME // to make traces shorter
            val sid = msg.cfg.sid
            val P = msg.cfg.party
            val delta = ac.params.delta
            val sid_state = ac.state.get(sid)
            val L = sid_state.L
            
            if (and {
                fac_access_check(msg.cfg, ac.params),
                not ((P, tp).in(sid_state.F_flag)),
            }){
                val i_late = L.keys().filterMap(k => {
                    val v = L.get(k)
                    if(k.to == P and (tp - v.time) >= delta){
                        Some(k.ctr)
                    }else{
                        None
                    }
                })

               {
                    post_state : ac.state.set(sid, {...sid_state,
                        F_flag: sid_state.F_flag.setAdd((P, tp)) // adding to F_flag
                    }),
                    effects : Set(
                        FAC_LogMessage(FetchRequest(msg)),
                        FAC_UpdateInput(Set(SIMControl)),
                        FAC_AppendHandle(SIMFetchHold({
                            cfg: msg.cfg,
                            t_prim: tp,
                            i_late: i_late,
                        }))
                    ),
                }
            }else{
                {
                    post_state : ac.state,
                    effects : Set(
                        FAC_UpdateInput(Set()),
                    )
                }
            }
        }).map(x => (x, gc_id_trans(clock)))
    }

    pure def fac_fetch_2(ac: FAC_System, clock: GC_System, sim_i_early: Set[Time]) : Set[(FAC_Transition, GC_Transition)] = {
        ac.input.fac_get_sim_control_messages().map(c => { // sim must have control
            ac.handles.fac_get_sim_fetch_hold_messages().map(msg => {
                val sid = msg.cfg.sid
                val L = ac.state.get(sid).L
                    {   
                        post_state : ac.state,
                        effects : Set( 
                            FAC_RemoveHandle(SIMFetchHold(msg)),
                            FAC_UpdateInput(Set(SIMFetchRelease({
                                cfg: msg.cfg,
                                t_prim: msg.t_prim,
                                i_late: msg.i_late,
                                i_early: sim_i_early
                            }))),
                            )  
                    }
            })
        }).flatten().map(x => (x, gc_id_trans(clock)))
    }    


    action fac_fetch_2_act : bool = all {
        f_ac_s.input.fac_get_sim_control_messages().size() > 0,
        f_ac_s.handles.fac_get_sim_fetch_hold_messages().size() > 0,
        nondet sim_i_early = 1.to(10).powerset().oneOf()
        nondet t = fac_fetch_2(f_ac_s, g_clock_s, sim_i_early).oneOf()
        fac_gc_step_with((fac, gc) => fac_fetch_2(fac, gc, sim_i_early))
    }     


    def fac_fetch_3(ac : FAC_System, clock: GC_System) : Set[(FAC_Transition, GC_Transition)] = {
        ac.input.fac_get_sim_fetch_release_messages().map(msg => 
            val sid = msg.cfg.sid
            val P = msg.cfg.party
            val t_prim = msg.t_prim
            val i_late = msg.i_late
            val state = ac.state.get(sid)
            val L = state.L
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
                post_state: ac.state.set(sid, {...state, 
                                L: new_L
                            }),
                effects: Set(
                    FAC_UpdateInput(Set(FetchResponse({cfg:msg.cfg, t_prim: msg.t_prim, data: msgs})))
                )
            }
        ).map(x => (x, gc_id_trans(clock)))
    }

    def fac_ok_1(fac : FAC_System, gc: GC_System) : Set[(FAC_Transition, GC_Transition)] = {
        fac.input.fac_get_ok_request_messages().map(msg => 
        {
            val sid = msg.cfg.sid
            val P   = msg.cfg.party
            val nonce = msg.cfg.nonce

            val fac_transition = {
                post_state: fac.state,
                effects: Set(
                    FAC_UpdateInput(Set()),
                    FAC_AppendHandle(OKRequestWait(msg))
                )
            }

            val gc_transition = {
                post_state: gc.state,
                effects: Set(
                    GC_UpdateInput(Set(OkayRequest({cfg: msg.cfg})))
                )
            }

            (fac_transition, gc_transition)
        })
    }
    
    def fac_ok_2(fac : FAC_System, gc: GC_System) : Set[(FAC_Transition, GC_Transition)] = {
        gc.input.get_okay_response_messages().map(msg2 => {
            fac.handles.fac_get_ok_request_wait_messages().map(msg => {
                if(msg2.cfg.nonce == msg.cfg.nonce){
                    val fac_transition = {
                        post_state: fac.state,
                        effects: Set(
                            FAC_UpdateInput(Set(OKResponse(msg))),
                            FAC_RemoveHandle(OKRequestWait(msg))
                        )
                    }

                    val gc_transition = {
                        post_state: gc.state,
                        effects: Set(
                            GC_UpdateInput(Set())
                        )
                    }                    
                    Set((fac_transition, gc_transition))
                }else{
                    Set()
                }
            }).flatten()
        }).flatten()
    }


    // sim might want to give control back to the environment
    pure def f_ac_sim_loses_ctrl(ac: FAC_System, clock: GC_System) : Set[(FAC_Transition, GC_Transition)] = {
        ac.input.fac_get_sim_control_messages().map(c =>  // sim must have control
                    {   
                        post_state : ac.state,
                        effects : Set( 
                            FAC_UpdateInput(Set()),
                            )  
        }).map(x => (x, gc_id_trans(clock)))
    }    


    def fac_main_listener(sys: FAC_System, clock: GC_System) : Set[(FAC_Transition, GC_Transition)] = {
        Set(
            fac_send_1(sys, clock),
            fac_send_2(sys, clock),
            fac_send_3(sys, clock),
            fac_send_4(sys, clock),
            fac_send_5(sys, clock),
            fac_send_6(sys, clock),            
            fac_fetch_1(sys, clock),
            fac_fetch_3(sys,clock),
            fac_ok_1(sys, clock),
            fac_ok_2(sys, clock),
            if (sys.params.responsive_simulator) Set() else f_ac_sim_loses_ctrl(sys, clock)
        ).flatten()
        
    }

    def fac_process_transitions(sys: FAC_System, clock: GC_System, transition: FAC_Transition) : FAC_System = {
        val new_sys = {...sys, state: transition.post_state}
        transition.effects.fold(new_sys, ((a1,a2) => fac_apply_effect(a1, clock, a2)))
    } 

    action fac_step : bool =  any {
        fac_gc_step_with(fac_main_listener),
        fac_fetch_2_act
    }

    action fac_gc_step_with(q: (FAC_System, GC_System) => Set[(FAC_Transition, GC_Transition)]): bool = {
        all{ 
            val ts = q(f_ac_s, g_clock_s)
            all {
                nondet transition = ts.oneOf()
                all {
                    f_ac_s' = fac_process_transitions(f_ac_s, g_clock_s, transition._1),
                    g_clock_s' = gc_process_transitions(g_clock_s, transition._2)
                }
            }
        }
    }    

    action fac_wait_gc_step_with(q: (GC_System) => Set[(GC_Transition)]): bool = {
        def qq = (fac1, gc1) => q(gc1).map(t => ({post_state: fac1.state, effects: Set()}, t)) 
        fac_gc_step_with(qq)

    }
 
    action fac_step_gc_wait_with(q: (FAC_System) => Set[(FAC_Transition)]): bool = {
        def qq = (fac1, gc1) => q(fac1).map(t => (t, {post_state: gc1.state, effects: Set()})) 
        fac_gc_step_with(qq)
    }
    

    pure def fac_initial(P: Set[Party], C: Set[Party], delta: Time, SIDs: Set[SID], responsive_simulator: bool, enable_logging: bool) : FAC_System = {
        {
            params : {
                P : P,
                C : C,
                delta: delta,
                SIDs: SIDs,
                responsive_simulator: responsive_simulator,
                enable_logging: enable_logging,
            },
            state : SIDs.mapBy(sid => {
                {
                    L : Map(),
                    ctr : 0,
                    F_flag : Set(),
                    S_flag : Set()
                }
            }),
            input : Set(),
            nonce : 0,        // to be able to match requests and responses in the trace
            log : Set(),
            handles : Set()
        }        
    }

    action fac_init(P: Set[Party], C: Set[Party], delta: Time, SIDs: Set[SID], responsive_simulator: bool, enable_logging: bool) : bool =  all {
        C.subseteq(P),
        f_ac_s' = fac_initial(P, C, delta, SIDs, responsive_simulator, enable_logging)
    }

}