module g_clock{

    import basicSpells.* from "../../spells/basicSpells"
    import g_clock_types.* from "./g_clock_types"
    import common.* from "../common"

    // mutable variable represents the system state
    var g_clock_s : GC_System

    // identity transition
    pure def gc_id_trans(clock : GC_System) : GC_Transition = {
        post_state : clock.state,
        effects : Set(),
    }

    // skip the input transition
    pure def gc_skip_trans(clock: GC_System) : GC_Transition = {
        {
            post_state: clock.state,
            effects : Set(
                GC_UpdateInput(Set())
            )
        }
    }    

    // sim might want to give control back to the environment
    pure def gc_sim_loses_ctrl(clock: GC_System) : Set[GC_Transition] = {
        clock.input.map(m => {
            match m {
                | GC_SIMControl => Set({
                    post_state : clock.state,
                    effects : Set( 
                        GC_UpdateInput(Set()),
                    )  
                })
                | _ => Set()
            }
        }).flatten()
    }    

    pure def gc_register(clock: GC_System) :  Set[GC_Transition] = { 
        clock.input.map(m => {
            match m {
                | RegisterRequest(msg) => Set({

                    post_state : {...clock.state, 
                        Reg : clock.state.Reg.union(Set({party: msg.cfg.party, sid: msg.cfg.sid})),
                    },
                    effects : Set(
                        GC_LogMessage(RegisterRequest(msg)),
                        GC_UpdateInput(Set(RegisterResponse(msg))),
                    )
                })
                | _ => Set()    
            }
        }).flatten()
    }

    pure def gc_time(clock: GC_System) : Set[GC_Transition] = {
        clock.input.map(m => {
            match m {
                | TimeRequest(msg) => {
                    val tr = TimeResponse({cfg: msg.cfg, time: clock.state.TIME})
                    Set({
                        post_state : clock.state,
                        effects : Set(
                            GC_LogMessage(TimeRequest(msg)),
                            GC_UpdateInput(Set(tr)),
                        )
                    })
                }
                | _ => Set()
            }
        }).flatten()
    }

    pure def gc_okay_1(clock: GC_System) : Set[GC_Transition] = {
        clock.input.map(m => {
            match m {
                | OkayRequest(msg) => Set({
                    if(clock.state.Reg.contains(({party: msg.cfg.party, sid: msg.cfg.sid}))) {
                        // Check if all registered parties are ready (including this one)
                        val flag = clock.state.Reg.filter(x => x.party.in(clock.params.C) == false).forall(rp => or {
                             clock.state.ok.contains(rp), 
                             rp == ({party: msg.cfg.party, sid: msg.cfg.sid}),
                             //(msg.cfg.party).in(clock.params.C)
                        })
                        val new_time = if (flag) clock.state.TIME + 1 else clock.state.TIME
                        // for whatever reason we are keeping corrupted parties in the ok set
                        val new_ok = if (flag) Set() else clock.state.ok.setAdd(({party: msg.cfg.party, sid: msg.cfg.sid}))
                        
                        {
                            post_state : {...clock.state, 
                                ok: new_ok,
                                TIME: new_time
                            },
                            effects : Set(
                                GC_LogMessage(OkayRequest(msg)),
                                GC_UpdateInput(Set(GC_SIMControl)),
                                GC_AppendHandle(SIMOkayRelease(msg)),
                            )
                        }
                    } else {
                        {
                            post_state : clock.state,
                            effects : Set(
                                GC_UpdateInput(Set()),
                            )
                        }
                    }
                })
                | _ => Set()
            }
        }).flatten()
    }

    pure def gc_okay_2(clock: GC_System) : Set[GC_Transition] = {
        clock.input.map(m => {
            match m {
                | GC_SIMControl => {
                    clock.handles.map(h => {
                        match h {
                            | SIMOkayRelease(q) => {
                                Set({
                                    post_state : clock.state,
                                    effects : Set(
                                        GC_UpdateInput(Set(OkayResponse(q))),
                                        GC_RemoveHandle(SIMOkayRelease(q)),
                                    )
                                })
                            }
                            | _ => Set()
                        }
                    }).flatten()
                }
                | _ => Set()
            }
        }).flatten()
    }

    pure def gc_main_listener(clock : GC_System) : Set[GC_Transition] = {
        Set(
            gc_register(clock),
            gc_time(clock),
            gc_okay_1(clock),
            gc_okay_2(clock),
            if (clock.params.responsive_simulator) Set() else gc_sim_loses_ctrl(clock),
        ).flatten()
    }  

    pure def gc_process_transitions(clock: GC_System, transition: GC_Transition) : GC_System = {
        val new_sys = {...clock, state: transition.post_state}
        transition.effects.fold(new_sys, ((a1,a2) => g_clock_apply_effect(a1,a2)))
    } 

    action gc_step_with(q: (GC_System) => Set[(GC_Transition)]): bool = {
        val trans = q(g_clock_s)

        all {
            nondet transition = trans.oneOf()
            g_clock_s' = gc_process_transitions(g_clock_s, transition)        
        }
    }   

    action gc_step : bool =  any { 
        gc_step_with(gc_main_listener)
    }

    pure def all_parties_at_all_sids(parties: Set[Party], sids: Set[SID]) : Set[{party: Party, sid: SID}] = {
        parties.map(p => sids.map(s => {party: p, sid: s})).flatten()
    }

    action gc_init(x : {C: Set[Party], time : int, Reg: Set[PartySID], responsive_simulator: bool, enable_logging: bool}): bool = {
        g_clock_s' = {
            params: {
                C : x.C,
                responsive_simulator: x.responsive_simulator,
                enable_logging: x.enable_logging,
            },
            state : {
                TIME : x.time,
                Reg : x.Reg,
                ok  : Set(),
            },
            nonce : 0,
            input : Set(),
            log : Set(),
            handles : Set(),
        }
    }

    action gc_stutter : bool = {
        g_clock_s' = g_clock_s
    }
    

}