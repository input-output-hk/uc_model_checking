module f_crs{
    
    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"
    
    import common.* from "../common"
    import f_crs_types.* from "./f_crs_types"

    var crs_s : CRS_System


    pure def crs_gen_1(crs: CRS_System) : Set[CRS_Transition] = {      
        crs.input.crs_get_gen_request_messages().map(p => {

            val sid = p.cfg.sid
            val party = p.cfg.party
            val sid_state = crs.state.get(sid)

            if (sid_state.crs != None) {
                // CRS already generated for this SID
                Set({
                    post_state: crs.state,
                    effects: Set(
                        CRS_LogMessage(GenRequest(p)),
                        CRS_AppendHandle(SIMGenRequestHold({cfg: p.cfg, crs: sid_state.crs.unwrap()})),
                        CRS_UpdateInput(Set(SIMControl))
                        )
                    })
            }else{
                crs.params.D.map(crs_val => {
                    Set({
                        post_state: crs.state.set(sid, {crs: Some(crs_val)}),
                        effects: Set(
                            CRS_LogMessage(GenRequest(p)),
                            CRS_AppendHandle(SIMGenRequestHold({cfg: p.cfg, crs: crs_val})),
                            CRS_UpdateInput(Set(SIMControl))
                        )
                    })
                }).flatten()
            }
        }).flatten()
    }

    pure def crs_gen_2(crs: CRS_System) : Set[CRS_Transition] = {      
        crs.input.crs_get_sim_control_messages().map(m => {
            crs.SIM_handles.crs_get_sim_gen_request_hold_messages().map(p => {
                Set({
                    post_state: crs.state,
                    effects: Set(
                        CRS_RemoveHandle(SIMGenRequestHold(p)),
                        CRS_UpdateInput(Set(SIMGenRequestRelease(p)))
                    )
                })
            }).flatten()
        }).flatten()
    }



    pure def crs_gen_3(crs: CRS_System) : Set[CRS_Transition] = {      
        crs.input.crs_get_sim_gen_request_release_messages().map(p => {
            {
                post_state: crs.state,
                effects: Set(
                    CRS_UpdateInput(Set(GenResponse(p)))
                )
            }
        })
    }

    // sim might want to give control back to the environment
    pure def crs_sim_loses_ctrl(crs: CRS_System) : Set[CRS_Transition] = {
        crs.input.crs_get_sim_control_messages().map(c =>  // sim must have control
            {
                post_state : crs.state,
                effects : Set(
                    CRS_UpdateInput(Set()),
                    )
            })
    }

    def crs_main_listener(crs: CRS_System) : Set[(CRS_Transition)] = {
        Set(
            crs_gen_1(crs),
            crs_gen_2(crs),
            crs_gen_3(crs),
            if (crs.params.responsive_simulator) Set() else crs_sim_loses_ctrl(crs)
        ).flatten()
    }

    def crs_process_transitions(sys: CRS_System, transition: CRS_Transition) : CRS_System = {
        val new_sys = {...sys, state: transition.post_state}
        transition.effects.fold(new_sys, ((a1,a2) => crs_apply_effect(a1,a2)))
    } 

    def crs_apply_effect(sys: CRS_System, eff: CRS_Effect): CRS_System = {
        match eff {
        | CRS_LogMessage(msg) => 
            {...sys, 
                log: sys.log.setAdd({idx: sys.log.size(), msg: msg})
            }
        | CRS_UpdateInput(msgs) => 
            {...sys, 
                input: msgs
            }
        | CRS_AppendHandle(msg) => 
            {...sys,
                SIM_handles: sys.SIM_handles.union(Set(msg)),
            }
        | CRS_RemoveHandle(handle_to_remove) =>
            {...sys,
                SIM_handles: sys.SIM_handles.exclude(Set(handle_to_remove))
            }
        }
    }        

    action crs_step_with(q: (CRS_System) => Set[(CRS_Transition)]): bool = {
        all{
            val ts = q(crs_s)
            all {
                ts.size() > 0,
                nondet transition = ts.oneOf()
                all {
                    crs_s' = crs_process_transitions(crs_s, transition)
                }
            }
        }
    } 

    action crs_step : bool =  all {
        crs_step_with(crs_main_listener)
    }    

    pure def crs_initial(P: Set[Party], D: Set[CRS], SIDs: Set[SID], responsive_simulator: bool) : CRS_System = {
        {
            params : {
                P: P,
                D: D,
                SIDs: SIDs,
                responsive_simulator: responsive_simulator,
            },
            state: SIDs.mapBy(sid => {
                crs: None,
            }),
            input: Set(),
            nonce: 0,
            log: Set(),
            SIM_handles: Set()
        }
    }

    action crs_init(P: Set[Party], D: Set[CRS], SIDs: Set[SID], responsive_simulator: bool) : bool =  all {
        crs_s' = crs_initial(P, D, SIDs, responsive_simulator)
    }

}