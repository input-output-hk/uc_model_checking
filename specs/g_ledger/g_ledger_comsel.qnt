module g_ledger_comsel{
    
    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"
    import common.* from "../common"
    import g_clock.* from "../g_clock/g_clock"
    import g_clock_types.* from "../g_clock/g_clock_types"    
    import g_ledger_types.* from "./g_ledger_types"
    import g_ledger.* from "./g_ledger"


    pure def led_comsel_1(led: LED_System, clock: GC_System) : Set[(LED_Transition, GC_Transition)] = {
        led.input.filterMap(m => match m {
            | ComSelRequest(p) => Some(p)
            | _ => None
        }).map(p => {
            if(led_access_check(p.cfg, led.params)){
                {
                    post_state: led.state,
                    effects: Set(
                        LED_UpdateInput(Set(LED_SIMControl)),
                        LED_AppendHandle(ComSelSIMHold({cfg: p.cfg, r: clock.state.TIME})),
                        LED_LogMessage(ComSelRequest(p))
                    )
                }
            }else{
                {
                    post_state: led.state,
                    effects: Set(
                        LED_UpdateInput(Set()),
                    )
                }
            }
        }).map(t => (t, gc_id_trans(clock)))        
    }

    action led_comsel_2_act : bool = {
        all {
            nondet sim_com = listSamplingSpace(g_ledger_s.params.comsel_c, g_ledger_s.params.P).oneOf()
            val ts = led_comsel_2(g_ledger_s, g_clock_s, sim_com)
            all {
                nondet transition = ts.oneOf()
                all {
                    g_ledger_s' = led_process_transitions(g_ledger_s, g_clock_s, transition._1),
                    g_clock_s' = gc_process_transitions(g_clock_s, transition._2)
                }
            }
        }
    }

    pure def led_comsel_clean(com: List[CommitteeMember], params: LED_SystemParams) : bool = {
        all{
            com.length() == params.comsel_c,
            com.toSet().size() == com.length(),  // all members distinct
            com.indices().forall(i => i == 0 or com[i-1] < com[i]),  // canonical ascending order
            com.toSet().forall(m => m.in(params.P)),
            params.comsel_b * com.listFilter(m => m.in(params.C)).length() <= params.comsel_a * com.length()
        }
    }

    pure def led_comsel_2(led: LED_System, clock: GC_System, sim_com: List[CommitteeMember]) : Set[(LED_Transition, GC_Transition)] = {
        led.input.filterMap(m => match m {
            | LED_SIMControl => Some(m)
            | _ => None
        }).map(_ => {
            led.handles.filterMap(h => match h {
                | ComSelSIMHold(p) => Some(p)
                | _ => None
            }).map(p => {
                {
                    post_state: led.state,
                    effects: Set(
                        LED_RemoveHandle(ComSelSIMHold(p)),
                        LED_UpdateInput(Set(ComSelSIMRelease({cfg: p.cfg, r: p.r, sim_com: sim_com}))) 
                    )
                }
            })
        }).flatten().map(t => (t, gc_id_trans(clock)))
    }

    pure def led_comsel_3(led: LED_System, clock: GC_System) : Set[(LED_Transition, GC_Transition)] = {
        led.input.filterMap(m => match m {
            | ComSelSIMRelease(p) => Some(p)
            | _ => None
        }).map(p => {
            if(led.state.Com.has(p.r)){
                // committee already assigned for this time
                {
                    post_state: led.state,
                    effects: Set(
                        LED_UpdateInput(Set(ComSelResponse({cfg: p.cfg, r:p.r, com: led.state.Com.get(p.r)}))),
                    )
                }                
            } else {
                if (not(led_comsel_clean(p.sim_com, led.params))) {

                    val all_coms = listSamplingSpace(led.params.comsel_c, led.params.P).filter(com => led_comsel_clean(com, led.params))

                    if (all_coms == Set()) {
                        // no valid committee can be formed
                        {
                            post_state: led.state,
                            effects: Set(
                                LED_UpdateInput(Set()),
                            )
                        }
                    }else{
                        val com = getOnlyElement2(all_coms)

                        {
                            post_state: {...led.state, 
                                Com: led.state.Com.put(p.r, com)
                            },
                            effects: Set(
                                LED_UpdateInput(Set(ComSelResponse({cfg: p.cfg, r: p.r, com: com}))),
                            )
                        }                 
                    }
                } else {
                    {
                        post_state: {...led.state, 
                            Com: led.state.Com.put(p.r, p.sim_com)
                        },
                        effects: Set(
                            LED_UpdateInput(Set(ComSelResponse({cfg: p.cfg, r:p.r, com: p.sim_com}))),
                        )
                    }
                }

            }
        }).map(t => (t, gc_id_trans(clock)))
    }

   def led_main_listener_comsel(led: LED_System, clock: GC_System) : (Set[(LED_Transition, GC_Transition)]) = {
        Set(
            led_comsel_1(led, clock),
            //led_comsel_2(led, clock), // a direct action instead
            led_comsel_3(led, clock),
        ).flatten()
    }

    
    action led_step_comsel : bool =  any {
        led_gc_step_with(led_main_listener_comsel),
        led_comsel_2_act
    }        
}