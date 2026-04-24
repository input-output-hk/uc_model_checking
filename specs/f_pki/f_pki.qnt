module f_pki {

    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"

    import common.* from "../common"
    import f_pki_types.* from "./f_pki_types"

    var f_pki_s : PKI_System

    val SIM_ALL_CERTS = 1.to(UNIVERSE_SIZE)

    pure def pki_register_1(pki: PKI_System) : Set[PKI_Transition] = {
        pki.input.pki_get_register_request_messages().map(p => {
            if (pki_access_check(p.cfg, pki.params)) {
                {
                    post_state: pki.state,
                    effects: Set(
                        PKI_LogMessage(PKI_RegisterRequest(p)),
                        PKI_AppendHandle(PKI_SIMRegisterRequestHold(p)),
                        PKI_UpdateInput(Set(PKI_SIMControl))
                    )
                }
            } else {
                pki_skip_trans(pki)
            }
        })
    }

    // TODO: rewrite with non-det if experiencing performance issues
    pure def pki_register_2(pki: PKI_System) : Set[PKI_Transition] = {      
        pki.input.pki_get_sim_control_messages().map(m => {
            pki.handles.pki_get_sim_register_request_hold_messages().map(p => {
                SIM_ALL_CERTS.map(cert => {
                    {
                        post_state: pki.state,
                        effects: Set(
                            PKI_RemoveHandle(PKI_SIMRegisterRequestHold(p)),
                            PKI_UpdateInput(Set(PKI_SIMRegisterRequestRelease({cfg: p.cfg, vk: p.vk, cert: cert})))
                        )
                    }
                })
            }).flatten()
        }).flatten()
    }

    pure def pki_register_3(pki: PKI_System) : Set[PKI_Transition] = {
        pki.input.pki_get_sim_register_request_release_messages().map(p => {

            val sid = p.cfg.sid
            val party = p.cfg.party
            val sid_state = pki.state.get(sid)

            if(sid_state.Reg.exists(r => r.party == party)){
                val rr = PKI_RegisterResponse({cfg: p.cfg, vk: p.vk, cert: None})
                {
                    post_state: pki.state,
                    effects: Set(
                        PKI_UpdateInput(Set(rr)),
                    )
                }   
            }else{
                {
                    post_state: 
                     pki.state.put(sid, {...sid_state,
                         Reg: sid_state.Reg.union(Set({party: party, vk: p.vk})),
                         Ver: sid_state.Ver.union(Set({party: party, vk: p.vk, cert: p.cert}))
                     }),
                    effects: Set( 
                        PKI_UpdateInput(Set(PKI_RegisterResponse({cfg: p.cfg, vk: p.vk, cert: Some(p.cert)}))),
                    )                    
                }
            }
        })
    }


    pure def pki_get_all_1(pki: PKI_System) : Set[PKI_Transition] = {
        pki.input.pki_get_get_all_request_messages().map(p => {
            if (pki_access_check(p.cfg, pki.params)) {
                {
                    post_state: pki.state,
                    effects: Set(
                        PKI_LogMessage(PKI_GetAllRequest(p)),
                        PKI_AppendHandle(PKI_SIMGetAllRequestWait(p)),
                        PKI_UpdateInput(Set(PKI_SIMControl))
                    )
                }
            } else {
                pki_skip_trans(pki)
            }
        })
    }

    pure def pki_get_all_2(pki: PKI_System) : Set[PKI_Transition] = {      
        pki.input.pki_get_sim_control_messages().map(m => {
            pki.handles.pki_get_sim_get_all_request_wait_messages().map(p => {
                {
                        post_state: pki.state,
                        effects: Set(
                            PKI_RemoveHandle(PKI_SIMGetAllRequestWait(p)),
                            PKI_UpdateInput(Set(PKI_GetAllResponse({
                                cfg: p.cfg,
                                vks: pki.state.get(p.cfg.sid).Reg.map(r => (r.party, r.vk)).setToMap()
                            })))
                        )
                }
            })
        }).flatten()
    }    


    pure def pki_retrieve_1(pki: PKI_System) : Set[PKI_Transition] = {
        pki.input.pki_get_retrieve_request_messages().map(p => {
            if (pki_access_check(p.cfg, pki.params)) {
                {
                    post_state: pki.state,
                    effects: Set(
                        PKI_LogMessage(PKI_RetrieveRequest(p)),
                        PKI_AppendHandle(PKI_SIMRetrieveRequestHold(p)),
                        PKI_UpdateInput(Set(PKI_SIMControl))
                    )
                }
            } else {
                pki_skip_trans(pki)
            }
        })
    }

    pure def pki_retrieve_2(pki: PKI_System) : Set[PKI_Transition] = {      
        pki.input.pki_get_sim_control_messages().map(m => {
            pki.handles.pki_get_sim_retrieve_request_hold_messages().map(p => {
                {
                        post_state: pki.state,
                        effects: Set(
                            PKI_RemoveHandle(PKI_SIMRetrieveRequestHold(p)),
                            PKI_UpdateInput(Set(PKI_SIMRetrieveRequestRelease(p)))
                        )
                }
            })
        }).flatten()
    }    


    pure def pki_retrieve_3(pki: PKI_System) : Set[PKI_Transition] = {
        pki.input.pki_get_sim_retrieve_request_release_messages().map(p => {

            val sid = p.cfg.sid
            val party = p.cfg.party
            val sid_state = pki.state.get(sid)

            val vk_res = {
                val elem = sid_state.Reg.find(x => x.party == p.Q)
                if (elem == None){
                    None
                }else{
                    Some(elem.unwrap().vk)
                }
            }

            {
                post_state: pki.state,
                effects: Set( 
                    PKI_UpdateInput(Set(PKI_RetrieveResponse({cfg: p.cfg, Q: p.Q, vk: vk_res})))
                )
            }
        })
    }    

    pure def pki_cert_verify_1(pki: PKI_System) : Set[PKI_Transition] = {
        pki.input.pki_get_cert_verify_request_messages().map(p => {
            if (pki_access_check(p.cfg, pki.params)) {
                val sid_state = pki.state.get(p.cfg.sid)
                val result = sid_state.Ver.exists(x => x.party == p.Q and x.vk == p.vk and x.cert == p.cert)
                {
                    post_state: pki.state,
                    effects: Set(
                        PKI_LogMessage(PKI_CertVerifyRequest(p)),
                        PKI_UpdateInput(Set(PKI_CertVerifyResponse({
                            cfg: p.cfg,
                            Q: p.Q,
                            vk: p.vk,
                            cert: p.cert,
                            result: result
                        })))
                    )
                }
            } else {
                pki_skip_trans(pki)
            }
        })
    }

    // sim might want to give control back to the environment
    pure def pki_sim_loses_ctrl(pki: PKI_System) : Set[PKI_Transition] = {
        pki.input.pki_get_sim_control_messages().map(c =>  // sim must have control
            {
                post_state : pki.state,
                effects : Set(
                    PKI_UpdateInput(Set()),
                    )
            })
    }

    pure def pki_main_listener(pki: PKI_System) : Set[PKI_Transition] = {
        Set(
            pki_register_1(pki),
            pki_register_2(pki),
            pki_register_3(pki),

            pki_retrieve_1(pki),
            pki_retrieve_2(pki),
            pki_retrieve_3(pki),

            pki_cert_verify_1(pki),   

            pki_get_all_1(pki),
            pki_get_all_2(pki),             

            if (pki.params.responsive_simulator) Set() else pki_sim_loses_ctrl(pki)
        ).flatten()
    }

    pure def pki_process_transitions(sys: PKI_System, transition: PKI_Transition) : PKI_System = {
        val new_sys = {...sys, state: transition.post_state}
        transition.effects.fold(new_sys, ((a1,a2) => pki_apply_effect(a1,a2)))
    } 

      

    action pki_step_with(q: (PKI_System) => Set[(PKI_Transition)]): bool = {
        all{
            val ts = q(f_pki_s)
            all {
                nondet transition = ts.oneOf()
                all {
                    f_pki_s' = pki_process_transitions(f_pki_s, transition)
                }
            }
        }
    } 

    action pki_step : bool =  all {
        pki_step_with(pki_main_listener)
    }    

    pure def pki_initial(x : {P: Set[Party], SIDs: Set[SID], enable_logging: bool, responsive_simulator: bool}) : PKI_System = {
        {
            params : {
                P: x.P,
                SIDs: x.SIDs,
                enable_logging: x.enable_logging,
                responsive_simulator: x.responsive_simulator,
            },
            state: x.SIDs.mapBy(sid => {
                Ver: Set(),
                Reg: Set(),
            }),
            input: Set(),
            nonce: 0, 
            log: Set(),
            handles: Set()
        }        
    }    

    action pki_init(x:{P: Set[Party], SIDs: Set[SID], enable_logging: bool, responsive_simulator: bool}) : bool =  all {
        f_pki_s' = pki_initial(x)
    }    

}