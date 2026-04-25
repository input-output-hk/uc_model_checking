module f_pki_properties {

    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"
    import common.* from "../common"
    import f_pki_types.*   from "./f_pki_types"
    import f_pki.*  from "./f_pki"

    val Parties = 1.to(PARTY_SIZE)
    val sids = 1.to(SID_SIZE)

    action step : bool =  {
        if(f_pki_s.log.size() < MAX_LOG_SIZE){
            any {
                pki_step,  
                env_pki_send_all_reqs,
                env_consume_responses,
            }   
        }else{
            f_pki_s' = f_pki_s
        }
    }
        
  

    action init = all {
        pki_init({P: Parties, SIDs: sids, enable_logging: true, responsive_simulator: RESPONSIVE_SIMULATOR}),
    }    

    // environment sends any of the requests
    action env_pki_send_all_reqs : bool = all {
        nondet from = Parties.oneOf()
        nondet top = Parties.oneOf()
        nondet sid = sids.oneOf()

        val cfg = {party: from, sid: sid, nonce: f_pki_s.nonce}
        any {
            env_pki_register_reqs(cfg),
            env_pki_retrieve_reqs(cfg),
            env_pki_verify_reqs(cfg),
            env_pki_get_all_reqs(cfg),
            env_pki_sim_reqs,
        }
    }

    // give control to the simulator
    action env_pki_sim_reqs : bool = all {
        f_pki_s.input.size() == 0, 
        f_pki_s.handles.size() > 0,
        f_pki_s' = {...f_pki_s, 
            input: Set(PKI_SIMControl),
            nonce: f_pki_s.nonce + NONCE_INCREMENT
        }
    }    

    // send register request
    action env_pki_register_reqs(cfg: MsgConfig) : bool = all {
        f_pki_s.input.size() == 0,
        
        nondet vk = 1.to(UNIVERSE_SIZE).oneOf()
        
        f_pki_s' = {...f_pki_s, 
            input: Set(PKI_RegisterRequest({cfg: cfg, vk: vk})),
            nonce: f_pki_s.nonce + NONCE_INCREMENT
        }
    }

    // send retrieve request
    action env_pki_retrieve_reqs(cfg: MsgConfig) : bool = all {
        f_pki_s.input.size() == 0,
        
        nondet Q = Parties.oneOf()
        
        f_pki_s' = {...f_pki_s, 
            input: Set(PKI_RetrieveRequest({cfg: cfg, Q: Q})),
            nonce: f_pki_s.nonce + NONCE_INCREMENT
        }
    }    

    // send get all request
    action env_pki_get_all_reqs(cfg: MsgConfig) : bool = all {
        f_pki_s.input.size() == 0,

        f_pki_s' = {...f_pki_s,
            input: Set(PKI_GetAllRequest({cfg: cfg})),
            nonce: f_pki_s.nonce + NONCE_INCREMENT
        }
    }

    // send verify request
    action env_pki_verify_reqs(cfg: MsgConfig) : bool = all {
        f_pki_s.input.size() == 0,
        
        nondet Q = Parties.oneOf()
        nondet vk = 1.to(UNIVERSE_SIZE).oneOf()
        nondet cert = 1.to(UNIVERSE_SIZE).oneOf()
        
        f_pki_s' = {...f_pki_s,  // Q: Party, vk: VK, cert: Certificate
            input: Set(PKI_CertVerifyRequest({cfg: cfg, Q: Q, vk: vk, cert: cert})),
            nonce: f_pki_s.nonce + NONCE_INCREMENT
        }
    }        


    pure def env_consume_response(pki: PKI_System) : Set[PKI_Transition] = {
         pki.input.pki_get_all_response_messages().map(msg_log =>
         {
            post_state : pki.state,
            effects : Set(
                PKI_UpdateInput(Set())
            )
         })
    }

    action env_consume_responses : bool =  all {
        // Precondition: there is a response message in input
        val response_msgs = env_consume_response(f_pki_s)
        
        // Non-deterministically choose one response message to consume
        nondet transition = response_msgs.oneOf()

        // Update the system state by consuming the message    
        f_pki_s' = pki_process_transitions(f_pki_s, transition),
    }



    // ================= PROPERTIES =================

    // ---- Liveness: must-fail invariants ----
    // Each of these should be VIOLATED by the model checker, witnessing that the
    // corresponding operation can actually complete.

    // Fails when all parties have registered in some session
    val RegisterLiveness = sids.forall(sid =>
        f_pki_s.state.get(sid).Reg.size() < Parties.size()
    )

    // Fails when at least one successful retrieve response appears in the log
    val RetrieveLiveness = f_pki_s.log.forall(e => match e.msg {
        | PKI_RetrieveResponse(r) => r.vk == None
        | _ => true
    })

    // Fails when at least one successful cert-verify response appears in the log
    val CertVerifyLiveness = f_pki_s.log.forall(e => match e.msg {
        | PKI_CertVerifyResponse(r) => r.result == false
        | _ => true
    })

    // Fails when a GetAll response with at least one entry appears in the log
    val GetAllLiveness = f_pki_s.log.forall(e => match e.msg {
        | PKI_GetAllResponse(r) => r.vks.keys().size() == 0
        | _ => true
    })

    // ---- Liveness: temporal properties ----
    // Each property says: if `step` is weakly fair then the system is live.

    // Eventually some party registers
    temporal RegisterLivenessTemp =
        step.weakFair(Set(f_pki_s)).implies(
            eventually(sids.exists(sid => f_pki_s.state.get(sid).Reg.size() > 0))
        )

    // If a party registered, eventually a retrieve for them succeeds
    temporal RetrieveLivenessTemp =
        step.weakFair(Set(f_pki_s)).implies(
            (sids.exists(sid => f_pki_s.state.get(sid).Reg.size() > 0)) implies
            eventually(f_pki_s.log.exists(e => match e.msg {
                | PKI_RetrieveResponse(r) => r.vk != None
                | _ => false
            }))
        )

    // If a party registered, eventually a cert-verify for them succeeds
    temporal CertVerifyLivenessTemp =
        step.weakFair(Set(f_pki_s)).implies(
            (sids.exists(sid => f_pki_s.state.get(sid).Ver.size() > 0)) implies
            eventually(f_pki_s.log.exists(e => match e.msg {
                | PKI_CertVerifyResponse(r) => r.result == true
                | _ => false
            }))
        )

    // ---- Safety properties ----

    // one entry per party in Reg and Ver lists
    val OneEntryPerPartyInRegAndVerLists = Parties.forall(p => sids.forall({sid =>
        and { 
            f_pki_s.state.get(sid).Ver.filter(x => x.party == p).size() <= 1,
            f_pki_s.state.get(sid).Reg.filter(x => x.party == p).size() <= 1
        }
    }))
    
    // RetrieveSucceedsForRegistered
    val RetrieveSucceedsForRegistered = f_pki_s.log.forall(entry1 => {
        match entry1.msg {
        | PKI_RegisterResponse(reg_resp) =>
            f_pki_s.log.forall(entry2 => {
                match entry2.msg {
                    | PKI_RetrieveResponse(ret_resp) => {
                        and {
                            reg_resp.cfg.sid == ret_resp.cfg.sid,
                            reg_resp.cfg.party == ret_resp.Q,
                            entry1.idx < entry2.idx
                        } implies {
                            ret_resp.vk == Some(
                                f_pki_s.state.get(reg_resp.cfg.sid).Reg.find(x => x.party == reg_resp.cfg.party).unwrap().vk
                            )
                        }
                    }
                    | _ => true
                }
            })
        | _ => true
        }
    })

    // RetrieveFailsForUnregistered
    val RetrieveFailsForUnregistered = f_pki_s.log.forall(entry1 => {
        match entry1.msg {
        | PKI_RetrieveResponse(ret_resp) => {
            
            val reg_responses = f_pki_s.log.filter(entry2 => {
                match entry2.msg {
                    | PKI_RegisterResponse(reg_resp) => {
                        and {
                            reg_resp.cfg.sid == ret_resp.cfg.sid,
                            reg_resp.cfg.party == ret_resp.Q,
                            entry2.idx < entry1.idx
                        }
                    }
                    | _ => false
                }
            }).size()

            if (reg_responses == 0) {
                ret_resp.vk == None
            } else {
                ret_resp.vk != None
            }

        }
        | _ => true
        }
    })    

    val CertVerifySucceedsForRegistered = f_pki_s.log.forall(entry1 => {
        match entry1.msg {
        | PKI_CertVerifyResponse(ver_resp) => {
            
            val reg_responses = f_pki_s.log.filter(entry2 => {
                match entry2.msg {
                    | PKI_RegisterResponse(reg_resp) => {
                        and {
                            reg_resp.cfg.sid == ver_resp.cfg.sid,
                            reg_resp.cfg.party == ver_resp.Q,
                            entry2.idx < entry1.idx,
                            reg_resp.cert == Some(ver_resp.cert),
                            reg_resp.vk == ver_resp.vk
                        }
                    }
                    | _ => false
                }
            }).size()

            if (reg_responses == 0) {
                ver_resp.result == false
            } else{
                ver_resp.result == true
            }

        }
        | _ => true
        }
    })    


    // GetAllCorrectness: the vks map in GetAllResponse exactly reflects prior successful registrations
    val GetAllCorrectness = f_pki_s.log.forall(entry1 => {
        match entry1.msg {
        | PKI_GetAllResponse(get_resp) => {
            val sid = get_resp.cfg.sid
            // Soundness: every party in vks has a prior successful RegisterResponse with matching vk
            val soundness = get_resp.vks.keys().forall(party =>
                f_pki_s.log.exists(entry2 => match entry2.msg {
                    | PKI_RegisterResponse(reg_resp) => and {
                        entry2.idx < entry1.idx,
                        reg_resp.cfg.sid == sid,
                        reg_resp.cfg.party == party,
                        reg_resp.cert != None,
                        reg_resp.vk == get_resp.vks.get(party)
                    }
                    | _ => false
                })
            )
            // Completeness: every prior successful registration appears in vks
            val completeness = f_pki_s.log.forall(entry2 => match entry2.msg {
                | PKI_RegisterResponse(reg_resp) =>
                    (and {
                        entry2.idx < entry1.idx,
                        reg_resp.cfg.sid == sid,
                        reg_resp.cert != None
                    }) implies reg_resp.cfg.party.in(get_resp.vks.keys())
                | _ => true
            })
            and { soundness, completeness }
        }
        | _ => true
        }
    })

    val AllProps = all {
        OneEntryPerPartyInRegAndVerLists,
        RetrieveSucceedsForRegistered,
        RetrieveFailsForUnregistered,
        CertVerifySucceedsForRegistered,
        GetAllCorrectness,
    }

}