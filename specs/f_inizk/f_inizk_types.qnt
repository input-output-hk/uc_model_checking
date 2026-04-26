module f_inizk_types{

    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"
    import common.* from "../common"
    import params.* from "../params"
    import g_ledger_types.* from "../g_ledger/g_ledger_types"    
    
    type NIZK_Witness = List[LED_Tx]
    type NIZK_Statement = {x: LED_Tx, h: int}
    type NIZK_Proof = int    
    type NIZK_Relation = Set[(NIZK_Statement, NIZK_Witness)]


    type NIZK_Message =
        | NIZK_ProveRequest({cfg: MsgConfig, x: NIZK_Statement, w: NIZK_Witness}) 
        | NIZK_ProveRelWait({cfg: MsgConfig, x: NIZK_Statement, w: NIZK_Witness})
        | NIZK_SIMProveHold({cfg: MsgConfig, x: NIZK_Statement, w: NIZK_Witness}) 
        | NIZK_SIMProveRelease({cfg: MsgConfig, x: NIZK_Statement, w: NIZK_Witness, pi: NIZK_Proof})
        | NIZK_ProveResponse({cfg: MsgConfig, x: NIZK_Statement, w: NIZK_Witness, pi: Option[NIZK_Proof]}) 

        | NIZK_VerifyRequest({cfg: MsgConfig, x: NIZK_Statement, pi: NIZK_Proof})
        | NIZK_SIMVerifyHold({cfg: MsgConfig, x: NIZK_Statement, pi: NIZK_Proof}) 
        | NIZK_SIMVerifyRelease({cfg: MsgConfig, x: NIZK_Statement, pi: NIZK_Proof, w: NIZK_Witness}) 
        | NIZK_VerifyRelWait({cfg: MsgConfig, x: NIZK_Statement, pi: NIZK_Proof, w: NIZK_Witness}) 
        | NIZK_VerifyResponse({cfg: MsgConfig, x: NIZK_Statement, pi: NIZK_Proof, b: bool})

        | NIZK_RelRequest({cfg: MsgConfig, x: NIZK_Statement, w: NIZK_Witness})
        | NIZK_RelRequestWait({cfg: MsgConfig, x: NIZK_Statement, w: NIZK_Witness})
        | NIZK_RelResponse({cfg: MsgConfig, x: NIZK_Statement, w: NIZK_Witness, b: bool})

        | NIZK_SIMControl
  

    pure def nizk_get_verify_rel_wait_messages(messages: Set[NIZK_Message]): Set[{cfg: MsgConfig, x: NIZK_Statement, pi: NIZK_Proof, w: NIZK_Witness}] = {
        messages.filterMap(m => {
        match m {
            | NIZK_VerifyRelWait(p) => Some(p)
            | _ => None
        }
        })
    }   

    pure def nizk_get_prove_rel_wait_messages(messages: Set[NIZK_Message]): Set[{cfg: MsgConfig, x: NIZK_Statement, w: NIZK_Witness}] = {
        messages.filterMap(m => {
        match m {
            | NIZK_ProveRelWait(p) => Some(p)
            | _ => None
        }
        })
    }       

    type NIZK_SystemParams = {
        P: Set[Party],
        C: Set[Party],
        SIDs: Set[SID], 

        enable_logging: bool,
        responsive_simulator: bool
    }        

    type NIZK_StateVars = SID -> 
        {
            Ver: Set[{x: NIZK_Statement, pi: NIZK_Proof, b: bool}], // verified proofs for statements with witness w
            //Rel:  NIZK_Relation,
        }    

    type NIZK_System = {
        params : NIZK_SystemParams,
        state : NIZK_StateVars,
        input : Set[NIZK_Message], // always not more than 1 message in the system
        nonce : Nonce,        // to be able to match requests and responses in the trace
        log : Set[{idx: int, msg: NIZK_Message }], // state: NIZK_StateVars
        handles : Set[NIZK_Message],
    }        

    type NIZK_Effect = 
      | NIZK_LogMessage(NIZK_Message)
      | NIZK_UpdateInput(Set[NIZK_Message])
      | NIZK_AppendHandle(NIZK_Message)
      | NIZK_RemoveHandle(NIZK_Message)     


    type NIZK_Transition = {
        post_state: NIZK_StateVars,
        effects: Set[NIZK_Effect]
    }

    pure def nizk_id_trans(nizk: NIZK_System) : NIZK_Transition = {
        post_state : nizk.state,
        effects : Set(),
    }    

   pure def nizk_skip_trans(nizk: NIZK_System) : NIZK_Transition = {
        post_state : nizk.state,
        effects : Set(NIZK_UpdateInput(Set())),
    }     

    pure def nizk_access_check(cfg: MsgConfig, params: NIZK_SystemParams): bool = {
            cfg.party.in(params.P) and cfg.sid.in(params.SIDs)
    }    

   pure def nizk_get_all_response_messages(messages: Set[NIZK_Message]): Set[NIZK_Message] = {
        messages.filterMap(m => {
        match m {
            | NIZK_ProveResponse(p) => Some(NIZK_ProveResponse(p))
            | NIZK_VerifyResponse(p) => Some(NIZK_VerifyResponse(p))  
            | _ => None
        }
        })
   }    

    // pure def nizk_get_sim_control_messages(messages: Set[NIZK_Message]): Set[NIZK_Message] = {
    //     messages.filter(m => {
    //     match m {
    //         | NIZK_SIMControl => true
    //         | _ => false
    //     }
    //     })
    // }          

    // pure def nizk_get_prove_request_messages(messages: Set[NIZK_Message]): Set[{cfg: MsgConfig, x: NIZK_Statement, w: NIZK_Witness}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | NIZK_ProveRequest(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }   
    // pure def nizk_get_rel_request_messages(messages: Set[NIZK_Message]): Set[{cfg: MsgConfig, x: NIZK_Statement, w: NIZK_Witness}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | NIZK_RelRequest(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def nizk_get_rel_request_wait_messages(messages: Set[NIZK_Message]): Set[{cfg: MsgConfig, x: NIZK_Statement, w: NIZK_Witness}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | NIZK_RelRequestWait(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def nizk_get_rel_response_messages(messages: Set[NIZK_Message]): Set[{cfg: MsgConfig, x: NIZK_Statement, w: NIZK_Witness, b: bool}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | NIZK_RelResponse(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }
    // pure def nizk_get_sim_prove_hold_messages(messages: Set[NIZK_Message]): Set[{cfg: MsgConfig, x: NIZK_Statement, w: NIZK_Witness}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | NIZK_SIMProveHold(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def nizk_get_sim_prove_release_messages(messages: Set[NIZK_Message]): Set[{cfg: MsgConfig, x: NIZK_Statement, w: NIZK_Witness, pi: NIZK_Proof}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | NIZK_SIMProveRelease(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def nizk_get_prove_response_messages(messages: Set[NIZK_Message]): Set[{cfg: MsgConfig, x: NIZK_Statement, w: NIZK_Witness, pi: Option[NIZK_Proof]}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | NIZK_ProveResponse(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def nizk_get_verify_request_messages(messages: Set[NIZK_Message]): Set[{cfg: MsgConfig, x: NIZK_Statement, pi: NIZK_Proof}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | NIZK_VerifyRequest(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def nizk_get_sim_verify_hold_messages(messages: Set[NIZK_Message]): Set[{cfg: MsgConfig, x: NIZK_Statement, pi: NIZK_Proof}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | NIZK_SIMVerifyHold(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def nizk_get_sim_verify_release_messages(messages: Set[NIZK_Message]): Set[{cfg: MsgConfig, x: NIZK_Statement, pi: NIZK_Proof, w: NIZK_Witness}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | NIZK_SIMVerifyRelease(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def nizk_get_verify_response_messages(messages: Set[NIZK_Message]): Set[{cfg: MsgConfig, x: NIZK_Statement, pi: NIZK_Proof, b: bool}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | NIZK_VerifyResponse(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    def nizk_apply_effect(sys: NIZK_System, eff: NIZK_Effect): NIZK_System = {
        match eff {
        | NIZK_LogMessage(msg) => 
            {...sys, 
                log: if(sys.params.enable_logging) sys.log.setAdd({idx: sys.log.size(), msg: msg}) else sys.log
            }
        | NIZK_UpdateInput(msgs) => 
            {...sys, 
                input: msgs,
                log: if(sys.params.enable_logging and msgs.nizk_get_all_response_messages().size() == 1) sys.log.setAdd({idx: sys.log.size(), msg: (msgs.pickSome())}) else sys.log
            }
        | NIZK_AppendHandle(msg) => 
            {...sys,
                handles: sys.handles.union(Set(msg)),
            }
        | NIZK_RemoveHandle(handle_to_remove) =>
            {...sys,
                handles: sys.handles.exclude(Set(handle_to_remove))
            }
        }
    }

    pure def nizk_Rel_sat(x: NIZK_Statement, w: NIZK_Witness): bool = {
        w.listHas(x.x)
        //sys.state.get(sid).Rel.contains((x, w))
    }         


}