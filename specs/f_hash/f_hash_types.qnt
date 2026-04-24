module f_hash_types {

    import basicSpells.* from "../../spells/basicSpells"
    import common.* from "../common"

    import g_ledger_types.* from "../g_ledger/g_ledger_types" 

    // define types of the system  
    type Hash_Output = int
    type Hash_Input = List[LED_Tx]

    type HASH_Message =
        | HashRequest({cfg: MsgConfig, x: Hash_Input}) 
        | SIMHashRequestHold({cfg: MsgConfig, x: Hash_Input})
        | SIMHashRequestRelease({cfg: MsgConfig, x: Hash_Input, sim_h: Hash_Output})     
        | HashResponse({cfg: MsgConfig, x: Hash_Input, h: Hash_Output})

        | HASH_SIMControl  

 

    type HASH_SystemParams = {
        P : Set[Party], // parties who can use the functionality
        SIDs: Set[SID], // TODO: is this correct?

        enable_logging: bool,
        responsive_simulator: bool,
    }

    type HASH_StateVars = 
        {
            hash_map : Hash_Input -> Hash_Output
        }

    type HASH_System = {
        params : HASH_SystemParams,
        state : SID -> HASH_StateVars,
        input : Set[HASH_Message], // always not more than 1 message in the system
        nonce : Nonce,        // to be able to match requests and responses in the trace
        log : Set[{idx: int, msg: HASH_Message, }], // state: HASH_StateVars
        handles : Set[HASH_Message],
    }  

    type HASH_Effect = 
      | HASH_LogMessage(HASH_Message)
      | HASH_UpdateInput(Set[HASH_Message])
      | HASH_AppendHandle(HASH_Message)
      | HASH_RemoveHandle(HASH_Message)      


    type HASH_Transition = {
        post_state: SID -> HASH_StateVars,
        effects: Set[HASH_Effect]
    }


    pure def hash_access_check(cfg: MsgConfig, params: HASH_SystemParams): bool =
        cfg.party.in(params.P) and cfg.sid.in(params.SIDs)

    pure def hash_id_trans(f_hash: HASH_System) : HASH_Transition = {
        post_state : f_hash.state,
        effects : Set(),
    }

    pure def hash_skip_trans(hash: HASH_System) : HASH_Transition = {
        {
            post_state: hash.state,
            effects : Set(
                HASH_UpdateInput(Set())
            )
        }
    }        

   pure def hash_get_all_response_messages(messages: Set[HASH_Message]): Set[HASH_Message] = {
        messages.filterMap(m => {
        match m {
            | HashResponse(p) => Some(HashResponse(p))
            | _ => None
        }
        })
    }        

//     pure def hash_get_hash_request_messages(messages: Set[HASH_Message]): Set[{cfg: MsgConfig, x: Hash_Input}] = {
//         messages.filterMap(m => {
//             match m {
//                 | HashRequest(p) => Some(p)
//                 | _ => None
//             }
//         })
//     }
//     pure def hash_get_sim_request_hold_messages(messages: Set[HASH_Message]): Set[{cfg: MsgConfig, x: Hash_Input}] = {
//         messages.filterMap(m => {
//             match m {
//                 | SIMHashRequestHold(p) => Some(p)
//                 | _ => None
//             }
//         })
//     }

//     pure def hash_get_sim_request_release_messages(messages: Set[HASH_Message]): Set[{cfg: MsgConfig, x: Hash_Input, sim_h: Hash_Output}] = {
//         messages.filterMap(m => {
//             match m {
//                 | SIMHashRequestRelease(p) => Some(p)
//                 | _ => None
//             }
//         })
//     }

//     pure def hash_get_response_messages(messages: Set[HASH_Message]): Set[{cfg: MsgConfig, x: Hash_Input, h: Hash_Output}] = {
//         messages.filterMap(m => {
//             match m {
//                 | HashResponse(p) => Some(p)
//                 | _ => None
//             }
//         })
//     }

//     pure def hash_get_sim_control_messages(messages: Set[HASH_Message]): Set[HASH_Message] = {
//         messages.filterMap(m => {
//             match m {
//                 | HASH_SIMControl => Some(HASH_SIMControl)
//                 | _ => None
//             }
//         })
//     }

    pure def hash_apply_effect(sys: HASH_System, eff: HASH_Effect): HASH_System = {
        match eff {
        | HASH_LogMessage(msg) => 
            {...sys, 
                log: if (sys.params.enable_logging) sys.log.setAdd({idx: sys.log.size(), msg: msg,}) else sys.log
            }
        | HASH_UpdateInput(msgs) => 
            {...sys, 
                input: msgs,
                log: if (sys.params.enable_logging and msgs.hash_get_all_response_messages().size() == 1) sys.log.setAdd({idx: sys.log.size(), msg: (msgs.pickSome()),}) else sys.log
            }
        | HASH_AppendHandle(msg) => 
            {...sys,
                handles: sys.handles.union(Set(msg)),
            }
        | HASH_RemoveHandle(handle_to_remove) =>
            {...sys,
                handles: sys.handles.exclude(Set(handle_to_remove))
            }
        }
    }        

}