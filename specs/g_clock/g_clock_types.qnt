module g_clock_types {

    //import uc.* from "../uc"
    import basicSpells.* from "../../spells/basicSpells"
    import common.* from "../common"    

    // define types of the system
    type PartySID = {party: Party, sid: SID}

    type GC_Message =
        | RegisterRequest({cfg: MsgConfig})
        | RegisterResponse({cfg: MsgConfig})

        | TimeRequest({cfg: MsgConfig})
        | TimeResponse({cfg: MsgConfig, time: Time})

        | OkayRequest({cfg: MsgConfig})
        | SIMOkayHold({cfg: MsgConfig})
        | SIMOkayRelease({cfg: MsgConfig})
        | OkayResponse({cfg: MsgConfig})

        | GC_SIMControl


    type GC_SystemParams = {
        // P : Nat
        // SIDs : Nat
        C : Set[Party], // corrupted parties

        responsive_simulator: bool,
        enable_logging: bool,

    }


    type GC_StateVars = {
        TIME : Time,
        Reg : Set[PartySID],
        ok  : Set[PartySID],
    }


   
    type GC_System = {
        params : GC_SystemParams,
        state : GC_StateVars,
        input : Set[GC_Message], // always not more than 1 message in the system
        nonce : Nonce,
        log : Set[{idx: int, msg: GC_Message, state: GC_StateVars}],
        handles : Set[GC_Message],
    }  
 
    type GC_Effect = 
      | GC_LogMessage(GC_Message)
      | GC_UpdateInput(Set[GC_Message])
      | GC_AppendHandle(GC_Message)
      | GC_RemoveHandle(GC_Message)      

    type GC_Transition = {
        post_state: GC_StateVars,
        effects: Set[GC_Effect]
    }    

    pure def g_clock_apply_effect(sys: GC_System, eff: GC_Effect): GC_System = {
        match eff {
        | GC_LogMessage(msg) => 
            {...sys, 
                log: if (sys.params.enable_logging) sys.log.setAdd({idx: sys.log.size(), msg: msg, state : sys.state}) else sys.log
            }
        | GC_UpdateInput(msgs) =>
            {...sys,
                input: msgs,
                log: if (sys.params.enable_logging and msgs.gc_get_all_response_messages().size() == 1) sys.log.setAdd({idx: sys.log.size(), msg: (msgs.pickSome()), state : sys.state}) else sys.log
            }
        | GC_AppendHandle(msg) => 
            {...sys,
                handles: sys.handles.union(Set(msg)),
            }
        | GC_RemoveHandle(handle_to_remove) =>
            {...sys,
                handles: sys.handles.exclude(Set(handle_to_remove))
            }
        }
    }   

    pure def gc_get_all_response_messages(messages: Set[GC_Message]): Set[GC_Message] = {
        messages.filterMap(m => {
        match m {
            | RegisterResponse(p) => Some(m)
            | TimeResponse(p) => Some(m)
            | OkayResponse(p) => Some(m)
            | _ => None
        }
        })
    }    

    pure def get_register_request_messages(messages: Set[GC_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
        match m {
            | RegisterRequest(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def get_time_request_messages(messages: Set[GC_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
        match m {
            | TimeRequest(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def gc_get_sim_control_messages(messages: Set[GC_Message]): Set[GC_Message] = {
        messages.filter(m => {
        match m {
            | GC_SIMControl => true
            | _ => false
        }
        })
    }

    pure def get_okay_request_messages(messages: Set[GC_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
        match m {
            | OkayRequest(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def get_okay_response_messages(messages: Set[GC_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
        match m {
            | OkayResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def get_register_response_messages(messages: Set[GC_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
        match m {
            | RegisterResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def get_time_response_messages(messages: Set[GC_Message]): Set[{cfg: MsgConfig, time: Time}] = {
        messages.filterMap(m => {
        match m {
            | TimeResponse(p) => Some(p)
            | _ => None
        }
        })
    }         



    pure def get_sim_okay_release_messages(messages: Set[GC_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
        match m {
            | SIMOkayRelease(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def get_sim_okay_hold_messages(messages: Set[GC_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
        match m {
            | SIMOkayHold(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def contains_sim_control(messages: Set[GC_Message]): bool = {
        messages.exists(m => {
            match m {
                | GC_SIMControl => true
                | _ => false
            }
        })
    }    
    
}