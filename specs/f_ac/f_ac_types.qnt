module f_ac_types {

    import basicSpells.* from "../../spells/basicSpells"
    import common.* from "../common"

    import g_clock_types.* from "../g_clock/g_clock_types"

    // define types of the system  
    type Data = int
    
    type FAC_Message =

        | SendRequest({cfg: MsgConfig, Q:Party, x:Data}) 
        | SIMSendHold({cfg: MsgConfig, ctr:Counter, Q:Party, t:Time, x:Data})
        | SIMSendRelease({cfg: MsgConfig, Q:Party, ctr: Counter, t:Time, x : Data})
        | ClockRequest({cfg: MsgConfig, Q:Party, ctr:Counter, t: Time, x: Data})
        | ClockRequestWait({cfg: MsgConfig, Q:Party, ctr:Counter, t: Time, x: Data})
        | ClockResponse({cfg: MsgConfig, Q:Party, ctr:Counter, t: Time, x: Data})        
        | SendResponse({cfg: MsgConfig, Q:Party, ctr:Counter, t: Time, x: Data})

        | FetchRequest({cfg: MsgConfig})
        | SIMFetchHold({cfg: MsgConfig, t_prim: Time, i_late: Set[Time]})
        | SIMFetchRelease({cfg: MsgConfig, t_prim: Time, i_late: Set[Time], i_early: Set[Time]})
        | FetchResponse({cfg: MsgConfig, t_prim: Time, data: Set[Data]})

        | OKRequest({cfg: MsgConfig})
        | OKRequestWait({cfg: MsgConfig})
        | OKResponse({cfg: MsgConfig})

        | SIMControl  

    type FAC_SystemParams = {
        P : Set[Party], // parties who can use the functionality
        C : Set[Party], // corrupted parties
        delta: Time,
        SIDs: Set[SID],
        responsive_simulator: bool,
        enable_logging: bool,
    }

    type FAC_StateVars = SID -> 
        {
            L : {to: Party, ctr: Counter} -> {from: Party, data: Data, time: Time},
            ctr : Counter,
            S_flag: Set[(Party, Time)],
            F_flag: Set[(Party, Time)],            
        }

    type FAC_System = {
        params : FAC_SystemParams,
        state : FAC_StateVars,
        input : Set[FAC_Message], // always not more than 1 message in the system
        nonce : Nonce,        // to be able to match requests and responses in the trace
        log : Set[{idx: int, msg: FAC_Message, time : Time, handles: Set[FAC_Message],  }], // state: FAC_StateVars
        handles : Set[FAC_Message],
    }  

    type FAC_Effect = 
      | FAC_LogMessage(FAC_Message)
      | FAC_UpdateInput(Set[FAC_Message])
      | FAC_AppendHandle(FAC_Message)
      | FAC_RemoveHandle(FAC_Message)      

    type FAC_Transition = {
        post_state: FAC_StateVars,
        effects: Set[FAC_Effect]
    }    

    pure def fac_apply_effect(sys: FAC_System, clock: GC_System, eff: FAC_Effect): FAC_System = {
        match eff {
        | FAC_LogMessage(msg) => 
            {...sys, 
                log: sys.log.setAdd({idx: sys.log.size(), msg: msg,  time: clock.state.TIME, handles: sys.handles})
            }
        | FAC_UpdateInput(msgs) => 
            {...sys, 
                input: msgs
            }
        | FAC_AppendHandle(msg) => 
            {...sys,
                handles: sys.handles.union(Set(msg)),
            }
        | FAC_RemoveHandle(handle_to_remove) =>
            {...sys,
                handles: sys.handles.exclude(Set(handle_to_remove))
            }
        }
    }    


    pure def fac_get_send_request_messages(messages: Set[FAC_Message]): Set[{cfg: MsgConfig, Q:Party, x:Data}] = {
        messages.filterMap(m => {
        match m {
            | SendRequest(p) => Some(p)
            | _ => None
        }
        })
    }            

    pure def fac_get_ok_request_wait_messages(messages: Set[FAC_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
        match m {
            | OKRequestWait(p) => Some(p)
            | _ => None
        }
        })
    }            
         
     

    pure def fac_get_all_response_messages(messages: Set[FAC_Message]): Set[FAC_Message] = {
        messages.filterMap(m => {
        match m {
            | SendResponse(p) => Some(SendResponse(p))
            | FetchResponse(p) => Some(FetchResponse(p))
            | OKResponse(p) => Some(OKResponse(p))  
            | _ => None
        }
        })
    }      

    pure def fac_get_sim_send_hold_messages(messages: Set[FAC_Message]): Set[{cfg: MsgConfig, ctr:Counter, Q:Party, t:Time, x:Data}] = {
        messages.filterMap(m => {
        match m {
            | SIMSendHold(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def fac_get_sim_send_release_messages(messages: Set[FAC_Message]): Set[{cfg: MsgConfig, Q:Party, ctr: Counter, t:Time, x : Data}] = {
        messages.filterMap(m => {
        match m {
            | SIMSendRelease(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def fac_get_send_response_messages(messages: Set[FAC_Message]): Set[{cfg: MsgConfig, Q:Party, ctr:Counter, t: Time, x: Data}] = {
        messages.filterMap(m => {
        match m {
            | SendResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def fac_get_fetch_request_messages(messages: Set[FAC_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
        match m {
            | FetchRequest(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def fac_get_sim_fetch_hold_messages(messages: Set[FAC_Message]): Set[{cfg: MsgConfig, t_prim: Time, i_late: Set[Time]}] = {
        messages.filterMap(m => {
        match m {
            | SIMFetchHold(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def fac_get_sim_fetch_release_messages(messages: Set[FAC_Message]): Set[{cfg: MsgConfig, t_prim: Time, i_late: Set[Time], i_early: Set[Time]}] = {
        messages.filterMap(m => {
        match m {
            | SIMFetchRelease(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def fac_get_fetch_response_messages(messages: Set[FAC_Message]): Set[{cfg: MsgConfig, t_prim: Time, data: Set[Data]}] = {
        messages.filterMap(m => {
        match m {
            | FetchResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def fac_get_ok_request_messages(messages: Set[FAC_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
        match m {
            | OKRequest(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def fac_get_clock_request_messages(messages: Set[FAC_Message]): Set[{cfg: MsgConfig, Q:Party, ctr:Counter, t: Time, x: Data}] = {
        messages.filterMap(m => {
        match m {
            | ClockRequest(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def fac_get_clock_request_wait_messages(messages: Set[FAC_Message]): Set[{cfg: MsgConfig, Q:Party, ctr:Counter, t: Time, x: Data}] = {
        messages.filterMap(m => {
        match m {
            | ClockRequestWait(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def fac_get_clock_response_messages(messages: Set[FAC_Message]): Set[{cfg: MsgConfig, Q:Party, ctr:Counter, t: Time, x: Data}] = {
        messages.filterMap(m => {
        match m {
            | ClockResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def fac_get_ok_response_messages(messages: Set[FAC_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
        match m {
            | OKResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def fac_get_sim_control_messages(messages: Set[FAC_Message]): Set[bool] = {
        messages.filterMap(m => {
        match m {
            | SIMControl => Some(true)
            | _ => None
        }
        })
    }

    pure def fac_access_check(cfg: MsgConfig, params: FAC_SystemParams): bool = {
            cfg.party.in(params.P) and cfg.sid.in(params.SIDs)
    } 

    
 

   
    
}