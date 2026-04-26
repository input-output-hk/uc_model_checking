module f_dif_types {

    import basicSpells.* from "../../spells/basicSpells"
    import common.* from "../common"
    import params.* from "../params"

    import g_clock_types.* from "../g_clock/g_clock_types"

    // define types of the system  
    type DIF_Data = {party: int, avk: int, sig: int}
    type DIF_Index = int
    
    type DIF_Message =

        | DIF_DiffuseRequest({cfg: MsgConfig, m: DIF_Data})
        | DIF_SIMDiffuseHold({cfg: MsgConfig, ctr:Counter, t:Time, m:DIF_Data})
        | DIF_SIMDiffuseRelease({cfg: MsgConfig, ctr: Counter, t:Time, m : DIF_Data})
        | DIF_DiffuseResponse({cfg: MsgConfig, ctr:Counter, t: Time, m: DIF_Data})

        | DIF_FetchRequest({cfg: MsgConfig})
        | DIF_SIMFetchHold({cfg: MsgConfig, t_prim: Time, i_late: Set[DIF_Index]})
        | DIF_SIMFetchRelease({cfg: MsgConfig, t_prim: Time, i_late: Set[DIF_Index], i_early: Set[DIF_Index]})
        | DIF_FetchResponse({cfg: MsgConfig, t_prim: Time, data: Set[DIF_Data]})

        | DIF_OKRequest({cfg: MsgConfig})
        | DIF_OKRequestWait({cfg: MsgConfig})
        | DIF_OKResponse({cfg: MsgConfig})

        | DIF_InjectRequest( {cfg: MsgConfig, Q:Party, m: DIF_Data})
        | DIF_InjectResponse({cfg: MsgConfig, Q:Party, m: DIF_Data})

        | DIF_SIMControl  



    type DIF_SystemParams = {
        P : Set[Party], // parties who can use the functionality
        C : Set[Party], // corrupted parties
        SIDs: Set[SID], // TODO: is this correct?

        delta: Time,

        enable_logging: bool,
        responsive_simulator: bool,
    }

    type DIF_StateVars = SID -> 
        {
            L : {to: Party, ctr: Counter} -> {data: DIF_Data, time: Time},
            ctr : Counter,
            F_flag: Set[(Party, Time)],
            D_flag: Set[(Party, Time)],            
        }

    type DIF_System = {
        params : DIF_SystemParams,
        state : DIF_StateVars,
        input : Set[DIF_Message], // always not more than 1 message in the system
        nonce : Nonce,        // to be able to match requests and responses in the trace
        log : Set[{idx: int, msg: DIF_Message, time : Time, handles: Set[DIF_Message] }], // state: DIF_StateVars
        handles : Set[DIF_Message],
    }  

    type DIF_Effect = 
      | DIF_LogMessage(DIF_Message)
      | DIF_UpdateInput(Set[DIF_Message])
      | DIF_AppendHandle(DIF_Message)
      | DIF_RemoveHandle(DIF_Message)      

    type DIF_Transition = {
        post_state: DIF_StateVars,
        effects: Set[DIF_Effect]
    }    


    pure def dif_apply_effect(sys: DIF_System, clock: GC_System, eff: DIF_Effect): DIF_System = {
        match eff {
        | DIF_LogMessage(msg) => 
            {...sys, 
                log: if (sys.params.enable_logging) sys.log.setAdd({idx: sys.log.size(), msg: msg,  time: clock.state.TIME, handles: sys.handles}) else sys.log
            }
        | DIF_UpdateInput(msgs) => 
            {...sys, 
                input: msgs,
                log: if (sys.params.enable_logging and msgs.dif_get_all_response_messages().size() == 1) sys.log.setAdd({idx: sys.log.size(), msg: (msgs.pickSome()), time: clock.state.TIME, handles: sys.handles}) else sys.log
            }
        | DIF_AppendHandle(msg) => 
            {...sys,
                handles: sys.handles.union(Set(msg)),
            }
        | DIF_RemoveHandle(handle_to_remove) =>
            {...sys,
                handles: sys.handles.exclude(Set(handle_to_remove))
            }
        }
    }

    pure def dif_get_all_response_messages(messages: Set[DIF_Message]): Set[DIF_Message] = {
        messages.filterMap(m => {
        match m {
            | DIF_DiffuseResponse(p) => Some(DIF_DiffuseResponse(p))
            | DIF_FetchResponse(p) => Some(DIF_FetchResponse(p))
            | DIF_InjectResponse(p) => Some(DIF_InjectResponse(p))  
            | DIF_OKResponse(p) => Some(DIF_OKResponse(p))
            | _ => None
        }
        })
    }      

    pure def dif_skip_trans(dif: DIF_System) : DIF_Transition = {
        {
            post_state: dif.state,
            effects : Set(
                DIF_UpdateInput(Set())
            )
        }
    }

    pure def dif_id_trans(dif: DIF_System) : DIF_Transition = {
        {
            post_state: dif.state,
            effects : Set()
        }
    }     


    pure def dif_get_inject_request_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig, Q:Party, m:DIF_Data}] = {
        messages.filterMap(m => {
        match m {
            | DIF_InjectRequest(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def dif_get_inject_response_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig, Q:Party, m:DIF_Data}] = {
        messages.filterMap(m => {
        match m {
            | DIF_InjectResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def dif_get_diffuse_request_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig, m: DIF_Data}] = {
        messages.filterMap(m => {
        match m {
            | DIF_DiffuseRequest(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def dif_get_sim_diffuse_hold_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig, ctr: Counter, t: Time, m: DIF_Data}] = {
        messages.filterMap(m => {
        match m {
            | DIF_SIMDiffuseHold(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def dif_get_sim_diffuse_release_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig, ctr: Counter, t: Time, m: DIF_Data}] = {
        messages.filterMap(m => {
        match m {
            | DIF_SIMDiffuseRelease(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def dif_get_diffuse_response_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig, ctr: Counter, t: Time, m: DIF_Data}] = {
        messages.filterMap(m => {
        match m {
            | DIF_DiffuseResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def dif_get_fetch_request_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
        match m {
            | DIF_FetchRequest(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def dif_get_sim_fetch_hold_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig, t_prim: Time, i_late: Set[DIF_Index]}] = {
        messages.filterMap(m => {
        match m {
            | DIF_SIMFetchHold(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def dif_get_sim_fetch_release_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig, t_prim: Time, i_late: Set[DIF_Index], i_early: Set[DIF_Index]}] = {
        messages.filterMap(m => {
        match m {
            | DIF_SIMFetchRelease(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def dif_get_fetch_response_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig, t_prim: Time, data: Set[DIF_Data]}] = {
        messages.filterMap(m => {
        match m {
            | DIF_FetchResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def dif_get_ok_request_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
        match m {
            | DIF_OKRequest(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def dif_get_ok_request_wait_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
        match m {
            | DIF_OKRequestWait(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def dif_get_ok_response_messages(messages: Set[DIF_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
        match m {
            | DIF_OKResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def dif_get_sim_control_messages(messages: Set[DIF_Message]): Set[DIF_Message] = {
        messages.filterMap(m => {
        match m {
            | DIF_SIMControl => Some(DIF_SIMControl)
            | _ => None
        }
        })
    }

    pure def dif_access_check(cfg: MsgConfig, params: DIF_SystemParams): bool = {
            cfg.party.in(params.P) and cfg.sid.in(params.SIDs)
    }     

    
}