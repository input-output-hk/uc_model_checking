module f_crs_types {

    import basicSpells.* from "../../spells/basicSpells"
    import common.* from "../common"

    // define types of the system  
    type CRS = int
    
    type CRS_Message =

        | GenRequest({cfg: MsgConfig}) 
        | SIMGenRequestHold({cfg: MsgConfig, crs: CRS})
        | SIMGenRequestRelease({cfg: MsgConfig, crs: CRS})     
        | GenResponse({cfg: MsgConfig, crs: CRS})

        | SIMControl  


    pure def crs_get_all_response_messages(messages: Set[CRS_Message]): Set[CRS_Message] = {
        messages.filterMap(m => {
        match m {
            | GenResponse(p) => Some(GenResponse(p))
            | _ => None
        }
        })
    }      

    type CRS_SystemParams = {
        P : Set[Party], // parties who can use the functionality
        D : Set[CRS], // set of available CRS
        SIDs: Set[SID], // TODO: is this correct?
        responsive_simulator: bool,
    }

    type CRS_StateVars = SID -> 
        {
            crs : Option[CRS]
        }

    type CRS_System = {
        params : CRS_SystemParams,
        state : CRS_StateVars,
        input : Set[CRS_Message], // always not more than 1 message in the system
        nonce : Nonce,        // to be able to match requests and responses in the trace
        log : Set[{idx: int, msg: CRS_Message, }], // state: CRS_StateVars
        SIM_handles : Set[CRS_Message],
    }  

    type CRS_Effect = 
      | CRS_LogMessage(CRS_Message)
      | CRS_UpdateInput(Set[CRS_Message])
      | CRS_AppendHandle(CRS_Message)
      | CRS_RemoveHandle(CRS_Message)      

    type CRS_Transition = {
        post_state: CRS_StateVars,
        effects: Set[CRS_Effect]
    }    

    pure def crs_get_gen_request_messages(messages: Set[CRS_Message]): Set[{cfg: MsgConfig}] = {
        messages.filterMap(m => {
        match m {
            | GenRequest(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def crs_get_sim_gen_request_hold_messages(messages: Set[CRS_Message]): Set[{cfg: MsgConfig, crs: CRS}] = {
        messages.filterMap(m => {
        match m {
            | SIMGenRequestHold(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def crs_get_sim_gen_request_release_messages(messages: Set[CRS_Message]): Set[{cfg: MsgConfig, crs: CRS}] = {
        messages.filterMap(m => {
        match m {
            | SIMGenRequestRelease(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def crs_get_gen_response_messages(messages: Set[CRS_Message]): Set[{cfg: MsgConfig, crs: CRS}] = {
        messages.filterMap(m => {
        match m {
            | GenResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    pure def crs_get_sim_control_messages(messages: Set[CRS_Message]): Set[CRS_Message] = {
        messages.filterMap(m => {
        match m {
            | SIMControl => Some(SIMControl)
            | _ => None
        }
        })
    }

    
}