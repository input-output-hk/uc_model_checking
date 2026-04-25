module f_sig_types {

    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"
    import common.* from "../common"    
    
    type SIG_PublicKey = int
    type SIG_Document = str
    type SIG_Signature = int    
    type SIG_VK = int

    type SIG_Message =
        | SIG_GenRequest({cfg: MsgConfig}) 
        | SIG_SIMGenHold({cfg: MsgConfig}) 
        | SIG_SIMGenRelease({cfg: MsgConfig, vk: SIG_VK})
        | SIG_GenResponse({cfg: MsgConfig, vk: SIG_VK}) 

        | SIG_SignRequest({cfg: MsgConfig, m: SIG_Document}) 
        | SIG_SIMSignHold({cfg: MsgConfig, m: SIG_Document,})  
        | SIG_SIMSignRelease({cfg: MsgConfig, m: SIG_Document, sig: SIG_Signature}) // output of SIM
        | SIG_SignResponse({cfg: MsgConfig, m: SIG_Document, sig: Option[SIG_Signature]})

        | SIG_VerifyRequest({cfg: MsgConfig, vk: SIG_VK, m: SIG_Document, sig: SIG_Signature}) // input to FSig
        | SIG_SIMVerifyHold({cfg: MsgConfig, vk: SIG_VK, m: SIG_Document, sig: SIG_Signature}) // input to SIM
        | SIG_SIMReleaseVerify({cfg: MsgConfig, vk: SIG_VK, m: SIG_Document, sig: SIG_Signature, b: bool}) // output of SIM
        | SIG_VerifyResponse({cfg: MsgConfig, vk: SIG_VK, m: SIG_Document, sig: SIG_Signature, b: bool})

        | SIG_SIMControl
  

    type SIG_SystemParams = {
        P: Set[Party],
        C: Set[Party],
        SIDs: Set[SID],
        S: Set[Party],

        ALL_VKs: Set[SIG_VK],
        ALL_SIGs: Set[SIG_Signature],
        responsive_simulator: bool,
    }

    type SIG_StateVars = SID -> 
        {
            VK : Set[{party:Party, vk: SIG_VK}],
            Ver: Set[{vk: SIG_VK, m: SIG_Document, sig: SIG_Signature, b: bool}],
        }    

    type SIG_System = {
        params : SIG_SystemParams,
        state : SIG_StateVars,
        input : Set[SIG_Message], // always not more than 1 message in the system
        nonce : Nonce,        // to be able to match requests and responses in the trace
        log : Set[{idx: int, msg: SIG_Message }], // state: SIG_StateVars
        handles : Set[SIG_Message],
    }        

    type SIG_Effect = 
      | SIG_LogMessage(SIG_Message)
      | SIG_UpdateInput(Set[SIG_Message])
      | SIG_AppendHandle(SIG_Message)
      | SIG_RemoveHandle(SIG_Message)     


    type SIG_Transition = {
        post_state: SIG_StateVars,
        effects: Set[SIG_Effect]
    }          

    pure def sig_signer_access_check(cfg: MsgConfig, params: SIG_SystemParams): bool = {
            cfg.party.in(params.S) and cfg.sid.in(params.SIDs)
    }    

    pure def sig_verifier_access_check(cfg: MsgConfig, params: SIG_SystemParams): bool = {
            cfg.party.in(params.P) and cfg.sid.in(params.SIDs)
    }    


   pure def sig_get_all_response_messages(messages: Set[SIG_Message]): Set[SIG_Message] = {
        messages.filterMap(m => {
        match m {
            | SIG_GenResponse(p) => Some(SIG_GenResponse(p))
            | SIG_SignResponse(p) => Some(SIG_SignResponse(p))
            | SIG_VerifyResponse(p) => Some(SIG_VerifyResponse(p))  
            | _ => None
        }
        })
   }


        pure def sig_get_gen_request_messages(messages: Set[SIG_Message]): Set[{cfg: MsgConfig}] = {
            messages.filterMap(m => {
            match m {
                | SIG_GenRequest(p) => Some(p)
                | _ => None
            }
            })
        }

        pure def sig_get_sim_gen_hold_messages(messages: Set[SIG_Message]): Set[{cfg: MsgConfig}] = {
            messages.filterMap(m => {
            match m {
                | SIG_SIMGenHold(p) => Some(p)
                | _ => None
            }
            })
        }

        pure def sig_get_sim_gen_release_messages(messages: Set[SIG_Message]): Set[{cfg: MsgConfig, vk: SIG_VK}] = {
            messages.filterMap(m => {
            match m {
                | SIG_SIMGenRelease(p) => Some(p)
                | _ => None
            }
            })
        }

        pure def sig_get_gen_response_messages(messages: Set[SIG_Message]): Set[{cfg: MsgConfig, vk: SIG_VK}] = {
            messages.filterMap(m => {
            match m {
                | SIG_GenResponse(p) => Some(p)
                | _ => None
            }
            })
        }

        pure def sig_get_sign_request_messages(messages: Set[SIG_Message]): Set[{cfg: MsgConfig, m: SIG_Document}] = {
            messages.filterMap(m => {
            match m {
                | SIG_SignRequest(p) => Some(p)
                | _ => None
            }
            })
        }

        pure def sig_get_sim_sign_hold_messages(messages: Set[SIG_Message]): Set[{cfg: MsgConfig, m: SIG_Document}] = {
            messages.filterMap(m => {
            match m {
                | SIG_SIMSignHold(p) => Some(p)
                | _ => None
            }
            })
        }

        pure def sig_get_sim_sign_release_messages(messages: Set[SIG_Message]): Set[{cfg: MsgConfig, m: SIG_Document, sig: SIG_Signature}] = {
            messages.filterMap(m => {
            match m {
                | SIG_SIMSignRelease(p) => Some(p)
                | _ => None
            }
            })
        }

        pure def sig_get_sign_response_messages(messages: Set[SIG_Message]): Set[{cfg: MsgConfig, m: SIG_Document, sig: Option[SIG_Signature]}] = {
            messages.filterMap(m => {
            match m {
                | SIG_SignResponse(p) => Some(p)
                | _ => None
            }
            })
        }

        pure def sig_get_verify_request_messages(messages: Set[SIG_Message]): Set[{cfg: MsgConfig, vk: SIG_VK, m: SIG_Document, sig: SIG_Signature}] = {
            messages.filterMap(m => {
            match m {
                | SIG_VerifyRequest(p) => Some(p)
                | _ => None
            }
            })
        }

        pure def sig_get_sim_verify_hold_messages(messages: Set[SIG_Message]): Set[{cfg: MsgConfig, vk: SIG_VK, m: SIG_Document, sig: SIG_Signature}] = {
            messages.filterMap(m => {
            match m {
                | SIG_SIMVerifyHold(p) => Some(p)
                | _ => None
            }
            })
        }

        pure def sig_get_sim_release_verify_messages(messages: Set[SIG_Message]): Set[{cfg: MsgConfig, vk: SIG_VK, m: SIG_Document, sig: SIG_Signature, b: bool}] = {
            messages.filterMap(m => {
            match m {
                | SIG_SIMReleaseVerify(p) => Some(p)
                | _ => None
            }
            })
        }

        pure def sig_get_verify_response_messages(messages: Set[SIG_Message]): Set[{cfg: MsgConfig, vk: SIG_VK, m: SIG_Document, sig: SIG_Signature, b: bool}] = {
            messages.filterMap(m => {
            match m {
                | SIG_VerifyResponse(p) => Some(p)
                | _ => None
            }
            })
        } 

        pure def sig_get_sim_control_messages(messages: Set[SIG_Message]): Set[SIG_Message] = {
            messages.filter(m => {
            match m {
                | SIG_SIMControl => true
                | _ => false
            }
            })
        }        

    def sig_apply_effect(sys: SIG_System, eff: SIG_Effect): SIG_System = {
        match eff {
        | SIG_LogMessage(msg) => 
            {...sys, 
                log: sys.log.setAdd({idx: sys.log.size(), msg: msg}) 
            }
        | SIG_UpdateInput(msgs) => 
            {...sys, 
                input: msgs
            }
        | SIG_AppendHandle(msg) => 
            {...sys,
                handles: sys.handles.union(Set(msg)),
            }
        | SIG_RemoveHandle(handle_to_remove) =>
            {...sys,
                handles: sys.handles.exclude(Set(handle_to_remove))
            }
        }
    }        

}