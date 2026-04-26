module f_atms_types {
    
    import basicSpells.* from "../../spells/basicSpells"
    import common.* from "../common"
    import params.* from "../params"

    // define types of the system  
    type ATMS_VK = int
    type ATMS_AVK = int
    type ATMS_Document = {h: int, avk: int} 
    type ATMS_Signature = int
    type ATMS_ASignature = int

    type ATMS_SystemParams = {
        P : Set[Party], 
        C : Set[Party], 
        SIDs: Set[SID], 
        t: int,

        responsive_simulator: bool,
        enable_logging: bool,
    }        

    type ATMS_StateVars = {
        VK: Set[{party: Party, vk: ATMS_VK}],
        VER: Set[{vk: ATMS_VK, m: ATMS_Document, sig: ATMS_Signature, b: bool}],
        VKs: Set[{avk: ATMS_AVK, vks: List[ATMS_VK]}],
        AVK: Set[{vks: List[ATMS_VK], avk: Option[ATMS_AVK]}], 
        AVER: Set[{avk: ATMS_AVK, m: ATMS_Document, sig: ATMS_ASignature, b: bool}],
        Signers: Set[{m: ATMS_Document, vk: ATMS_VK}], 
        CVK: Set[ATMS_VK]
    }    

    type ATMS_Message =

        | GenRequest({cfg: MsgConfig}) 
        | SIMGenHold({cfg: MsgConfig})
        | SIMGenRelease({cfg: MsgConfig, sim_vk: Option[ATMS_VK]})
        | GenResponse({cfg: MsgConfig, vk: ATMS_VK})

        | AKeyRequest({cfg: MsgConfig, vks: List[ATMS_VK]})
        | SIMAKeyHold({cfg: MsgConfig, vks: List[ATMS_VK]})
        | SIMAKeyRelease({cfg: MsgConfig, vks: List[ATMS_VK], sim_avk: Option[ATMS_AVK]})
        | AKeyResponse({cfg: MsgConfig, vks: List[ATMS_VK], avk: Option[ATMS_AVK]})

        | ACheckRequest({cfg: MsgConfig, vks: List[ATMS_VK], avk: ATMS_AVK})
        | SIMACheckHold({cfg: MsgConfig, vks: List[ATMS_VK], avk: ATMS_AVK})
        | SIMACheckRelease({cfg: MsgConfig, vks: List[ATMS_VK], avk: ATMS_AVK, sim_avk: Option[ATMS_AVK]})
        | ACheckResponse({cfg: MsgConfig, vks: List[ATMS_VK], avk: ATMS_AVK, b: bool})            

        | SignRequest({cfg: MsgConfig, m: ATMS_Document})
        | SIMSignHold({cfg: MsgConfig, m: ATMS_Document})
        | SIMSignRelease({cfg: MsgConfig, m: ATMS_Document, sim_sig: Option[ATMS_Signature]})
        | SignResponse({cfg: MsgConfig, m: ATMS_Document, sig: Option[ATMS_Signature]})        

        | VerifyRequest({cfg: MsgConfig, vk: ATMS_VK, m: ATMS_Document, sig: ATMS_Signature})
        | SIMVerifyHold({cfg: MsgConfig, vk: ATMS_VK, m: ATMS_Document, sig: ATMS_Signature})
        | SIMVerifyRelease({cfg: MsgConfig, vk: ATMS_VK, m: ATMS_Document, sig: ATMS_Signature, sim_b: bool})
        | VerifyResponse({cfg: MsgConfig, vk: ATMS_VK, m: ATMS_Document, sig: ATMS_Signature, b: bool})        
 
        | ASignRequest({cfg: MsgConfig, m: ATMS_Document, vks: List[ATMS_VK], S: List[{vk:ATMS_VK, sig: ATMS_Signature}]})
        | SIMASignHold({cfg: MsgConfig, m: ATMS_Document, vks: List[ATMS_VK], S: List[{vk:ATMS_VK, sig: ATMS_Signature}]})
        | SIMASignRelease({cfg: MsgConfig, m: ATMS_Document, vks: List[ATMS_VK], S: List[{vk:ATMS_VK, sig: ATMS_Signature}], sim_avk: Option[ATMS_AVK], sim_tau: Option[ATMS_ASignature], sim_bs : List[bool]})
        | ASignResponse({cfg: MsgConfig, m: ATMS_Document, vks: List[ATMS_VK], S: List[{vk:ATMS_VK, sig: ATMS_Signature}], avk: Option[ATMS_AVK], bs : List[bool], tau: Option[ATMS_ASignature]})

        | AVerifyRequest({cfg: MsgConfig, avk: ATMS_AVK, m: ATMS_Document, sig: ATMS_ASignature})
        | SIMAVerifyHold({cfg: MsgConfig, avk: ATMS_AVK, m: ATMS_Document, sig: ATMS_ASignature})
        | SIMAVerifyRelease({cfg: MsgConfig, avk: ATMS_AVK, m: ATMS_Document, sig: ATMS_ASignature, sim_b: bool, sim_vks: Option[List[ATMS_VK]]})
        | AVerifyResponse({cfg: MsgConfig, avk: ATMS_AVK, m: ATMS_Document, sig: ATMS_ASignature, b: bool})                

        | SIMControl   


    type ATMS_System = {
        params : ATMS_SystemParams,
        state : SID -> ATMS_StateVars,
        input : Set[ATMS_Message], 
        nonce : Nonce,        
        log : Set[{idx: int, msg: ATMS_Message, state: SID -> ATMS_StateVars }],
        handles : Set[ATMS_Message],
    }  

   type ATMS_Effect = 
      | ATMS_LogMessage(ATMS_Message)
      | ATMS_UpdateInput(Set[ATMS_Message])
      | ATMS_AppendHandle(ATMS_Message)
      | ATMS_RemoveHandle(ATMS_Message)   

    type ATMS_Transition = {
        post_state: SID -> ATMS_StateVars,
        effects: Set[ATMS_Effect]
    }    

    def atms_apply_effect(sys: ATMS_System, eff: ATMS_Effect): ATMS_System = {
        
        match eff {
        | ATMS_LogMessage(msg) => 
            {...sys, 
                log: if(sys.params.enable_logging) sys.log.setAdd({idx: sys.log.size(), msg: msg, state: sys.state}) else sys.log, 
            }
        | ATMS_UpdateInput(msgs) => 
            {...sys, 
                input: msgs,
                log: if(sys.params.enable_logging and msgs.atms_get_all_response_messages().size() == 1) sys.log.setAdd({idx: sys.log.size(), msg: msgs.pickSome(), state: sys.state}) else sys.log,
            }
        | ATMS_AppendHandle(msg) => 
            {...sys,
                handles: sys.handles.union(Set(msg)),
            }
        | ATMS_RemoveHandle(handle_to_remove) =>
            {...sys,
                handles: sys.handles.exclude(Set(handle_to_remove))
            }
        }
    }

    def atms_process_transitions(sys: ATMS_System, transition: ATMS_Transition) : ATMS_System = {
        val new_sys = {...sys, state: transition.post_state}
        transition.effects.fold(new_sys, ((a1,a2) => atms_apply_effect(a1,a2)))
    }         

    pure def atms_access_check(cfg: MsgConfig, params: ATMS_SystemParams): bool = {
            cfg.party.in(params.P) and cfg.sid.in(params.SIDs)
    }  

    pure def atms_get_all_response_messages(messages: Set[ATMS_Message]): Set[ATMS_Message] = {
        messages.filterMap(m => {
        match m {
            | GenResponse(p) => Some(GenResponse(p))
            | AKeyResponse(p) => Some(AKeyResponse(p))
            | ACheckResponse(p) => Some(ACheckResponse(p))
            | SignResponse(p) => Some(SignResponse(p))
            | VerifyResponse(p) => Some(VerifyResponse(p))
            | ASignResponse(p) => Some(ASignResponse(p))
            | AVerifyResponse(p) => Some(AVerifyResponse(p))
            | _ => None
        }
        })
    }

    // pure def atms_get_gen_request_messages(messages: Set[ATMS_Message]): Set[{cfg: MsgConfig}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | GenRequest(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def atms_get_sim_gen_hold_messages(messages: Set[ATMS_Message]): Set[{cfg: MsgConfig}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | SIMGenHold(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def atms_get_sim_gen_release_messages(messages: Set[ATMS_Message]): Set[{cfg: MsgConfig, sim_vk: Option[ATMS_VK]}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | SIMGenRelease(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    pure def atms_get_gen_response_messages(messages: Set[ATMS_Message]): Set[{cfg: MsgConfig, vk: ATMS_VK}] = {
        messages.filterMap(m => {
        match m {
            | GenResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    // pure def atms_get_akey_request_messages(messages: Set[ATMS_Message]): Set[{cfg: MsgConfig, vks: List[ATMS_VK]}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | AKeyRequest(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def atms_get_sim_akey_hold_messages(messages: Set[ATMS_Message]): Set[{cfg: MsgConfig, vks: List[ATMS_VK]}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | SIMAKeyHold(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def atms_get_sim_akey_release_messages(messages: Set[ATMS_Message]): Set[{cfg: MsgConfig, vks: List[ATMS_VK], sim_avk: Option[ATMS_AVK]}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | SIMAKeyRelease(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    pure def atms_get_akey_response_messages(messages: Set[ATMS_Message]): Set[{cfg: MsgConfig, vks: List[ATMS_VK], avk: Option[ATMS_AVK]}] = {
        messages.filterMap(m => {
        match m {
            | AKeyResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    // pure def atms_get_acheck_request_messages(messages: Set[ATMS_Message]): Set[{cfg: MsgConfig, vks: List[ATMS_VK], avk: ATMS_AVK}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | ACheckRequest(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def atms_get_sim_acheck_hold_messages(messages: Set[ATMS_Message]): Set[{cfg: MsgConfig, vks: List[ATMS_VK], avk: ATMS_AVK}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | SIMACheckHold(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def atms_get_sim_acheck_release_messages(messages: Set[ATMS_Message]): Set[{cfg: MsgConfig, vks: List[ATMS_VK], avk: ATMS_AVK, sim_avk: Option[ATMS_AVK]}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | SIMACheckRelease(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    pure def atms_get_acheck_response_messages(messages: Set[ATMS_Message]): Set[{cfg: MsgConfig, vks: List[ATMS_VK], avk: ATMS_AVK, b: bool}] = {
        messages.filterMap(m => {
        match m {
            | ACheckResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    // pure def atms_get_sign_request_messages(messages: Set[ATMS_Message]): Set[{cfg: MsgConfig, m: ATMS_Document}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | SignRequest(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def atms_get_sim_sign_hold_messages(messages: Set[ATMS_Message]): Set[{cfg: MsgConfig, m: ATMS_Document}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | SIMSignHold(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def atms_get_sim_sign_release_messages(messages: Set[ATMS_Message]): Set[{cfg: MsgConfig, m: ATMS_Document, sim_sig: Option[ATMS_Signature]}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | SIMSignRelease(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    pure def atms_get_sign_response_messages(messages: Set[ATMS_Message]): Set[{cfg: MsgConfig, m: ATMS_Document, sig: Option[ATMS_Signature]}] = {
        messages.filterMap(m => {
        match m {
            | SignResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    // pure def atms_get_verify_request_messages(messages: Set[ATMS_Message]): Set[{cfg: MsgConfig, vk: ATMS_VK, m: ATMS_Document, sig: ATMS_Signature}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | VerifyRequest(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def atms_get_sim_verify_hold_messages(messages: Set[ATMS_Message]): Set[{cfg: MsgConfig, vk: ATMS_VK, m: ATMS_Document, sig: ATMS_Signature}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | SIMVerifyHold(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def atms_get_sim_verify_release_messages(messages: Set[ATMS_Message]): Set[{cfg: MsgConfig, vk: ATMS_VK, m: ATMS_Document, sig: ATMS_Signature, sim_b: bool}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | SIMVerifyRelease(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    pure def atms_get_verify_response_messages(messages: Set[ATMS_Message]): Set[{cfg: MsgConfig, vk: ATMS_VK, m: ATMS_Document, sig: ATMS_Signature, b: bool}] = {
        messages.filterMap(m => {
        match m {
            | VerifyResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    // pure def atms_get_asign_request_messages(messages: Set[ATMS_Message]): Set[{cfg: MsgConfig, m: ATMS_Document, vks: List[ATMS_VK], S: List[{vk:ATMS_VK, sig: ATMS_Signature}]}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | ASignRequest(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def atms_get_sim_asign_hold_messages(messages: Set[ATMS_Message]): Set[{cfg: MsgConfig, m: ATMS_Document, vks: List[ATMS_VK], S: List[{vk:ATMS_VK, sig: ATMS_Signature}]}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | SIMASignHold(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def atms_get_sim_asign_release_messages(messages: Set[ATMS_Message]): Set[{cfg: MsgConfig, m: ATMS_Document, vks: List[ATMS_VK], S: List[{vk:ATMS_VK, sig: ATMS_Signature}], sim_avk: Option[ATMS_AVK], sim_tau: Option[ATMS_ASignature], sim_bs : List[bool]}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | SIMASignRelease(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    pure def atms_get_asign_response_messages(messages: Set[ATMS_Message]): Set[{cfg: MsgConfig, m: ATMS_Document, vks: List[ATMS_VK], S: List[{vk:ATMS_VK, sig: ATMS_Signature}], avk: Option[ATMS_AVK], bs : List[bool], tau: Option[ATMS_ASignature]}] = {
        messages.filterMap(m => {
        match m {
            | ASignResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    // pure def atms_get_averify_request_messages(messages: Set[ATMS_Message]): Set[{cfg: MsgConfig, avk: ATMS_AVK, m: ATMS_Document, sig: ATMS_ASignature}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | AVerifyRequest(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def atms_get_sim_averify_hold_messages(messages: Set[ATMS_Message]): Set[{cfg: MsgConfig, avk: ATMS_AVK, m: ATMS_Document, sig: ATMS_ASignature}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | SIMAVerifyHold(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def atms_get_sim_averify_release_messages(messages: Set[ATMS_Message]): Set[{cfg: MsgConfig, avk: ATMS_AVK, m: ATMS_Document, sig: ATMS_ASignature, sim_b: bool, sim_vks: Option[List[ATMS_VK]]}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | SIMAVerifyRelease(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // } 

    pure def atms_get_averify_response_messages(messages: Set[ATMS_Message]): Set[{cfg: MsgConfig, avk: ATMS_AVK, m: ATMS_Document, sig: ATMS_ASignature, b: bool}] = {
        messages.filterMap(m => {
        match m {
            | AVerifyResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    // pure def atms_get_sim_control_messages(messages: Set[ATMS_Message]): Set[ATMS_Message] = {
    //     messages.filter(m => {
    //     match m {
    //         | SIMControl => true
    //         | _ => false
    //     }
    //     })
    // }


}