module g_ledger_types{
    
    import basicSpells.* from "../../spells/basicSpells"
    import g_clock_types.* from "../g_clock/g_clock_types"  
    import common.* from "../common"

    // define types of the system  
    type LED_Msg = {h : int, avk: int}
    type LED_Tx = 
        | Normal({tx: int, b: bool})
        | Install({cID: int, code: int})
        | Update({msg: LED_Msg, sigma: int})

    type Pointer = int
    //type TX = int
    type LED_ValidatorState = bool // (List[Party], TX) => bool
    type LED_Log = List[{tx:LED_Tx, time: Time}] // mentally this is a list of (tx, time) pairs indexed by Pointer
    type CommitteeMember = int
    type Committee = List[CommitteeMember]
    type LED_Block = int
    type LED_Buffer = Pointer -> {tx: LED_Tx, time: Time}

    type LED_Message =

        | SubmitRequest({cfg: MsgConfig, tx: LED_Tx}) 
        | SubmitTimeRequestHold({cfg: MsgConfig, tx:LED_Tx})
        | SubmitSIMHold({cfg: MsgConfig, tx:LED_Tx, eID: Pointer, t: Time})
        | SubmitSIMRelease({cfg: MsgConfig, tx:LED_Tx, eID: Pointer, t: Time})
        | SubmitResponse({cfg: MsgConfig, tx:LED_Tx, eID: Pointer, t: Time})
        | OKRequest({cfg: MsgConfig})
        | OKRequestWait({cfg: MsgConfig})
        | OKResponse({cfg: MsgConfig})

        | ReadRequest({cfg: MsgConfig})
        | ReadSIMHold({cfg: MsgConfig})
        | ReadSIMRelease({cfg: MsgConfig, I : List[Pointer], ptr_prim : Pointer})        
        | ReadTimeRequestHold({cfg: MsgConfig, I : List[Pointer], ptr_prim : Pointer })
        | ReadLoopComputation({cfg: MsgConfig, I : List[Pointer], ptr_prim : Pointer, buffer: LED_Buffer, log: LED_Log, t: Time})
        | ReadLoopComputationFinish({cfg: MsgConfig, ptr_prim : Pointer, buffer: LED_Buffer, log: LED_Log, t: Time})
        | ReadResponse({cfg: MsgConfig, result: List[LED_Tx]})

        // EXTENSIONS
        | ComSelRequest({cfg: MsgConfig})
        | ComSelSIMHold({cfg: MsgConfig, r: Time})
        | ComSelSIMRelease({cfg: MsgConfig, r: Time, sim_com: List[CommitteeMember]})
        | ComSelResponse({cfg: MsgConfig, r: Time, com: List[CommitteeMember]})

        | APLRequest({cfg: MsgConfig})
        | APLSIMHold({cfg: MsgConfig, r: Time})
        | APLSIMRelease({cfg: MsgConfig, r: Time, sim_p: Pointer})
        | APLResponse({cfg: MsgConfig, r: Time, p: Pointer})

        | GenesisRequest({cfg: MsgConfig})
        | GenesisSIMHold({cfg: MsgConfig})
        | GenesisSIMRelease({cfg: MsgConfig})
        | GenesisResponse({cfg: MsgConfig, genesis: LED_Block})

        | EvalRequest({cfg: MsgConfig, log: List[LED_Tx], tx: LED_Tx})
        | EvalAverWait({cfg: MsgConfig, log: List[LED_Tx], tx: LED_Tx, cID : int, avk: int, msg: LED_Msg})
        | EvalResponse({cfg: MsgConfig, log: List[LED_Tx], tx: LED_Tx, b: bool})

        | LED_SIMControl      
    

    type LED_SystemParams = {
        P : Set[Party], 
        C : Set[Party], 
        SIDs: Set[SID], 

        delta_latency: Time,
        delta_slack: Time,
        delta_apl: Time,

        comsel_a: int,
        comsel_b: int,
        comsel_c: int,  

        enable_logging: bool,
        responsive_simulator: bool,
    }    


    type LED_StateVars = {
        buffer: LED_Buffer,
        eID: Pointer,
        log: LED_Log,
        ptr: Party -> Pointer,
        Val: LED_ValidatorState,
        Com: Time -> Committee,  // ComSel state
        APL: Time -> Pointer,  // APL state
        genesis: LED_Block,     // genesis state
        mem: int -> {hs: List[int], avk: Option[int]} // eval state Hash_Output -> {hs: List[Hash_Out], avk: option ATMS_AVK}        
    }


    type LED_System = {
        params : LED_SystemParams,
        state : LED_StateVars,
        input : Set[LED_Message], 
        nonce : Nonce,        
        log : Set[{idx: int, msg: LED_Message, time : Time, state: LED_StateVars,  }], // state: LED_StateVars
        handles : Set[LED_Message],
    }  


   type LED_Effect = 
      | LED_LogMessage(LED_Message)
      | LED_UpdateInput(Set[LED_Message])
      | LED_AppendHandle(LED_Message)
      | LED_RemoveHandle(LED_Message)   


    type LED_Transition = {
        post_state: LED_StateVars,
        effects: Set[LED_Effect]
    }    

    pure def led_id_trans(led : LED_System) : LED_Transition = {
        post_state : led.state,
        effects : Set(),
    } 

    pure def led_skip_trans(led: LED_System) : LED_Transition = {
        {
            post_state: led.state,
            effects : Set(
                LED_UpdateInput(Set())
            )
        }
    }        

    pure def led_access_check(cfg: MsgConfig, params: LED_SystemParams): bool = {
            cfg.party.in(params.P) and cfg.sid.in(params.SIDs)
    }    


    pure def led_apply_effect(sys: LED_System, clock: GC_System, eff: LED_Effect): LED_System = {
        match eff {
        | LED_LogMessage(msg) => 
            {...sys, 
                log: if (sys.params.enable_logging) sys.log.setAdd({idx: sys.log.size(), msg: msg,  time: clock.state.TIME, state: sys.state}) else sys.log
            }
        | LED_UpdateInput(msgs) => 
            {...sys, 
                input: msgs,
                log: if (sys.params.enable_logging and msgs.led_get_all_response_messages().size() == 1) sys.log.setAdd({idx: sys.log.size(), msg: msgs.pickSome(), time: clock.state.TIME, state: sys.state}) else sys.log
            }
        | LED_AppendHandle(msg) => 
            {...sys,
                handles: sys.handles.union(Set(msg)),
            }
        | LED_RemoveHandle(handle_to_remove) =>
            {...sys,
                handles: sys.handles.exclude(Set(handle_to_remove))
            }
        }
    }

    pure def led_get_all_response_messages(messages: Set[LED_Message]): Set[LED_Message] = {
        messages.filterMap(m => {
        match m {
            | SubmitResponse(p) => Some(SubmitResponse(p))
            | OKResponse(p) => Some(OKResponse(p))
            | ReadResponse(p) => Some(ReadResponse(p))  
            | ComSelResponse(p) => Some(ComSelResponse(p))
            | APLResponse(p) => Some(APLResponse(p))
            | GenesisResponse(p) => Some(GenesisResponse(p))
            | _ => None
        }
        })
    }    
    pure def led_get_normal_tx(tx: LED_Tx): Set[{tx: int, b: bool}] = {
        match tx {
            | Normal(p) => Set(p)
            | _ => Set()
        }
    }


    pure def led_get_install_tx(tx: LED_Tx): Set[{cID: int, code: int}] = {
        match tx {
            | Install(p) => Set(p)
            | _ => Set()
        }
    }

    pure def led_get_update_tx(tx: LED_Tx): Set[{msg: LED_Msg, sigma: int}] = {
        match tx {
            | Update(p) => Set(p)
            | _ => Set()
        }
    }
    pure def led_get_eval_request_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig, log: List[LED_Tx], tx: LED_Tx}] = {
        messages.filterMap(m => {
        match m {
            | EvalRequest(p) => Some(p)
            | _ => None
        }
        })
    }

    // pure def led_get_read_loop_computation_messages(messages: Set[LED_Message]) : Set[{cfg: MsgConfig, I : List[Pointer], ptr_prim : Pointer, buffer: LED_Buffer, log: LED_Log, t: Time}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | ReadLoopComputation(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def led_get_read_loop_computation_finish_messages(messages: Set[LED_Message]) : Set[{cfg: MsgConfig, buffer: LED_Buffer, ptr_prim : Pointer, log: LED_Log, t: Time}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | ReadLoopComputationFinish(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }    


    // pure def led_get_eval_aver_wait_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig, log: List[LED_Tx], tx: LED_Tx, cID : int, avk: int, msg: LED_Msg}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | EvalAverWait(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def led_get_eval_response_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig, log: List[LED_Tx], tx: LED_Tx, b: bool}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | EvalResponse(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }


    // pure def led_get_genesis_request_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | GenesisRequest(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def led_get_genesis_sim_hold_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | GenesisSIMHold(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def led_get_genesis_sim_release_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | GenesisSIMRelease(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def led_get_genesis_response_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig, genesis: LED_Block}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | GenesisResponse(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }
    // pure def led_get_apl_request_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | APLRequest(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def led_get_apl_sim_hold_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig, r: Time}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | APLSIMHold(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def led_get_apl_sim_release_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig, r: Time, sim_p: Pointer}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | APLSIMRelease(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def led_get_apl_response_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig, r: Time, p: Pointer}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | APLResponse(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def led_get_submit_request_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig, tx: LED_Tx}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | SubmitRequest(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def led_get_comsel_request_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | ComSelRequest(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def led_get_comsel_sim_hold_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig, r: Time}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | ComSelSIMHold(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def led_get_comsel_sim_release_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig, r: Time, sim_com: List[CommitteeMember]}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | ComSelSIMRelease(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def led_get_comsel_response_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig, r: Time, com: List[CommitteeMember]}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | ComSelResponse(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }


    // pure def led_get_submit_time_request_hold_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig, tx:LED_Tx}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | SubmitTimeRequestHold(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def led_get_submit_sim_hold_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig, tx:LED_Tx, eID: Pointer, t: Time}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | SubmitSIMHold(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def led_get_submit_sim_release_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig, tx:LED_Tx, eID: Pointer, t: Time}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | SubmitSIMRelease(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    pure def led_get_submit_response_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig, tx:LED_Tx, eID: Pointer, t: Time}] = {
        messages.filterMap(m => {
        match m {
            | SubmitResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    // pure def led_get_ok_request_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | OKRequest(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def led_get_ok_request_wait_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | OKRequestWait(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }



    // pure def led_get_ok_response_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | OKResponse(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def led_get_read_request_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | ReadRequest(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def led_get_read_sim_hold_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | ReadSIMHold(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def led_get_read_sim_release_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig, I : List[Pointer], ptr_prim : Pointer}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | ReadSIMRelease(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    // pure def led_get_read_time_request_hold_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig, I : List[Pointer], ptr_prim : Pointer}] = {
    //     messages.filterMap(m => {
    //     match m {
    //         | ReadTimeRequestHold(p) => Some(p)
    //         | _ => None
    //     }
    //     })
    // }

    pure def led_get_read_response_messages(messages: Set[LED_Message]): Set[{cfg: MsgConfig, result: List[LED_Tx]}] = {
        messages.filterMap(m => {
        match m {
            | ReadResponse(p) => Some(p)
            | _ => None
        }
        })
    }

    // pure def led_get_sim_control_messages(messages: Set[LED_Message]): Set[LED_Message] = {
    //     messages.filter(m => {
    //     match m {
    //         | LED_SIMControl => true
    //         | _ => false
    //     }
    //     })
    // }


    
}