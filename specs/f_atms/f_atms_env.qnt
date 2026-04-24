module f_atms_env{
    
    import basicSpells.* from "../../spells/basicSpells"
    import f_atms_types.* from "./f_atms_types"
    import f_atms.* from "./f_atms"
    import common.* from "../common"    

    val Parties = 1.to(PARTY_SIZE)
    val Corrupted = 1.to(CORRUPTED_PARTY_SIZE)
    val sids = 1.to(SID_SIZE)
    val threshold = 1

    val ALL_DOC = 1.to(UNIVERSE_SIZE).map(i => {h: i, avk: i})
    val list_length = PARTY_SIZE

    action init : bool = all {
        atms_init({ 
            P: Parties,
            C: Corrupted,
            SIDs: sids,
            t: threshold,
            responsive_simulator: true,
            enable_logging: true,
        })
    }

   action step : bool =  {
        if(f_atms_s.log.size() < MAX_LOG_SIZE){
            any {
                atms_step,  
                env_send_all_reqs,
                env_consume_responses,
            }
        }else{
            f_atms_s' = f_atms_s
        }
    }


    pure def env_consume_response(sys: ATMS_System) : Set[ATMS_Transition] = {
         sys.input.atms_get_all_response_messages().map(msg_log =>
         {
            post_state : sys.state,
            effects : Set(
                ATMS_UpdateInput(Set())
            )
         })
    }        

    action env_consume_responses : bool =  all {
        // Precondition: there is a response message in input
        val response_msgs = env_consume_response(f_atms_s)
        
        all {
        // Non-deterministically choose one response message to consume
            nondet transition = response_msgs.oneOf()

        // Update the system state by consuming the message    
            f_atms_s' = atms_process_transitions(f_atms_s, transition)
        }
   }        

    action env_send_all_reqs : bool = all {
        nondet party = Parties.oneOf()
        nondet sid = sids.oneOf()
        val cfg = {party: party, sid: sid, nonce: f_atms_s.nonce}
        any {
            env_gen_reqs(cfg),
            env_akey_reqs(cfg),
            env_acheck_reqs(cfg),
            env_sign_reqs(cfg),
            env_ver_reqs(cfg),
            env_asign_reqs(cfg),
            env_aver_reqs(cfg),
            env_atms_sim_reqs,
        }
    }

    action env_gen_reqs(cfg: MsgConfig) : bool = all {
        // Precondition: input must be empty
        f_atms_s.input.size() == 0,

        f_atms_s' = {...f_atms_s, 
            input: Set(GenRequest({cfg: cfg})),
            nonce: f_atms_s.nonce + 1
        }
    }
 
    action env_akey_reqs(cfg: MsgConfig) : bool = all {
        // Precondition: input must be empty
        f_atms_s.input.size() == 0,
        nondet vks = allUpToListSamplingSpace(list_length, ALL_VKS).oneOf() 
        //listSamplingSpace(list_length, ALL_VK).oneOf()

        f_atms_s' = {...f_atms_s, 
            input: Set(AKeyRequest({cfg: cfg, vks: vks})),
            nonce: f_atms_s.nonce + 1
        }
    }

    action env_acheck_reqs(cfg: MsgConfig) : bool = all {
        // Precondition: input must be empty
        f_atms_s.input.size() == 0,

        nondet avk = ALL_AVKS.oneOf()
        nondet vks = listSamplingSpace(list_length, ALL_VKS).oneOf()

        f_atms_s' = {...f_atms_s, 
            input: Set(ACheckRequest({cfg: cfg, avk: avk, vks: vks})),
            nonce: f_atms_s.nonce + 1
        }
    }

    action env_sign_reqs(cfg: MsgConfig) : bool = all {
        // Precondition: input must be empty
        f_atms_s.input.size() == 0,
        nondet m = ALL_DOC.oneOf()

        f_atms_s' = {...f_atms_s, 
            input: Set(SignRequest({cfg: cfg, m: m})),
            nonce: f_atms_s.nonce + 1
        }
    }

    action env_atms_sim_reqs : bool = all {
        f_atms_s.input.size() == 0,
        f_atms_s.handles.size() > 0,

        f_atms_s' = {...f_atms_s, 
            input: Set(SIMControl),
            nonce: f_atms_s.nonce + 1
        },
    }


    action env_ver_reqs(cfg: MsgConfig) : bool = all {
        // Precondition: input must be empty
        f_atms_s.input.size() == 0,
        nondet m = ALL_DOC.oneOf()
        nondet sig = ALL_SIGS.oneOf()
        nondet vk  = ALL_VKS.oneOf()

        f_atms_s' = {...f_atms_s, 
            input: Set(VerifyRequest({cfg: cfg, vk: vk, m: m, sig: sig})),
            nonce: f_atms_s.nonce + 1
        }
    }

    action env_asign_reqs(cfg: MsgConfig) : bool = all {
        // Precondition: input must be empty
        f_atms_s.input.size() == 0,
        nondet m = ALL_DOC.oneOf()
        nondet vks = listSamplingSpace(list_length, ALL_VKS).oneOf()
        nondet vks_tozip = listSamplingSpace(list_length, ALL_VKS).oneOf()
        nondet sigs_tozip = listSamplingSpace(list_length, ALL_VKS).oneOf()
        val S = vks_tozip.indices().map(i => {vk: vks_tozip[i], sig: sigs_tozip[i]}).setToList()

        f_atms_s' = {...f_atms_s, 
            input: Set(ASignRequest({cfg: cfg, m: m, vks: vks, S: S})),
            nonce: f_atms_s.nonce + 1
        }
    }

    action env_aver_reqs(cfg: MsgConfig) : bool = all {
        // Precondition: input must be empty
        f_atms_s.input.size() == 0,
        nondet m = ALL_DOC.oneOf()
        nondet sig = ALL_SIGS.oneOf()
        nondet avk = ALL_AVKS.oneOf()

        f_atms_s' = {...f_atms_s, 
            input: Set(AVerifyRequest({cfg: cfg, avk: avk, m: m, sig: sig})),
            nonce: f_atms_s.nonce + 1
        }
    }    


}