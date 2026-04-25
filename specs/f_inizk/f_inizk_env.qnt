module f_inizk_env{


    import basicSpells.* from "../../spells/basicSpells"
    import f_inizk_types.* from "./f_inizk_types"
    import f_inizk.* from "./f_inizk"
    import f_hash_types.* from "../f_hash/f_hash_types"
    import g_ledger_types.* from "../g_ledger/g_ledger_types"
    import f_hash.* from "../f_hash/f_hash"
    import common.* from "../common"    

   
   val Parties = 1.to(PARTY_SIZE)
   val Corrupted = 1.to(CORRUPTED_PARTY_SIZE)
   val sids = 1.to(SID_SIZE)


  val ALL_STATEMENTS = 1.to(UNIVERSE_SIZE).map(x => { x : Normal({b: true, tx: x}), h : 49 + x })
  val ALL_WITNESSES = 1.to((UNIVERSE_SIZE + 1) / 2).map(x => { List(Normal({b: true, tx: x})) })

    action step : bool = {
        if(f_nizk_s.log.size() < MAX_LOG_SIZE){
            any {
                nizk_step,  
                all{
                    hash_step,
                    f_nizk_s' = f_nizk_s
                },
                env_send_all_reqs,
                env_consume_responses,
            }
        } else {
            all {
                f_nizk_s' = f_nizk_s,
                f_hash_s' = f_hash_s,
            }
        }
    }


    pure def env_consume_response(sys: NIZK_System) : Set[NIZK_Transition] = {
         sys.input.nizk_get_all_response_messages().map(msg_log =>
         {
            post_state : sys.state,
            effects : Set(
                NIZK_UpdateInput(Set())
            )
         })
    }



    action env_consume_responses : bool =  all {
        // Precondition: there is a response message in input
        val response_msgs = env_consume_response(f_nizk_s)
        
        all {
        // Non-deterministically choose one response message to consume
            nondet transition = response_msgs.oneOf()

        // Update the system state by consuming the message    
            f_nizk_s' = nizk_process_transitions(f_nizk_s, transition),
            f_hash_s' = f_hash_s
        }
   }      

    action env_send_all_reqs : bool = all {
        f_nizk_s.input.size() == 0 and f_hash_s.input.size() == 0,

        nondet party = Parties.oneOf()
        nondet sid = sids.oneOf()
        val cfg = {party: party, sid: sid, nonce: f_nizk_s.nonce}
        any {
            env_prove_reqs(cfg),
            env_verify_reqs(cfg),
            env_sim_reqs,
        },
        f_hash_s' = f_hash_s
        
    }     

    action env_prove_reqs(cfg: MsgConfig) : bool = all {
        // Precondition: input must be empty
        f_nizk_s.input.size() == 0,

        nondet x = ALL_STATEMENTS.oneOf()
        nondet w = ALL_WITNESSES.oneOf()
        env_prove_reqs_args(cfg, x, w)
    }    

    action env_prove_reqs_args(cfg: MsgConfig, x: NIZK_Statement, w: NIZK_Witness) : bool = all {
        f_nizk_s' = {...f_nizk_s, 
            input: Set(NIZK_ProveRequest({cfg: cfg, x: x, w: w}) ),
            nonce: f_nizk_s.nonce + NONCE_INCREMENT
        }
    }

    action env_verify_reqs(cfg: MsgConfig) : bool = all {
        nondet x = ALL_STATEMENTS.oneOf()
        nondet pi = ALL_PROOFS.oneOf()
        env_verify_reqs_args(cfg, x, pi)
    }

    action env_verify_reqs_args(cfg: MsgConfig, x: NIZK_Statement, pi: NIZK_Proof) : bool = all {
        f_nizk_s' = {...f_nizk_s, 
            input: Set(NIZK_VerifyRequest({cfg: cfg, x: x, pi: pi}) ),
            nonce: f_nizk_s.nonce + NONCE_INCREMENT
        }
    }

    action env_sim_reqs: bool = all{
        f_nizk_s.handles.size() > 0,
        f_nizk_s' = {...f_nizk_s,
           input: Set(NIZK_SIMControl),
           nonce: f_nizk_s.nonce + NONCE_INCREMENT
        }
    }

    
    action init = all {
        nizk_init({P: Parties, C: Corrupted, SIDs: sids, responsive_simulator: RESPONSIVE_SIMULATOR, enable_logging: true}),
        hash_init({P: Parties, SIDs: sids, responsive_simulator: true, enable_logging: false}),
    }    

}