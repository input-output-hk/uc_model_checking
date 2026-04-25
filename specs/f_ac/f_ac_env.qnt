module f_ac_env{
    
    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"
    import common.* from "../common"
    import g_clock_types.* from "../g_clock/g_clock_types"
    import g_clock.* from "../g_clock/g_clock"    
    import f_ac_types.*   from "./f_ac_types"
    import f_ac.*  from "./f_ac"

  
    val Parties = 1.to(PARTY_SIZE)
    val Corrupted = 1.to(CORRUPTED_PARTY_SIZE)
    val sids = 1.to(SID_SIZE)
    val delta_param: int = 2

    action step : bool = if (f_ac_s.log.size() < MAX_LOG_SIZE) {
        any {
            fac_step,
            gc_step_fac_wait,
            env_fac_send_all_reqs,
            env_consume_responses,
        }
    } else {
        all {
            f_ac_s' = f_ac_s,
            g_clock_s' = g_clock_s,
        }
    }

    action gc_step_fac_wait : bool =  all {
        f_ac_s' = f_ac_s,
        gc_step,        
    }

    action init = {
        val clock_regs = sids.map(sid => Parties.map(party => ({party: party, sid: sid}))).flatten()

        all {
            fac_init(Parties, Corrupted, delta_param, sids, RESPONSIVE_SIMULATOR, true),
            gc_init({C: Corrupted, time: 0, Reg: clock_regs, responsive_simulator: RESPONSIVE_SIMULATOR, enable_logging: false})
        }
    }

    action env_fac_send_all_reqs : bool = all {
        nondet from = Parties.oneOf()
        nondet top = Parties.oneOf()
        nondet sid = sids.oneOf()
        val cfg = {party: from, sid: sid, nonce: f_ac_s.nonce}

        any {
            env_fac_send_reqs(cfg, top),
            env_fac_fetch_reqs(cfg),
            env_ok_reqs(cfg),
            env_fac_sim_reqs,
            //env_gc_sim_reqs,
        }
        
    }

 
    // // Environment interaction: non-deterministically add requests when input is empty
    action env_fac_send_reqs(cfg:MsgConfig, top: Party) : bool = all {
        // Precondition: input must be empty
        g_clock_s.input.size() == 0 and f_ac_s.input.size() == 0,
        
        // Non-deterministically choose a party and SID
        nondet data = 1.to(UNIVERSE_SIZE).oneOf()
        f_ac_s' = {...f_ac_s, 
            input: Set(SendRequest({cfg: cfg, Q: top, x: data})),
            nonce: f_ac_s.nonce + NONCE_INCREMENT

        },
        gc_stutter
    }




    action env_fac_fetch_reqs(cfg: MsgConfig) : bool = all {
        // Precondition: input must be empty
        g_clock_s.input.size() == 0 and f_ac_s.input.size() == 0,

        // Option 2: Fetch
        f_ac_s' = {...f_ac_s, 
            input: Set(FetchRequest({cfg: cfg})),
            nonce: f_ac_s.nonce + NONCE_INCREMENT

        },
        gc_stutter   
    }

    action env_ok_reqs(cfg: MsgConfig) : bool = all {
        // Precondition: input must be empty
        g_clock_s.input.size() == 0 and f_ac_s.input.size() == 0,

        // Option 3: OK
        f_ac_s' = {...f_ac_s, 
            input: Set(OKRequest({cfg: cfg})),
            nonce: f_ac_s.nonce + NONCE_INCREMENT

        },   
        gc_stutter     
    }

    action env_fac_sim_reqs : bool = all {
        g_clock_s.input.size() == 0 and f_ac_s.input.size() == 0,
        f_ac_s' = {...f_ac_s, 
            input: Set(SIMControl),
            nonce: f_ac_s.nonce + NONCE_INCREMENT

        },
        gc_stutter
    }

    action env_gc_sim_reqs : bool = all {
        g_clock_s.input.size() == 0 and f_ac_s.input.size() == 0,
        g_clock_s' = {...g_clock_s, 
            input: Set(GC_SIMControl),
            nonce: g_clock_s.nonce + NONCE_INCREMENT
        },
        f_ac_s' = f_ac_s,
    }

    pure def env_consume_response(sys: FAC_System) : Set[FAC_Transition] = {
         sys.input.fac_get_all_response_messages().map(msg_log =>
         {
            post_state : sys.state,
            effects : Set(
                FAC_LogMessage(msg_log),
                FAC_UpdateInput(Set())
            )
         })
    }

    action env_consume_responses : bool =  all {
        // Precondition: there is a response message in input
        val response_msgs = env_consume_response(f_ac_s)
        
        // Non-deterministically choose one response message to consume
        nondet transition = response_msgs.oneOf()

        // Update the system state by consuming the message    
        f_ac_s' = fac_process_transitions(f_ac_s, g_clock_s, transition),
        gc_stutter
    }

}