module f_dif_env{
    

    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"
    import common.* from "../common"
    import g_clock_types.* from "../g_clock/g_clock_types"
    import g_clock.* from "../g_clock/g_clock"    
    import f_dif_types.*   from "./f_dif_types"
    import f_dif.*  from "./f_dif" 
    

    val Parties = 1.to(PARTY_SIZE)
    val Corrupted = 1.to(CORRUPTED_PARTY_SIZE)
    val sids = 1.to(SID_SIZE)
    val delta_param: int = 1


    action step : bool = any {
        dif_step,  
        gc_step_fac_wait,
        env_fac_send_all_reqs,
        env_consume_responses,
    }

    action gc_step_fac_wait : bool =  all {
        f_dif_s' = f_dif_s,
        gc_step,        
    }

    action init = {
        val clock_regs = sids.map(sid => Parties.map(party => ({party: party, sid: sid}))).flatten()
        all{
            dif_init({P: Parties, C: Corrupted, SIDs: sids, delta: delta_param, enable_logging: true, responsive_simulator: true}),
            gc_init({C: Corrupted,time: 0, Reg: clock_regs, responsive_simulator: true, enable_logging: false}),
        }
    }

    action env_fac_send_all_reqs : bool = all {
        nondet from = Parties.oneOf()
        nondet corrupted = Corrupted.oneOf()
        nondet top = Parties.oneOf()
        nondet sid = sids.oneOf()

        any {
            env_dif_send_reqs(from, top, sid),
            env_dif_inject_reqs(corrupted, top, sid),
            env_dif_fetch_reqs(from, sid),
            env_ok_reqs(from, sid),
            env_dif_sim_reqs,
            env_gc_sim_reqs,
        }
    }

 
    // // Environment interaction: non-deterministically add requests when input is empty
    action env_dif_send_reqs(from:Party, top: Party, sid: SID) : bool = all {
        // Precondition: input must be empty
        g_clock_s.input.size() == 0 and f_dif_s.input.size() == 0,
        
        // Non-deterministically choose a party and SID
        nondet data = 1.to(UNIVERSE_SIZE).oneOf()
        val cfg = {party: from, sid: sid, nonce: f_dif_s.nonce}
        f_dif_s' = {...f_dif_s, 
            input: Set(DIF_DiffuseRequest({cfg: cfg, m: {party: from, avk: top, sig: data}})),
            nonce: f_dif_s.nonce + 1

        },
        gc_stutter
    }

    // // Environment interaction: non-deterministically add requests when input is empty
    action env_dif_inject_reqs(from:Party, top: Party, sid: SID) : bool = all {
        // Precondition: input must be empty
        g_clock_s.input.size() == 0 and f_dif_s.input.size() == 0,
        
        // Non-deterministically choose a party and SID
        nondet data = 1.to(UNIVERSE_SIZE).oneOf()
        val cfg = {party: from, sid: sid, nonce: f_dif_s.nonce}
        f_dif_s' = {...f_dif_s, 
            input: Set(DIF_InjectRequest({cfg: cfg, Q: top, m: {party: from, avk: top, sig: data}})),
            nonce: f_dif_s.nonce + 1

        },
        gc_stutter
    }



    action env_dif_fetch_reqs(from:Party, sid: SID) : bool = all {
        // Precondition: input must be empty
        g_clock_s.input.size() == 0 and f_dif_s.input.size() == 0,
        
        // Non-deterministically choose a party and SID
        val cfg = {party: from, sid: sid, nonce: f_dif_s.nonce}

        // Option 2: Fetch
        f_dif_s' = {...f_dif_s, 
            input: Set(DIF_FetchRequest({cfg: cfg})),
            nonce: f_dif_s.nonce + 1

        },
        gc_stutter   
    }

    action env_ok_reqs(from:Party, sid: SID) : bool = all {
        // Precondition: input must be empty
        g_clock_s.input.size() == 0 and f_dif_s.input.size() == 0,
        
        // Non-deterministically choose a party and SID
        val cfg = {party: from, sid: sid, nonce: f_dif_s.nonce}

        // Option 3: OK
        f_dif_s' = {...f_dif_s, 
            input: Set(DIF_OKRequest({cfg: cfg})),
            nonce: f_dif_s.nonce + 1

        },   
        gc_stutter     
    }

    action env_dif_sim_reqs : bool = all {
        g_clock_s.input.size() == 0, 
        f_dif_s.input.size() == 0,
        f_dif_s.handles.size() > 0,

        f_dif_s' = {...f_dif_s, 
            input: Set(DIF_SIMControl),
            nonce: f_dif_s.nonce + 1
        },
        gc_stutter
    }

    action env_gc_sim_reqs : bool = all {
        g_clock_s.input.size() == 0, 
        f_dif_s.input.size() == 0,
        g_clock_s.handles.size() > 0,

        g_clock_s' = {...g_clock_s, 
            input: Set(GC_SIMControl),
            nonce: g_clock_s.nonce + 1
        },
        f_dif_s' = f_dif_s,
    }

    pure def env_consume_response(sys: DIF_System) : Set[DIF_Transition] = {
         sys.input.dif_get_all_response_messages().map(msg_log =>
         {
            post_state : sys.state,
            effects : Set(
                DIF_UpdateInput(Set())
            )
         })
    }

    action env_consume_responses : bool =  all {
        // Precondition: there is a response message in input
        val response_msgs = env_consume_response(f_dif_s)
        
        // Non-deterministically choose one response message to consume
        nondet transition = response_msgs.oneOf()

        // Update the system state by consuming the message    
        f_dif_s' = dif_process_transitions(f_dif_s, g_clock_s, transition),
        gc_stutter
    }


}