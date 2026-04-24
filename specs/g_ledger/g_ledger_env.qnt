module g_ledger_env {
    
    import basicSpells.* from "../../spells/basicSpells"
    import g_ledger_types.* from "./g_ledger_types"
    import g_ledger.* from "./g_ledger"
    import g_ledger_comsel.* from "./g_ledger_comsel" 
    import g_ledger_apl.* from "./g_ledger_apl" 
    import g_ledger_ival.* from "./g_ledger_ival" 
    import f_atms.* from "../f_atms/f_atms"
    import f_atms_types.* from "../f_atms/f_atms_types"        
    import g_ledger_genesis.* from "./g_ledger_genesis" 
    import g_clock_types.* from "../g_clock/g_clock_types"
    import g_clock.* from "../g_clock/g_clock"
    import common.* from "../common"    

   val ALL_TXS = 1.to(UNIVERSE_SIZE).map(x => Normal({b: true, tx: x})).union(
       1.to(UNIVERSE_SIZE).map(x => Normal({b: false, tx: x})) )

   val Parties = 1.to(PARTY_SIZE)
   val C = 1.to(CORRUPTED_PARTY_SIZE)
   val sids = 1.to(SID_SIZE)

   val delta_latency = 1
   val delta_slack = 1
   val delta_apl = 1
   val atms_t = 1

    action step : bool =  {
        if(g_ledger_s.log.size() < MAX_LOG_SIZE){
            any {
                all {
                    any {
                        led_step,  
                        led_step_comsel,
                        led_step_apl,
                        led_step_genesis,
                        gc_step_led_wait,
                        env_led_send_all_reqs,
                        env_consume_responses,
                    },
                    f_atms_s' = f_atms_s
                },
                all {
                    led_step_ival,
                }
            }
        }else{
            all {
                env_consume_responses,  
                f_atms_s' = f_atms_s
            }
        }
    }

    action gc_step_led_wait : bool =  all {
        g_ledger_s' = g_ledger_s,
        gc_step,        
    }

    pure def env_consume_response(sys: LED_System) : Set[LED_Transition] = {
         sys.input.led_get_all_response_messages().map(msg_log =>
         {
            post_state : sys.state,
            effects : Set(
                LED_UpdateInput(Set())
            )
         })
    }

    action env_consume_responses : bool =  all {
        // Precondition: there is a response message in input
        val response_msgs = env_consume_response(g_ledger_s)
        
        all {
        // Non-deterministically choose one response message to consume
            response_msgs.size() > 0,
            nondet transition = response_msgs.oneOf()

        // Update the system state by consuming the message    
            g_ledger_s' = led_process_transitions(g_ledger_s, g_clock_s, transition),
            gc_stutter
        }
   }        
  
    action init = all {
        led_init({P: Parties, C: C, SIDs: sids, delta_latency: delta_latency, delta_slack: delta_slack, delta_apl: delta_apl, Val:  true, comsel_a: 1, comsel_b: 1, comsel_c: 2, responsive_simulator: true, enable_logging: true}),
        gc_init({C: C,time: 0, Reg: Parties.map(x => sids.map(y => {party: x, sid: y})).flatten(), responsive_simulator: true, enable_logging: false} ),
        atms_init({P : Parties, 
        C : C, 
        SIDs: sids, 
        t: (atms_t - 1),
        responsive_simulator: true,
        enable_logging: false,}),
    }

    action env_led_send_all_reqs : bool = all {
        nondet from = Parties.oneOf()
        nondet top = Parties.oneOf()
        nondet sid = sids.oneOf()
        nondet tx = ALL_TXS.oneOf()
        any {
            env_led_submit_reqs(from, sid, tx), 
            env_led_read_reqs(from, sid),
            env_ok_reqs(from, sid),
            env_led_apl_reqs(from, sid),
            env_led_genesis_reqs(from, sid),
            env_led_comsel_reqs(from, sid)
        }
    }

 
    // Environment interaction: non-deterministically add requests when input is empty
    action env_led_submit_reqs(from:Party, sid: SID, tx: LED_Tx) : bool = all {
        // Precondition: input must be empty
        g_clock_s.input.size() == 0 and g_ledger_s.input.size() == 0,
        
        // Non-deterministically choose a party and SID
        val cfg = {party: from, sid: sid, nonce: g_ledger_s.nonce}
        g_ledger_s' = {...g_ledger_s, 
            input: Set(SubmitRequest({cfg: cfg, tx: tx})),
            nonce: g_ledger_s.nonce + 1

        },
        gc_stutter
    }

    // Environment interaction: non-deterministically add requests when input is empty
    action env_led_comsel_reqs(from:Party, sid: SID) : bool = all {
        // Precondition: input must be empty
        g_clock_s.input.size() == 0 and g_ledger_s.input.size() == 0,
        
        // Non-deterministically choose a party and SID
        val cfg = {party: from, sid: sid, nonce: g_ledger_s.nonce}
        g_ledger_s' = {...g_ledger_s, 
            input: Set(ComSelRequest({cfg: cfg})),
            nonce: g_ledger_s.nonce + 1
        },
        gc_stutter
    }    

    action env_led_apl_reqs(from:Party, sid: SID) : bool = all {
        // Precondition: input must be empty
        g_clock_s.input.size() == 0 and g_ledger_s.input.size() == 0,
        
        // Non-deterministically choose a party and SID
        val cfg = {party: from, sid: sid, nonce: g_ledger_s.nonce}
        g_ledger_s' = {...g_ledger_s, 
            input: Set(APLRequest({cfg: cfg})),
            nonce: g_ledger_s.nonce + 1
        },
        gc_stutter
    }        

    action env_led_genesis_reqs(from:Party, sid: SID) : bool = all {
        // Precondition: input must be empty
        g_clock_s.input.size() == 0 and g_ledger_s.input.size() == 0,
        
        // Non-deterministically choose a party and SID
        val cfg = {party: from, sid: sid, nonce: g_ledger_s.nonce}
        g_ledger_s' = {...g_ledger_s, 
            input: Set(GenesisRequest({cfg: cfg})),
            nonce: g_ledger_s.nonce + 1
        },
        gc_stutter
    }            

    action env_led_read_reqs(from:Party, sid: SID) : bool = all {
        // Precondition: input must be empty
        g_clock_s.input.size() == 0 and g_ledger_s.input.size() == 0,
        
        // Non-deterministically choose a party and SID
        val cfg = {party: from, sid: sid, nonce: g_ledger_s.nonce}

        // Option 2: Fetch
        g_ledger_s' = {...g_ledger_s, 
            input: Set(ReadRequest({cfg: cfg})),
            nonce: g_ledger_s.nonce + 1

        },
        gc_stutter   
    }

    action env_ok_reqs(from:Party, sid: SID) : bool = all {
        // Precondition: input must be empty
        g_clock_s.input.size() == 0 and g_ledger_s.input.size() == 0,
        
        // Non-deterministically choose a party and SID
        val cfg = {party: from, sid: sid, nonce: g_ledger_s.nonce}

        // Option 3: OK
        g_ledger_s' = {...g_ledger_s, 
            input: Set(OKRequest({cfg: cfg})),
            nonce: g_ledger_s.nonce + 1

        },   
        gc_stutter     
    }

    action env_led_sim_reqs : bool = all {
        g_clock_s.input.size() == 0, 
        g_ledger_s.input.size() == 0,
        g_ledger_s.handles.size() > 0,

        g_ledger_s' = {...g_ledger_s, 
            input: Set(LED_SIMControl),
            nonce: g_ledger_s.nonce + 1
        },
        gc_stutter
    }


       
}