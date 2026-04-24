module g_ledger_ival_env {
    
    import basicSpells.* from "../../spells/basicSpells"
    import g_ledger_types.* from "./g_ledger_types"
    import g_ledger.* from "./g_ledger"

    import g_ledger_ival.* from "./g_ledger_ival" 

    import g_clock_types.* from "../g_clock/g_clock_types"
    import g_clock.* from "../g_clock/g_clock"

    import f_atms_types.* from "../f_atms/f_atms_types"
    import f_atms.* from "../f_atms/f_atms"    
    import common.* from "../common"    

   val ALL_TXS = 1.to(5)
   val ALL_DOC = Set(
         {h:1, avk:1},
         {h:1, avk:2},
         {h:3, avk:3},
         {h:4, avk:4},
         {h:5, avk:5},
    )
   val Parties = Set(1,2)
   val C = Set()
   val sids = Set(1)
   val delta_latency = 1
   val delta_slack = 1
   val delta_apl = 1 

   val ATMS_T = 1

    action step : bool = any {
        env_led_send_all_reqs,
        env_consume_responses,
        led_step_ival,
        env_g_clock_steps,
        env_f_atms_steps,
        env_g_ledger_steps,
    }
 
    action env_g_ledger_steps : bool = all {
        led_step,
        f_atms_s'  = f_atms_s
    }

    action env_g_clock_steps : bool = all {
        gc_step,
        g_ledger_s' = g_ledger_s,
        f_atms_s'  = f_atms_s
    }

    action env_f_atms_steps : bool = all {
        atms_step,
        g_ledger_s' = g_ledger_s,
        g_clock_s' = g_clock_s        
    }    

    action env_led_send_all_reqs : bool = all {
        g_clock_s.input.size() == 0 and g_ledger_s.input.size() == 0 and f_atms_s.input.size() == 0,
        nondet from = Parties.oneOf()
        nondet sid = sids.oneOf()
        val cfg = {party: from, sid: sid, nonce: g_ledger_s.nonce}
        any {
            env_led_submit_normal_tx(cfg),
            env_led_submit_install_tx(cfg),
            env_led_submit_update_tx(cfg),
            env_led_read(cfg)
        },
        all{
            f_atms_s'  = f_atms_s,
            g_clock_s' = g_clock_s
        }
    }

    action env_led_read(cfg: MsgConfig) : bool = all {
        // // Non-deterministically choose a party and SID
        g_ledger_s' = {...g_ledger_s, 
            input: Set(ReadRequest({cfg: cfg})),
            nonce: g_ledger_s.nonce + 1
        },       
    }        

    action env_led_submit_normal_tx(cfg: MsgConfig) : bool = all {
        // Precondition: input must be empty

        nondet tx = ALL_TXS.oneOf()
        nondet b = Set(true, false).oneOf()

        // Non-deterministically choose a party and SID
        g_ledger_s' = {...g_ledger_s, 
            input: Set(SubmitRequest({cfg: cfg, tx: Normal({b: b, tx: tx})})),
            nonce: g_ledger_s.nonce + 1
        }
    }



    action env_led_submit_install_tx(cfg: MsgConfig) : bool = all {
        nondet cid = Set(1,2,3).oneOf()
        nondet code = Set(1,2,3).oneOf()
        // Non-deterministically choose a party and SID
        g_ledger_s' = {...g_ledger_s, 
            input: Set(SubmitRequest({cfg: cfg, tx: Install({cID: cid, code: code})})),
            nonce: g_ledger_s.nonce + 1
        }
    }    

    action env_led_submit_update_tx(cfg: MsgConfig) : bool = all {
        nondet msg = ALL_DOC.oneOf()
        nondet sigma = Set(1,2,3).oneOf()
        // Non-deterministically choose a party and SID
        g_ledger_s' = {...g_ledger_s, 
            input: Set(SubmitRequest({cfg: cfg, tx: Update({msg: msg, sigma: sigma})})),
            nonce: g_ledger_s.nonce + 1
        }
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
            nondet transition = response_msgs.oneOf()
            g_ledger_s' = led_process_transitions(g_ledger_s, g_clock_s, transition),
            f_atms_s'  = f_atms_s,
            g_clock_s' = g_clock_s            
        }
    }

    action init = all {
        led_init({P: Parties, C: C, SIDs: sids, delta_latency: delta_latency, delta_slack: delta_slack, delta_apl: delta_apl, Val:  true, comsel_a: 1, comsel_b: 1, comsel_c: 2, responsive_simulator: true, enable_logging: true}),
        gc_init({C: C,time: 0, Reg: Parties.map(x => sids.map(y => {party: x, sid: y})).flatten(), responsive_simulator: true, enable_logging: false} ),        

        atms_init({
            P : Parties, 
            C : C, 
            SIDs: sids, 
            t: ATMS_T,
            enable_logging: false,
            responsive_simulator: true,
        })
    }           

    val P1 = g_ledger_s.log.size() == 0
    val P2 = g_ledger_s.state.mem.keys().size() == 0
    val P3 = g_ledger_s.log.map(x => x.msg).led_get_eval_request_messages().size() == 0
    val P4 = g_ledger_s.log.map(x => x.msg).led_get_read_response_messages().filter(x => x.result.length() > 1).size() == 0

    
}