module p_bridge_testing{
    
    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"

    import common.* from "../common"

    import f_pki_types.* from "../f_pki/f_pki_types"    
    import f_pki.* from "../f_pki/f_pki"

    import f_hash_types.* from "../f_hash/f_hash_types"    
    import f_hash.* from "../f_hash/f_hash"    

    import f_atms_types.* from "../f_atms/f_atms_types"    
    import f_atms.* from "../f_atms/f_atms"    

    import g_ledger_types.* from "../g_ledger/g_ledger_types" 
    import g_ledger.* from "../g_ledger/g_ledger" 
    import g_ledger_comsel.* from "../g_ledger/g_ledger_comsel"
    import g_ledger_apl.* from "../g_ledger/g_ledger_apl"
    import g_ledger_ival.* from "../g_ledger/g_ledger_ival"

    import g_clock_types.* from "../g_clock/g_clock_types"    
    import g_clock.* from "../g_clock/g_clock"

    import f_dif_types.* from "../f_dif/f_dif_types"
    import f_dif.* from "../f_dif/f_dif"

    import f_inizk_types.* from "../f_inizk/f_inizk_types"    
    import f_inizk.* from "../f_inizk/f_inizk" 

    import p_bridge_types.* from "./p_bridge_types" 
    import p_bridge.* from "./p_bridge" 
    import p_bridge_bkgen.* from "./p_bridge_bkgen"     
    import p_bridge_rot.* from "./p_bridge_rot"
    import p_bridge_br.* from "./p_bridge_br"
    import p_bridge_inizk.* from "./p_bridge_inizk"
    import p_bridge_led2.* from "./p_bridge_led2"
    import p_bridge_env.* from "./p_bridge_env"

    val sid = 1
    val Parties = Set(1,2)
    val C = Set()
    val SIDs = Set(sid)
    val P1 = Set(1,2)
    val P2 = Set(1,2)   

    action init = all {
        p_bridge_env_init({
            P: Parties, 
            SIDs: SIDs, 
            C: C, 
            r0: 0, 
            delta: 1, 
            t: 1, 
            R: 4, 
            delta_latency: 1, 
            delta_slack: 1, 
            delta_apl: 1, 
            comsel_a: 1, 
            comsel_b: 1, 
            comsel_c: 1})
    }

    action step = {
        val x = {Parties: Parties, SIDs: SIDs}
        p_bridge_env_step(x)
    }    

    action brd_consume_responses = p_bridge_consume_responses
    action led1_consume_responses = g_ledger1_response_messages

    action bkgen_request_message(x:{party: int, sid: SID}) : bool = all {
        all_inputs_empty,
        all {        
            val cfg = {party: x.party, sid: x.sid, nonce: p_bridge_s.nonce}
            any {
                p_bridge_bkgen_request(cfg),
            },
            only_brd_changes,
        }
    }    

    action rot_request_message(x:{party: int, sid: SID}) : bool = all {
        all_inputs_empty,
        all {        
            val cfg = {party: x.party, sid: x.sid, nonce: p_bridge_s.nonce}
            any {
                p_bridge_rot_request(cfg),
            },
            only_brd_changes,
        }
    }  

    action br_request_message(x:{party: int, sid: SID}) : bool = all {
        all_inputs_empty,
        all {        
            val cfg = {party: x.party, sid: x.sid, nonce: p_bridge_s.nonce}
            any {
                p_bridge_br_request(cfg),
            },
            only_brd_changes,
        }
    }

    action certify_request_message(x:{party: int, sid: SID, rot: BRD_ROT, tx: LED_Tx}) : bool = all {
        all_inputs_empty,
        all {        
            val cfg = {party: x.party, sid: x.sid, nonce: p_bridge_s.nonce}
            any {
                p_bridge_certify_request(cfg, x.rot, x.tx),
            },
            only_brd_changes,
        }
    } 
    action verify_request_message(x:{party: int, sid: SID, rot: BRD_ROT, tx: LED_Tx, pi: int, h: int}) : bool = all {
        all_inputs_empty,
        all {        
            val cfg = {party: x.party, sid: x.sid, nonce: p_bridge_s.nonce}
            any {
                p_bridge_verify_request(cfg, x.rot, x.tx, x.pi, x.h),
            },
            only_brd_changes,
        }
    }   

    action led_request_message(x:{party: int, sid: SID}) : bool = all {
        all_inputs_empty,
        all {        
            val cfg = {party: x.party, sid: x.sid, nonce: p_bridge_s.nonce}
            any {
                g_ledger1_submit_request(cfg)
            },
            only_led1_changes,
        }
    }             

    run bkgen_cycle(party: int) : bool = {
        bkgen_request_message({party: party, sid: sid}).then(
            50.reps(i => 
                if(p_bridge_s.input.brd_get_bkgen_response_messages().size() == 0) step
                else brd_cstutter
            ).expect(
                p_bridge_s.input.brd_get_bkgen_response_messages().size() == 1)
        )
    }

    run rot_cycle(party: int) : bool = {
        rot_request_message({party: party, sid: sid}).then(
            50.reps(i => 
                if(p_bridge_s.input.brd_get_rot_response_messages().size() == 0) step
                else brd_cstutter
            ).expect(
                p_bridge_s.input.brd_get_rot_response_messages().size() == 1)
        )
    }    

    run br_cycle(party: int) : bool = {
        br_request_message({party: party, sid: sid}).then(
            100.reps(i => 
                if(p_bridge_s.input.brd_get_bridge_response_messages().size() == 0) step
                else brd_cstutter
            ).expect(
                p_bridge_s.input.brd_get_bridge_response_messages().size() == 1)
        )
    }

    run led1_submit_req_cycle(party: int) : bool = {
        led_request_message({party: party, sid: sid}).then(
            100.reps(i => 
                if(g_ledger_s.input.led_get_submit_response_messages().size() == 0) step
                else brd_cstutter
            ).expect(
                g_ledger_s.input.led_get_submit_response_messages().size() == 1)
        )
    }

    run certify_cycle(party: int, rot: BRD_ROT, tx: LED_Tx) : bool = {
        certify_request_message({party: party, sid: sid, rot: rot, tx: tx}).then(
            100.reps(i => 
                if(p_bridge_s.input.brd_get_certify_response_messages().size() == 0) step
                else brd_cstutter
            ).expect(
                p_bridge_s.input.brd_get_certify_response_messages().size() == 1)
        )
    }    

    run verify_cycle(party: int, rot: BRD_ROT, tx: LED_Tx, pi: int, h: int) : bool = {
        verify_request_message({party: party, sid: sid, rot: rot, tx: tx, pi: pi, h: h}).then(
            100.reps(i => 
                if(p_bridge_s.input.brd_get_verify_response_messages().size() == 0) step
                else brd_cstutter
            ).expect(
                p_bridge_s.input.brd_get_verify_response_messages().size() == 1)
        )
    }     


    // tests

    run test1 : bool = {

        init
        // generate keys
        .then(bkgen_cycle(1)).then(brd_consume_responses)
        .then(bkgen_cycle(2)).then(brd_consume_responses)
        // first key assignment
        .then(rot_cycle(1)).then(brd_consume_responses)
        .then(rot_cycle(2)).then(brd_consume_responses)

        // send some tx to led1 and 
        // go through entire bridge lifecycle ones
        .then(led1_submit_req_cycle(1)).then(led1_consume_responses)
        .then(4.reps(i => {
                    (br_cycle(1)).then(brd_consume_responses)
                        .then(br_cycle(2)).then(brd_consume_responses)}))
        .then(led1_submit_req_cycle(1)).then(led1_consume_responses)
        .then(4.reps(i => {
                    (br_cycle(1)).then(brd_consume_responses)
                        .then(br_cycle(2)).then(brd_consume_responses)}))
        .then(led1_submit_req_cycle(1)).then(led1_consume_responses)
        .then(4.reps(i => {
                    (br_cycle(1)).then(brd_consume_responses)
                        .then(br_cycle(2)).then(brd_consume_responses)}))
       // certify
       // verify                 
              

    }
    
}