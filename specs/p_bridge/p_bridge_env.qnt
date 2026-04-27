module p_bridge_env{ 
    
    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"

    import common.* from "../common"
    import params.* from "../params"

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
    
    
   
    pure def brd_main_listener(sys: BRD_CSystem) : Set[BRD_CTransition] = {
        brd_bkgen_main_listener(sys)
            .union(brd_rot_main_listener(sys))
            .union(brd_br_main_listener(sys))
            .union(brd_inizk_main_listener(sys))
    }

    action brd_step_pure : bool = {
        all{
            val sys = {
                brd: p_bridge_s,
                clock: g_clock_s,
                atms: f_atms_s,
                pki: f_pki_s,
                ledger1: g_ledger_s,
                ledger2: g_ledger_s2,
                hash: f_hash_s,
                dif: f_dif_s,
                nizk: f_nizk_s,
            }
            val ts = brd_main_listener(sys)
            all {
                nondet transition = ts.oneOf()
                val new_sys = brd_process_ctransitions(sys, transition)
                all {
                    p_bridge_s' = new_sys.brd,
                    g_clock_s' = new_sys.clock,
                    f_atms_s' = new_sys.atms,
                    f_pki_s' = new_sys.pki,
                    g_ledger_s' = new_sys.ledger1,
                    g_ledger_s2' = new_sys.ledger2,
                    f_hash_s' = new_sys.hash,
                    f_dif_s' = new_sys.dif,
                    f_nizk_s' = new_sys.nizk,
                }
            }
        }
    }     

    action brd_step : bool =  all {
        brd_step_pure
    }    

    action brd_cstutter = all {
        p_bridge_s' = p_bridge_s,
        g_clock_s' = g_clock_s,
        f_atms_s' = f_atms_s,
        f_pki_s' = f_pki_s,
        g_ledger_s' = g_ledger_s,
        g_ledger_s2' = g_ledger_s2,
        f_hash_s' = f_hash_s,
        f_dif_s' = f_dif_s,
        f_nizk_s' = f_nizk_s,
    }


    action p_bridge_env_step(x:{Parties: Set[Party], SIDs: Set[SID]}) : bool = any {
        brd_step,
        all { // clock steps
            gc_step,
            p_bridge_s' = p_bridge_s,
            f_atms_s' = f_atms_s,
            f_pki_s' = f_pki_s,
            g_ledger_s' = g_ledger_s,
            g_ledger_s2' = g_ledger_s2,
            f_hash_s' = f_hash_s,
            f_dif_s' = f_dif_s,
            f_nizk_s' = f_nizk_s,
        },
        all { // pki steps
            pki_step,
            p_bridge_s' = p_bridge_s,
            g_clock_s' = g_clock_s,
            f_atms_s' = f_atms_s,
            g_ledger_s' = g_ledger_s,
            g_ledger_s2' = g_ledger_s2,
            f_hash_s' = f_hash_s,
            f_dif_s' = f_dif_s,
            f_nizk_s' = f_nizk_s,
        },
        all { // atms steps
            atms_step,
            p_bridge_s' = p_bridge_s,
            g_clock_s' = g_clock_s,
            f_pki_s' = f_pki_s,
            g_ledger_s' = g_ledger_s,
            g_ledger_s2' = g_ledger_s2,
            f_hash_s' = f_hash_s,
            f_dif_s' = f_dif_s,
            f_nizk_s' = f_nizk_s,
        },
        all{ // ledger1 steps
            any {
                led_step,
                led_step_apl,
                led_step_comsel,
            },
            p_bridge_s' = p_bridge_s,
            f_atms_s' = f_atms_s,
            f_pki_s' = f_pki_s,
            f_hash_s' = f_hash_s,
            g_ledger_s2' = g_ledger_s2,
            f_dif_s' = f_dif_s,
            f_nizk_s' = f_nizk_s,
        },
        all { // ledger1 ival steps
            led_step_ival,
            p_bridge_s' = p_bridge_s,
            f_pki_s' = f_pki_s,
            g_ledger_s2' = g_ledger_s2,
            f_hash_s' = f_hash_s,
            f_dif_s' = f_dif_s,
            f_nizk_s' = f_nizk_s,
        },
        all{ // ledger2 steps
            any {
                led2_step,
            },
            p_bridge_s' = p_bridge_s,
            f_atms_s' = f_atms_s,
            f_pki_s' = f_pki_s,
            f_hash_s' = f_hash_s,
            g_ledger_s' = g_ledger_s,
            f_dif_s' = f_dif_s,
            f_nizk_s' = f_nizk_s,
        },  
        all { // ledger2 ival steps
            led2_step_ival,
            p_bridge_s' = p_bridge_s,
            f_pki_s' = f_pki_s,
            g_ledger_s' = g_ledger_s,
            f_hash_s' = f_hash_s,
            f_dif_s' = f_dif_s,
            f_nizk_s' = f_nizk_s,
        },              
        all { // hash steps
            hash_step,
            p_bridge_s' = p_bridge_s,
            g_clock_s' = g_clock_s,
            f_atms_s' = f_atms_s,
            f_pki_s' = f_pki_s,
            g_ledger_s' = g_ledger_s,
            g_ledger_s2' = g_ledger_s2,
            f_dif_s' = f_dif_s,
            f_nizk_s' = f_nizk_s,
        },
        all { // diffuse steps
            dif_step,
            p_bridge_s' = p_bridge_s,
            f_atms_s' = f_atms_s,
            f_pki_s' = f_pki_s,
            g_ledger_s' = g_ledger_s,
            g_ledger_s2' = g_ledger_s2,
            f_hash_s' = f_hash_s,
            f_nizk_s' = f_nizk_s,
        },
        all { // nizk steps
            nizk_step,
            p_bridge_s' = p_bridge_s,
            g_clock_s' = g_clock_s,
            f_atms_s' = f_atms_s,
            f_pki_s' = f_pki_s,
            g_ledger_s' = g_ledger_s,
            g_ledger_s2' = g_ledger_s2,
            f_dif_s' = f_dif_s,
        }
    }

   pure def g_ledger1_response_messages_trans(sys: LED_System) : Set[LED_Transition] = {
         sys.input.filterMap(z => match z {| SubmitResponse(p) => Some(p) | _ => None}).map(msg_log =>
         {
            post_state : sys.state,
            effects : Set(
                LED_LogMessage(SubmitResponse(msg_log)),
                LED_UpdateInput(Set())
            )
         })
    }

    action g_ledger1_response_messages : bool =  all {
        // Precondition: there is a response message in input
        val response_msgs = g_ledger1_response_messages_trans(g_ledger_s)
        
        all {
        // Non-deterministically choose one response message to consume
            nondet transition = response_msgs.oneOf()

        // Update the system state by consuming the message    
            g_ledger_s' = led_process_transitions(g_ledger_s, g_clock_s, transition),
            f_atms_s' = f_atms_s,
            f_pki_s' = f_pki_s,
            g_clock_s' = g_clock_s, 
            g_ledger_s2' = g_ledger_s2,
            f_hash_s' = f_hash_s,          
            f_dif_s' = f_dif_s,   
            p_bridge_s' = p_bridge_s,
            f_nizk_s' = f_nizk_s,
        }
   }            

    def all_inputs_empty : bool = all {
        p_bridge_s.input.size() == 0,
        f_atms_s.input.size() == 0,
        f_pki_s.input.size() == 0,
        g_clock_s.input.size() == 0,
        g_ledger_s.input.size() == 0,
        g_ledger_s2.input.size() == 0,
        f_hash_s.input.size() == 0,
        f_dif_s.input.size() == 0,
        f_nizk_s.input.size() == 0,
    }

    action only_brd_changes : bool = all {
        g_clock_s' = g_clock_s,
        f_atms_s' = f_atms_s,
        f_pki_s' = f_pki_s,
        g_ledger_s' = g_ledger_s,
        g_ledger_s2' = g_ledger_s2,
        f_hash_s' = f_hash_s,
        f_dif_s' = f_dif_s,
        f_nizk_s' = f_nizk_s,
    }

    action only_led1_changes : bool = all {
        p_bridge_s' = p_bridge_s,
        g_clock_s' = g_clock_s,
        f_atms_s' = f_atms_s,
        f_pki_s' = f_pki_s,
        g_ledger_s2' = g_ledger_s2,
        f_hash_s' = f_hash_s,
        f_dif_s' = f_dif_s,
        f_nizk_s' = f_nizk_s,
    }

    action p_bridge_request_messages(x:{Parties: Set[Party], SIDs: Set[SID]}) : bool = all {
        all_inputs_empty,

        all {        
            nondet from = x.Parties.oneOf()
            nondet sid = x.SIDs.oneOf()

            val cfg = {party: from, sid: sid, nonce: p_bridge_s.nonce}
            any {
                p_bridge_bkgen_request(cfg),
                p_bridge_rot_request(cfg),
                p_bridge_br_request(cfg),

                all { // certify request
                    p_bridge_s.state.bst.get(from).rot != None,
                    nondet x = g_ledger_s.state.log.toSet().oneOf()
                    p_bridge_certify_request(cfg, p_bridge_s.state.bst.get(from).rot.unwrap(), x.tx),
                },
                // all { // verify request
                //     p_bridge_s.state.bst.get(from).rot != None,
                    
                //     nondet x = g_ledger_s.state.log.toSet().oneOf()
                //     nondet pi = ALL_PROOFS.oneOf() // TODO: determine appropriate range
                //     nondet h = HASH_ALL_HASHES.oneOf() // TODO: determine appropriate range
                //     p_bridge_verify_request(cfg, p_bridge_s.state.bst.get(from).rot.unwrap(), x.tx, pi, h),
                // },
                all { // specific case for verify requests
                    nondet p = p_bridge_s.log.map(x => x.msg).brd_get_certify_response_messages().oneOf()
                    all {
                        p.res != None,
                        p_bridge_verify_request(cfg, p.rot, p.x, p.res.unwrap().pi, p.res.unwrap().h),
                    }
                }
            },
            only_brd_changes,
        }
    }    

    action g_ledger1_request_messages(x:{Parties: Set[Party], SIDs: Set[SID]}) : bool = all {
        all_inputs_empty,   

        and { g_ledger_s.state.buffer.keys().size() < 1
           , g_ledger_s.state.log.length() < 3 },        

        all {        
            nondet from = x.Parties.oneOf()
            nondet sid = x.SIDs.oneOf()

            val cfg = {party: from, sid: sid, nonce: g_ledger_s.nonce}
            any {
                g_ledger1_submit_request(cfg),
            },
            only_led1_changes               
        }
    }    

    action g_ledger1_submit_request(cfg: MsgConfig) : bool = all {
        nondet tx = 1.to(100).oneOf() // TODO: determine appropriate range
        val ntx = SubmitRequest({cfg: cfg, tx: Normal({tx: tx, b: true})})

        g_ledger_s' = {...g_ledger_s, 
            input: Set(ntx),
            nonce: g_ledger_s.nonce + NONCE_INCREMENT
        }
    }


    action p_bridge_bkgen_request(cfg: MsgConfig) : bool = all {
        p_bridge_s' = {...p_bridge_s, 
            input: Set(BKGenRequest({cfg: cfg})),
            nonce: p_bridge_s.nonce + NONCE_INCREMENT
        }
    }


    action p_bridge_rot_request(cfg: MsgConfig) : bool = all {
        p_bridge_s' = {...p_bridge_s, 
            input: Set(RoTRequest({cfg: cfg})),
            nonce: p_bridge_s.nonce + NONCE_INCREMENT
        }
    }

    action p_bridge_br_request(cfg: MsgConfig) : bool = all {
        p_bridge_s' = {...p_bridge_s, 
            input: Set(BridgeRequest({cfg: cfg})),
            nonce: p_bridge_s.nonce + NONCE_INCREMENT
        }
    }    

    action p_bridge_certify_request(cfg: MsgConfig, rot: BRD_ROT, x: LED_Tx) : bool = all {
        p_bridge_s' = {...p_bridge_s, 
            input: Set(CertifyRequest({cfg: cfg, rot: rot, x: x})),
            nonce: p_bridge_s.nonce + NONCE_INCREMENT
        }
    }

    action p_bridge_verify_request(cfg: MsgConfig, rot: BRD_ROT, x: LED_Tx, pi: int, h: int) : bool = all {
        p_bridge_s' = {...p_bridge_s, 
            input: Set(BRD_VerifyRequest({cfg: cfg, rot: rot, x: x, proof: {pi: pi, h: h} })),
            nonce: p_bridge_s.nonce + NONCE_INCREMENT
        }
    }
 
    pure def p_bridge_consume_response(brd: BRD_System) : Set[BRD_Transition] = {
         brd.input.brd_get_all_response_messages().map(msg_log => {
            post_state : brd.state,
            effects : Set(
                BRD_LogMessage(msg_log),
                BRD_UpdateInput(Set())
            )
         })
    }

    action p_bridge_consume_responses : bool =  all {
        val response_msgs = p_bridge_s.p_bridge_consume_response()
        nondet transition = response_msgs.oneOf()

        p_bridge_s' = brd_process_transitions(p_bridge_s, transition),
        g_clock_s' = g_clock_s,
        f_atms_s' = f_atms_s,
        f_pki_s' = f_pki_s,
        g_ledger_s' = g_ledger_s,
        g_ledger_s2' = g_ledger_s2,
        f_hash_s' = f_hash_s,
        f_dif_s' = f_dif_s,
        f_nizk_s' = f_nizk_s,
    }

    action p_bridge_env_init(x: {P: Set[Party], SIDs: Set[SID], C: Set[Party], r0: int, delta: int, t: int, R: int, delta_latency: int, delta_slack: int, delta_apl: int, comsel_a: int, comsel_b: int, comsel_c: int}) : bool = all {
        val r0 = x.r0
        val delta = x.delta
        val t = x.t
        val R = x.R
        all {
            hash_init({P: x.P, SIDs: x.SIDs, enable_logging: false, responsive_simulator: true}),
            pki_init({P: x.P, SIDs: x.SIDs, enable_logging: false, responsive_simulator: true}),
            gc_init({C: x.C, time: r0, Reg: all_parties_at_all_sids(x.P, x.SIDs), enable_logging: false, responsive_simulator: true}),
            dif_init({P : x.P, C:  x.C, SIDs: x.SIDs, delta:delta, enable_logging: false, responsive_simulator: true}),
            brd_init({P : x.P, C : x.C, SIDs: x.SIDs, r0: r0, delta: delta, R: R, }),
            atms_init({P : x.P, C : x.C, SIDs: x.SIDs, t: (t-1), enable_logging: false, responsive_simulator: true}),
            g_ledger_s' = led_initial({P: x.P, C: x.C, SIDs: x.SIDs, delta_latency: x.delta_latency, delta_slack: x.delta_slack, delta_apl: x.delta_apl, Val: true, comsel_a: x.comsel_a, comsel_b: x.comsel_b, comsel_c: x.comsel_c, enable_logging: false, responsive_simulator: true}),
            g_ledger_s2' = led_initial({P: x.P, C: x.C, SIDs: x.SIDs, delta_latency: x.delta_latency, delta_slack: x.delta_slack, delta_apl: x.delta_apl, Val: true, comsel_a: x.comsel_a, comsel_b: x.comsel_b, comsel_c: x.comsel_c, enable_logging: false, responsive_simulator: true}),
            nizk_init({P: x.P, C: x.C, SIDs: x.SIDs, enable_logging: false, responsive_simulator: true}),
        }
    }

}