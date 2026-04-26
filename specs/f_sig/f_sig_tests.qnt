module f_sig_tests{
    
    import basicSpells.* from "../../spells/basicSpells"
    import common.* from "../common"   
    import params.* from "../params"
    import f_sig_types.* from "./f_sig_types"
    import f_sig.* from "./f_sig"
    import f_sig_env.* from "./f_sig_env" 


    run gen_cycle(cfg: MsgConfig) : bool = {
        (env_gen_reqs(cfg))
        .then(sig_step)
        .then(sig_gen_2_act) // forces simulator to act on the handle
        .then(sig_step)
        .then(env_consume_responses)        
    }    

    run sign_cycle(cfg: MsgConfig, msg: SIG_Document) : bool = {
        (env_sign_reqs_msg(cfg, msg))
        .then(sig_step)
        .then(sig_sign_2_act) // forces simulator to act on the handle
        .then(sig_step)
        .then(env_consume_responses)        
    }

    // Helper cycle for sign without VK (returns None immediately)
    run sign_cycle_no_vk(cfg: MsgConfig, msg: SIG_Document) : bool = {
        (env_sign_reqs_msg(cfg, msg))
        .then(sig_step)  // Returns SignResponse with None immediately
        .then(env_consume_responses)        
    }

    run verify_cycle(cfg: MsgConfig, vk: SIG_VK, m: SIG_Document, sig: SIG_Signature) : bool = {
        (env_verify_reqs_args(cfg, vk, m, sig))
        .then(sig_step)
        .then(sig_verify_2_act) // forces simulator to act on the handle
        .then(sig_step)
        .then(env_consume_responses)        
    }
    
    // Test 1: Basic completeness - gen, sign, verify
    run test_completeness = {
        val cfg = {party: 1, sid: 1, nonce: 1}
        
        init
        .then(gen_cycle(cfg)) // generate vk
        .then({ // sign 
            sign_cycle({...cfg, nonce: 2}, "Message 1")
        })
        .then({ // verify
            nondet gen_res = f_sig_s.log.map(x => x.msg).sig_get_gen_response_messages().filter(x => x.cfg.nonce == 1).oneOf()
            nondet sign_res = f_sig_s.log.map(x => x.msg).sig_get_sign_response_messages().filter(x => x.cfg.nonce == 2).oneOf()
            verify_cycle({...cfg, nonce: 3}, gen_res.vk, "Message 1", sign_res.sig.unwrap())
        })
        .expect( // expect verification to be true
            nondet verify_res = f_sig_s.log.map(x => x.msg).sig_get_verify_response_messages().filter(x => x.cfg.nonce == 3).oneOf()
            verify_res.b == true
        )
    }

    // Test 2: Sign without generating VK should fail
    run test_sign_without_vk = {
        val cfg = {party: 1, sid: 1, nonce: 1}
        
        init
        .then({ // try to sign without generating VK first
            sign_cycle_no_vk(cfg, "Message 1")
        })
        .expect( // expect signature to be None
            nondet sign_res = f_sig_s.log.map(x => x.msg).sig_get_sign_response_messages().filter(x => x.cfg.nonce == 1).oneOf()
            sign_res.sig == None
        )
    }

    // Test 3: Verify consistency - same signature should always give same result
    run test_verify_consistency = {
        val cfg = {party: 1, sid: 1, nonce: 1}
        
        init
        .then(gen_cycle(cfg))
        .then({
            sign_cycle({...cfg, nonce: 2}, "Message 1")
        })
        .then({ // verify first time
            nondet gen_res = f_sig_s.log.map(x => x.msg).sig_get_gen_response_messages().filter(x => x.cfg.nonce == 1).oneOf()
            nondet sign_res = f_sig_s.log.map(x => x.msg).sig_get_sign_response_messages().filter(x => x.cfg.nonce == 2).oneOf()
            verify_cycle({...cfg, nonce: 3}, gen_res.vk, "Message 1", sign_res.sig.unwrap())
        })
        .then({ // verify second time with same signature
            nondet gen_res = f_sig_s.log.map(x => x.msg).sig_get_gen_response_messages().filter(x => x.cfg.nonce == 1).oneOf()
            nondet sign_res = f_sig_s.log.map(x => x.msg).sig_get_sign_response_messages().filter(x => x.cfg.nonce == 2).oneOf()
            verify_cycle({...cfg, nonce: 4}, gen_res.vk, "Message 1", sign_res.sig.unwrap())
        })
        .expect( // both verifications should return true
            all {
                nondet verify_res1 = f_sig_s.log.map(x => x.msg).sig_get_verify_response_messages().filter(x => x.cfg.nonce == 3).oneOf()
                nondet verify_res2 = f_sig_s.log.map(x => x.msg).sig_get_verify_response_messages().filter(x => x.cfg.nonce == 4).oneOf()
                and {
                    verify_res1.b == true,
                    verify_res2.b == true
                }
            }
        )
    }

    // Test 4: Multiple parties can generate VKs independently
    run test_multiple_parties_gen = {
        val cfg1 = {party: 1, sid: 1, nonce: 1}
        val cfg2 = {party: 2, sid: 1, nonce: 2}
        
        init
        .then(gen_cycle(cfg1))
        .then(gen_cycle(cfg2))
        .expect( // both parties should have generated VKs
            all {
                nondet gen_res1 = f_sig_s.log.map(x => x.msg).sig_get_gen_response_messages().filter(x => x.cfg.nonce == 1).oneOf()
                nondet gen_res2 = f_sig_s.log.map(x => x.msg).sig_get_gen_response_messages().filter(x => x.cfg.nonce == 2).oneOf()
                gen_res1.vk != gen_res2.vk  // Different VKs
            }
        )
    }

    // Test 5: Sign different messages with same VK
    run test_sign_multiple_messages = {
        val cfg = {party: 1, sid: 1, nonce: 1}
        
        init
        .then(gen_cycle(cfg))
        .then({
            sign_cycle({...cfg, nonce: 2}, "Message 1")
        })
        .then({
            sign_cycle({...cfg, nonce: 3}, "Message 2")
        })
        .expect( // both signatures should be generated
            all {
                nondet sign_res1 = f_sig_s.log.map(x => x.msg).sig_get_sign_response_messages().filter(x => x.cfg.nonce == 2).oneOf()
                nondet sign_res2 = f_sig_s.log.map(x => x.msg).sig_get_sign_response_messages().filter(x => x.cfg.nonce == 3).oneOf()
                and {
                    sign_res1.sig != None,
                    sign_res2.sig != None
                }
            }
        )
    }

    // Test 6: Verify wrong message fails
    run test_verify_wrong_message = {
        val cfg = {party: 1, sid: 1, nonce: 1}
        
        init
        .then(gen_cycle(cfg))
        .then({
            sign_cycle({...cfg, nonce: 2}, "Message 1")
        })
        .then({ // verify with different message
            nondet gen_res = f_sig_s.log.map(x => x.msg).sig_get_gen_response_messages().filter(x => x.cfg.nonce == 1).oneOf()
            nondet sign_res = f_sig_s.log.map(x => x.msg).sig_get_sign_response_messages().filter(x => x.cfg.nonce == 2).oneOf()
            verify_cycle({...cfg, nonce: 3}, gen_res.vk, "Message 2", sign_res.sig.unwrap())
        })
        .expect( // verification should fail (message mismatch)
            nondet verify_res = f_sig_s.log.map(x => x.msg).sig_get_verify_response_messages().filter(x => x.cfg.nonce == 3).oneOf()
            verify_res.b == false
        )
    }

    // Test 7: One VK per party
    run test_one_vk_per_party = {
        val cfg = {party: 1, sid: 1, nonce: 1}
        
        init
        .then(gen_cycle(cfg))
        .then(gen_cycle({...cfg, nonce: 2}))  // try to gen again
        .expect( // party should still have only one VK (the first one remains)
            all {
                nondet gen_res1 = f_sig_s.log.map(x => x.msg).sig_get_gen_response_messages().filter(x => x.cfg.nonce == 1).oneOf()
                nondet gen_res2 = f_sig_s.log.map(x => x.msg).sig_get_gen_response_messages().filter(x => x.cfg.nonce == 2).oneOf()
                gen_res1.vk == gen_res2.vk  // Same VK returned
            }
        )
    }

    // Test 8: Cross-party verification - party 2 verifies party 1's signature
    run test_cross_party_verify = {
        val cfg1 = {party: 1, sid: 1, nonce: 1}
        val cfg2 = {party: 2, sid: 1, nonce: 4}
        
        init
        .then(gen_cycle(cfg1))  // party 1 generates VK
        .then({
            sign_cycle({...cfg1, nonce: 2}, "Message 1")  // party 1 signs
        })
        .then({ // party 2 verifies party 1's signature
            nondet gen_res = f_sig_s.log.map(x => x.msg).sig_get_gen_response_messages().filter(x => x.cfg.nonce == 1).oneOf()
            nondet sign_res = f_sig_s.log.map(x => x.msg).sig_get_sign_response_messages().filter(x => x.cfg.nonce == 2).oneOf()
            verify_cycle(cfg2, gen_res.vk, "Message 1", sign_res.sig.unwrap())
        })
        .expect( // verification should succeed
            nondet verify_res = f_sig_s.log.map(x => x.msg).sig_get_verify_response_messages().filter(x => x.cfg.nonce == 4).oneOf()
            verify_res.b == true
        )
    }

    // Test 9: Multiple signatures verify independently
    run test_multiple_signatures = {
        val cfg = {party: 1, sid: 1, nonce: 1}
        
        init
        .then(gen_cycle(cfg))
        .then({
            sign_cycle({...cfg, nonce: 2}, "Message 1")
        })
        .then({
            sign_cycle({...cfg, nonce: 3}, "Message 2")
        })
        .then({ // verify first signature
            nondet gen_res = f_sig_s.log.map(x => x.msg).sig_get_gen_response_messages().filter(x => x.cfg.nonce == 1).oneOf()
            nondet sign_res1 = f_sig_s.log.map(x => x.msg).sig_get_sign_response_messages().filter(x => x.cfg.nonce == 2).oneOf()
            verify_cycle({...cfg, nonce: 4}, gen_res.vk, "Message 1", sign_res1.sig.unwrap())
        })
        .then({ // verify second signature
            nondet gen_res = f_sig_s.log.map(x => x.msg).sig_get_gen_response_messages().filter(x => x.cfg.nonce == 1).oneOf()
            nondet sign_res2 = f_sig_s.log.map(x => x.msg).sig_get_sign_response_messages().filter(x => x.cfg.nonce == 3).oneOf()
            verify_cycle({...cfg, nonce: 5}, gen_res.vk, "Message 2", sign_res2.sig.unwrap())
        })
        .expect( // both verifications should succeed
            all {
                nondet verify_res1 = f_sig_s.log.map(x => x.msg).sig_get_verify_response_messages().filter(x => x.cfg.nonce == 4).oneOf()
                nondet verify_res2 = f_sig_s.log.map(x => x.msg).sig_get_verify_response_messages().filter(x => x.cfg.nonce == 5).oneOf()
                and {
                    verify_res1.b == true,
                    verify_res2.b == true
                }
            }
        )
    }
}