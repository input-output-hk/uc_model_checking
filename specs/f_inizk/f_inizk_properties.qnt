module f_inizk_properties{

    import basicSpells.* from "../../spells/basicSpells"
    import f_inizk_types.* from "./f_inizk_types"
    import f_inizk.* from "./f_inizk"
    import f_inizk_env.* from "./f_inizk_env"
    import common.* from "../common"
    import params.* from "../params"

    import f_hash_types.* from "../f_hash/f_hash_types"
    import f_hash.* from "../f_hash/f_hash"    

    // ========== CO-PROPERTIES (must fail) ==========
    
    val P1 = sids.forall(sid => 
        f_nizk_s.state.get(sid).Ver.size() <= 4
    )
    
    // ProofNonMalleability: The same proof shouldn't verify different statements
    // (unless relation allows it - but this would be suspicious)
    val ProofNonMalleability = sids.forall(sid => {
        val sid_state = f_nizk_s.state.get(sid)
        sid_state.Ver.forall(entry1 => {
            sid_state.Ver.forall(entry2 => {
                and {
                    entry1.pi == entry2.pi,
                    entry1.b == true,
                    entry2.b == true
                } implies (entry1.x == entry2.x)
            })
        })
    })
    
    // Verify must follow prove (can't verify without a proof being generated)
    val VerifyAfterProve = f_nizk_s.log.forall(log => {
        match log.msg {
            | NIZK_VerifyResponse(vr) => {
                (vr.b == true) implies {
                    f_nizk_s.log.exists(log2 => {
                        match log2.msg {
                            | NIZK_ProveResponse(pr) => {
                                and {
                                    log2.idx < log.idx,
                                    pr.cfg.sid == vr.cfg.sid,
                                    pr.x == vr.x,
                                    pr.pi == Some(vr.pi)
                                }
                            }
                            | _ => false
                        }
                    })
                }
            }
            | _ => true
        }
    })
 

    // ========== PROPERTIES (must hold) ==========
    
    // Handles are properly managed (no invalid handles)
    val NoOrphanedHolds = f_nizk_s.handles.forall(h => {
        match h {
            | NIZK_SIMProveHold(_) => true
            | NIZK_SIMVerifyHold(_) => true
            | NIZK_VerifyRelWait(_) => true
            | NIZK_ProveRelWait(_) => true
            | NIZK_RelRequestWait(_) => true
            | _ => false
        }
    })

    // Input buffer has at most one message (as per design)
    val InputBound = f_nizk_s.input.size() <= 1
    
    // Prove Soundness: R(x,w) = 0 ⇒ P.prove(x,w) = ⊥
    // If the relation doesn't hold, prove returns None
    val ProveSoundness = f_nizk_s.log.forall(log => {
        match log.msg {
            | NIZK_ProveResponse(pr) => {
                sids.forall(sid => {
                    val sid_state = f_nizk_s.state.get(sid)
                    (not(nizk_Rel_sat(pr.x, pr.w))) implies (pr.pi == None)
                })
            }
            | _ => true
        }
    })

    // Verify Soundness: ∀x : (∀w : R(x,w) = 0) ⇒ ∀ Q, π: Q.Verify(x,π) = 0
    // If no witness exists in the relation for statement x, then verification must fail
    val VerifySoundness = f_nizk_s.log.forall(log => {
        match log.msg {
            | NIZK_VerifyResponse(vr) => {
                val sid_state = f_nizk_s.state.get(vr.cfg.sid)
                val no_witness_exists = ALL_WITNESSES.forall(w => not(nizk_Rel_sat(vr.x, w)))
                no_witness_exists implies (vr.b == false)
            }
            | _ => true
        }
    })
    
    // Completeness: ∀ x,w : R(x,w) = 1 ⇒ ∀ P Q, π: P.Prove(x,w) = π ⇒ Q.Verify(x, π) = 1
    // If prove succeeds with proof π, then verify(x, π) must return true
    val Completeness = f_nizk_s.log.forall(log1 => {
        match log1.msg {
            | NIZK_ProveResponse(pr) => {
                f_nizk_s.log.forall(log2 => {
                    match log2.msg {
                        | NIZK_VerifyResponse(vr) => {
                            and {
                                log1.idx < log2.idx,  // prove comes before verify
                                pr.cfg.sid == vr.cfg.sid,
                                pr.pi != None,
                                pr.x == vr.x,
                                pr.pi.unwrap() == vr.pi
                            } implies (vr.b == true)
                        }
                        | _ => true
                    }
                })
            }
            | _ => true
        }
    })
    
    // VerConsistent: ∀ x, π: Ver[x, π] = 1 ⇒ ∃ w: R(x,w) = 1
    // If Ver table says a proof is valid, there must exist a witness in the relation
    val VerConsistent = sids.forall(sid => {
        val sid_state = f_nizk_s.state.get(sid)
        sid_state.Ver.forall(entry => {
            (entry.b == true) implies {
                ALL_WITNESSES.exists(w => nizk_Rel_sat(entry.x, w))
            }
        })
    })

    // No flip-flop for Ver table: verification results are consistent
    // Once Ver[x,π] is set to a value, it shouldn't change to the opposite
    val NoVerFlipFlop = sids.forall(sid => {
        val sid_state = f_nizk_s.state.get(sid)
        sid_state.Ver.forall(entry => {
            not ({...entry, b: not(entry.b)}.in(sid_state.Ver))
        })
    })

    // ========== LIVENESS (must-fail invariants) ==========

    // MUST FAIL -- fails when at least one successful prove response appears in the log
    val ProveLiveness = f_nizk_s.log.forall(e => match e.msg {
        | NIZK_ProveResponse(r) => r.pi == None
        | _ => true
    })

    // MUST FAIL -- fails when at least one successful verify response appears in the log
    val VerifyLiveness = f_nizk_s.log.forall(e => match e.msg {
        | NIZK_VerifyResponse(r) => r.b == false
        | _ => true
    })

    // ========== COMBINED PROPERTIES ==========
    
    val AllSoundnessProps = and {
        ProveSoundness,
        VerifySoundness,
    }

    val AllConsistencyProps = and {
        VerConsistent,
        NoVerFlipFlop,
    }

    val AllBasicProps = and {
        NoOrphanedHolds,
        InputBound,
    }

    val AllProps = and {
        AllBasicProps,
        AllSoundnessProps,
        Completeness,
        AllConsistencyProps,
        //ProofNonMalleability,
        //VerifyAfterProve,
    }

}