module f_atms_properties{
    
    import basicSpells.* from "../../spells/basicSpells"
    import f_atms_types.* from "./f_atms_types"
    import f_atms.* from "./f_atms"
    import f_atms_env.* from "./f_atms_env"
    import common.* from "../common"    
    import params.* from "../params"
   

    // ====== CO-Properties (must fail) ======
    val P1 = f_atms_s.log.size() <= 10
    val P2 = f_atms_s.log.map(x => x.msg).atms_get_gen_response_messages().size() <= 2
    val P3 = f_atms_s.log.map(x => x.msg).atms_get_akey_response_messages().size() <= 2
    val P4 = f_atms_s.log.map(x => x.msg).atms_get_acheck_response_messages().size() <= 2
    val P5 = f_atms_s.log.map(x => x.msg).atms_get_sign_response_messages().size() <= 2
    val P6 = f_atms_s.log.map(x => x.msg).atms_get_verify_response_messages().size() <= 2
    val P7 = f_atms_s.log.map(x => x.msg).atms_get_asign_response_messages().size() <= 2
    val P8 = f_atms_s.log.map(x => x.msg).atms_get_averify_response_messages().size() <= 2



    // ====== Properties (must hold) ======

    // Handles are properly managed (no orphaned holds)
    val NoOrphanedHolds = f_atms_s.handles.forall(h => {
        match h {
            | SIMAKeyHold(_) => true
            | SIMACheckHold(_) => true
            | SIMASignHold(_) => true
            | SIMAVerifyHold(_) => true
            | SIMGenHold(_) => true
            | SIMSignHold(_) => true
            | SIMVerifyHold(_) => true
            | _ => false
        }
    })
    // Input buffer has at most one message (as per design)
    val InputBound = f_atms_s.input.size() <= 1        
    

// no deadlock
temporal NeverStuck = {
  // WF is required because TLA+ semantics treat stuttering as a valid step even if not explicitly modeled
  step.weakFair(Set(f_atms_s)).implies ({
        not (eventually(
                always(
                    or {
                        f_atms_s.input != Set()
                    }
                )
        ))
    })
}

// Property: VK, VER, VKs, AVK, AVER are functional
val VK_Functional = sids.forall(sid => {
    val sid_state = f_atms_s.state.get(sid)
    sid_state.VK.forall(vk1 => {
        sid_state.VK.forall(vk2 => {
            (vk1.party == vk2.party) implies (vk1 == vk2)
        })
    })
})

val VER_Functional = sids.forall(sid => {
    val sid_state = f_atms_s.state.get(sid)
    sid_state.VER.forall(ver1 => {
        sid_state.VER.forall(ver2 => {
            and {
                ver1.vk == ver2.vk,
                ver1.m == ver2.m,
                ver1.sig == ver2.sig
            } implies (ver1 == ver2)
        })
    })
})

val VKs_Functional = sids.forall(sid => {
    val sid_state = f_atms_s.state.get(sid)
    sid_state.VKs.forall(vks1 => {
        sid_state.VKs.forall(vks2 => {
            (vks1.avk == vks2.avk) implies (vks1 == vks2)
        })
    })
})

val AVK_Functional = sids.forall(sid => {
    val sid_state = f_atms_s.state.get(sid)
    sid_state.AVK.forall(avk1 => {
        sid_state.AVK.forall(avk2 => {
            (avk1.vks == avk2.vks) implies (avk1 == avk2)
        })
    })
})

val AVER_Functional = sids.forall(sid => {
    val sid_state = f_atms_s.state.get(sid)
    sid_state.AVER.forall(aver1 => {
        sid_state.AVER.forall(aver2 => {
            and {
                aver1.avk == aver2.avk,
                aver1.m == aver2.m,
                aver1.sig == aver2.sig
            } implies (aver1 == aver2)
        })
    })
})

// Property: VK, VKs, AVK are injective (for example, if VK[P] and VK[Q] defined and VK[P] = VK[Q] then P = Q)
val VK_Injective = sids.forall(sid => {
    val sid_state = f_atms_s.state.get(sid)
    sid_state.VK.forall(vk1 => {
        sid_state.VK.forall(vk2 => {
            (vk1.vk == vk2.vk) implies (vk1.party == vk2.party)
        })
    })
})


val VKs_Injective = sids.forall(sid => {
    val sid_state = f_atms_s.state.get(sid)
    sid_state.VKs.forall(vks1 => {
        sid_state.VKs.forall(vks2 => {
            (vks1.vks == vks2.vks and vks1.vks != List()) implies (vks1.avk == vks2.avk)
        })
    })
})

val AVK_Injective = sids.forall(sid => {
    val sid_state = f_atms_s.state.get(sid)
    sid_state.AVK.forall(avk1 => {
        sid_state.AVK.forall(avk2 => {
            and {
                avk1.avk != None,
                avk2.avk != None,
                avk1.avk == avk2.avk,
            } implies (avk1.vks == avk2.vks)
        })
    })
})



// Determinism of Gen, acheck, ver, aver properties (requests with same input lead to same output)
// go through a log: if there are two responses with the same inputs then their outputs must be the same

// TODO: check the preconditions, for example should Acheck be deterministic across parties, or not?
val Gen_Deterministic = f_atms_s.log.map(x => x.msg).atms_get_gen_response_messages().forall(msg1 => {
    f_atms_s.log.map(x => x.msg).atms_get_gen_response_messages().forall(msg2 => {
        (and {
            msg1.cfg.party == msg2.cfg.party,
            msg1.cfg.sid == msg2.cfg.sid,
        }) implies (msg1.vk == msg2.vk)
    })
})
 
val ACheck_Deterministic = f_atms_s.log.map(x => x.msg).atms_get_acheck_response_messages().forall(msg1 => {
    f_atms_s.log.map(x => x.msg).atms_get_acheck_response_messages().forall(msg2 => {
        (and {
            msg1.cfg.party == msg2.cfg.party,
            msg1.cfg.sid == msg2.cfg.sid,
            msg1.vks == msg2.vks,
            msg1.avk == msg2.avk,
        }) implies (msg1.b == msg2.b)
    })
})

val Verify_Deterministic = f_atms_s.log.map(x => x.msg).atms_get_verify_response_messages().forall(msg1 => {
    f_atms_s.log.map(x => x.msg).atms_get_verify_response_messages().forall(msg2 => {
        (and {
            msg1.cfg.party == msg2.cfg.party,
            msg1.cfg.sid == msg2.cfg.sid,
            msg1.vk == msg2.vk,
            msg1.m == msg2.m,
            msg1.sig == msg2.sig,
        }) implies (msg1.b == msg2.b)
    })
})

val AVerify_Deterministic = f_atms_s.log.map(x => x.msg).atms_get_averify_response_messages().forall(msg1 => {
    f_atms_s.log.map(x => x.msg).atms_get_averify_response_messages().forall(msg2 => {
        (and {
            msg1.cfg.party == msg2.cfg.party,
            msg1.cfg.sid == msg2.cfg.sid,
            msg1.avk == msg2.avk,
            msg1.m == msg2.m,
            msg1.sig == msg2.sig,
        }) implies (msg1.b == msg2.b)
    })
})

// Non-bottom responses: True for Gen and Ver by typing.
// Sign non-bottom response: Sign Response is non-bottom if the party has entry in VK
 
val Sign_NonBottom_Response = f_atms_s.log.forall(entry2 => {
    match entry2.msg {
    | SignResponse(sign_resp) =>
        val has_vk = f_atms_s.log.exists(entry1 => match entry1.msg {
            | GenResponse(gen) => and {
                gen.cfg.party == sign_resp.cfg.party,
                gen.cfg.sid == sign_resp.cfg.sid,
                entry1.idx < entry2.idx
            }
            | _ => false
        })
        has_vk implies (sign_resp.sig != None)
    | _ => true
    }
})

val Signers_Correctness_1 = sids.forall(sid => {
    val sid_state = f_atms_s.state.get(sid)
    sid_state.Signers.forall(signer =>
        f_atms_s.log.map(x => x.msg).union(f_atms_s.input).atms_get_sign_response_messages().exists(sign =>
            and {
                sign.cfg.sid == sid,
                sign.m == signer.m,
                sign.sig != None,
                // Check that this vk belongs to the signing party
                sid_state.VK.exists(vk_entry =>
                    and {
                        vk_entry.vk == signer.vk,
                        vk_entry.party == sign.cfg.party
                    }
                )
            }
        )
    )
})

val Signers_Correctness_2 = sids.forall(sid => {
    val sid_state = f_atms_s.state.get(sid)
    sid_state.VER.forall(ver =>
        if (and {
            ver.b == true,
            sid_state.VK.exists(vk_entry =>
                and {
                    vk_entry.vk == ver.vk,
                    not (f_atms_s.params.C.contains(vk_entry.party))
                }
            )
        }) {
            sid_state.Signers.exists(signer =>
                and {
                    signer.vk == ver.vk,
                    signer.m == ver.m
                }
            )
        } else {
            true
        }
    )
})

// ACheck correctness: Any vks that have been aggregated to an avk ≠ None will pass ACheck
// for all AKey responses in the log, if avk != None then ACheck of that avk and vks must be true
val ACheck_Correctness = f_atms_s.log.forall(entry1 => {
    match entry1.msg {
    | AKeyResponse(akey_resp) =>
        match akey_resp.avk {
        | None => true
        | Some(avk_val) =>
            f_atms_s.log.forall(entry2 => {
                match entry2.msg {
                | ACheckResponse(acheck_resp) =>
                    (and {
                        entry1.idx < entry2.idx,
                        acheck_resp.cfg.sid == akey_resp.cfg.sid,
                        acheck_resp.vks == akey_resp.vks,
                        acheck_resp.avk == avk_val
                    }) implies (acheck_resp.b == true)
                | _ => true
                }
            })
        }
    | _ => true
    }
})

//Sig Correctness: Any signature generated by an honest party will later pass verification. (As in F-Sig.) 
// for all sig responses in the log, if party is honest then Verify response of that sig must be true
val Sig_Correctness = f_atms_s.log.map(x => x.msg).atms_get_sign_response_messages().forall(sign_resp => {
    match sign_resp.sig {
        | None => true
        | Some(sig_val) => {
            if (not (f_atms_s.params.C.contains(sign_resp.cfg.party))) {
                // Party is honest, find their VK from Gen responses
                val gen_responses = f_atms_s.log.map(x => x.msg).atms_get_gen_response_messages().filter(gen => 
                    and {
                        gen.cfg.party == sign_resp.cfg.party,
                        gen.cfg.sid == sign_resp.cfg.sid
                    }
                )
                gen_responses.forall(gen_resp => {
                    // For this VK, check all Verify responses
                    f_atms_s.log.map(x => x.msg).atms_get_verify_response_messages().forall(verify_resp => {
                        (and {
                            verify_resp.cfg.sid == sign_resp.cfg.sid,
                            verify_resp.vk == gen_resp.vk,
                            verify_resp.m == sign_resp.m,
                            verify_resp.sig == sig_val
                        }) implies (verify_resp.b == true)
                    })
                })
            } else {
                true  // Not an honest party, no constraint
            }
        }
    }
})


// Avk correctness: Any bunch of honestly generated vk's can be aggregated to an avk ≠ ⊥. 
// for all AVK responses in the log, if all vks are from honest parties then avk != None
val AVK_Correctness = f_atms_s.log.forall(log1 => {
    Set(log1.msg).atms_get_akey_response_messages().forall(akey_resp => {
    // Find all Gen responses to determine which parties own which VKs

    val sid_state = log1.state.get(akey_resp.cfg.sid)
    // Check if all VKs in this akey_resp.vks are from honest parties
    val all_vks_from_honest = akey_resp.vks.indices().forall(i => {
            val vk = akey_resp.vks[i]   
            sid_state.VK.exists(x => and {
                x.vk == vk,
                //not (f_atms_s.params.C.contains(x.party))
                f_atms_s.params.P.exclude(f_atms_s.params.C).contains(x.party)
            })
    })
    
    // If all VKs are from honest parties and the list is non-empty, then avk should not be None
    (and {
        all_vks_from_honest,
        akey_resp.vks != List()
    }) implies (akey_resp.avk != None)
    })
})

//ASign Correctness: Given a set of at least t + 1 honestly generated signatures (obtained from P.Sign for some honest P) on the same message, one can compute an aggregate signature which will verify.
val ASign_Correctness = f_atms_s.log.forall(asign_entry => {
    match asign_entry.msg {
    | ASignResponse(asign_resp) =>
        val honest_sigs_count = asign_resp.S.indices().filter(s => {
            f_atms_s.log.exists(e => match e.msg {
                | GenResponse(gen) => and {
                    gen.vk == asign_resp.S[s].vk,
                    gen.cfg.sid == asign_resp.cfg.sid,
                    e.idx < asign_entry.idx,
                    not (f_atms_s.params.C.contains(gen.cfg.party))
                }
                | _ => false
            })
        }).size()

        val weird_check = and {
            asign_resp.avk != None,
            asign_resp.S.indices().forall(i => {
                asign_resp.vks.indices().filter(j => asign_resp.vks[j] == asign_resp.S[i].vk).size() == 1
            })
        }

        val all_sigs_authentic = asign_resp.S.indices().forall(s => {
            f_atms_s.log.forall(gen_entry => {
                match gen_entry.msg {
                | GenResponse(gen) =>
                    if (and {
                        gen.vk == asign_resp.S[s].vk,
                        gen.cfg.sid == asign_resp.cfg.sid,
                        gen_entry.idx < asign_entry.idx,
                        not (f_atms_s.params.C.contains(gen.cfg.party))
                    }) {
                        f_atms_s.log.exists(sign_entry => match sign_entry.msg {
                            | SignResponse(sign) => and {
                                sign.cfg.party == gen.cfg.party,
                                sign.cfg.sid == asign_resp.cfg.sid,
                                sign.m == asign_resp.m,
                                sign.sig == Some(asign_resp.S[s].sig),
                                sign_entry.idx < asign_entry.idx
                            }
                            | _ => false
                        })
                    } else {
                        true
                    }
                | _ => true
                }
            })
        })

        val tau_verifies = match asign_resp.tau {
        | None => true
        | Some(tau_val) =>
            match asign_resp.avk {
            | None => true
            | Some(avk_val) =>
                f_atms_s.log.forall(averify_entry => {
                    match averify_entry.msg {
                    | AVerifyResponse(averify_resp) =>
                        (and {
                            averify_resp.cfg.sid == asign_resp.cfg.sid,
                            averify_resp.avk == avk_val,
                            averify_resp.m == asign_resp.m,
                            averify_resp.sig == tau_val,
                            asign_entry.idx < averify_entry.idx
                        }) implies (averify_resp.b == true)
                    | _ => true
                    }
                })
            }
        }

        (and {
            honest_sigs_count >= f_atms_s.params.t + 1,
            all_sigs_authentic,
            weird_check
        }) implies (and {
            asign_resp.tau != None,
            tau_verifies
        })
    | _ => true
    }
})

// Sign Unforgeability: No signature under an honest Party's key in VK verifies unless generated by that party in the past.
val Sign_Unforgeability = f_atms_s.log.forall(verify_entry => {
    match verify_entry.msg {
    | VerifyResponse(verify_resp) =>
        if (verify_resp.b == true) {
            val gen_entries = f_atms_s.log.filter(e => match e.msg {
                | GenResponse(gen) => and {
                    gen.vk == verify_resp.vk,
                    gen.cfg.sid == verify_resp.cfg.sid,
                    e.idx < verify_entry.idx
                }
                | _ => false
            })
            gen_entries.forall(gen_entry => {
                match gen_entry.msg {
                | GenResponse(gen_resp) =>
                    if (not (f_atms_s.params.C.contains(gen_resp.cfg.party))) {
                        f_atms_s.log.exists(sign_entry => match sign_entry.msg {
                            | SignResponse(sign_resp) => and {
                                sign_resp.cfg.party == gen_resp.cfg.party,
                                sign_resp.cfg.sid == verify_resp.cfg.sid,
                                sign_resp.m == verify_resp.m,
                                sign_resp.sig == Some(verify_resp.sig),
                                sign_entry.idx < verify_entry.idx
                            }
                            | _ => false
                        })
                    } else {
                        true
                    }
                | _ => true
                }
            })
        } else {
            true
        }
    | _ => true
    }
})

/*
ASign Unforgeability: If (avk, m, σ) verifies for some P (Aver response exists), 
then for every vks queried to P.ACheck such that P.ACheck(avk, vk*) = 1, 
the number of vk ∈ vk* for which 
     vk does not appear in VK
  OR there is an honest P such that there is a Gen response and SignResponse (for message m)
  OR there is some corrupt P such that there is Gen response with that vk 
is at least t. 
*/

val ASign_Unforgeability = f_atms_s.log.forall(averify_entry => {
    match averify_entry.msg {
    | AVerifyResponse(averify_resp) =>
        if (averify_resp.b == true) {
            val valid_acheck_entries = f_atms_s.log.filter(e => match e.msg {
                | ACheckResponse(acheck) => and {
                    acheck.cfg.sid == averify_resp.cfg.sid,
                    acheck.avk == averify_resp.avk,
                    acheck.b == true,
                    e.idx < averify_entry.idx
                }
                | _ => false
            })
            valid_acheck_entries.forall(acheck_entry => {
                match acheck_entry.msg {
                | ACheckResponse(acheck_resp) =>
                    val valid_vk_count = acheck_resp.vks.indices().filter(i => {
                        val vk = acheck_resp.vks[i]

                        val no_gen = not (f_atms_s.log.exists(e => match e.msg {
                            | GenResponse(gen) => and {
                                gen.vk == vk,
                                gen.cfg.sid == averify_resp.cfg.sid,
                                e.idx < averify_entry.idx
                            }
                            | _ => false
                        }))

                        val honest_signed = f_atms_s.log.exists(e => match e.msg {
                            | GenResponse(gen) => and {
                                gen.vk == vk,
                                gen.cfg.sid == averify_resp.cfg.sid,
                                e.idx < averify_entry.idx,
                                not (f_atms_s.params.C.contains(gen.cfg.party)),
                                f_atms_s.log.exists(e2 => match e2.msg {
                                    | SignResponse(sign) => and {
                                        sign.cfg.party == gen.cfg.party,
                                        sign.cfg.sid == averify_resp.cfg.sid,
                                        sign.m == averify_resp.m,
                                        sign.sig != None,
                                        e2.idx < averify_entry.idx
                                    }
                                    | _ => false
                                })
                            }
                            | _ => false
                        })

                        val corrupt_party = f_atms_s.log.exists(e => match e.msg {
                            | GenResponse(gen) => and {
                                gen.vk == vk,
                                gen.cfg.sid == averify_resp.cfg.sid,
                                e.idx < averify_entry.idx,
                                f_atms_s.params.C.contains(gen.cfg.party)
                            }
                            | _ => false
                        })

                        or { no_gen, honest_signed, corrupt_party }
                    }).size()

                    valid_vk_count >= f_atms_s.params.t + 1
                | _ => true
                }
            })
        } else {
            true
        }
    | _ => true
    }
})

val AllFunctional = all {
    VK_Functional,
    VER_Functional,
    AVK_Functional,
    AVER_Functional,
    VKs_Functional
}

val AllInjective = all {
    VK_Injective,
    VKs_Injective,
    AVK_Injective
}

val AllDeterministic = all {
    Gen_Deterministic,
    ACheck_Deterministic,
    Verify_Deterministic,
    AVerify_Deterministic
}

val AllCorrectness = all {
    ACheck_Correctness,
    Sig_Correctness,
    AVK_Correctness,
    ASign_Correctness,
    Sign_NonBottom_Response,
    Signers_Correctness_1,
    Signers_Correctness_2,
    ASign_ThresholdNecessary,    
}

val AllUnforgeability = all {
    Sign_Unforgeability,
    ASign_Unforgeability
}


val ASign_ThresholdNecessary = f_atms_s.log.map(x => x.msg).atms_get_asign_response_messages().forall(asign =>
    (asign.tau != None) implies (asign.S.length() >= f_atms_s.params.t + 1)
)

val AVK_ThresholdNecessary = f_atms_s.log.map(x => x.msg).atms_get_akey_response_messages().forall(akey =>
    (akey.avk != None) implies (akey.vks.length() >= f_atms_s.params.t + 1)
)

val AllProps = all {
    AllFunctional,
    AllInjective,
    AllDeterministic,
    AllCorrectness,
    AllUnforgeability,
    ASign_ThresholdNecessary,
    AVK_ThresholdNecessary,
}



}