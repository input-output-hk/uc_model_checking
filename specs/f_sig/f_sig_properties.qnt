module f_sig_properties {
    
    import basicSpells.* from "../../spells/basicSpells"
    import f_sig_types.* from "./f_sig_types"
    import f_sig.* from "./f_sig"
    import f_sig_env.* from "./f_sig_env"
    import common.* from "../common"    


    // MUST FAILS   
    val GenLiveness  = sids.forall(sid => f_sig_s.state.get(sid).VK.size() <= 1) 
    val SignLiveness = sids.forall(sid => f_sig_s.state.get(sid).Ver.size() <= 5) 
    // If Ver containts some vk entry  then VK table must  contain respective entry
    val AdvCanVerify = sids.forall(sid => {
        val sid_state = f_sig_s.state.get(sid)
        sid_state.Ver.forall(
            entry => sid_state.VK.exists(p => p.vk == entry.vk)
        )
    })

    // MUST HOLD

    // Handles are properly managed (no orphaned holds)
    val NoOrphanedHolds = f_sig_s.handles.forall(h => {
        match h {
            | SIG_SIMGenHold(_) => true
            | SIG_SIMSignHold(_) => true
            | SIG_SIMVerifyHold(_) => true
            | _ => false
        }
    })
    // Input buffer has at most one message (as per design)
    val InputBound = f_sig_s.input.size() <= 1    

    // Property: party has at most one verification key
    val OneVKperUser = sids.forall(sid => {
        val sid_state = f_sig_s.state.get(sid)
        Parties.forall(p => {
            sid_state.VK.filter(x => x.party == p).size() <= 1
        })
    })
    // Property: verification keys are unique 
    val VKOwnershipUnique = sids.forall(sid => {
        val sid_state = f_sig_s.state.get(sid)
        sid_state.VK.forall(entry1 => {
            sid_state.VK.forall(entry2 => {
                (entry1.vk == entry2.vk) implies (entry1.party == entry2.party)
            })
        })
    })


    // Property: verification results are consistent (no contradictory entries)
      val VerConsistent = sids.forall(sid => {
        val sid_state = f_sig_s.state.get(sid)
        sid_state.Ver.forall(entry => {
            not ({...entry, b : not(entry.b)}.in(sid_state.Ver))
        })})

     
    // Completeness: If sign response has signature then later it verifies successfully
    val Completeness = f_sig_s.log.forall(log1 => {
        match log1.msg {
            | SIG_SignResponse(p) => {
                f_sig_s.log.forall(log2 => {
                    match log2.msg {
                        | SIG_VerifyResponse(q) => {
                            and {
                                log1.idx < log2.idx, // sign response comes before verify response
                                p.cfg.sid == q.cfg.sid,
                               p.sig != None,
                                p.m == q.m,
                                q.vk == f_sig_s.state.get(q.cfg.sid).VK.find(vk_entry => vk_entry.party == p.cfg.party).unwrap().vk, 
                            } implies ({
                                and {
                                    p.sig.unwrap() == q.sig,
                                    //true
                                } implies {
                                    q.b == true
                                    //false
                                }
                            })
                        }
                        | _ => true
                    }
                })
            }
            |_ => true
        }
    })



    // Unforgeability: Cannot verify a signature for a message that was never signed
    // by the party whose VK is used, unless the party is corrupted
    val Unforgeability = f_sig_s.log.forall(log => {
        match log.msg {
            | SIG_VerifyResponse(vr) => {
                val sid_state = f_sig_s.state.get(vr.cfg.sid)
                val maybe_owner = sid_state.VK.find(vk_entry => vk_entry.vk == vr.vk)
                
                (vr.b == true and maybe_owner != None) implies {
                    val owner = maybe_owner.unwrap().party
                    or {
                        // Either the owner is corrupted
                        owner.in(f_sig_s.params.C),
                        // Or there exists a sign response for this message by this party
                        f_sig_s.log.exists(log2 => {
                            match log2.msg {
                                | SIG_SignResponse(sr) => {
                                    and {
                                        log2.idx < log.idx,
                                        sr.cfg.party == owner,
                                        sr.cfg.sid == vr.cfg.sid,
                                        sr.m == vr.m,
                                        sr.sig == Some(vr.sig)
                                    }
                                }
                                | _ => false
                            }})}}}
            | _ => true
        }})

    // No signature reuse: A signature that verifies for one message shouldn't verify for a different message (assuming honest VK owner)
    // FAILS
    val NoSignatureReuse = sids.forall(sid => {
        val sid_state = f_sig_s.state.get(sid)
        sid_state.Ver.forall(entry1 => {
            sid_state.Ver.forall(entry2 => {
                and {
                    entry1.vk == entry2.vk,
                    entry1.sig == entry2.sig,
                    entry1.b == true,
                    entry2.b == true,
                    sid_state.VK.exists(vk_entry => {
                        and {
                            vk_entry.vk == entry1.vk,
                            not(vk_entry.party.in(f_sig_s.params.C))
                        }
                    })
                } implies (entry1.m == entry2.m)
            })
        })
    })

    // Can only sign if VK has been generated for that party
    val VKBeforeSign = f_sig_s.log.forall(log => {
        match log.msg {
            | SIG_SignResponse(sr) => {
                (sr.sig != None) implies {
                    f_sig_s.log.exists(log2 => {
                        match log2.msg {
                            | SIG_GenResponse(gr) => {
                                and {
                                    log2.idx < log.idx,
                                    gr.cfg.party == sr.cfg.party,
                                    gr.cfg.sid == sr.cfg.sid
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

    val AllProps = all {
        OneVKperUser,
        VKOwnershipUnique,
        VerConsistent,
        Completeness,
        Unforgeability,
        VKBeforeSign,
        // NoSignatureReuse,  // FAILS
        NoOrphanedHolds,
        InputBound   
    }

}