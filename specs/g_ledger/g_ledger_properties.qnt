module g_ledger_properties {

    import basicSpells.* from "../../spells/basicSpells"
    import g_ledger_types.* from "./g_ledger_types"
    import g_ledger.* from "./g_ledger"
    import g_ledger_env.* from "./g_ledger_env"
    import g_clock_types.* from "../g_clock/g_clock_types"
    import g_clock.* from "../g_clock/g_clock"
    import common.* from "../common"    

    import g_ledger_env from "./g_ledger_env" 


 
// must fail
val MF1 = g_ledger_s.input.size() == 0
val MF2 = g_ledger_s.log.size() <= 7
val MF3 = g_ledger_s.log.map(x => x.msg).led_get_submit_response_messages().size() <= 2
val MF4 = g_ledger_s.log.map(x => x.msg).led_get_read_response_messages().size() <= 5
val MF5 = g_clock_s.input.size() == 0
val MF6 = g_clock_s.state.TIME <= 3
val MF7 = g_ledger_s.state.Com.keys().size() <= 3
val MF8 = g_ledger_s.state.APL.keys().size() <= 2
val MF9 = g_ledger_s.state.ptr.keys().size() <= 0

// Liveness: if a tx is submitted at time t by some party, and 
// the transaction remains valid for some number of rounds, then every other party will see it as part of 
// its (local) log at time t + ∆_latency + ∆_slack.
val LED_Liveness = g_ledger_s.log.forall(log_entry1 => {
    match log_entry1.msg {
        | SubmitResponse(p) => {
            // Only valid transactions (b=true for Normal txs) are guaranteed to appear in logs
            val is_valid_tx = match p.tx {
                | Normal(x) => x.b
                | _ => true
            }
            if (not(is_valid_tx)) true
            else {
                g_ledger_s.log.forall(log_entry2 => {
                    match log_entry2.msg {
                        | ReadRequest(q) => {
                            g_ledger_s.log.forall(log_entry3 => {
                                match log_entry3.msg {
                                    | ReadResponse(d) => {
                                        and {
                                            (log_entry1.time + delta_latency + delta_slack < log_entry2.time),
                                            log_entry1.idx < log_entry2.idx,
                                            d.cfg.nonce == q.cfg.nonce
                                        } implies {
                                            (d.result.listHas(p.tx))
                                        }
                                    }
                                    | _ => true
                                }
                            })
                        }
                        | _ => true
                    }
                })
            }
        }
        | _ => true
    }
})

 
// Consistency: for any two parties P and Q, with local logs log[0:ptr[P]] 
// and log[0:ptr[Q]] we either have log[0:ptr[P]]is a prefix of log[0:ptr[Q]] or vice versa.
val LED_Consistency = g_ledger_s.log.forall(log_entry1 => {
    match log_entry1.msg {
        | ReadResponse(p) => {
            g_ledger_s.log.forall(log_entry2 => {
                match log_entry2.msg {
                    | ReadResponse(q) => {
                        or {
                            p.result.isPrefixOf(q.result),
                            q.result.isPrefixOf(p.result),
                        }
                    }
                    | _ => true
                }
            })
        }   
        | _ => true     
    }
})

// Validity: For any (honest) P with local log log_P, all transactions in log_P are valid. 
val LED_Validity = g_ledger_s.log.forall(log_entry1 => {
    match log_entry1.msg {
        | ReadRequest(p) => {
            g_ledger_s.log.forall(log_entry2 => {
                match log_entry2.msg {
                    ReadResponse(q) => {
                        (p.cfg.nonce == q.cfg.nonce) implies {
                            q.result.indices().forall(i => {
                                val tx = q.result[i]
                                // NOTE: expecting that Val is pure def
                                led_valid(log_entry1.state.Val, log_entry2.state.log.mySlice(0,i), tx).valid 
                            })
                        }
                    }
                    | _ => true
                }       
            })
        }   
        | _ => true
    }
})

// Persistence: once a tx is added to log, it remains there forever.
val LED_Persistance = g_ledger_s.log.forall(log_entry1 => {
    match log_entry1.msg {
        | ReadResponse(p) => {
            g_ledger_s.log.forall(log_entry2 => {
                match log_entry2.msg {
                    | ReadResponse(q) => {
                        (log_entry1.idx < log_entry2.idx and p.cfg.party == q.cfg.party) implies {
                            p.result.isPrefixOf(q.result) //or q.result.isPrefixOf(p.result)
                        }
                    }
                    | _ => true
                }
            })
        }
        | _ => true
    }
})

// Agreed Prefi
val LED_AgreedPrefix = g_ledger_s.log.forall(log_entry1 => {
    match log_entry1.msg {
        | ReadResponse(p) => {
            g_ledger_s.log.forall(log_entry2 => {
                match log_entry2.msg {
                    | ReadRequest(q) => {
                        (q.cfg.nonce == p.cfg.nonce) implies {
                        p.result.length() >= log_entry1.state.log.indices().filter(i => {
                            log_entry2.time - log_entry1.state.log[i].time >= g_ledger_s.params.delta_slack
                        }).size()

                        }
                    }   
                    | _ => true
                }
            })

        }
        | _ => true
    }
})


val LED_AllProps = all {
    LED_Liveness,
    LED_Consistency,
    LED_Validity,
    LED_Persistance,
    LED_AgreedPrefix,
}

temporal NeverStuck = {
  // WF is required because TLA+ semantics treat stuttering as a valid step even if not explicitly modeled
  step.weakFair(Set(g_ledger_s)).implies ({
    step.weakFair(Set(g_clock_s)).implies ({
        not (eventually(
                always(
                    or {
                        g_ledger_s.input != Set(),
                        g_clock_s.input != Set()
                    }
                )
        ))
    })
  })
}

// ========== ComSel properties ===========
//c-Correctness:  for all ComSel responses which propose a committee Com the following is true: |Com| = c and Com ⊆ P
val ComSel_c_Correctness = g_ledger_s.log.map(x => x.msg).filterMap(z => match z {| ComSelResponse(p) => Some(p) | _ => None }).forall(comsel_resp => {
    and {
        // |Com| = c
        comsel_resp.com.length() == g_ledger_s.params.comsel_c,
        // Com ⊆ P (all committee members are in the set of parties P)
        comsel_resp.com.indices().forall(i => {
            g_ledger_s.params.P.contains(comsel_resp.com[i])
        })
    }
})


//(a/b)-Security: For all ComSel responses whihc propose committee Com the following is true:  b * |Com ⋂ C| ≤ a * |Com|
val ComSel_ab_Security = g_ledger_s.log.map(x => x.msg).filterMap(z => match z {| ComSelResponse(p) => Some(p) | _ => None }).forall(comsel_resp => {
    // Count |Com ⋂ C| - number of corrupt parties in the committee
    val corrupt_in_committee = comsel_resp.com.indices().filter(i => {
        g_ledger_s.params.C.contains(comsel_resp.com[i])
    }).size()
    
    // b * |Com ⋂ C| ≤ a * |Com|
    g_ledger_s.params.comsel_b * corrupt_in_committee <= g_ledger_s.params.comsel_a * comsel_resp.com.length()
})

// Agreement: For all P and Q we have that if Com = P.ComSel() and Com’ = Q.ComSel() in the same round, then Com = Com’. 
val ComSel_Agreement = g_ledger_s.log.forall(log_entry1 => {
    match log_entry1.msg {
        | ComSelResponse(p) => {
            g_ledger_s.log.forall(log_entry2 => {
                match log_entry2.msg {
                    | ComSelResponse(q) => {
                        // If both ComSel responses are in the same round, their committees must be equal
                        (p.r == q.r) implies {
                            p.com == q.com
                        }
                    }
                    | _ => true
                }
            })
        }
        | _ => true
    }
})

val ComSel_AllProps = all {
    ComSel_c_Correctness,
    ComSel_ab_Security,
    ComSel_Agreement,
}

// ========== APL properties ===========

// Agreement: For any two parties P and Q requesting apl in the same round r the returned pointers are the same.
val APL_Agreement = g_ledger_s.log.forall(log_entry1 => {
    match log_entry1.msg {
        | APLResponse(p) => {
            g_ledger_s.log.forall(log_entry2 => {
                match log_entry2.msg {
                    | APLResponse(q) => {
                        // If both APL responses are in the same round, their pointers must be equal
                        (p.r == q.r) implies {
                            p.p == q.p
                        }
                    }
                    | _ => true
                }
            })
        }
        | _ => true
    }
})

// Monotonicity: For any honest party P that requests APL in rounds r and r' ≥ r we have that p_r' ≥ p_r.
val APL_Monotonicity = g_ledger_s.log.forall(log_entry1 => {
    match log_entry1.msg {
        | APLResponse(p) => {
            g_ledger_s.log.forall(log_entry2 => {
                match log_entry2.msg {
                    | APLResponse(q) => {
                        // For the same honest party in rounds r and r' where r' >= r
                        and {
                            p.cfg.party == q.cfg.party,
                            g_ledger_s.params.P.contains(p.cfg.party),  // honest party
                            not(g_ledger_s.params.C.contains(p.cfg.party)),  // not corrupt
                            q.r >= p.r
                        } implies {
                            q.p >= p.p
                        }
                    }
                    | _ => true
                }
            })
        }
        | _ => true
    }
})

// delta-Growth: for any honest party P that requests APL in rounds r and r' ≥ r + delta with values p_r and p_{r'} we have that p_{r'} ≥ p_r + 1. 
val APL_DeltaGrowth =g_ledger_s.log.forall(log_entry1 => {
    match log_entry1.msg {
        | APLResponse(p) => {
            g_ledger_s.log.forall(log_entry2 => {
                match log_entry2.msg {
                    | APLResponse(q) => {
                        // For the same honest party in rounds r and r' where r' >= r + delta
                        and {
                            p.cfg.party == q.cfg.party,
                            p.cfg.sid == q.cfg.sid,
                            g_ledger_s.params.P.contains(p.cfg.party),  
                            not(g_ledger_s.params.C.contains(p.cfg.party)),  // not corrupt
                            q.r >= p.r + g_ledger_s.params.delta_apl
                        } implies {
                            q.p >= p.p + 1
                        }
                    }
                    | _ => true
                }
            })
        }
        | _ => true
    }
})


val APL_AllProps = all {
    APL_Agreement,
    APL_Monotonicity,
    //APL_DeltaGrowth,
}


/// ===== GENESIS properties =====
// Genesis Correctness: For any party P we have that the response of P.Gensis is genesis (which is a parameter of G-Ledger).
val Genesis_Correctness = g_ledger_s.log.forall(log_entry => {
    match log_entry.msg {
        | GenesisResponse(resp) => {
            resp.genesis == g_ledger_s.state.genesis
        }
        | _ => true
    }
})

val AllProps = all {
    LED_AllProps,
    ComSel_AllProps,
    APL_AllProps,
    Genesis_Correctness,
}


}

