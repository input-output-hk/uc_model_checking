module f_dif_properties {
    
    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"
    import common.* from "../common"
    import g_clock_types.* from "../g_clock/g_clock_types"
    import g_clock.* from "../g_clock/g_clock"    
    import f_dif_types.*   from "./f_dif_types"
    import f_dif.*  from "./f_dif"
    import f_dif_env.*  from "./f_dif_env" 


// ========== CO-PROPERTIES (should fail - used for model checking bounds) ==========

val P1 = f_dif_s.input.size() == 0
val P2 = f_dif_s.log.size() <= 7
val P3 = f_dif_s.log.map(x => x.msg).dif_get_ok_response_messages().size() <= 2
val P4 = f_dif_s.log.map(x => x.msg).dif_get_fetch_response_messages().size() <= 2
val P5 = f_dif_s.log.map(x => x.msg).dif_get_diffuse_response_messages().size() <= 4
val P6 = g_clock_s.input.size() == 0
val P7 = g_clock_s.input.map(x => x).gc_get_all_response_messages().size() == 0
val P8 = g_clock_s.state.TIME <= 5
val P9 = f_dif_s.log.map(x => x.msg).dif_get_fetch_response_messages().forall(m => m.data.size() == 0)
val P10 = f_dif_s.handles.dif_get_sim_diffuse_hold_messages().size() <= 2

 

// At time t a message is delivered at most once to the same party
val NoDuplication = f_dif_s.log.forall(log1 => {
    match log1.msg {
        | DIF_FetchResponse(fr1) => {
            f_dif_s.log.forall(log2 => {
                match log2.msg {
                    | DIF_FetchResponse(fr2) => {
                        and {
                            log1.idx != log2.idx,
                            fr1.cfg.party == fr2.cfg.party,
                            fr1.cfg.sid == fr2.cfg.sid,
                            log1.time == log2.time
                        } implies {
                            // Data sets should be disjoint (no message delivered twice)
                            fr1.data.intersect(fr2.data).size() == 0
                        }
                    }
                    | _ => true
                }
            })
        }
        | _ => true
    }
})


// Broadcast property: At every moment L[*,ctr] > 1 then {P | L[P,ctr]} == P
// (not true because fetch breaks L)
val BroadcastToAll = sids.forall(sid => {
    val sid_state = f_dif_s.state.get(sid)
    // For each counter value in L
    sid_state.L.keys().forall(k1 => {
        // If message was diffused (same counter appears for multiple recipients)
        val same_ctr_keys = sid_state.L.keys().filter(k2 => k2.ctr == k1.ctr)
        (same_ctr_keys.size() > 1) implies {
            // Then it should exist for all parties
            f_dif_s.params.P.forall(party => {
                same_ctr_keys.exists(k => k.to == party)
            })
        }
    })
})

// ========== PROPERTIES ==========

// ∆-delayed Liveness: if P diffuses a message m in some round t, and Q attempts to fetch in some
// round t' ≥ t + ∆ then Q receives m in some round t'' ∈ [t, t'].
val DDL = {
    f_dif_s.log.forall(msg1 =>
        match msg1.msg {
            | DIF_DiffuseRequest(p) => {
                f_dif_s.log.forall(msg3 =>
                    match msg3.msg {
                        | DIF_FetchResponse(d) => {
                            val is_late_fetch = p.cfg.sid == d.cfg.sid and d.t_prim >= msg1.time + delta_param
                            is_late_fetch implies {
                                p.m.in(d.data) or
                                f_dif_s.log.exists(msg4 =>
                                    match msg4.msg {
                                        | DIF_FetchResponse(d2) =>
                                            d2.cfg.party == d.cfg.party and
                                            d2.cfg.sid == d.cfg.sid and
                                            msg4.idx < msg3.idx and
                                            p.m.in(d2.data)
                                        | _ => false
                                    }
                                )
                            }
                        }
                        | _ => true
                    }
                )
            }
            | _ => true
        }
    )
}

// Authentication: If a nonempty set of messages was delivered then they were indeed diffused or injected in the past.
val Auth = f_dif_s.log.forall(log_entry1 => {
    match log_entry1.msg {
        | DIF_FetchResponse(p) => { 
            (p.data != Set()) implies p.data.forall(data => {
                f_dif_s.log.exists(log_entry2 => {
                    match log_entry2.msg {
                        | DIF_DiffuseRequest(q) => q.m == data
                        | DIF_InjectRequest(q) => and {
                            q.m == data,
                            q.Q == p.cfg.party
                        }
                        | _ => false
                    }
                })
            })
        }
        | _ => true
    }
})

// ========== DIFFUSION-SPECIFIC PROPERTIES ==========



// Diffused messages have the same data for all recipients
val DiffusionConsistency = sids.forall(sid => {
    val sid_state = f_dif_s.state.get(sid)
    sid_state.L.keys().forall(k1 => {
        sid_state.L.keys().forall(k2 => {
            (k1.ctr == k2.ctr) implies {
                sid_state.L.get(k1).data == sid_state.L.get(k2).data
            }
        })
    })
})


// Only corrupted parties can inject
val InjectAccessControl = f_dif_s.log.forall(log_entry => {
    match log_entry.msg {
        | DIF_InjectRequest(p) => {
            p.cfg.party.in(f_dif_s.params.C)
        }
        | _ => true
    }
})

// ========== BASIC SYSTEM PROPERTIES ==========

// Handles are properly managed (no orphaned holds)
val NoOrphanedHolds = f_dif_s.handles.forall(h => {
    match h {
        | DIF_SIMDiffuseHold(_) => true
        | DIF_SIMFetchHold(_) => true
        | DIF_ClockRequestWait(_) => true
        | DIF_OKRequestWait(_) => true
        | _ => false
    }
})

// Input buffer has at most one message (as per design)
val InputBound = f_dif_s.input.size() <= 1

// Messages in L have valid counters
val ValidCounters = sids.forall(sid => {
    val sid_state = f_dif_s.state.get(sid)
    sid_state.L.keys().forall(k => {
        k.ctr > 0 and k.ctr <= sid_state.ctr
    })
})



// Liveness: system is never stuck
temporal NeverStuck = {
  // WF is required because TLA+ semantics treat stuttering as a valid step even if not explicitly modeled
  step.weakFair(Set(f_dif_s)).implies ({
    step.weakFair(Set(g_clock_s)).implies ({
        not (eventually(
                always(
                    or {
                        f_dif_s.input != Set(),
                        g_clock_s.input != Set()
                    }
                )
        ))
    })
  })
}

// ========== LIVENESS (must-fail invariants) ==========

// MUST FAIL -- fails when at least one diffuse response appears in the log
val DiffuseLiveness = f_dif_s.log.forall(e => match e.msg {
    | DIF_DiffuseResponse(_) => false
    | _ => true
})

// MUST FAIL -- fails when at least one non-empty fetch response appears in the log
val FetchLiveness = f_dif_s.log.forall(e => match e.msg {
    | DIF_FetchResponse(r) => r.data.size() == 0
    | _ => true
})

// MUST FAIL -- fails when at least one inject response appears in the log
val InjectLiveness = f_dif_s.log.forall(e => match e.msg {
    | DIF_InjectResponse(_) => false
    | _ => true
})

// MUST FAIL -- fails when at least one OK response appears in the log
val OKLiveness = f_dif_s.log.forall(e => match e.msg {
    | DIF_OKResponse(_) => false
    | _ => true
})

// ========== COMBINED PROPERTIES ==========

val AllBasicProps = and {
    NoOrphanedHolds,
    InputBound,
    ValidCounters,
}

val AllAccessProps = and {
    InjectAccessControl,
}

val AllDiffusionProps = and {
    DiffusionConsistency,
}

val AllSecurityProps = and {
    Auth,
    DDL,
    NoDuplication,
}

val AllProps = and {
    AllBasicProps,
    AllAccessProps,
    AllDiffusionProps,
    AllSecurityProps,
}

    
}
