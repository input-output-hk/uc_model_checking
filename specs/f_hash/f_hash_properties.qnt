module f_hash_properties{
    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"
    import common.* from "../common"
    import f_hash_types.*   from "./f_hash_types"
    import f_hash.*  from "./f_hash"
    import g_ledger_types.* from "../g_ledger/g_ledger_types" 
    import params.* from "../params"

    val Parties = 1.to(PARTY_SIZE)
    val sids = 1.to(SID_SIZE)
    
    // STEP function
    action step : bool = if (f_hash_s.log.size() < MAX_LOG_SIZE) {
        any {
            hash_step,
            env_hash_send_all_reqs,
            env_consume_responses,
            env_hash_sim_reqs,
        }
    } else {
        f_hash_s' = f_hash_s
    }

    // INIT function
    action init = all {
        hash_init({P: Parties, SIDs: sids, responsive_simulator: RESPONSIVE_SIMULATOR, enable_logging: true}),
    }

    // environment gives control to the simulator
    action env_hash_sim_reqs : bool = all {
        f_hash_s.input.size() == 0, 
        f_hash_s.handles.size() > 0,
        f_hash_s' = {...f_hash_s, 
            input: Set(HASH_SIMControl),
            nonce: f_hash_s.nonce + NONCE_INCREMENT
        }
    }       

    // environment sends requests to the functionality
    action env_hash_send_all_reqs : bool = all {
        // Precondition: input must be empty
        f_hash_s.input.size() == 0,
        
        // Non-deterministically choose a party and SID
        all {
            nondet from = Parties.oneOf() 
            nondet sid = sids.oneOf()
            nondet x = 1.to(UNIVERSE_SIZE).oneOf()

            val cfg = {party: from, sid: sid, nonce: f_hash_s.nonce}
            all {
                f_hash_s' = {...f_hash_s,
                    input: Set(HashRequest({cfg: cfg, x: List(Normal({tx: x, b: true})) })),
                    nonce: f_hash_s.nonce + NONCE_INCREMENT
                }
            }
        }
    }

    // environment consumes response messages from the functionality
    pure def env_consume_response(sys: HASH_System) : Set[HASH_Transition] = {
         sys.input.hash_get_all_response_messages().map(msg_log =>
         {
            post_state : sys.state,
            effects : Set(
                HASH_UpdateInput(Set())
            )
         })
    }

    action env_consume_responses : bool =  all {
        // Precondition: there is a response message in input
        val response_msgs = env_consume_response(f_hash_s)
        
        // Non-deterministically choose one response message to consume
        nondet transition = response_msgs.oneOf()

        // Update the system state by consuming the message    
        f_hash_s' = hash_process_transitions(f_hash_s, transition),
    }



    // Correctness: For all x and all P and Q we have that P.Hash(x) = Q.Hash(x).
    // This property checks that all parties get the same hash for the same input value
    val HASH_Correctness: bool = {
        f_hash_s.log.forall(entry1 => {
            match entry1.msg {
                | HashResponse(r1) => {
                    f_hash_s.log.forall(entry2 => {
                        match entry2.msg {
                            | HashResponse(r2) => {
                                // If two responses have the same input x, they must have the same hash h
                                r1.cfg.sid == r2.cfg.sid and r1.x == r2.x implies r1.h == r2.h
                            }
                            | _ => true
                        }
                    })
                }
                | _ => true
            }
        })
    }

    // =======================  Properties  ================================

    // Collision resistance: For all x and y and all P and Q we have that if x ≠ y ⇒ P.Hash(x) ≠ Q.Hash(y).
    // This property checks that different inputs produce different hashes
    val HASH_CollisionResistance: bool = {
        f_hash_s.log.forall(entry1 => {
            match entry1.msg {
                | HashResponse(r1) => {
                    f_hash_s.log.forall(entry2 => {
                        match entry2.msg {
                            | HashResponse(r2) => {
                                // If two responses have different inputs, they must have different hashes
                                r1.cfg.sid == r2.cfg.sid and r1.x != r2.x implies r1.h != r2.h
                            }
                            | _ => true
                        }
                    })
                }
                | _ => true
            }
        })
    }

    // MUST FAIL -- fails when at least one hash has been computed in some session
    val HashLiveness = sids.forall(sid =>
        f_hash_s.state.get(sid).hash_map.keys().size() == 0
    )

    val AllProps = all {
        HASH_Correctness,
        HASH_CollisionResistance,
    }

}