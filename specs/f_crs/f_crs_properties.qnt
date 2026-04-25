module f_crs_properties{
    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"
    import common.* from "../common"
    import f_crs_types.*   from "./f_crs_types"
    import f_crs.*  from "./f_crs"

    val Parties = Set(1,2,3)
    val sids = Set(4,5)
    val D = Set(1001,1002,1003)

    action step : bool = if (crs_s.log.size() < MAX_LOG_SIZE) {
        any {
            crs_step,
            env_crs_send_all_reqs,
            env_consume_responses,
        }
    } else {
        crs_s' = crs_s
    }


    action init = all {
        crs_init(Parties, D, sids, RESPONSIVE_SIMULATOR),
    }

    action env_crs_send_all_reqs : bool = all {
        // Precondition: input must be empty
        crs_s.input.size() == 0,
        
        // Non-deterministically choose a party and SID
        nondet from = Parties.oneOf()
        nondet sid = sids.oneOf()
        val cfg = {party: from, sid: sid, nonce: crs_s.nonce}

        crs_s' = {...crs_s,
            input: Set(GenRequest({cfg: cfg})),
            nonce: crs_s.nonce + NONCE_INCREMENT
        },
    }

    pure def env_consume_response(sys: CRS_System) : Set[CRS_Transition] = {
         sys.input.crs_get_all_response_messages().map(msg_log =>
         {
            post_state : sys.state,
            effects : Set(
                CRS_LogMessage(msg_log),
                CRS_UpdateInput(Set())
            )
         })
    }

    action env_consume_responses : bool =  all {
        // Precondition: there is a response message in input
        val response_msgs = env_consume_response(crs_s)
        
        // Non-deterministically choose one response message to consume
        nondet transition = response_msgs.oneOf()

        // Update the system state by consuming the message    
        crs_s' = crs_process_transitions(crs_s, transition),
    }

// for any two GenResponse messages in the log, their CRS values are the same if their SIDs are the same
val CRS_same =  crs_s.log.forall(log_entry1 => {
    match log_entry1.msg {
        | GenResponse(p1) => 
            crs_s.log.forall(log_entry2 => {
                match log_entry2.msg {
                    | GenResponse(p2) => 
                        p1.cfg.sid == p2.cfg.sid implies (p1.crs == p2.crs)
                    | _ => true
                }
            })
        | _ => true
    }
})

// must fail (CRS might be different for two different SIDs)
val CRS_different =  crs_s.log.forall(log_entry1 => {
    match log_entry1.msg {
        | GenResponse(p1) =>
            crs_s.log.forall(log_entry2 => {
                match log_entry2.msg {
                    | GenResponse(p2) =>
                        p1.crs == p2.crs
                    | _ => true
                }
            })
        | _ => true
    }
})

    val AllProps = all {
        CRS_same,
    }

}