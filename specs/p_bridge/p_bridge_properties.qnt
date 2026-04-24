module p_bridge_properties{
    
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
    import p_bridge_types.* from "./p_bridge_types"
    
    import p_bridge.* from "./p_bridge" 
    import p_bridge_bkgen.* from "./p_bridge_bkgen"     
    import p_bridge_rot.* from "./p_bridge_rot"
    import p_bridge_br.* from "./p_bridge_br"
    import p_bridge_led2.* from "./p_bridge_led2"    
    import p_bridge_env.* from "./p_bridge_env" 
   

    val Parties = 1.to(PARTY_SIZE)
    val C = 1.to(CORRUPTED_PARTY_SIZE)
    // Bridge BKGen runs once per party (not per SID), so the clock can only advance
    // when each party has exactly one SID to OK. SID_SIZE > 1 permanently blocks the clock.
    val SIDs = Set(1)
    val P1 = Parties
    val P2 = Parties

    action init = all {
        p_bridge_env_init({
            P: Parties, 
            SIDs: SIDs, 
            C: C, 
            r0: 0, 
            delta: 1, 
            t: 2, 
            R: 4, 
            delta_latency: 2, 
            delta_slack: 2, 
            delta_apl: 2, 
            comsel_a: 2, 
            comsel_b: 2, 
            comsel_c: 2})        
        // p_bridge_env_init({
        //     P: Parties, 
        //     SIDs: SIDs, 
        //     C: C, 
        //     r0: 0, 
        //     delta: 1, 
        //     t: 1, 
        //     R: 4, 
        //     delta_latency: 1, 
        //     delta_slack: 1, 
        //     delta_apl: 1, 
        //     comsel_a: 1, 
        //     comsel_b: 1, 
        //     comsel_c: 1})
    }

    action step = {
        if(p_bridge_s.log.size() < MAX_LOG_SIZE){
            val  x = {Parties: Parties, SIDs: SIDs}
            any { 
                p_bridge_env_step(x),
                p_bridge_consume_responses,
                g_ledger1_response_messages,
                p_bridge_request_messages(x),
                g_ledger1_request_messages(x),        
            }
        }else{
            all {
                p_bridge_consume_responses,
            }
        }

    }

    // ====== Co-Properties (must fail) ======
    // Each Qn should be violated by a healthy simulation. Holding means the model
    // can't reach that milestone and something is broken.

    // Any log entry produced (trivial progress check).
    val Q1 = p_bridge_s.log.size() <= 1

    // Phase 1 — BKGen: a validator key is generated for any party.
    val Q3 = p_bridge_s.log.map(x => x.msg).filterMap(z => match z {
        | BKGenResponse(_) => Some(true) | _ => None
    }).size() == 0

    // Phase 2 — RoT: RoT completes for party 1.
    val Q2 = p_bridge_s.state.bst.get(1).rot == None

    // Phase 3 — BRAlgorithm: bridge rotation commits a hash for party 1 (case3_6 fired).
    val Q4 = p_bridge_s.state.bst.get(1).h == None

    // Phase 4 — Certify: certify produces a valid proof (res != None) for some request.
    val Q5 = p_bridge_s.log.map(x => x.msg).brd_get_certify_response_messages().forall(p => p.res == None)

    // Phase 5 — Verify: at least one successful verify response.
    val Q6 = p_bridge_s.log.map(x => x.msg).brd_get_verify_response_messages().filter(x => x.res == true).size() == 0
    // (VIOLATED) Bridge democracy: 
    // For all rounds r and all parties P, we have 
    // that bst[P].aux.Com @ r ≠ bst[P].aux.Com @ (r + R) (doesn’t hold – authoritarian regime possible)
    val Bridge_Democracy = p_bridge_s.log.map(x => x.msg).filterMap(z => match z {
        | BridgeBRAlgorithmResponse(p) => Some(p)
        | _ => None
    }).forall(p =>
        p_bridge_s.log.map(x => x.msg).filterMap(z => match z {
            | BridgeBRAlgorithmResponse(q) => Some(q)
            | _ => None
        }).forall(q =>
            and {
                p.cfg.party == q.cfg.party,
                p.args.r + p_bridge_s.params.R == q.args.r,
                p.resp.bst != None,
                q.resp.bst != None
            } implies {
                val p_bst = p.resp.bst.unwrap()
                val q_bst = q.resp.bst.unwrap()
                (p_bst.aux != None and q_bst.aux != None) implies p_bst.aux.unwrap().com != q_bst.aux.unwrap().com
            }
        )
    )



    // ====== Properties (must hold )======

    val RoT_Agreement = p_bridge_s.log.map(x => x.msg).filterMap(z => match z {
        RoTResponse(p) => Some(p)
        | _ => None
    }).forall(rsp1 =>
        p_bridge_s.log.map(x => x.msg).filterMap(z => match z {
            RoTResponse(p) => Some(p)
            | _ => None
        }).forall(rsp2 =>
            rsp1.avk == rsp2.avk
        )
    )

    val Bridge_State_Consistency = p_bridge_s.log.map(x => x.msg).filterMap(z => match z {
        | BridgeBRAlgorithmResponse(p) => Some(p)
        | _ => None
    }).forall(p =>
        p_bridge_s.log.map(x => x.msg).filterMap(z => match z {
            | BridgeBRAlgorithmResponse(q) => Some(q)
            | _ => None
        }).forall(q =>
            (p.args.r == q.args.r and p.resp.bst != None) implies {
                val p_bst = p.resp.bst.unwrap()
                val q_bst = q.resp.bst.unwrap()
                and {
                    p_bst.rot == q_bst.rot,
                    p_bst.cID == q_bst.cID,
                    p_bst.h == q_bst.h,

                    
                    p_bst.avk_prim == q_bst.avk_prim,
                    p_bst.aux == q_bst.aux,
                    p_bst.aux_prim == q_bst.aux_prim,
                }
            }
        )
    )

    //Bridge aux update: For all rounds r and all parties P, we have that bst[P].aux @ (r + R) = bst[P].aux’ @ r.
    val Bridge_Aux_Update = p_bridge_s.log.map(x => x.msg).filterMap(z => match z {
        | BridgeBRAlgorithmResponse(p) => Some(p)
        | _ => None
}).forall(p =>
        p_bridge_s.log.map(x => x.msg).filterMap(z => match z {
            | BridgeBRAlgorithmResponse(q) => Some(q)
            | _ => None
        }).forall(q =>
            and {
                p.cfg.party == q.cfg.party,
                p.args.r + p_bridge_s.params.R + p_bridge_s.params.delta + 2 == q.args.r,
                p.resp.bst != None,
                q.resp.bst != None,
                p.resp.bst.unwrap().aux_prim != None,  // exclude warmup rounds before first rotation
            } implies {
                val p_bst = p.resp.bst.unwrap()
                val q_bst = q.resp.bst.unwrap()
                q_bst.aux == p_bst.aux_prim  // aux at r+R+delta+2 equals aux_prim prepared at r
            }
        )
    )


    // Bridge state persistence: For all rounds r and r’ in the same rotation period and all parties P,
    // bst[P].(rot, cID, h, avk’, aux, aux’) @ r = bst[P].(rot, cID, h, avk’, aux, aux’) @ r’
    // as long as 1 ≤ (r % R), (r’ % R) ≤ ∆ + 1 and floor(r/R) == floor(r’/R) (same period).
    val Bridge_State_Persistence = p_bridge_s.log.map(x => x.msg).filterMap(z => match z {
        | BridgeBRAlgorithmResponse(p) => Some(p)
        | _ => None
    }).forall(p =>
        p_bridge_s.log.map(x => x.msg).filterMap(z => match z {
            | BridgeBRAlgorithmResponse(q) => Some(q)
            | _ => None
        }).forall(q =>
            and {
                p.cfg.party == q.cfg.party,
                p.args.r % p_bridge_s.params.R >= 1,
                q.args.r % p_bridge_s.params.R >= 1,

                p.args.r % p_bridge_s.params.R <= p_bridge_s.params.delta + 1,
                q.args.r % p_bridge_s.params.R <= p_bridge_s.params.delta + 1,

                // same rotation period: h and other fields update each period, so cross-period comparison is invalid
                p.args.r / p_bridge_s.params.R == q.args.r / p_bridge_s.params.R,

                p.resp.bst != None,
                q.resp.bst != None,
                p.resp.bst.unwrap().aux_prim != None,  // exclude warmup rounds before first rotation
            } implies {
                val p_bst = p.resp.bst.unwrap()
                val q_bst = q.resp.bst.unwrap()
                and {
                    p_bst.rot == q_bst.rot,
                    p_bst.cID == q_bst.cID,
                    p_bst.h == q_bst.h,

                    p_bst.avk_prim == q_bst.avk_prim,
                    p_bst.aux == q_bst.aux,
                    p_bst.aux_prim == q_bst.aux_prim,
                }
            }
        )
    )

    /*
    Bridge Soundness: 
    If there are at most t corrupted parties (i.e., |C| ≤ t through out), 
    then for any round r if an honest party Q using Q.Verify(rot, x, π) in round r successfully verifies a proof π against some statement x 
    , then there is some round r’ ≤ r (?), 
    such that for all parties P honest in round r’ we have that Rel(x, L) holds, 
    where L = P.Read() and 𝓁 = G-Ledger.P.APL() in round r’.     
    */
    val Bridge_Soundness = p_bridge_s.log.map(x => x.msg).brd_get_verify_response_messages().forall(p => {
        and {
            p.res == true,
            g_ledger_s.state.APL.has(p.r)
        } implies {
            
            val apl = g_ledger_s.state.APL.get(p.r)
            val L1 = g_ledger_s.state.log.listMap(x => x.tx)
            val L = L1.slice(0, apl)
            (nizk_Rel_sat({x: p.x, h: p.proof.h}, L))
        }
    })


   /*
    Bridge Liveness:
    If there are at most t corrupted parties (i.e., |C| ≤ t through out), then for any rounds r and r’ with r + δ ≤ r’, 
    if an honest party P in round r asks to Certify x such that relation Rel(x, L) holds where L = take(L, 𝓁) 
    with 𝓁 = G-Ledger1.P.APL() and L = P.Read() in round r, 
    then a proof π will be generated that will verify from any honest party Q view in round r’ 
    as long as Q uses the rot value that it generated in round r_0 + 1.    
   */

   val Bridge_Liveness = p_bridge_s.log.map(x => x.msg).brd_get_certify_response_messages().forall(p => {
    p_bridge_s.log.map(x => x.msg).brd_get_verify_response_messages().forall(q => {

        val apl = p.apl
        val L1 = p.L1
        val L = L1.slice(0, apl)
        val small_delta = 6 // delta_latency + delta_slack + Delta

        and {
            p.x == q.x, // tx are the same
            p.rot == q.rot, // same rot
            p.res != None, // certify produced a proof
            p.res.unwrap() == q.proof, // proof matches
            p.r + small_delta < q.r, // certify happened before verify
        } implies {
            and {
                q.res == true
            }
        }

    })
})

    val AllProps = all {
        Bridge_State_Consistency,
        Bridge_Liveness,
        Bridge_Aux_Update,
        Bridge_State_Persistence,
        Bridge_Soundness,
    }

}