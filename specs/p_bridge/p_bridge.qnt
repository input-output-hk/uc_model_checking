module p_bridge {

    import basicSpells.* from "../../spells/basicSpells"
    import common.* from "../common"

    import p_bridge_types.* from "./p_bridge_types"

    import f_pki_types.* from "../f_pki/f_pki_types"    
    import f_pki.* from "../f_pki/f_pki"    

    import f_atms_types.* from "../f_atms/f_atms_types"    
    import f_atms.* from "../f_atms/f_atms"    
    
    import g_clock_types.* from "../g_clock/g_clock_types"    
    import g_clock.* from "../g_clock/g_clock"

    import g_ledger_types.* from "../g_ledger/g_ledger_types"
    import g_ledger.* from "../g_ledger/g_ledger"

    import f_dif_types.* from "../f_dif/f_dif_types"
    import f_dif.* from "../f_dif/f_dif"    

    import f_hash_types.* from "../f_hash/f_hash_types"
    import f_hash.* from "../f_hash/f_hash"        
    
    import f_inizk_types.* from "../f_inizk/f_inizk_types"
    import f_inizk.* from "../f_inizk/f_inizk"  


    
    var p_bridge_s : BRD_System

    pure def brd_process_transitions(sys: BRD_System, transition: BRD_Transition) : BRD_System = {
        val new_sys = {...sys, state: transition.post_state}
        transition.effects.fold(new_sys, ((a1,a2) => brd_apply_effect(a1,a2)))
    }

    pure def brd_apply_effect(sys: BRD_System, eff: BRD_Effect) : BRD_System = {
        match eff {
        | BRD_LogMessage(msg) => 
            {...sys, 
                log: sys.log.setAdd({idx: sys.log.size(), msg: msg})
            }
        | BRD_UpdateInput(msgs) => 
            {...sys, 
                input: msgs
            }
        | BRD_AppendHandle(msg) => 
            {...sys,
                handles: sys.handles.union(Set(msg)),
            }
        | BRD_RemoveHandle(handle_to_remove) =>
            {...sys,
                handles: sys.handles.exclude(Set(handle_to_remove))
            }
        }
    }


    pure def brd_process_ctransitions(sys: BRD_CSystem, transition: BRD_CTransition) : BRD_CSystem = {
        {
            brd: brd_process_transitions(sys.brd, transition.brd),
            clock: sys.clock.gc_process_transitions(transition.clock),
            atms: sys.atms.atms_process_transitions(transition.atms),
            pki: sys.pki.pki_process_transitions(transition.pki), 
            ledger1: sys.ledger1.led_process_transitions(sys.clock, transition.ledger1),
            ledger2: sys.ledger2.led_process_transitions(sys.clock, transition.ledger2),
            dif: sys.dif.dif_process_transitions(sys.clock, transition.dif),
            hash: sys.hash.hash_process_transitions(transition.hash),
            nizk: sys.nizk.nizk_process_transitions(transition.nizk),
        }
    } 



    action brd_init(x : {P: Set[Party], C: Set[Party], SIDs: Set[SID], r0: Time, delta: int, R: int }) : bool =  all {
        x.C.subseteq(x.P),
        
        x.r0 >= 0,
        x.R >= 0,
        x.R >= x.delta + 3,
        x.delta >= 1, // check that

        p_bridge_s' = {
            params : {
                P: x.P,
                C: x.C,
                SIDs: x.SIDs, 
                // P1: P1,
                // P2: P2,
                r0: x.r0,
                delta: x.delta, 
                //t: t, 
                R: x.R, 
            },
            state : {
                bst: x.P.mapBy(p => {
                    bvk: None,
                    rot: None,
                    cID: None,
                    m: Set(),
                    aux_prim: None,
                    aux: None,
                    avk_prim: None,
                    h: None,
                    apl: None,
                }),
                bkgen_flag: Set(),
                rot_flag: Set(),
                bridge_flag: Set(),                
            },
            input : Set(),
            nonce : 0, 
            log : Set(),
            handles : Set()
        }
    }
    
}