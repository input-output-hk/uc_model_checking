module f_hash {

    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"
    
    import common.* from "../common"
    
    import f_hash_types.* from "./f_hash_types"

    var f_hash_s : HASH_System

    val ALL_HASHES = 50.to(50 + UNIVERSE_SIZE)


    // sending hash requests
    pure def hash_hash_1(f_hash: HASH_System) : Set[HASH_Transition] = {
        f_hash.input.filterMap(m => match m { | HashRequest(p) => Some(p) | _ => None}).map(p => {
            if (hash_access_check(p.cfg, f_hash.params)) {
                {
                    post_state: f_hash.state,
                    effects: Set(
                        HASH_LogMessage(HashRequest(p)),
                        HASH_AppendHandle(SIMHashRequestHold(p)),
                        HASH_UpdateInput(Set(HASH_SIMControl))
                    )
                }
            } else {
                hash_skip_trans(f_hash)
            }
        })
    }
    
    action hash_hash_2_act : bool = {
        all{
            nondet sim_h = ALL_HASHES.oneOf()
            val ts = hash_hash_2(f_hash_s, sim_h)
            all {
                nondet transition = ts.oneOf()
                all {
                    f_hash_s' = hash_process_transitions(f_hash_s, transition)
                }
            }
        }
    }

    pure def hash_hash_2(f_hash: HASH_System, sim_h : Hash_Output) : Set[HASH_Transition] = {
        f_hash.input.filterMap(m => match m {
            | HASH_SIMControl => Some(m)
            | _ => None
        }).map(_ => {
            f_hash.handles.filterMap(h => match h {
                | SIMHashRequestHold(p) => Some(p)
                | _ => None
            }).map(p => {
                Set({
                    post_state: f_hash.state,
                    effects: Set(
                        HASH_RemoveHandle(SIMHashRequestHold(p)),
                        HASH_UpdateInput(Set(SIMHashRequestRelease({cfg: p.cfg, x: p.x, sim_h: sim_h})))
                    )
                })
            }).flatten()
        }).flatten()
    }

    pure def san_hash(h: Hash_Output, f_hash_s: HASH_System, sid: SID) : Option[Hash_Output] = {
        if(clean_hash(h, f_hash_s.state.get(sid).hash_map)){
            Some(h)
        } else {
            ALL_HASHES.find(hh => clean_hash(hh, f_hash_s.state.get(sid).hash_map))
        }
    }

    pure def clean_hash(h: Hash_Output, hash_map : Hash_Input -> Hash_Output) : bool = {
        hash_map.values().forall(existing_h => existing_h != h)
    }

    pure def hash_hash_3(f_hash: HASH_System) : Set[HASH_Transition] = {
        f_hash.input.filterMap(m => match m {
            | SIMHashRequestRelease(p) => Some(p)
            | _ => None
        }).map(p => {

            val sid = p.cfg.sid
            val party = p.cfg.party
            val sid_state = f_hash.state.get(sid)

            if(sid_state.hash_map.has(p.x)){
                {
                    post_state: f_hash.state,
                    effects: Set(
                        HASH_UpdateInput(Set(HashResponse({cfg: p.cfg, x: p.x, h: sid_state.hash_map.get(p.x)})))
                    )
                }
            }else{
                val maybe_hash = san_hash(p.sim_h, f_hash, sid)
                if(maybe_hash == None){
                    {
                        post_state: f_hash.state,
                        effects: Set(
                            HASH_UpdateInput(Set()) // TODO: consider error message
                        )
                    }
                }else{
                    val hash = maybe_hash.unwrap()
                    {
                        post_state: f_hash.state.put(sid, {hash_map: sid_state.hash_map.put(p.x, hash)}),
                        effects: Set(
                            HASH_UpdateInput(Set(HashResponse({cfg: p.cfg, x: p.x, h: hash}))),
                        )
                    }
                }
            }   

        })
    }   

    pure def hash_sim_loses_ctrl(hash: HASH_System) : Set[HASH_Transition] = {
        hash.input.filterMap(m => match m {
            | HASH_SIMControl => Some(m)
            | _ => None
        }).map(c =>  
            {
                post_state : hash.state,
                effects : Set(
                    HASH_UpdateInput(Set()),
                )
            })
    }    

    def hash_main_listener(hash: HASH_System) : Set[(HASH_Transition)] = {
        Set(
            hash_hash_1(hash),
            hash_hash_3(hash),
            if (hash.params.responsive_simulator) Set() else hash_sim_loses_ctrl(hash)
        ).flatten()
    }     

    def hash_process_transitions(sys: HASH_System, transition: HASH_Transition) : HASH_System = {
        val new_sys = {...sys, state: transition.post_state}
        transition.effects.fold(new_sys, ((a1,a2) => hash_apply_effect(a1,a2)))
    }     

    action hash_step_with(q: (HASH_System) => Set[(HASH_Transition)]): bool = {
        all{
            val ts = q(f_hash_s)
            all {
                nondet transition = ts.oneOf()
                all {
                    f_hash_s' = hash_process_transitions(f_hash_s, transition)
                }
            }
        }
    } 


    action hash_step : bool =  any {
        hash_hash_2_act,
        hash_step_with(hash_main_listener)
    }        


    pure def hash_initial(x : {P: Set[Party], SIDs: Set[SID], responsive_simulator: bool, enable_logging: bool}) : HASH_System = {
        {
            params : {
                P: x.P,
                SIDs: x.SIDs,
                responsive_simulator: x.responsive_simulator,
                enable_logging: x.enable_logging,   
            },
            state: x.SIDs.mapBy(sid => {
                hash_map: Map(),
            }),
            input: Set(),
            nonce: 0, 
            log: Set(),
            handles: Set()
        }
    }

    action hash_init(x : {P: Set[Party], SIDs: Set[SID], responsive_simulator: bool, enable_logging: bool}) : bool = all {
        f_hash_s' = hash_initial(x)
    }   

}