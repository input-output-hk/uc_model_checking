module f_atms{

    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"
    import common.* from "../common"   
    import f_atms_types.* from "./f_atms_types"

    
    var f_atms_s : ATMS_System
 
    val ALL_VKS: Set[ATMS_VK] = 1.to(UNIVERSE_SIZE)
    val ALL_SIGS: Set[ATMS_Signature] = 1.to(UNIVERSE_SIZE)
    val ALL_AVKS: Set[ATMS_AVK] = 1.to(UNIVERSE_SIZE)
    val ALL_ASIGS: Set[ATMS_ASignature] = 1.to(UNIVERSE_SIZE)  

    // SIM gives control back to the environment
    pure def atms_sim_loses_ctrl(atms: ATMS_System) : Set[ATMS_Transition] = {
        atms.input.filterMap(m => match m {
            | SIMControl => Some(m)
            | _ => None
        }).map(c =>  // sim must have control
            {   
                post_state : atms.state,
                effects : Set( 
                    ATMS_UpdateInput(Set())
                )  
            })
    }    

    pure def atms_id_trans(atms : ATMS_System) : ATMS_Transition = {
        post_state : atms.state,
        effects : Set(),
    }    
    
    pure def atms_skip_trans(atms: ATMS_System) : ATMS_Transition = {
        {
            post_state: atms.state,
            effects : Set(
                ATMS_UpdateInput(Set())
            )
        }
    }    
 

    // key generation
    pure def atms_gen_1(atms: ATMS_System) : Set[ATMS_Transition] = {
        atms.input.filterMap(m => match m {
            | GenRequest(p) => Some(p)
            | _ => None
        }).map(p =>{
            if(atms_access_check(p.cfg, atms.params)) {
                {
                    post_state: atms.state,
                    effects: Set(
                        ATMS_LogMessage(GenRequest(p)),
                        ATMS_UpdateInput(Set(SIMControl)),
                        ATMS_AppendHandle(SIMGenHold(p))
                    )
                }
            } else {
                {
                    post_state: atms.state,
                    effects: Set(
                        ATMS_UpdateInput(Set())
                    )
                }
            }
        })
    }

    action atms_gen_2_act = all {
        // f_atms_s.input.atms_get_sim_control_messages().size() > 0,
        // f_atms_s.handles.atms_get_sim_gen_hold_messages().size() > 0,
        nondet sim_vk = ALL_VKS.oneOf()
        nondet t = atms_gen_2(f_atms_s, Some(sim_vk)).oneOf()
        f_atms_s' = atms_process_transitions(f_atms_s, t) 
    }
 
    pure def atms_gen_2(atms: ATMS_System, sim_vk: Option[ATMS_VK]): Set[ATMS_Transition] = {
        atms.input.filterMap(m => match m {
            | SIMControl => Some(m)
            | _ => None
        }).map(_ => {
            atms.handles.filterMap(h => match h {
                | SIMGenHold(p) => Some(p)
                | _ => None
            }).map(p => {
                {
                    post_state: atms.state,
                    effects: Set(
                        ATMS_UpdateInput(Set(SIMGenRelease({cfg: p.cfg, sim_vk: sim_vk}))),
                        ATMS_RemoveHandle(SIMGenHold(p))
                    )
                }
            })
        }).flatten()
    }

    pure def clean_vk(vk: Option[ATMS_VK], sid_state: ATMS_StateVars) : bool = {
        and {
            vk != None,
            not (vk.unwrap().in(sid_state.CVK.union(sid_state.VK.map(x => x.vk)))),
            or {
                not (sid_state.VER.exists(x => x.vk == vk.unwrap())),
                sid_state.VER.exists(x => x.vk == vk.unwrap() and x.b == false)
            }
        }
    }

    pure def san_vk(vk: Option[ATMS_VK], atms: ATMS_System, sid: SID) : Option[ATMS_VK] = {
        val sid_state = atms.state.get(sid)
        if(clean_vk(vk, sid_state)){
            vk
        }else{ 
            ALL_VKS.find(vk => clean_vk(Some(vk), sid_state))
        }        
    }

    pure def atms_gen_3(atms: ATMS_System): Set[ATMS_Transition] = {
        atms.input.filterMap(m => match m {
            | SIMGenRelease(p) => Some(p)
            | _ => None
        }).map(p => {
            val P = p.cfg.party
            val sid = p.cfg.sid
            val sid_state = atms.state.get(sid)
            val maybe_vk = sid_state.VK.find(x => x.party == P) // TODO: make sure that VK is functional

            if(maybe_vk != None){
                {
                    post_state: atms.state,
                    effects: Set(
                        ATMS_UpdateInput(Set(GenResponse({cfg: p.cfg, vk: maybe_vk.unwrap().vk}))),
                    )
                }
            }else{

                val maybe_new_vk = san_vk(p.sim_vk, atms, sid)

                if(maybe_new_vk != None and clean_vk(maybe_new_vk, sid_state)){
                    {
                        post_state: atms.state.set(sid, {...sid_state,
                            VK: sid_state.VK.setAdd({party: P, vk: maybe_new_vk.unwrap()})
                        }),
                        effects: Set(
                            ATMS_UpdateInput(Set(GenResponse({cfg: p.cfg, vk: maybe_new_vk.unwrap()}))),
                        )
                    }
                }else{
                    {
                        post_state: atms.state,
                        effects: Set(
                            ATMS_UpdateInput(Set()),
                        )
                    } // run out of VKS, raise error?
                }
            }
        })
    }

    pure def atms_akey_1(atms: ATMS_System) : Set[ATMS_Transition] = {
        atms.input.filterMap(m => match m {
            | AKeyRequest(p) => Some(p)
            | _ => None
        }).map(p =>{
            if(atms_access_check(p.cfg, atms.params)) {
                {
                    post_state: atms.state,
                    effects: Set(
                        ATMS_LogMessage(AKeyRequest(p)),
                        ATMS_UpdateInput(Set(SIMControl)),
                        ATMS_AppendHandle(SIMAKeyHold(p))
                    )
                }                
            } else {
                {
                    post_state: atms.state,
                    effects: Set(
                        ATMS_UpdateInput(Set())
                    )
                }
            }
        })
    }    

    action atms_akey_2_act : bool = all {
        // f_atms_s.input.atms_get_sim_control_messages().size() > 0,
        // f_atms_s.handles.atms_get_sim_akey_hold_messages().size() > 0,
        nondet sim_avk = ALL_AVKS.oneOf()
        nondet t = atms_akey_2(f_atms_s, Some(sim_avk)).oneOf()
        f_atms_s' = atms_process_transitions(f_atms_s, t) 
    }    

    pure def atms_akey_2(atms: ATMS_System, sim_avk: Option[ATMS_AVK]): Set[ATMS_Transition] = {
        atms.input.filterMap(m => match m {
            | SIMControl => Some(m)
            | _ => None
        }).map(_ => {
            atms.handles.filterMap(h => match h {
                | SIMAKeyHold(p) => Some(p)
                | _ => None
            }).map(p => {
                {
                    post_state: atms.state,
                    effects: Set(
                        ATMS_UpdateInput(Set(SIMAKeyRelease({cfg: p.cfg, vks: p.vks, sim_avk: sim_avk}))),
                        ATMS_RemoveHandle(SIMAKeyHold(p))
                    )
                }
            })
        }).flatten()
    }

    def clean_avk(avk: Option[ATMS_AVK], vks: List[ATMS_VK], sid_state: ATMS_StateVars, C: Set[Party]) : bool = {
        if(avk != None){
            and {
                not (sid_state.AVK.exists(x => x.avk == avk)),
                or {
                    not (sid_state.AVER.exists(x => x.avk == avk.unwrap())),
                    sid_state.AVER.exists(x => x.avk == avk.unwrap() and x.b == false)
                }                
            }
        }else{
            //  (∃ vk ∈ vk. ∀ P ∉ C. vk ≠ VK[P])
            vks.indices().exists(i => {
                sid_state.VK.forall(x => {
                    (not (C.contains(x.party))) implies (x.vk != vks[i]) 
                })
            })
        }
    }

     def san_avk(avk: Option[ATMS_AVK], vks: List[ATMS_VK], atms: ATMS_System, sid: SID) : Option[ATMS_AVK] = {
        val sid_state = atms.state.get(sid)

        if(clean_avk(avk, vks, sid_state, atms.params.C)){
            avk
        }else{ // TODO: add None here for checking?
            val x = ALL_AVKS.find(avk => clean_avk(Some(avk), vks, sid_state, atms.params.C))
            x
        }
    }

    pure def atms_akey_3(atms: ATMS_System): Set[ATMS_Transition] = {
        atms.input.filterMap(m => match m {
            | SIMAKeyRelease(p) => Some(p)
            | _ => None
        }).map(p => {
            val P = p.cfg.party
            val sid = p.cfg.sid
            val sid_state = atms.state.get(sid)
            val maybe_avk = sid_state.AVK.find(x => x.vks == p.vks) // TODO: make sure that AVK is functional
            // cached Some(x): return it; cached None: re-validate against current state first
            val use_cache = maybe_avk != None and (
                maybe_avk.unwrap().avk != None or
                clean_avk(None, p.vks, sid_state, atms.params.C)
            )
            if(use_cache){
                val cached_avk = if (p.vks.length() < atms.params.t + 1) None
                    else maybe_avk.unwrap().avk
                Set({
                    post_state: atms.state,
                    effects: Set(
                        ATMS_UpdateInput(Set(AKeyResponse({cfg: p.cfg, vks: p.vks, avk: cached_avk}))),
                    )
                })
            }else{
                val maybe_new_avk = if (p.vks.length() < atms.params.t + 1) None
                    else san_avk(p.sim_avk, p.vks, atms, p.cfg.sid)

                if (clean_avk(maybe_new_avk, p.vks, sid_state, atms.params.C)){ 
                    
                    val new_AVK = sid_state.AVK.setAdd({vks: p.vks, avk: maybe_new_avk})

                    val new_VKs = if(maybe_new_avk != None) {
                        sid_state.VKs.setAdd({avk: maybe_new_avk.unwrap(), vks: p.vks})
                    }else{
                        sid_state.VKs
                    }
                    val new_CVK = p.vks.foldl(sid_state.CVK, (cvk, key) => {
                        if(sid_state.VK.exists(x => x.vk == key)){
                            cvk
                        }else{
                            cvk.setAdd(key)
                        }
                    })

                    Set({
                        post_state: atms.state.set(sid, {...sid_state,
                            AVK: new_AVK,
                            VKs: new_VKs,
                            CVK: new_CVK
                        }),
                        effects: Set(
                            ATMS_UpdateInput(Set(AKeyResponse({cfg: p.cfg, vks: p.vks, avk: maybe_new_avk}))),
                        )
                    })
                }else{
                    Set({
                        post_state: atms.state,
                        effects: Set(
                            ATMS_UpdateInput(Set()),
                        )
                    }) // run out of VKS, raise error?
                }
            }
        }).flatten()
    }

    pure def atms_acheck_1(atms: ATMS_System) : Set[ATMS_Transition] = {
        atms.input.filterMap(m => match m {
            | ACheckRequest(p) => Some(p)
            | _ => None
        }).map(p =>{
            if(atms_access_check(p.cfg, atms.params)) {
                Set({
                    post_state: atms.state,
                    effects: Set(
                        ATMS_LogMessage(ACheckRequest(p)),
                        ATMS_UpdateInput(Set(SIMControl)),
                        ATMS_AppendHandle(SIMACheckHold(p))
                    )
                })             
            } else {
                Set({
                    post_state: atms.state,
                    effects: Set(
                        ATMS_UpdateInput(Set())
                    )
                })
            }
        }).flatten()
    }

    action atms_acheck_2_act = all {
        // f_atms_s.input.atms_get_sim_control_messages().size() > 0,
        // f_atms_s.handles.atms_get_sim_acheck_hold_messages().size() > 0,
        nondet sim_avk = ALL_AVKS.oneOf()
        nondet t = atms_acheck_2(f_atms_s, Some(sim_avk)).oneOf()
        f_atms_s' = atms_process_transitions(f_atms_s, t) 
    }     
 
    pure def atms_acheck_2(atms: ATMS_System, sim_avk: Option[ATMS_AVK]): Set[ATMS_Transition] = {
        atms.input.filterMap(m => match m {
            | SIMControl => Some(m)
            | _ => None
        }).map(_ => {
            atms.handles.filterMap(h => match h {
                | SIMACheckHold(p) => Some(p)
                | _ => None
            }).map(p => {
                Set({
                    post_state: atms.state,
                    effects: Set(
                        ATMS_UpdateInput(Set(SIMACheckRelease({cfg: p.cfg, vks: p.vks, avk:p.avk, sim_avk: sim_avk}))),
                        ATMS_RemoveHandle(SIMACheckHold(p))
                    )
                })
            }).flatten()
        }).flatten()
    }

    pure def atms_acheck_3(atms: ATMS_System): Set[ATMS_Transition] = {
        atms.input.filterMap(m => match m {
            | SIMACheckRelease(p) => Some(p)
            | _ => None
        }).map(p => {
            val sid_state = atms.state.get(p.cfg.sid)
            val maybe_avk_record = sid_state.AVK.find(x => x.vks == p.vks) // TODO: AVK must be functional

            if(maybe_avk_record != None){
                val avk = maybe_avk_record.unwrap().avk
                val b = Some(p.avk) == avk
                Set({
                    post_state: atms.state,
                    effects: Set(
                        ATMS_UpdateInput(Set(ACheckResponse({cfg: p.cfg, vks: p.vks, avk: p.avk, b: b}))), // TODO
                    )
                })
            }else{

                val san_sim_avk_maybe = san_avk(p.sim_avk, p.vks, atms, p.cfg.sid)
                if(not (clean_avk(san_sim_avk_maybe, p.vks, sid_state, atms.params.C))){
                    Set({
                        post_state: atms.state,
                        effects: Set(
                            ATMS_UpdateInput(Set()),
                        )
                    }) // run out of VKS, raise error?
                }else{
                    val new_AVK = sid_state.AVK.setAdd({vks: p.vks, avk: san_sim_avk_maybe})

                    val new_VKs = if(san_sim_avk_maybe != None){
                        sid_state.VKs.setAdd({avk: san_sim_avk_maybe.unwrap(), vks: p.vks})
                    }else{
                        sid_state.VKs
                    }
                    val new_CVK = p.vks.foldl(sid_state.CVK, (cvk, key) => {
                        if(sid_state.VK.exists(x => x.vk == key)){
                            cvk
                        }else{
                            cvk.setAdd(key)
                        }
                    })                    

                    val b = Some(p.avk) == san_sim_avk_maybe

                    Set({
                        post_state: atms.state.set(p.cfg.sid, {...sid_state,
                            AVK: new_AVK,
                            VKs: new_VKs,
                            CVK: new_CVK
                        }),
                        effects: Set(
                            ATMS_UpdateInput(Set(ACheckResponse({cfg: p.cfg, vks: p.vks, avk: p.avk, b: b}))),
                        )
                    })
                }
            }
        }).flatten()
    }

    pure def atms_sign_1(atms: ATMS_System) : Set[ATMS_Transition] = {
        atms.input.filterMap(m => match m {
            | SignRequest(p) => Some(p)
            | _ => None
        }).map(p =>{
            if(atms_access_check(p.cfg, atms.params)) {
                if (atms.state.get(p.cfg.sid).VK.exists(x => x.party == p.cfg.party)){
                    Set({
                        post_state: atms.state,
                        effects: Set(
                            ATMS_LogMessage(SignRequest(p)),
                            ATMS_UpdateInput(Set(SIMControl)),
                            ATMS_AppendHandle(SIMSignHold(p))
                        )
                    })
                }else{
                    Set({
                        post_state: atms.state,
                        effects: Set(
                            ATMS_LogMessage(SignRequest(p)),
                            ATMS_UpdateInput(Set(SignResponse({cfg: p.cfg, m: p.m, sig: None}))),
                        )
                    })
                }
            } else {
                Set({
                    post_state: atms.state,
                    effects: Set(
                        ATMS_UpdateInput(Set())
                    )
                })
            }
        }).flatten()
    }

    action atms_sign_2_act = all {
        // f_atms_s.input.atms_get_sim_control_messages().size() > 0,
        // f_atms_s.handles.atms_get_sim_sign_hold_messages().size() > 0,
        nondet sim_sig = ALL_SIGS.oneOf()
        nondet t = atms_sign_2(f_atms_s, Some(sim_sig)).oneOf()
        f_atms_s' = atms_process_transitions(f_atms_s, t) 
    }         

    pure def atms_sign_2(atms: ATMS_System, sim_sig: Option[ATMS_Signature]): Set[ATMS_Transition] = {
        atms.input.filterMap(m => match m {
            | SIMControl => Some(m)
            | _ => None
        }).map(_ => {
            atms.handles.filterMap(h => match h {
                | SIMSignHold(p) => Some(p)
                | _ => None
            }).map(p => {
                {
                    post_state: atms.state,
                    effects: Set(
                        ATMS_UpdateInput(Set(SIMSignRelease({cfg: p.cfg, m: p.m, sim_sig: sim_sig}))),
                        ATMS_RemoveHandle(SIMSignHold(p))
                    )
                }
            })
        }).flatten()
    }

    pure def clean_sig(sig: Option[ATMS_Signature], vk: ATMS_VK, m: ATMS_Document, sid_state: ATMS_StateVars) : bool = {
        and{
            sig != None,
            or{
                not (sid_state.VER.exists(x => and {
                    x.m == m,
                    x.sig == sig.unwrap(),
                    x.vk == vk
                })),
                sid_state.VER.exists(x => and {
                    x.m == m,
                    x.sig == sig.unwrap(),
                    x.vk == vk,
                    x.b == true
                })
            }
        }
    }    

    pure def san_sig(sig: Option[ATMS_Signature], vk: ATMS_VK, m : ATMS_Document, atms: ATMS_System, sid: SID) : Option[ATMS_Signature] = {
        val sid_state = atms.state.get(sid)
        if(clean_sig(sig, vk, m, sid_state)){
                sig
            }else{  
                ALL_SIGS.find(x => clean_sig(Some(x), vk, m, sid_state) )
            }
    }

    pure def atms_sign_3(atms: ATMS_System): Set[ATMS_Transition] = {
        atms.input.filterMap(m => match m {
            | SIMSignRelease(p) => Some(p)
            | _ => None
        }).map(p => {
            val sid_state = atms.state.get(p.cfg.sid)
            // runtime error if not true [must never happen]
            val vk_record = sid_state.VK.find(x => x.party == p.cfg.party).unwrap()

            val maybe_new_sig = san_sig(p.sim_sig, vk_record.vk, p.m, atms, p.cfg.sid)

            if(maybe_new_sig != None){
                val new_sig = maybe_new_sig.unwrap()
                Set({
                    post_state: atms.state.set(p.cfg.sid, {...sid_state, 
                        Signers: sid_state.Signers.setAdd({m: p.m, vk: vk_record.vk}),
                        VER: sid_state.VER.setAdd({vk: vk_record.vk, m: p.m, sig: new_sig, b: true})
                    }),
                    effects: Set(
                        ATMS_UpdateInput(Set(SignResponse({cfg: p.cfg, m: p.m, sig: maybe_new_sig}))),
                    )
                })
            }else{
                    Set({
                        post_state: atms.state,
                        effects: Set(
                            ATMS_UpdateInput(Set()),
                        )
                    }) // run out of sigs, raise error?
            }
            
        }).flatten()

    }

    pure def atms_ver_1(atms: ATMS_System) : Set[ATMS_Transition] = {
        atms.input.filterMap(m => match m {
            | VerifyRequest(p) => Some(p)
            | _ => None
        }).map(p =>{
            if(atms_access_check(p.cfg, atms.params)) {
                Set({
                    post_state: atms.state,
                    effects: Set(
                        ATMS_LogMessage(VerifyRequest(p)),
                        ATMS_UpdateInput(Set(SIMControl)),
                        ATMS_AppendHandle(SIMVerifyHold(p))
                    )
                })
            } else {
                Set({
                    post_state: atms.state,
                    effects: Set(
                        ATMS_UpdateInput(Set())
                    )
                })
            }
        }).flatten()
    } 

    pure def atms_ver_2(atms: ATMS_System) : Set[ATMS_Transition] = {
        atms.input.filterMap(m => match m {
            | SIMControl => Some(m)
            | _ => None
        }).map(_ => {
            atms.handles.filterMap(h => match h {
                | SIMVerifyHold(p) => Some(p)
                | _ => None
            }).map(p => {
                Set(true,false).map(b => {
                    {
                        post_state: atms.state,
                        effects: Set(
                            ATMS_UpdateInput(Set(SIMVerifyRelease({cfg: p.cfg, vk: p.vk, m: p.m, sig: p.sig, sim_b: b}))),
                            ATMS_RemoveHandle(SIMVerifyHold(p))
                        )
                    }
                })
            }).flatten()
        }).flatten()
    }

    pure def atms_ver_3(atms: ATMS_System): Set[ATMS_Transition] = {
        atms.input.filterMap(m => match m {
            | SIMVerifyRelease(p) => Some(p)
            | _ => None
        }).map(p => {
            val sid_state = atms.state.get(p.cfg.sid)

            val ver_record = sid_state.VER.find(x => and {
                x.vk == p.vk,
                x.m == p.m,
                x.sig == p.sig,
            })

            if(ver_record != None){
                Set({
                    post_state: atms.state,
                    effects: Set(
                        ATMS_UpdateInput(Set(VerifyResponse({cfg: p.cfg, vk: p.vk, m: p.m, sig: p.sig,  b: ver_record.unwrap().b}))),
                    )
                })
            }else{

                val new_b = if (atms.params.P.exists(P => {
                    and {
                        not (P.in(atms.params.C)),
                        sid_state.VK.exists(x => x.party == P and x.vk == p.vk)
                    }
                })){
                    false
                }else{
                    p.sim_b
                }

                val new_VER = sid_state.VER.setAdd({vk: p.vk, m: p.m, sig: p.sig, b: new_b})

                val new_CVK = if(sid_state.VK.exists(x => x.vk == p.vk)){
                    sid_state.CVK
                }else{
                    sid_state.CVK.setAdd(p.vk)
                }

                Set({
                    post_state: atms.state.set(p.cfg.sid, {...sid_state,
                        VER: new_VER,
                        CVK: new_CVK

                    }),
                    effects: Set(
                        ATMS_UpdateInput(Set(VerifyResponse({cfg: p.cfg, vk: p.vk, m: p.m, sig: p.sig, b: new_b}))),
                    )
                })
            }
        }).flatten()
    }

    pure def atms_asign_1(atms : ATMS_System) : Set[ATMS_Transition] = {
        atms.input.filterMap(m => match m {
            | ASignRequest(p) => Some(p)
            | _ => None
        }).map(p =>{
            if(atms_access_check(p.cfg, atms.params)) {
                Set({
                    post_state: atms.state,
                    effects: Set(
                        ATMS_LogMessage(ASignRequest(p)),
                        ATMS_UpdateInput(Set(SIMControl)),
                        ATMS_AppendHandle(SIMASignHold(p))
                    )
                })
            } else {
                Set({
                    post_state: atms.state,
                    effects: Set(
                        ATMS_UpdateInput(Set())
                    )
                })
            }
        }).flatten()
    }

    action atms_asign_2_act = all {
        // f_atms_s.input.atms_get_sim_control_messages().size() > 0,
        // f_atms_s.handles.atms_get_sim_asign_hold_messages().size() > 0,
        nondet sim_avk = ALL_AVKS.oneOf()
        nondet sim_tau = ALL_ASIGS.oneOf()
        //val sim_bs = List(true, false, true) // TODO: generate this thing somehow
        nondet sim_bs = listSamplingSpace(PARTY_SIZE, Set(true,false)).oneOf()
        nondet t = atms_asign_2(f_atms_s, Some(sim_avk), Some(sim_tau), sim_bs).oneOf()
        f_atms_s' = atms_process_transitions(f_atms_s, t) 
    }         

    pure def atms_asign_2(atms: ATMS_System, sim_avk: Option[ATMS_AVK], sim_tau: Option[ATMS_ASignature], sim_bs : List[bool]): Set[ATMS_Transition] = {
        atms.input.filterMap(m => match m {
            | SIMControl => Some(m)
            | _ => None
        }).map(_ => {
            atms.handles.filterMap(h => match h {
                | SIMASignHold(p) => Some(p)
                | _ => None
            }).map(p => {
                {
                    post_state: atms.state,
                    effects: Set(
                        ATMS_UpdateInput(Set(SIMASignRelease({cfg: p.cfg, m: p.m, vks: p.vks, S: p.S, sim_avk:sim_avk, sim_tau: sim_tau, sim_bs: sim_bs}))),
                        ATMS_RemoveHandle(SIMASignHold(p))
                    )
                } 
            })
        }).flatten()
    }

    pure def clean_asig(tau: Option[ATMS_ASignature], avk: ATMS_AVK, m: ATMS_Document, sid_state: ATMS_StateVars) : bool = {
        and {
            tau != None,
            or {
                not (sid_state.AVER.exists(x => and {
                    x.m == m,
                    x.sig == tau.unwrap(),
                    x.avk == avk
                })),
                sid_state.AVER.exists(x => and {
                    x.m == m,
                    x.sig == tau.unwrap(),
                    x.avk == avk,
                    x.b == true
                })
            }
        }
    }

    pure def san_asig(tau: Option[ATMS_ASignature], avk: ATMS_AVK, m : ATMS_Document, atms: ATMS_System, sid: SID) : Option[ATMS_ASignature] = {
        val sid_state = atms.state.get(sid)
        if(clean_asig(tau, avk, m, sid_state)){
                tau
            }else{  
                ALL_ASIGS.find(x => clean_asig(Some(x), avk, m, sid_state) )
            }
    }


    pure def atms_asign_3(atms: ATMS_System): Set[ATMS_Transition] = {
        atms.input.filterMap(m => match m {
            | SIMASignRelease(p) => Some(p)
            | _ => None
        }).map(p => {
            val sid_state = atms.state.get(p.cfg.sid)

            val Y = if(sid_state.AVK.exists(x => x.vks == p.vks)){
                sid_state
            }else{
                val avk = san_avk(p.sim_avk, p.vks, atms, p.cfg.sid)
                val new_AVK = sid_state.AVK.setAdd({vks: p.vks, avk: avk})
                val new_VKs = if(avk != None){
                    sid_state.VKs.setAdd({avk: avk.unwrap(), vks: p.vks})
                }else{
                    sid_state.VKs
                } 
                val new_CVK = p.vks.foldl(sid_state.CVK, (cvk, key) => {
                        if(sid_state.VK.exists(x => x.vk == key)){
                            cvk
                        }else{
                            cvk.setAdd(key)
                        }
                    })
                {...sid_state, CVK: new_CVK, AVK: new_AVK, VKs: new_VKs}
            }
            val sid_state = Y

            val avk = sid_state.AVK.find(x => x.vks == p.vks).unwrap().avk

            if(or {
                    avk == None,
                    not (p.S.indices().forall(i => {
                        (p.vks.indices().filter(j => p.vks[j] == p.S[i].vk)).size() == 1
                    })),
                }){
                    Set({
                        post_state: atms.state.set(p.cfg.sid, sid_state),  // TODO: update the state here?
                        effects: Set( 
                            ATMS_UpdateInput(Set(ASignResponse({cfg: p.cfg, m: p.m, vks: p.vks, S: p.S, avk: avk, bs : p.sim_bs, tau: None}))),
                        )
                    })
                }else{

                    val X = range(0,p.S.length()).foldl({ver:sid_state.VER, bs: p.sim_bs, cvk: sid_state.CVK}, (Z,i) => {
                        val vk = p.S[i].vk
                        val sig = p.S[i].sig

                        val new_bs = if(Z.ver.exists(x => x.vk == vk and x.m == p.m and x.sig == sig)) {
                            Z.bs.replaceAt(i, Z.ver.find(x => x.vk == vk and x.m == p.m and x.sig == sig).unwrap().b)
                            
                        } else {
                            if(atms.params.P.exclude(atms.params.C).exists(Pp => {
                                sid_state.VK.exists(x => x.party == Pp and x.vk == vk)
                            }) ){
                                Z.bs.replaceAt(i, false)
                            }else{
                                Z.bs
                            }
                        }

                        val new_ver = Z.ver.setAdd({vk: vk, m: p.m, sig: sig, b: new_bs[i]})

                        val new_cvk = if(sid_state.VK.exists(x => x.vk == vk)){
                            Z.cvk
                        }else{
                            Z.cvk.setAdd(vk)
                        }

                        {ver:new_ver, bs:new_bs, cvk: new_cvk}
                    })


                    if(p.S.indices().filter(i => X.bs[i]).size() <= atms.params.t){
                        //Map().get(X.bs)
                        //Set()

                        Set({
                            post_state: atms.state.set(p.cfg.sid, {...sid_state,
                                VER: X.ver,
                                CVK: X.cvk
                            }),
                            effects: Set( 
                                ATMS_UpdateInput(Set(ASignResponse({cfg: p.cfg, m: p.m, vks: p.vks, S: p.S, avk: avk, bs : X.bs, tau: None}))),
                                //ATMS_LogMessage(SIMASignRelease(p))
                            )
                        })
                    }else{
                        val maybe_new_tau = san_asig(p.sim_tau, avk.unwrap(), p.m, atms, p.cfg.sid)
                        if(clean_asig(maybe_new_tau, avk.unwrap(), p.m, sid_state) and maybe_new_tau != None){
                            val new_tau = maybe_new_tau.unwrap()
                            Set({
                                post_state: atms.state.set(p.cfg.sid, {...sid_state,
                                    AVER: sid_state.AVER.setAdd({avk: avk.unwrap(), m: p.m, sig: new_tau, b: true}),
                                    VER: X.ver,
                                    CVK: X.cvk
                                }),
                                effects: Set(
                                    ATMS_UpdateInput(Set(ASignResponse({cfg: p.cfg, m: p.m, vks: p.vks, S: p.S, avk: avk, bs : X.bs, tau: maybe_new_tau}))),
                                )
                            })
                        }else{
                            Set({
                                post_state: atms.state,
                                effects: Set(
                                    ATMS_UpdateInput(Set()),
                                )
                            }) // run out of VKS, raise error?
                        }   
                    }
                }
            }).flatten()
    
    }

    pure def atms_aver_1(atms: ATMS_System) : Set[ATMS_Transition] = {
        atms.input.filterMap(m => match m {
            | AVerifyRequest(p) => Some(p)
            | _ => None
        }).map(p =>{
            if(atms_access_check(p.cfg, atms.params)) {
                val sid_state = atms.state.get(p.cfg.sid)
                Set({
                    post_state: atms.state,
                    effects: Set(
                        ATMS_LogMessage(AVerifyRequest(p)),
                        ATMS_UpdateInput(Set(SIMControl)),
                        ATMS_AppendHandle(SIMAVerifyHold(p))
                    )
                })
            } else {
                Set({
                    post_state: atms.state,
                    effects: Set(
                        ATMS_UpdateInput(Set())
                    )
                })
            }
        }).flatten()
    }

    action atms_aver_2_act = all {
        // f_atms_s.input.atms_get_sim_control_messages().size() > 0,
        // f_atms_s.handles.atms_get_sim_averify_hold_messages().size() > 0,
        nondet sim_vks_set = ALL_VKS.powerset().oneOf()
        nondet sim_b = Set(true,false).oneOf()
        nondet t = atms_aver_2(f_atms_s, Some(sim_vks_set.setToList()), sim_b).oneOf()
        f_atms_s' = atms_process_transitions(f_atms_s, t) 
    }             

    pure def atms_aver_2(atms: ATMS_System, sim_vks: Option[List[ATMS_VK]], sim_b: bool) : Set[ATMS_Transition] = {
        atms.input.filterMap(m => match m {
            | SIMControl => Some(m)
            | _ => None
        }).map(_ => {
            atms.handles.filterMap(h => match h {
                | SIMAVerifyHold(p) => Some(p)
                | _ => None
            }).map(p => {
                {
                        post_state: atms.state,
                        effects: Set(
                            ATMS_UpdateInput(Set(SIMAVerifyRelease({cfg: p.cfg, avk: p.avk, m: p.m, sig: p.sig, sim_b: sim_b, sim_vks: sim_vks}))),
                            ATMS_RemoveHandle(SIMAVerifyHold(p))
                        )
                }
            })
        }).flatten()
    }

    pure def atms_aver_3(atms: ATMS_System): Set[ATMS_Transition] = {
        atms.input.filterMap(m => match m {
            | SIMAVerifyRelease(p) => Some(p)
            | _ => None
        }).map(p => {
            val sid = p.cfg.sid
            val sid_state = atms.state.get(sid)

            val maybe_aver_entry = sid_state.AVER.find(x => and { // TODO: AVER must be functional
                x.avk == p.avk,
                x.m == p.m,
                x.sig == p.sig,
            })

            if(maybe_aver_entry != None){
                val aver_entry = maybe_aver_entry.unwrap()
                Set({
                    post_state: atms.state,
                    effects: Set(
                        ATMS_UpdateInput(Set(AVerifyResponse({cfg: p.cfg, avk: p.avk, m: p.m, sig: p.sig, b: aver_entry.b}))),
                    )
                })
            }else{

                val maybe_vk_star = if(sid_state.VKs.exists(x => x.avk == p.avk)){
                    Some(sid_state.VKs.find(x => x.avk == p.avk).unwrap().vks)
                }else{
                    p.sim_vks
                }

                if(or {
                    maybe_vk_star == None,
                    and {
                        maybe_vk_star != None,
                        (sid_state.AVK.exists(x => maybe_vk_star.unwrap() == x.vks)),
                        not (sid_state.AVK.exists(x => x.avk == Some(p.avk)))
                    }
                    ,
                    clean_avk(Some(p.avk), maybe_vk_star.unwrap(), sid_state, atms.params.C)
                    }){
                    Set({
                        post_state: atms.state.set(sid, {...sid_state,
                            AVER: sid_state.AVER.setAdd({avk: p.avk, m: p.m, sig: p.sig, b: false})
                        }),
                        effects: Set(
                            ATMS_UpdateInput(Set(AVerifyResponse({cfg: p.cfg, avk: p.avk, m: p.m, sig: p.sig, b: false}))),
                        )
                    })
                }else{
                    val existing_avk_entry = sid_state.AVK.find(x => x.avk == Some(p.avk))
                    val vk_star = if (existing_avk_entry != None) {
                        existing_avk_entry.unwrap().vks
                    } else {
                        maybe_vk_star.unwrap()
                    }
                    val new_AVK = if (existing_avk_entry != None) {
                        sid_state.AVK
                    } else {
                        sid_state.AVK.setAdd({vks: vk_star, avk: Some(p.avk)})
                    }
                    val new_VKs = sid_state.VKs.setAdd({avk: p.avk, vks: vk_star})
                    val new_CVK = vk_star.foldl(sid_state.CVK, (cvk, key) => {
                        if(sid_state.VK.exists(x => x.vk == key)){
                            cvk
                        }else{
                            cvk.setAdd(key)
                        }
                    })
                    

                    val VKS_C = sid_state.VK.filter(vk_record => {
                        vk_record.party.in(atms.params.C)                             
                    }).map(vk_record => vk_record.vk)

                    val Z = vk_star.indices().fold({q_ctr : 0}, (acc, i) => {
                        val vk = vk_star[i]
                        if(or { 
                            not (sid_state.VK.exists(x => x.vk == vk)),
                            vk.in(VKS_C.union(sid_state.Signers.filter(x => x.m == p.m).map(x => x.vk)))
                        }){
                            {q_ctr: acc.q_ctr + 1}
                        }else{
                            acc
                        }
                    })

                    val new_b = if(Z.q_ctr <= atms.params.t) false else true


                    Set({
                        post_state: atms.state.set(sid, {...sid_state,
                            AVK: new_AVK, // BUG: problem with duplication
                            VKs: new_VKs,
                            CVK: new_CVK,
                            AVER: sid_state.AVER.setAdd({avk: p.avk, m: p.m, sig: p.sig, b: new_b})
                        }),
                        effects: Set(
                            ATMS_UpdateInput(Set(AVerifyResponse({cfg: p.cfg, avk: p.avk, m: p.m, sig: p.sig, b: new_b}))),
                        )
                    })
                }
            }
        }).flatten()
    } 

    action atms_step_with(q: (ATMS_System) => Set[(ATMS_Transition)]): bool = {
        all{
            val ts = q(f_atms_s)
            all {
                nondet transition = ts.oneOf()
                all {
                    f_atms_s' = atms_process_transitions(f_atms_s, transition),
                }
            }
        }
    }

   pure def atms_main_listener(atms: ATMS_System) : (Set[ATMS_Transition]) = {
        Set(
            atms_gen_1(atms),
            atms_gen_3(atms),

            atms_akey_1(atms),
            atms_akey_3(atms),

            atms_acheck_1(atms),
            atms_acheck_3(atms),

            atms_sign_1(atms),
            atms_sign_3(atms),

            atms_ver_1(atms),
            atms_ver_2(atms),
            atms_ver_3(atms),

            atms_asign_1(atms),
            atms_asign_3(atms),

            atms_aver_1(atms),
            atms_aver_3(atms),

            if(atms.params.responsive_simulator) Set() else atms.atms_sim_loses_ctrl(),
        ).flatten()
    }

    action atms_step : bool =  any {
        atms_step_with(atms_main_listener),
        atms_gen_2_act,
        atms_akey_2_act,
        atms_acheck_2_act,
        atms_sign_2_act,
        atms_asign_2_act,
        atms_aver_2_act,
    }

   action atms_init(params: ATMS_SystemParams) : bool = all {
        params.C.subseteq(params.P),

        f_atms_s' = {
            params : params,
            state : params.SIDs.mapBy(sid => {
                    CVK: Set(),
                    VK: Set(),
                    VKs: Set(),
                    AVK: Set(),
                    VER: Set(),
                    AVER: Set(),
                    Signers: Set(),
                }
            ),
            input : Set(),
            nonce : 0,
            log : Set(),
            handles : Set(),
        }
    }        




}