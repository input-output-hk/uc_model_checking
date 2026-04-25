module f_sig{

    import common.* from "../common"
    import basicSpells.* from "../../spells/basicSpells"
    import rareSpells.* from "../../spells/rareSpells"    
    import f_sig_types.* from "./f_sig_types"
 
    var f_sig_s : SIG_System

    // SIM gives control back to the environment
    pure def sig_sim_loses_ctrl(sig: SIG_System) : Set[SIG_Transition] = {
        sig.input.sig_get_sim_control_messages().map(c =>  // sim must have control
            {   
                post_state : sig.state,
                effects : Set( 
                    SIG_UpdateInput(Set())
                )  
            })
    }    

    pure def sig_gen_1(sig: SIG_System): Set[SIG_Transition] = {
        sig.input.sig_get_gen_request_messages().map(p => {
            if(sig_signer_access_check(p.cfg, sig.params)){
                {
                    post_state: sig.state,
                    effects: Set(
                        SIG_LogMessage(SIG_GenRequest(p)),
                        SIG_UpdateInput(Set(SIG_SIMControl)),
                        SIG_AppendHandle(SIG_SIMGenHold({cfg: p.cfg}))
                    )
                }
            }else{
                {
                    post_state: sig.state,
                    effects: Set( // ignore unauthorized gen request, log it or not?
                        SIG_UpdateInput(Set())
                    )
                }
            }           
        })
    }

    pure def sig_gen_2(sig: SIG_System, sim_vk: SIG_VK): Set[SIG_Transition] = {
        sig.input.sig_get_sim_control_messages().map(q => {
            sig.handles.sig_get_sim_gen_hold_messages().map(p => {
                {
                    post_state: sig.state,
                    effects: Set(
                        SIG_UpdateInput(Set(SIG_SIMGenRelease({cfg: p.cfg, vk: sim_vk}))),
                        SIG_RemoveHandle(SIG_SIMGenHold(p))
                    )
                }     
            })
        }).flatten()
    }

    action sig_gen_2_act : bool = all {
        f_sig_s.input.sig_get_sim_control_messages().size() > 0,
        f_sig_s.handles.sig_get_sim_gen_hold_messages().size() > 0,
        nondet sim_vk = f_sig_s.params.ALL_VKs.oneOf()
        nondet t = sig_gen_2(f_sig_s, sim_vk).oneOf()
        f_sig_s' =  sig_process_transitions(f_sig_s, t),
    }    

    pure def sig_gen_3(sig: SIG_System): Set[SIG_Transition] = {
        sig.input.sig_get_sim_gen_release_messages().map(p => {
            val party = p.cfg.party
            val sid = p.cfg.sid
            val sid_state = sig.state.get(sid)

            val maybe_vk = sid_state.VK.find(x => x.party == party)

            if(maybe_vk != None){
               {
                    post_state: sig.state,
                    effects: Set(
                        SIG_UpdateInput(Set(SIG_GenResponse({cfg:p.cfg, vk: maybe_vk.unwrap().vk})))
                    )
                }                                    
            }else{
                 def predicate = (vk) => {
                    or { 
                        sid_state.VK.exists(y => y.vk == vk),
                        sid_state.Ver.exists(y => y.vk == vk)
                    }
                }

                val maybe_new_vk = if (predicate(p.vk)){
                    sig.params.ALL_VKs.find(x => not (predicate(x))) // make sure this always succeeds
                }else{
                    Some(p.vk)
                }

                if (maybe_new_vk == None){
                    // we exhausted all possible VKs, so system cannot proceed
                    {
                        post_state: sig.state,
                        effects: Set(
                            SIG_UpdateInput(Set()), // return the original vk even if it collides
                        )
                    }
                }else{
                    val new_vk = maybe_new_vk.unwrap()
                    {
                        post_state: sig.state.set(sid, {...sid_state, VK: sid_state.VK.setAdd({party: party, vk: new_vk})}),
                        effects: Set(
                            SIG_UpdateInput(Set(SIG_GenResponse({cfg:p.cfg, vk: new_vk}))),
                        )
                    }
                }
            }
        })
    }

    pure def sig_sign_1(sig: SIG_System): Set[SIG_Transition] = {
        sig.input.sig_get_sign_request_messages().map(p => {
            if(sig_signer_access_check(p.cfg, sig.params)){
                val party = p.cfg.party
                val sid = p.cfg.sid
                val sid_state = sig.state.get(sid)                

                val maybe_vk = sid_state.VK.find(x => x.party == party)

                if(maybe_vk == None){
                    {
                        post_state: sig.state,
                        effects: Set(
                            SIG_LogMessage(SIG_SignRequest(p)),
                            SIG_UpdateInput(Set(SIG_SignResponse({cfg:p.cfg, m: p.m, sig: None})))
                        )
                    }
                }else{
                    {
                        post_state: sig.state,
                        effects: Set(
                            SIG_LogMessage(SIG_SignRequest(p)),
                            SIG_UpdateInput(Set(SIG_SIMControl)),
                            SIG_AppendHandle(SIG_SIMSignHold({cfg: p.cfg, m: p.m}))
                        )
                    }
                }
            }else{
                {
                    post_state: sig.state,
                    effects: Set( // ignore unauthorized sign request, log it or not?
                        SIG_UpdateInput(Set())
                    )
                }
            }           
        })
    }

    pure def sig_sign_2(sig: SIG_System, sim_sig: SIG_Signature): Set[SIG_Transition] = {
        sig.input.sig_get_sim_control_messages().map(q => {
            sig.handles.sig_get_sim_sign_hold_messages().map(p => {
                {
                    post_state: sig.state,
                    effects: Set(
                        SIG_UpdateInput(Set(SIG_SIMSignRelease({cfg: p.cfg, m: p.m, sig: sim_sig}))),
                        SIG_RemoveHandle(SIG_SIMSignHold(p))
                    )
                }     
            })
        }).flatten()
    }

    action sig_sign_2_act : bool = all {
        f_sig_s.input.sig_get_sim_control_messages().size() > 0,
        f_sig_s.handles.sig_get_sim_sign_hold_messages().size() > 0,
        nondet sim_sig = f_sig_s.params.ALL_SIGs.oneOf()
        nondet t = sig_sign_2(f_sig_s, sim_sig).oneOf()
        f_sig_s' =  sig_process_transitions(f_sig_s, t),
        
    }

    pure def sig_sign_3(sig: SIG_System): Set[SIG_Transition] = {
        sig.input.sig_get_sim_sign_release_messages().map(q => {

            val party = q.cfg.party
            val sid = q.cfg.sid
            val sid_state = sig.state.get(sid)                
            val maybe_vk = sid_state.VK.find(x => x.party == party)
            val vk = maybe_vk.unwrap().vk

   
            def predicate = (sig) => {
                sid_state.Ver.exists(x => {
                    and {
                        x.vk == vk,
                        x.m == q.m,
                        x.sig == sig,
                        x.b == false 
                    }
                })
            }

            val maybe_new_sig = if (predicate(q.sig)){
                sig.params.ALL_SIGs.find(x => not (predicate(x))) // make sure this always succeeds
            }else{
                Some(q.sig)
            }

            if(maybe_new_sig == None){
                // we exhausted all possible signatures, so system cannot proceed
                {
                    post_state: sig.state,
                    effects: Set(
                        SIG_UpdateInput(Set()),
                    )
                }
            }else{
                val new_sig = maybe_new_sig.unwrap()
                {
                    post_state: sig.state.set(sid, {...sid_state, Ver: sid_state.Ver.setAdd({vk: vk, m: q.m, sig: new_sig, b: true})}),
                    effects: Set(
                        SIG_UpdateInput(Set(SIG_SignResponse({cfg: q.cfg, m: q.m, sig: Some(new_sig)}))),
                    )
                }
            }
        })
    }

    pure def sig_verify_1(sig: SIG_System): Set[SIG_Transition] = {
        sig.input.sig_get_verify_request_messages().map(p => {
            if(sig_verifier_access_check(p.cfg, sig.params)){           
                {
                    post_state: sig.state,
                    effects: Set(
                        SIG_LogMessage(SIG_VerifyRequest(p)),
                        SIG_UpdateInput(Set(SIG_SIMControl)),
                        SIG_AppendHandle(SIG_SIMVerifyHold(p))
                    )
                }
            }else{
                {
                    post_state: sig.state,
                    effects: Set( // ignore unauthorized verify request, log it or not?
                        SIG_UpdateInput(Set())
                    )
                }
            }           
        })
    }

    pure def sig_verify_2(sig: SIG_System, sim_b: bool): Set[SIG_Transition] = {
        sig.input.sig_get_sim_control_messages().map(p => {
            sig.handles.sig_get_sim_verify_hold_messages().map(q => {
                    {
                        post_state: sig.state,
                        effects: Set(
                            SIG_UpdateInput(Set(SIG_SIMReleaseVerify({cfg: q.cfg, vk:q.vk, m:q.m, sig:q.sig, b: sim_b}))),
                            SIG_RemoveHandle(SIG_SIMVerifyHold(q))
                        )
                    }
            })
        }).flatten()
    }

    action sig_verify_2_act : bool = all {
        f_sig_s.input.sig_get_sim_control_messages().size() > 0,
        f_sig_s.handles.sig_get_sim_verify_hold_messages().size() > 0,
        nondet sim_b = Set(true, false).oneOf()
        nondet t = sig_verify_2(f_sig_s, sim_b).oneOf()
        f_sig_s' =  sig_process_transitions(f_sig_s, t),
        
    }    

    pure def sig_verify_3(sig: SIG_System): Set[SIG_Transition] = {
        sig.input.sig_get_sim_release_verify_messages().map(p => {

            val party = p.cfg.party
            val sid = p.cfg.sid
            val sid_state = sig.state.get(sid)                

            if(sid_state.Ver.exists(x => x.vk == p.vk and x.m == p.m and x.sig == p.sig)){
                val b = sid_state.Ver.find(x => x.vk == p.vk and x.m == p.m and x.sig == p.sig).unwrap().b
                {
                    post_state: sig.state,
                    effects: Set(
                        SIG_UpdateInput(Set(SIG_VerifyResponse({...p, b: b}))),
                    )
                }
            }else{
                val new_b = if(sid_state.VK.exists(x => x.vk == p.vk and not (x.party.in(sig.params.C)))){
                    false
                }else{
                    p.b
                }

                {
                    post_state: sig.state.set(sid, {...sid_state, Ver: sid_state.Ver.setAdd({vk: p.vk, m: p.m, sig: p.sig, b: new_b})}),
                    effects: Set(
                        SIG_UpdateInput(Set(SIG_VerifyResponse({...p, b: new_b}))),
                    )
                }
            }
        })
    }

   pure def sig_main_listener(sig: SIG_System) : (Set[SIG_Transition]) = {
        Set(
            sig_gen_1(sig),
           //sig_gen_2(f_sig_s),
            sig_gen_3(sig),

            sig_sign_1(sig),
           //sig_sign_2(f_sig_s),
            sig_sign_3(sig),

            sig_verify_1(sig),
            //sig_verify_2(sig),
            sig_verify_3(sig),

            if (sig.params.responsive_simulator) Set() else sig_sim_loses_ctrl(sig),

        ).flatten()
    }

    def sig_process_transitions(sys: SIG_System, transition: SIG_Transition) : SIG_System = {
        val new_sys = {...sys, state: transition.post_state}
        transition.effects.fold(new_sys, ((a1,a2) => sig_apply_effect(a1,a2)))
    } 

    action sig_step_with(q: (SIG_System) => Set[(SIG_Transition)]): bool = {
        all{
            val ts = q(f_sig_s)
            all {
                nondet transition = ts.oneOf()
                all {
                    f_sig_s' = sig_process_transitions(f_sig_s, transition),
                }
            }
        }
    }        
    
    action sig_step : bool =  any {
        sig_step_with(sig_main_listener),
        sig_sign_2_act,
        sig_gen_2_act,
        sig_verify_2_act
    }        


   action sig_init(P: Set[Party], C: Set[Party], SIDs: Set[SID], S: Set[Party], ALL_VKs: Set[SIG_VK], ALL_SIGs: Set[SIG_Signature], responsive_simulator: bool) : bool =  all {
        C.subseteq(P),
        S.subseteq(P),

        f_sig_s' = {
            params : {
                P: P,
                C: C,
                SIDs: SIDs,
                S: S,

                ALL_VKs: ALL_VKs,
                ALL_SIGs: ALL_SIGs,
                responsive_simulator: responsive_simulator,
            },
            state : SIDs.mapBy(sid => {
                VK: Set(),
                Ver: Set(),
            }),
            input : Set(),
            nonce : 0,
            log : Set(),
            handles : Set(),
        }
    }

}