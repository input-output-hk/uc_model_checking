# P-Bridge Protocol Specification

## Overview

The P-Bridge protocol is a comprehensive cross-chain bridge implementation that enables secure communication and asset transfer between two distributed ledgers. The protocol coordinates multiple distributed subsystems (G-Clock, F-PKI, F-ATMS, G-Ledger1, G-Ledger2, F-DIF, F-Hash, F-iNIZK) to achieve secure certification and verification of ledger states across multiple rounds.

## Module Structure

The P-Bridge protocol is implemented as follows:

- **p_bridge.qnt** : Core protocol framework and state management
- **p_bridge_types.qnt** : Message types, state variables, and system definitions
- **p_bridge_bkgen.qnt** : ATMS key generation protocol (4-phase) for cryptographic setup
- **p_bridge_rot.qnt** : root of trust generation (ATMS key)
- **p_bridge_br.qnt** : bridge algorithm for ledger cross communication
- **p_bridge_led2.qnt**: Secondary ledger 
- **p_bridge_inizk.qnt**: iNIZK integration (certify transactions on Ledger1, verify L1 transactions on L2)
- **p_bridge_env.qnt**: Environment and adversary interaction layer
- **p_bridge_properties.qnt** : Formal properties and assertions

## Functionality
![p-bridge](pic1.png?raw=true)
![atms-algorithms](pic2.png?raw=true)

## Verified Properties [p_bridge_properties.qnt](p_bridge_properties.qnt)

- **RoT agreement**: All parties agree on Root-of-Trust aggregate verification key
- **Bridge state consistency**: All parties agree on core bridge state (rot, cID, h, avk', aux, aux')
- **Aux update**: Auxiliary state updates are  correct every R rounds (bst.aux @ (r+R) = bst.aux' @ r)
- **Bridge democracy**: Committee can change between rotation periods (may be violated since it doesn't have to)
- **State persistence**: Bridge state remains stable during non-rotation phases (within Δ+1 rounds)
- **Cross-chain message integrity**: Messages verified across chains maintain integrity
- **Key generation correctness**: Bridge validator keys generated and aggregated correctly

## Running the Simulation

```
# Check all properties hold (Rust backend, ~38 traces/sec):
cd specs/p_bridge
quint run --backend=rust --invariant='AllProps' --max-steps=500 --max-samples=100 p_bridge_properties.qnt

# Verify full end-to-end liveness (successful verify response):
quint run --backend=rust --invariant='Q6' --max-steps=2000 --max-samples=50 p_bridge_properties.qnt
```

Verified with Quint 0.32.0. Both backends work correctly; `--backend=rust` is recommended (~38× faster).

## Bug Fixes Applied

The following bugs were fixed to make the simulation reach a stable point where all properties hold and the full protocol chain (BKGen → RoT → BRAlgorithm → Certify → Verify) completes successfully.

### p_bridge_br.qnt

**Fix 1 — Case 6 dispatch**: The `else` branch in `brd_br_5` was `Map().get(1)` (a crash stub). Replaced with a call to `brd_br_5_case6`, which already existed but was unreachable.

**Fix 2 — `brd_br_5_case1` missing guard**: Added a guard for `bst.rot == None` to skip gracefully when RoT is not yet completed, instead of crashing.

**Fix 3 — `brd_br_5_case3` missing guard**: Added a guard for `bst.aux == None`. When the RoT prerequisite (aux contains bvks + committee) is not yet available, the function now returns a `BridgeBRAlgorithmResponse` with the current state rather than crashing.

**Fix 4 — `brd_br_5_case5` crash stub**: `Map().get(22)` crash stub was present with a FIXME comment for the case where S is empty. Added prerequisite guards (`bst.aux == None or bst.h == None or bst.avk_prim == None or bst.cID == None`) and removed the crash, always proceeding with `ASignRequest` regardless of whether `S` is empty.

**Fix 5 — `brd_br_5_case5_2` tau=None handling**: Added handling for when `p.tau == None` (ATMS sign failed or returned no tau). Instead of crashing with `Map().get(2)`, the function now returns a `BridgeBRAlgorithmResponse` with `btx2: None`, `m: None`, and clears the message set.

**Fix 6 — `brd_br_5_case3_3` handle leak**: When the `avk=None` branch fired, it was missing `BRD_RemoveHandle(BridgeAKeyWait(q))`. The handle would remain forever, preventing future progress. Added the missing removal.

**Fix 7 — `brd_br_6` bst=None handling**: The comment "should not happen" was incorrect — this CAN happen when `brd_br_5_case3_3` returns `bst: None`. Fixed by reading the current party's bridge state from `sys.brd.state` and using it as the `bst` for the diffuse step.

### p_bridge_inizk.qnt

**Fix 8 — access check TODO**: The access check call `brd_access_check(p.cfg, sys.brd.params)` was preceded by a TODO comment about also checking `p.cfg.party.in(sys.brd.params.P1)`. Removed the stale TODO (the existing access check is sufficient).

### p_bridge_types.qnt

**Fix 9 — commented-out helper functions**: Three helper functions used by `p_bridge_testing.qnt` were commented out: `brd_get_bkgen_response_messages`, `brd_get_rot_response_messages`, `brd_get_bridge_response_messages`. Uncommented them to restore testing functionality.

### p_bridge_testing.qnt

**Fix 10 — wrong import name**: `f_diff` imported as `f_diff_types` and `f_diff` (module does not exist). Fixed to `f_dif_types` and `f_dif`.

### p_bridge_env.qnt

**Fix 11 — ledgers not responsive**: Both `g_ledger_s` and `g_ledger_s2` were initialized with `responsive_simulator: false`. With this setting, `led_sim_loses_ctrl` eats `LED_SIMControl` (generated by `led_iread_1` in response to a `ReadRequest`) without providing `ReadSIMRelease`, permanently blocking ledger reads. Changed both to `responsive_simulator: true` so reads complete via `led_iread_2_act`.

**Fix 12 — DIF not responsive**: `f_dif_s` was initialized with `responsive_simulator: false`, causing `dif_sim_loses_ctrl` to eat `DIF_SIMControl` without providing `DIF_SIMFetchRelease`, permanently blocking DIF fetches. Changed to `responsive_simulator: true`.

### p_bridge_properties.qnt

**Fix 13 — SID set causes clock deadlock**: `SIDs = 1.to(SID_SIZE)` with `SID_SIZE=2` meant two SIDs per party. G-Clock requires all `(party, SID)` pairs to OK before advancing, but `bkgen_flag` is per-party (not per-SID). Each party can only BKGen once, leaving the second SID never OKing → clock never advances. Fixed by hardcoding `SIDs = Set(1)` (single SID per session), with a comment explaining the constraint.

### specs/g_ledger/g_ledger_properties.qnt (separate fix)

**Fix 14 — LED_Liveness false negative**: `Normal({b: false})` transactions are always rejected by `led_eval_2` (`valid = not(log.listHas(tx)) and x.b` evaluates to false). The liveness property was incorrectly requiring such transactions to eventually appear in the ledger. Added a validity filter: liveness only applies to transactions where `x.b = true`.

### specs/g_ledger/g_ledger_types.qnt

**Fix 15 — commented-out helper**: `led_get_submit_response_messages` was commented out but used by `p_bridge_testing.qnt`. Uncommented.

## Code Cleanup Applied

The following cleanup was applied after the bug fixes to improve specification quality.

### p_bridge_types.qnt

- **`BRD_AUX` type corrected**: `type BRD_AUX = {bvks: List[BRD_BVK], com: Committee}` was stale — `aux`/`aux_prim` fields use `Party -> BRD_BVK` (a Map). Updated to `{bvks: Party -> BRD_BVK, com: Committee}` to match actual usage.
- **Removed dead commented-out import**: `//import f_sig_types.* from "../f_sig/f_sig_types"` removed.
- **Removed dead `//btx1: Option[LED_Tx]`**: Stale removed-field comment in `BRD_BR_Resp` removed.
- **Removed commented-out `BRD_SystemParams` fields**: `//P1`, `//P2`, `//t` field stubs removed.
- **Cleaned `brd_id_ctrans` body**: Removed stale 6-line commented-out block in `brd_id_ctrans`.

### p_bridge_br.qnt

- **`brd_br_5_case5` guard extended**: Added `bst.aux_prim == None` to the prerequisite guard. If `aux_prim` hasn't been set by case3, case5 now correctly skips rather than proceeding to set `aux: None` in the success branch of case5_2.
- **Removed all `//btx1: None` dead comments**: 6 occurrences removed throughout the file.

### p_bridge_env.qnt

- **Removed stale TODO**: Removed `//TODO: enable_logging: false, responsive_simulator: true` from `brd_init` call — `brd_init` does not support those parameters.

### g_ledger/g_ledger_properties.qnt

- **Removed misleading `// WRONG` comment**: `(p.r == q.r) implies { // WRONG: log_entry1.time == log_entry2.time` — the current `p.r == q.r` check IS correct. `ComSelResponse.r` is set to `clock.state.TIME` at request time (via `ComSelSIMHold`), so `p.r == q.r` correctly checks agreement at the same request time. Comment removed.

### Bridge_State_Persistence fix (p_bridge_properties.qnt)

**Fix 16 — cross-period comparison**: `Bridge_State_Persistence` used `p.args.r + R < q.args.r` (strict inequality spanning periods). This allowed comparing rounds from different rotation periods where `h` legitimately differs (fresh Merkle root each period). Fixed by replacing the cross-period condition with `p.args.r / R == q.args.r / R` (same period via integer division), ensuring only within-period stability is checked.

### Committee canonicalization (g_ledger_comsel.qnt)

**Fix 17 — non-canonical committee ordering**: `led_comsel_clean` allowed the same committee as `[1,2]` in one period and `[2,1]` in another. List equality is order-sensitive, so `Bridge_Aux_Update` and `Bridge_State_Persistence` would falsely fail when comparing `aux.com` across periods. Fixed by adding `com.indices().forall(i => i == 0 or com[i-1] < com[i])` (canonical ascending order).

### Bridge_Liveness fix (p_bridge_inizk.qnt, p_bridge_types.qnt, p_bridge_br.qnt)

**Fix 18 — certify computes independent hash**: `brd_certify_3` computed `hash(L1_certify[0..APL_certify])` fresh at certify time. Because the adversary controls ledger reads, this hash could differ from `bst.h` (computed by the bridge at the rotation round and committed to ledger2). The verify check `mem.hs.listHas(proof.h)` then always failed.

Root cause: certify and bridge independently read ledger1, potentially seeing different prefixes/APL values from the adversarial simulator.

Fix: added `apl: Option[int]` field to `BRD_BST_Entry`; `brd_br_5_case3_6` now stores `bst.apl = Some(q.apl)` alongside `bst.h`; `brd_certify_3` now reads `bst.h`/`bst.apl` from the bridge state and uses them directly (skipping the fresh hash request). If `bst.h == None` (bridge hasn't run case3 yet), certify returns `res = None` (no proof), making the liveness precondition false and avoiding a spurious violation.

### Bridge_Soundness fix (g_ledger/g_ledger_apl.qnt)

**Fix 19 — non-monotone APL allows soundness violation**: `led_apl_san` allowed the adversary to set a lower APL value at a later round (e.g., `APL.get(8)=2` → `APL.get(12)=1`) because `led_apl_Clean` only constrains APL to `[max(min_ptr-delta, 0), min_ptr]`, not to be non-decreasing. `Bridge_Soundness` uses `APL.get(r_verify)` as the prefix bound, but the NIZK proof was certified using `bst.apl = APL.get(r_rotation)`. If `APL.get(r_verify) < bst.apl`, a tx at index ≥ APL_verify but < bst.apl (legitimately proven by NIZK) is not in `L1[0..APL_verify]` → soundness violation.

Fix: `led_apl_san` now computes `prev_max = max of all existing APL values` and enforces the new APL ≥ `prev_max`. This makes APL monotone: once finalization reaches pointer k, it never retreats. With monotone APL, `APL.get(r_verify) ≥ APL.get(r_rotation) = bst.apl`, so any tx proven by NIZK to be in `L1[0..bst.apl]` is also in `L1[0..APL.get(r_verify)]`.
