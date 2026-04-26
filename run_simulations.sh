#!/bin/bash

SPECS_DIR="$(dirname "$0")/specs"
MAX_SAMPLES=100
MAX_STEPS=300

PASS=0
FAIL=0
ERRORS=()

run_sim() {
    local name="$1"
    local file="$2"
    local module="$3"

    echo ""
    echo "========================================"
    echo "  $name"
    echo "========================================"

    if quint run \
        --backend=rust \
        --verbosity=1 \
        --max-samples="$MAX_SAMPLES" \
        --max-steps="$MAX_STEPS" \
        --invariant=AllProps \
        --main="$module" \
        "$file"
    then
        echo "RESULT: ok"
        PASS=$((PASS + 1))
    else
        echo "RESULT: FAILED"
        FAIL=$((FAIL + 1))
        ERRORS+=("$name")
    fi
}

# Must-fail: a violation is expected (coverage check).
# If quint finds no violation, the model can't reach that state — that's a failure.
run_must_fail() {
    local name="$1"
    local file="$2"
    local module="$3"
    local invariant="$4"

    echo ""
    echo "  [must-fail] $invariant"

    if quint run \
        --backend=rust \
        --verbosity=1 \
        --max-samples="$MAX_SAMPLES" \
        --max-steps="$MAX_STEPS" \
        --invariant="$invariant" \
        --main="$module" \
        "$file"
    then
        # exit 0 means no violation found — bad for a must-fail property
        echo "  FAILED: expected violation not found for $invariant"
        FAIL=$((FAIL + 1))
        ERRORS+=("$name/$invariant")
    else
        # exit non-0 means violation found — expected
        PASS=$((PASS + 1))
    fi
}

run_must_fails() {
    local name="$1"
    local file="$2"
    local module="$3"
    shift 3
    local invariants=("$@")

    echo ""
    echo "========================================"
    echo "  $name [must-fail suite]"
    echo "========================================"

    for inv in "${invariants[@]}"; do
        run_must_fail "$name" "$file" "$module" "$inv"
    done
}

# ====== Safety simulations (must hold) ======


run_sim "f_hash"   "$SPECS_DIR/f_hash/f_hash_properties.qnt"     "f_hash_properties"
run_sim "f_sig"    "$SPECS_DIR/f_sig/f_sig_properties.qnt"       "f_sig_properties"
run_sim "f_dif"    "$SPECS_DIR/f_dif/f_dif_properties.qnt"       "f_dif_properties"
run_sim "f_inizk"  "$SPECS_DIR/f_inizk/f_inizk_properties.qnt"   "f_inizk_properties"
run_sim "f_atms"   "$SPECS_DIR/f_atms/f_atms_properties.qnt"     "f_atms_properties"
run_sim "f_pki"    "$SPECS_DIR/f_pki/f_pki_properties.qnt"       "f_pki_properties"
run_sim "g_clock"  "$SPECS_DIR/g_clock/g_clock_properties.qnt"   "g_clock_properties"
run_sim "g_ledger" "$SPECS_DIR/g_ledger/g_ledger_properties.qnt" "g_ledger_properties"
run_sim "p_bridge" "$SPECS_DIR/p_bridge/p_bridge_properties.qnt" "p_bridge_properties"

# ====== Coverage checks (must fail) ======


run_must_fails "f_hash" "$SPECS_DIR/f_hash/f_hash_properties.qnt" "f_hash_properties" \
    "MF1"

run_must_fails "f_sig" "$SPECS_DIR/f_sig/f_sig_properties.qnt" "f_sig_properties" \
    "GenLiveness" \
    "SignLiveness"

run_must_fails "f_dif" "$SPECS_DIR/f_dif/f_dif_properties.qnt" "f_dif_properties" \
    "DiffuseLiveness" \
    "FetchLiveness" \
    "InjectLiveness" \
    "OKLiveness"

run_must_fails "f_inizk" "$SPECS_DIR/f_inizk/f_inizk_properties.qnt" "f_inizk_properties" \
    "ProveLiveness" \
    "VerifyLiveness"

run_must_fails "f_pki" "$SPECS_DIR/f_pki/f_pki_properties.qnt" "f_pki_properties" \
    "RegisterLiveness" \
    "RetrieveLiveness" \
    "CertVerifyLiveness" \
    "GetAllLiveness"

run_must_fails "g_clock" "$SPECS_DIR/g_clock/g_clock_properties.qnt" "g_clock_properties" \
    "MF1" \
    "MF2" \
    "MF3"

run_must_fails "g_ledger" "$SPECS_DIR/g_ledger/g_ledger_properties.qnt" "g_ledger_properties" \
    "MF1" \
    "MF2" \
    "MF3" \
    "MF4" \
    "MF5" \
    "MF6" \
    "MF7" \
    "MF8" \
    "MF9" 

run_must_fails "p_bridge" "$SPECS_DIR/p_bridge/p_bridge_properties.qnt" "p_bridge_properties" \
    "Q1" \
    "Q3" \
    "Q2" \
    "Q4" \
    "Q5" \
    "Q6" 

echo ""
echo "========================================"
echo "  SUMMARY"
echo "========================================"
echo "  passed: $PASS"
echo "  failed: $FAIL"
if [ ${#ERRORS[@]} -gt 0 ]; then
    echo "  failures: ${ERRORS[*]}"
fi
echo "========================================"
