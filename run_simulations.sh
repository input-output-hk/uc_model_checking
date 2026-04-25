#!/bin/bash

SPECS_DIR="$(dirname "$0")/specs"
MAX_SAMPLES=100
MAX_STEPS=500

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

run_sim "f_crs"    "$SPECS_DIR/f_crs/f_crs_properties.qnt"       "f_crs_properties"
run_sim "f_hash"   "$SPECS_DIR/f_hash/f_hash_properties.qnt"     "f_hash_properties"
run_sim "f_sig"    "$SPECS_DIR/f_sig/f_sig_properties.qnt"       "f_sig_properties"
run_sim "f_dif"    "$SPECS_DIR/f_dif/f_dif_properties.qnt"       "f_dif_properties"
run_sim "f_inizk"  "$SPECS_DIR/f_inizk/f_inizk_properties.qnt"   "f_inizk_properties"
run_sim "f_ac"     "$SPECS_DIR/f_ac/f_ac_properties.qnt"         "f_ac_properties"
run_sim "f_atms"   "$SPECS_DIR/f_atms/f_atms_properties.qnt"     "f_atms_properties"
run_sim "f_pki"    "$SPECS_DIR/f_pki/f_pki_properties.qnt"       "f_pki_properties"
run_sim "g_clock"  "$SPECS_DIR/g_clock/g_clock_properties.qnt"   "g_clock_properties"
run_sim "g_ledger" "$SPECS_DIR/g_ledger/g_ledger_properties.qnt" "g_ledger_properties"
run_sim "p_bridge" "$SPECS_DIR/p_bridge/p_bridge_properties.qnt" "p_bridge_properties"

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
