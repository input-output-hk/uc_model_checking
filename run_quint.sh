#!/bin/bash

# Usage: run_quint.sh {simulate|must-fails|verify} [--max-steps=N] [--max-samples=N] [--module=X]
#
#   simulate    quint run AllProps for each functionality (SIMULATION=true)
#               default --max-steps=400 --max-samples=100
#
#   must-fails  quint run each must-fail invariant for each functionality
#               (SIMULATION=true) default --max-steps=1000 --max-samples=100
#
#   verify      quint verify AllProps for functionalities except f_atms,
#               g_ledger, p_bridge (SIMULATION=false)
#               --max-steps / --max-samples are not accepted in this mode
#
#   --module=X  optional: run only the named functionality
#               (e.g. --module=f_hash). For verify, X must not be
#               f_atms, g_ledger, or p_bridge.

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SPECS_DIR="$SCRIPT_DIR/specs"
PARAMS_FILE="$SPECS_DIR/params.qnt"

# ---- raise Node heap for quint (avoids OOM on long simulate/must-fails runs) ----
# Override with: QUINT_NODE_HEAP_MB=16384 ./run_quint.sh ...
QUINT_NODE_HEAP_MB="${QUINT_NODE_HEAP_MB:-8192}"
export NODE_OPTIONS="${NODE_OPTIONS:-} --max-old-space-size=$QUINT_NODE_HEAP_MB"

# ---- prerequisite: quint >= 0.32.0 ----
REQUIRED_QUINT_VERSION="0.32.0"

if ! command -v quint >/dev/null 2>&1; then
    echo "Error: 'quint' is not installed or not on PATH" >&2
    echo "Install: https://quint-lang.org" >&2
    exit 1
fi

ACTUAL_QUINT_VERSION="$(quint --version 2>&1 | grep -oE '[0-9]+\.[0-9]+\.[0-9]+' | head -n1)"
if [ -z "$ACTUAL_QUINT_VERSION" ]; then
    echo "Error: could not parse quint version (got: $(quint --version 2>&1))" >&2
    exit 1
fi
if [ "$(printf '%s\n%s\n' "$REQUIRED_QUINT_VERSION" "$ACTUAL_QUINT_VERSION" | sort -V | head -n1)" != "$REQUIRED_QUINT_VERSION" ]; then
    echo "Error: quint $ACTUAL_QUINT_VERSION is older than required $REQUIRED_QUINT_VERSION" >&2
    exit 1
fi

usage() {
    echo "Usage: $0 {simulate|must-fails|verify} [--max-steps=N] [--max-samples=N] [--module=X]"
    echo "  --max-steps / --max-samples are valid only for simulate and must-fails."
    echo "  --module=X runs only that functionality (e.g. --module=f_hash)."
}

MODE="${1:-}"
case "$MODE" in
    simulate|must-fails|verify) shift ;;
    *)
        usage
        exit 1
        ;;
esac

# ---- defaults per mode ----
case "$MODE" in
    simulate)   MAX_STEPS=400;  MAX_SAMPLES=100; SIM_FLAG=true ;;
    must-fails) MAX_STEPS=1000; MAX_SAMPLES=100; SIM_FLAG=true ;;
    verify)                                      SIM_FLAG=false ;;
esac

# ---- parse optional flag overrides ----
MODULE_FILTER=""
while [ $# -gt 0 ]; do
    case "$1" in
        --max-steps=*)
            if [ "$MODE" = "verify" ]; then
                echo "Error: --max-steps is not valid for verify mode" >&2
                exit 1
            fi
            MAX_STEPS="${1#--max-steps=}"
            ;;
        --max-samples=*)
            if [ "$MODE" = "verify" ]; then
                echo "Error: --max-samples is not valid for verify mode" >&2
                exit 1
            fi
            MAX_SAMPLES="${1#--max-samples=}"
            ;;
        --module=*)
            MODULE_FILTER="${1#--module=}"
            ;;
        *)
            echo "Error: unknown argument: $1" >&2
            usage
            exit 1
            ;;
    esac
    shift
done

perl -pi -e "s/^\s*val SIMULATION = .*/    val SIMULATION = $SIM_FLAG/" "$PARAMS_FILE"
echo "params.qnt: SIMULATION = $SIM_FLAG"
if [ "$MODE" != "verify" ]; then
    echo "settings: --max-steps=$MAX_STEPS --max-samples=$MAX_SAMPLES"
fi
echo "node heap: ${QUINT_NODE_HEAP_MB} MB"

# ---- functionality registry: "name|file|module" ----
ALL_FUNCS=(
    "f_hash|specs/f_hash/f_hash_properties.qnt|f_hash_properties"
    "f_sig|specs/f_sig/f_sig_properties.qnt|f_sig_properties"
    "f_dif|specs/f_dif/f_dif_properties.qnt|f_dif_properties"
    "f_inizk|specs/f_inizk/f_inizk_properties.qnt|f_inizk_properties"
    "f_atms|specs/f_atms/f_atms_properties.qnt|f_atms_properties"
    "f_pki|specs/f_pki/f_pki_properties.qnt|f_pki_properties"
    "g_clock|specs/g_clock/g_clock_properties.qnt|g_clock_properties"
    "g_ledger|specs/g_ledger/g_ledger_properties.qnt|g_ledger_properties"
    "p_bridge|specs/p_bridge/p_bridge_properties.qnt|p_bridge_properties"
)

# verify-eligible: excludes f_atms, g_ledger, p_bridge (too large for Apalache)
VERIFY_FUNCS=(
    "f_hash|specs/f_hash/f_hash_properties.qnt|f_hash_properties"
    "f_sig|specs/f_sig/f_sig_properties.qnt|f_sig_properties"
    "f_dif|specs/f_dif/f_dif_properties.qnt|f_dif_properties"
    "f_inizk|specs/f_inizk/f_inizk_properties.qnt|f_inizk_properties"
    "f_pki|specs/f_pki/f_pki_properties.qnt|f_pki_properties"
    "g_clock|specs/g_clock/g_clock_properties.qnt|g_clock_properties"
)

# ---- validate --module against the active registry ----
if [ -n "$MODULE_FILTER" ]; then
    if [ "$MODE" = "verify" ]; then
        ACTIVE_FUNCS=("${VERIFY_FUNCS[@]}")
    else
        ACTIVE_FUNCS=("${ALL_FUNCS[@]}")
    fi
    found=0
    for entry in "${ACTIVE_FUNCS[@]}"; do
        IFS="|" read -r n _ _ <<< "$entry"
        if [ "$n" = "$MODULE_FILTER" ]; then
            found=1
            break
        fi
    done
    if [ "$found" = 0 ]; then
        echo "Error: --module=$MODULE_FILTER not in $MODE registry" >&2
        echo "Available for $MODE:" >&2
        for entry in "${ACTIVE_FUNCS[@]}"; do
            IFS="|" read -r n _ _ <<< "$entry"
            echo "  $n" >&2
        done
        exit 1
    fi
    echo "module filter: $MODULE_FILTER"
fi

# ---- hardcoded must-fail invariant lists per functionality ----
# Returns the must-fail invariant list for a functionality, space-separated.
mustfail_list() {
    case "$1" in
        f_hash)   echo "MF1" ;;
        f_sig)    echo "MF1 MF2 MF3 MF4" ;;
        f_dif)    echo "MF1 MF2 MF3 MF4 MF5 MF6 MF7 MF8" ;; # MF9 MF10 MF11 MF12 MF13 MF14 MF15 MF16
        f_inizk)  echo "MF1 MF3" ;;
        f_atms)   echo "MF1 MF3 MF4 MF5 MF6 MF7 MF8" ;; # MF2
        f_pki)    echo "MF1 MF2 MF3 MF4" ;;
        g_clock)  echo "MF1 MF2" ;; # MF3
        g_ledger) echo "MF1 MF2" ;; # MF6 MF7 MF8 MF9
        p_bridge) echo "MF1 MF2 MF3 MF4 MF5 " ;; # MF6 MF7
        *)        echo "" ;;
    esac
}

PASS=0
FAIL=0
ERRORS=()

# ---- mode runners ----

run_simulate() {
    local name="$1"
    local file="$2"
    local module="$3"

    echo ""
    echo "========================================"
    echo "  $name [simulate]"
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
run_must_fail_one() {
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
        echo "  FAILED: expected violation not found for $invariant"
        FAIL=$((FAIL + 1))
        ERRORS+=("$name/$invariant")
    else
        PASS=$((PASS + 1))
    fi
}

run_must_fails() {
    local name="$1"
    local file="$2"
    local module="$3"
    local invariants
    invariants="$(mustfail_list "$name")"

    echo ""
    echo "========================================"
    echo "  $name [must-fail suite]"
    echo "========================================"

    if [ -z "$invariants" ]; then
        echo "  (no must-fail invariants registered)"
        return
    fi

    for inv in $invariants; do
        run_must_fail_one "$name" "$file" "$module" "$inv"
    done
}

run_verify() {
    local name="$1"
    local file="$2"
    local module="$3"

    echo ""
    echo "========================================"
    echo "  $name [verify]"
    echo "========================================"

    if quint verify \
        --invariant=AllProps \
        --backend=tlc \
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

# ---- dispatch ----

# Returns true if entry's name passes the module filter (or filter is empty).
filter_pass() {
    local entry="$1"
    [ -z "$MODULE_FILTER" ] && return 0
    IFS="|" read -r n _ _ <<< "$entry"
    [ "$n" = "$MODULE_FILTER" ]
}

case "$MODE" in
    simulate)
        for entry in "${ALL_FUNCS[@]}"; do
            filter_pass "$entry" || continue
            IFS="|" read -r name file module <<< "$entry"
            run_simulate "$name" "$SCRIPT_DIR/$file" "$module"
        done
        ;;
    must-fails)
        for entry in "${ALL_FUNCS[@]}"; do
            filter_pass "$entry" || continue
            IFS="|" read -r name file module <<< "$entry"
            run_must_fails "$name" "$SCRIPT_DIR/$file" "$module"
        done
        ;;
    verify)
        for entry in "${VERIFY_FUNCS[@]}"; do
            filter_pass "$entry" || continue
            IFS="|" read -r name file module <<< "$entry"
            run_verify "$name" "$SCRIPT_DIR/$file" "$module"
        done
        ;;
esac

echo ""
echo "========================================"
echo "  SUMMARY ($MODE)"
echo "========================================"
echo "  passed: $PASS"
echo "  failed: $FAIL"
if [ ${#ERRORS[@]} -gt 0 ]; then
    echo "  failures: ${ERRORS[*]}"
fi
echo "========================================"

exit $FAIL
