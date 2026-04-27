# Model Checking UC Ideal Functionalities in Quint

## Overview
This project implements formal verification of Universal Composability (UC) protocols using the Quint specification language. It provides a framework for modeling UC ideal functionalities and their properties.

## Quint Setup
For this project we used Quint 0.32.0 and Apalache 0.51.1. The helper script `run_quint.sh` requires Quint ≥ 0.32.0.

## Checking the specification

### Recommended: `run_quint.sh`

A helper script wraps the three common workflows. It toggles the `SIMULATION` flag in [specs/params.qnt](specs/params.qnt) and runs Quint over the appropriate set of functionalities.

```bash
./run_quint.sh {simulate|must-fails|verify} [--max-steps=N] [--max-samples=N] [--module=X]
```

- `--max-steps` / `--max-samples` are valid only for `simulate` and `must-fails` (verify uses Apalache and does not accept them).
- `--module=X` runs only the named functionality (e.g. `--module=f_hash`). For `verify`, X must be one of the verify-eligible modules (not `f_atms`, `g_ledger`, or `p_bridge`).

| Mode         | `SIMULATION` | Command                                | Default settings                       | Functionalities |
|--------------|--------------|----------------------------------------|----------------------------------------|------------------|
| `simulate`   | `true`       | `quint run --invariant=AllProps`       | `--max-steps=500 --max-samples=100`    | all 9 |
| `must-fails` | `true`       | `quint run --invariant=<MFn>` (per inv.) | `--max-steps=1000 --max-samples=100`   | all 9, each must-fail invariant individually |
| `verify`     | `false`      | `quint verify --invariant=AllProps`    | Apalache defaults                      | `f_hash`, `f_sig`, `f_dif`, `f_inizk`, `f_pki`, `g_clock` (excludes `f_atms`, `g_ledger`, `p_bridge` — too large for Apalache) |

The script:
- Checks that `quint` ≥ 0.32.0 is on `PATH` before doing anything
- Sets Node heap to 8 GB by default (overridable via `QUINT_NODE_HEAP_MB=16384 ./run_quint.sh ...`)
- Prints a summary at the end with pass/fail counts and the list of failures

```bash
./run_quint.sh simulate                              # quick sanity over all functionalities
./run_quint.sh simulate --max-steps=2000             # deeper traces, default sample count
./run_quint.sh must-fails --max-samples=500          # more samples per must-fail
./run_quint.sh simulate --module=f_hash              # run only one functionality
./run_quint.sh verify                                # exhaustive check on smaller specs
```

Below are the underlying Quint commands for one-off use.

### Simulation
In simulation mode the Quint will produce random traces and check the invariant after each "step".

```bash
quint run <spec_file> --invariant=<property> [--max-steps=n] [--max-samples=m] [--backend rust]
```

For example, to use simulator on all the specified properties of hash functions you can run the following code:
```bash
quint run specs/f_hash/f_hash_properties.qnt --invariant=AllProps --max-steps=500 --max-samples=10000 --backend rust
```
### Verification with TLC
We can instruct Quint to compile its specification to TLA+ and use TLC to check the property. In this
mode TLC will try to exhaustively cover the entire model to generate a counterexample.

```bash
quint verify <spec_file> [--invariant <property>] [--temporal <properties>] --backend=tlc
```
For example, to check with TLC all specified properties of F-PKI you can run the following code:
```bash
quint verify specs/f_pki/f_pki_properties.qnt --invariant AllProps --backend=tlc
```

However, you might want to reduce the model size in [params.qnt](specs/params.qnt) (set `SIMULATION = false`).

### Verification with Apalache
In this mode the Quint spec will be compiled and checked with the Apalache SMT solver.

```bash
quint verify <spec_file> [--invariant=<property>] 
```

For example, to verify all specified properties of Global clock with Apalache SMT solver you can run the following code:
```bash
quint verify specs/g_clock/g_clock_properties.qnt --invariant AllProps 
```

## Project Structure

### Specifications

Model size parameters (`UNIVERSE_SIZE`, `PARTY_SIZE`, `MAX_LOG_SIZE`, etc.) are in [params.qnt](specs/params.qnt). The `SIMULATION` flag there switches between two parameter profiles: a small profile suitable for `quint verify` (Apalache) and a larger profile suitable for `quint run` (sampling). `run_quint.sh` sets the flag automatically per mode.

#### Cryptographic Functionalities
- [**F-iNIZK**](specs/f_inizk/) - non-interactive ZK proofs for (i - interactive) relations
- [**F-ATMS**](specs/f_atms/) - Advanced threshold multi-signature scheme
- [**F-Hash**](specs/f_hash/) - Hash functionality with collision resistance
- [**F-PKI**](specs/f_pki/) - Public Key Infrastructure (registration, retrieval, verification)
- [**F-Diffuse**](specs/f_dif/) - Message diffuse protocol
- [**F-Sig**](specs/f_sig/) - digital signature primitive

#### Global Functionalities
- [**G-Clock**](specs/g_clock/) - Global clock for time synchronization
- [**G-Ledger**](specs/g_ledger/) - Global ledger with committee selection, APL, reads, and submits

#### Protocol
- [**P-Bridge**](specs/p_bridge/) - Cross-chain bridge protocol built from the above functionalities


## Key Features

### Modelling Approach
- **Choreographic modeling**: Explicit message passing between processes (parties, simulator, environment)
- **Local context transitions**: Each process defines reaction rules based on incoming messages
- **Custom effects**: logging for protocol control flow, transitions
- **Property checking**: Invariant verification via Apalache, TLC, and simulation














