# SuroV3: A Tiny Configurable RISC-V Core (SpinalHDL)

SuroV3 is a compact, parameterizable RV32I core written in SpinalHDL. This project is a successor to the SystemVerilog-based [Suro-V](https://github.com/mohammed-nurulhoque/surov) core.

SuroV3 introduces a thin abstraction layer for the instruction definitions (Check out [InstImpl.scala](hw/spinal/surov3/InstImpl.scala)) (kind of like stageables in VexRiscv, but simpler) 



## Current Status

RV32I with Zicntr (cycle/instret counters). Runs Dhrystone benchmark.

**Parameters**: `issueWidth` (1, 2, 4) and `enableDualPort` (single/dual register file ports)

### Dhrystone Benchmark Results

| Width | RF Ports | CPI | DMIPS/MHz |
|-------|----------|-----|-----------|
| 1 | Single | 2.485 | 0.528 |
| 1 | Dual | 2.344 | 0.560 |
| 2 | Single | 1.933 | 0.678 |
| 2 | Dual | 1.813 | 0.723 |
| 4 | Single | 1.655 | 0.793 |
| 4 | Dual | 1.540 | 0.852 |

*Note: Compiled with standard `-O3 -fno-inline` without tuning for issue width. Better ILP extraction expected with unrolling and scheduling optimizations.*

## Building and Simulation

**Prerequisites**: SpinalHDL. The repo forks [SpinalHDL Base Project](https://github.com/SpinalHDL/SpinalTemplateSbt.git). Follow its instruction to setup the environment.

**Simulation**: `Surov3CoreSim` takes ELF executables as arguments (`hw/a.out` by default), converts them to binary, and runs the simulation. Generates a JSON log file (e.g., `hw/a.log`) with per-cycle snapshots.

**Trace Viewer**: `Surov3TraceTui` is an interactive TUI that consumes the log file `hw/a.log` to browse cycle-by-cycle pipeline state.

**System Calls**: The simulator handles `exit` (93) and `write` (64) syscalls. The syscall number is in `a5` (not `a7` as usual).


## 📝 TODO

* Full, automated execution and reporting for all riscv-isa-tests, dhrystone coremark and embench.

* Enable RISCV Formal

* Support Zmmul

* Documentation of the pipeline stages and memory interface.

* Addition of more parameterization options.

* Support more extensions