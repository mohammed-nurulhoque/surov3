# SuroV3: A Tiny Configurable RISC-V Core (SpinalHDL)

SuroV3 is a compact, parameterizable RV32I core written in SpinalHDL. This project is a successor to the SystemVerilog-based [Suro-V](https://github.com/mohammed-nurulhoque/surov) core.

SuroV3 introduces a thin abstraction layer for the instruction definitions (Check out [InstImpl.scala](hw/spinal/surov3/InstImpl.scala)) (kind of like stageables in VexRiscv, but simpler) 



## Current Status

RV32I/RV32E with Zicntr (cycle/instret counters) and optional Zba extension. Runs Dhrystone benchmark.

**Parameters**:
- `issueWidth`: 1, 2, or 4
- `enableDualPort`: single/dual register file ports
- `syncRF`: synchronous or asynchronous RF reads
- `enableZba`: Zba address generation extension
- `regCount`: 32 (RV32I) or 16 (RV32E)

### Dhrystone Benchmark Results

| Config | CPI | DMIPS/MHz |
|--------|-----|-----------|
| w1_r32_sp_async | 2.477 | 0.542 |
| w1_r32_sp_sync | 2.619 | 0.512 |
| w1_r32_dp_async | 2.351 | 0.571 |
| w1_r32_dp_sync | 2.494 | 0.538 |
| w1_r32_sp_async+zba | 2.496 | 0.544 |
| w1_r32_dp_async+zba | 2.369 | 0.573 |
| w1_r32_dp_sync+zba | 2.513 | 0.540 |
| w1_r16_sp_async | 2.476 | 0.515 |
| w1_r16_sp_async+zba | 2.494 | 0.518 |
| w1_r16_dp_async+zba | 2.373 | 0.545 |
| w1_r16_sp_sync+zba | 2.646 | 0.488 |
| w1_r16_dp_sync+zba | 2.524 | 0.512 |
| w2_r32_sp_async+zba | 1.887 | 0.719 |
| w2_r32_sp_sync+zba | 1.977 | 0.687 |
| w2_r32_dp_async+zba | 1.770 | 0.767 |
| w2_r32_dp_sync+zba | 1.861 | 0.730 |
| w4_r32_dp_async+zba | 1.527 | 0.889 |
| w4_r32_dp_sync+zba | 1.599 | 0.849 |

Config naming: `w{issueWidth}_r{regCount}_{sp|dp}_{async|sync}[+zba]`

*Note: Compiled with `-O3 -fno-inline`. Better ILP extraction expected with unrolling and scheduling optimizations.*

## Building and Simulation

**Prerequisites**: SpinalHDL. The repo forks [SpinalHDL Base Project](https://github.com/SpinalHDL/SpinalTemplateSbt.git). Follow its instruction to setup the environment.

**Simulation**: `Surov3CoreSim` takes ELF executables as arguments (`hw/a.out` by default), converts them to binary, and runs the simulation. Generates a JSON log file (e.g., `hw/a.log`) with per-cycle snapshots.

**Trace Viewer**: `Surov3TraceTui` is an interactive TUI that consumes the log file `hw/a.log` to browse cycle-by-cycle pipeline state.

**System Calls**: The simulator handles `exit` (93) and `write` (64) syscalls. The syscall number is in `a5` (not `a7` as usual).


## 📝 TODO

* Full, automated execution and reporting for all riscv-isa-tests, coremark and embench.

* Enable RISCV Formal

* Support Zmmul

* Documentation of the pipeline stages and memory interface.