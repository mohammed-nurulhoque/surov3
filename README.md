# SuroV3: A Tiny Configurable RISC-V Core (SpinalHDL)

SuroV3 is a compact, parameterizable RV32I core written in SpinalHDL. This project is a successor to the SystemVerilog-based [Suro-V](https://github.com/mohammed-nurulhoque/surov) core.

SuroV3 introduces a thin abstraction layer for the instruction definitions (Check out [InstImpl.scala](hw/spinal/surov3/InstImpl.scala)) (kind of like stageables in VexRiscv, but simpler) 



## Current Status

rv32i but without load/store. Load/store were working before I implemnted superscalar (commit 24fee6e5c6). Didn't fix it yet.

**Parameters**: Currently supports enableDualPort and issueWidth

matmul.s is a kernel that computes 2x2 x 2x2 distance product. Tested 1, 2 & 4 issueWidth and single/dual port


| Width | Dual-port | kGE | Cycles |
| ---- | --- |--- | --- |
| 1 | No | 5.5 | 96 |
| 1 | Yes | 6.8 | 81
| 2 | No | 10  | 70 |
| 2 | Yes | 11 |  59 |
| 4 | No | 20  | 62 |

## Building and Simulation

**Prerequisites**: SpinalHDL. The repo forks [SpinalHDL Base Project](https://github.com/SpinalHDL/SpinalTemplateSbt.git). Follow its instruction to setup the environment

**Simulation Requirements**: The core's testbench/simulation object expects a binary at `hw/a.bin` which has entry at 0x1000.

**System Calls** The sim script simulates exit and write syscalls with codes 93 and 64. The syscall number is held at *a5* (Not a7 as is usual)


## 📝 TODO

* Full, automated execution and reporting for all riscv-isa-tests, dhrystone coremark and embench.

* Enable RISCV Formal

* Support Zmmul

* Documentation of the pipeline stages and memory interface.

* Addition of more parameterization options.

* Support more extensions