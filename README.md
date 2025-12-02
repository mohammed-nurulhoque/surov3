# SuroV3: A Tiny Configurable RISC-V Core (SpinalHDL)

SuroV3 is a compact, parameterizable RV32I core written in SpinalHDL. This project is a successor to the SystemVerilog-based [Suro-V](https://github.com/mohammed-nurulhoque/surov) core.

SuroV3 introduces a thin abstraction layer for the instruction definitions (Check out [InstImpl.scala](https://github.com/mohammed-nurulhoque/surov3/blob/master/hw/spinal/surov3/InstImpl.scala)) (kind of like stageables in VexRiscv, but simpler) 



## Current Status

### commit 24fee6e5

**ISA Compliance**: Passes a large portion of the riscv-isa-tests. (didn't fail any. I just haven't run them in bulk yet).

**Parameters**: Currently supports the enableDualPort parameter for the Register File.

### commit b4f480e2

Multi-issue kinda working without load/store
    
passed riscv-test addi at one point with 4-wide pipeline, at nearly 1 IPC

## Building and Simulation

**Prerequisites**: SpinalHDL. The repo forks [SpinalHDL Base Project](https://github.com/SpinalHDL/SpinalTemplateSbt.git). Follow its instruction to setup the environment

**Simulation Requirements**: The core's testbench/simulation object expects a binary at `hw/a.bin` which has entry at 0x1000.


## 📝 TODO

* Full, automated execution and reporting for all riscv-isa-tests, dhrystone and coremark.

* Enable RISCV Formal

* Documentation of the pipeline stages and memory interface.

* Addition of more parameterization options.