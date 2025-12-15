#!/bin/bash

# Benchmark script for Surov3 RISC-V core
# Tests various configurations and tabulates results

# Don't use set -e as we handle errors manually

SUROV3_DIR="/config/surov3"
DHRYSTONE_DIR="/config/riscv-tests/benchmarks/dhrystone"
GCC="$HOME/riscv-gcc/bin/riscv64-unknown-elf-gcc"
JAVA_CMD="/usr/lib/jvm/java-25-openjdk-amd64/bin/java"
CLASSPATH="$SUROV3_DIR/.metals/.tmp/classpath_034807C4C089F1DD04CBF7E388AA0C4B.jar"

# Ensure we're in the right directory
cd "$SUROV3_DIR"

# Output directory
OUTDIR="/tmp/surov3_bench"
mkdir -p "$OUTDIR"

# Results files
RESULTS="$OUTDIR/results.txt"
CSV="$OUTDIR/results.csv"

echo "Configuration Results" > "$RESULTS"
echo "===================" >> "$RESULTS"
printf "%-40s %8s %8s %6s %10s %s\n" "Config" "Cycles" "Insns" "CPI" "DMIPS/MHz" "Status" >> "$RESULTS"
echo "----------------------------------------------------------------------------------------" >> "$RESULTS"

echo "config,issueWidth,regCount,dualPort,syncRF,zba,cycles,insns,cpi,dmips,status" > "$CSV"


# Function to compile dhrystone
# Returns 0 on success, 1 on failure
compile_dhrystone() {
    local regCount=$1
    local zba=$2
    local outfile=$3
    
    # Determine march and lib path based on config
    local march
    local libpath
    if [ "$regCount" -eq 16 ]; then
        if [ "$zba" = "true" ]; then
            march="rv32e_zicsr_zba"
            libpath="$HOME/r55/sw/lib/rv32e_zba/"
        else
            march="rv32e_zicsr"
            libpath="$HOME/r55/sw/lib/rv32e/"
        fi
    else
        if [ "$zba" = "true" ]; then
            march="rv32i_zicsr_zba"
            libpath="$HOME/r55/sw/lib/rv32i_zba/"
        else
            march="rv32i_zicsr"
            libpath="$HOME/r55/sw/lib/rv32i/"
        fi
    fi
    
    local abi
    if [ "$regCount" -eq 16 ]; then
        abi="ilp32e"
    else
        abi="ilp32"
    fi
    
    (
        cd /config/riscv-tests
        $GCC -DPREALLOCATE=1 -static -std=gnu99 -O3 \
            -o "$outfile" \
            "$DHRYSTONE_DIR/dhrystone.c" \
            "$DHRYSTONE_DIR/dhrystone_main.c" \
            -I benchmarks/common/ -I env setStats.c \
            -specs=nano.specs \
            -L "$libpath" -lmylib -lc_nano -lm -lgcc \
            -nostdlib \
            -march=$march -mabi=$abi \
            -Wno-implicit-int -Wno-implicit-function-declaration \
            -fno-inline \
            -T ~/riscv-tests/env/p/link.ld \
            2>/dev/null
    )
}

# Function to run simulation and extract metrics
# Returns: cycles insns cpi dmips status
run_sim() {
    local exe=$1
    local regCount=$2
    local dualPort=$3
    local issueWidth=$4
    local syncRF=$5
    local zba=$6
    local timeout_sec=120
    
    # Delete cached simulation to force regeneration with new config
    rm -rf "$SUROV3_DIR/simWorkspace/SimDUT"
    
    # Build config args
    local cfg_args="--regCount=$regCount --dualPort=$dualPort --issueWidth=$issueWidth --syncRF=$syncRF --zba=$zba"
    
    # Run simulation with timeout, capture output (must be in surov3 dir)
    local output
    local exit_code=0
    output=$(cd "$SUROV3_DIR" && timeout $timeout_sec "$JAVA_CMD" -classpath "$CLASSPATH" \
        -Duser.dir="$SUROV3_DIR" surov3.Surov3CoreSim $cfg_args "$exe" 2>&1) || exit_code=$?
    
    # Check for timeout or crash
    if [ $exit_code -eq 124 ]; then
        echo "N/A N/A N/A N/A TIMEOUT"
        return
    fi
    
    # Check exit status from simulation
    local sim_status
    sim_status=$(echo "$output" | grep -oP 'EXIT STATUS: \K\d+' 2>/dev/null) || sim_status=""
    
    if [ -z "$sim_status" ]; then
        echo "N/A N/A N/A N/A SIM_FAIL(no_exit)"
        return
    fi
    
    if [ "$sim_status" != "0" ]; then
        echo "N/A N/A N/A N/A SIM_FAIL($sim_status)"
        return
    fi
    
    # Extract metrics from output like:
    # Completed 60301 instructions in 123093 Cycles
    # Cycles_Per_Instruction: 1.977
    # DMIPS_Per_MHz: 0.687
    
    local cycles insns cpi dmips
    cycles=$(echo "$output" | grep -oP 'in \K\d+(?= Cycles)' 2>/dev/null | head -1)
    insns=$(echo "$output" | grep -oP 'Completed \K\d+(?= instructions)' 2>/dev/null | head -1)
    cpi=$(echo "$output" | grep -oP 'Cycles_Per_Instruction: \K[0-9.]+' 2>/dev/null | head -1)
    dmips=$(echo "$output" | grep -oP 'DMIPS_Per_MHz: \K[0-9.]+' 2>/dev/null | head -1)
    
    # Default to N/A if empty
    [ -z "$cycles" ] && cycles="N/A"
    [ -z "$insns" ] && insns="N/A"
    [ -z "$cpi" ] && cpi="N/A"
    [ -z "$dmips" ] && dmips="N/A"
    
    echo "$cycles $insns $cpi $dmips OK"
}

# Function to test one configuration
test_config() {
    local regCount=$1
    local dualPort=$2
    local issueWidth=$3
    local syncRF=$4
    local zba=$5
    
    # Short readable config name
    local dp_str; dp_str=$([ "$dualPort" = "true" ] && echo "dp" || echo "sp")
    local rf_str; rf_str=$([ "$syncRF" = "true" ] && echo "sync" || echo "async")
    local zba_str; zba_str=$([ "$zba" = "true" ] && echo "+zba" || echo "")
    local config_name="w${issueWidth}_r${regCount}_${dp_str}_${rf_str}${zba_str}"
    
    echo "========================================"
    echo "Testing: $config_name"
    echo "  Config: regCount=$regCount dualPort=$dualPort issueWidth=$issueWidth syncRF=$syncRF zba=$zba"
    
    # Compile dhrystone (reuse if same regCount+zba)
    local exe_key="r${regCount}_zba${zba}"
    local exe="$OUTDIR/dhrystone_${exe_key}.riscv"
    if [ ! -f "$exe" ]; then
        compile_dhrystone "$regCount" "$zba" "$exe"
    fi
    
    if [ ! -f "$exe" ]; then
        printf "%-40s %8s %8s %6s %10s %s\n" "$config_name" "N/A" "N/A" "N/A" "N/A" "COMPILE_FAIL" >> "$RESULTS"
        echo "$config_name,$issueWidth,$regCount,$dualPort,$syncRF,$zba,,,,,COMPILE_FAIL" >> "$CSV"
        echo "  Compile failed"
        return
    fi
    
    # Run simulation with config args
    local result
    result=$(run_sim "$exe" "$regCount" "$dualPort" "$issueWidth" "$syncRF" "$zba")
    read -r cycles insns cpi dmips status <<< "$result"
    
    printf "%-40s %8s %8s %6s %10s %s\n" "$config_name" "$cycles" "$insns" "$cpi" "$dmips" "$status" >> "$RESULTS"
    echo "$config_name,$issueWidth,$regCount,$dualPort,$syncRF,$zba,$cycles,$insns,$cpi,$dmips,$status" >> "$CSV"
    echo ""
    echo "  RESULT: CPI=$cpi DMIPS/MHz=$dmips ($status)"
    echo ""
}

echo "Starting Surov3 benchmark suite..."
echo "Output directory: $OUTDIR"
echo ""

START_TIME=$(date +%s)

# Define configurations to test
# Format: regCount dualPort issueWidth syncRF zba
#
# Sensible reductions:
# - issueWidth=1: test key combinations
# - issueWidth>1: always zba=true, regCount=32
# - issueWidth=4: always dualPort=true (otherwise too slow)

configs=(
    # issueWidth=1 baseline tests
    "32 false 1 false false"   # baseline: async RF, single port, no zba
    "32 false 1 true false"    # sync RF impact
    "32 true 1 false false"    # dual port impact
    "32 true 1 true false"     # sync RF + dual port
    "32 false 1 false true"    # zba impact (async)
    "32 true 1 false true"     # zba + dual port (async)
    "32 true 1 true true"      # all features w=1
    
    # RV32E (regCount=16) - minimal and most efficient test
    "16 false 1 false false"   # RV32E baseline
    "16 false 1 false true"   # RV32E baseline
    "16 true 1 false true"   # RV32E baseline
    "16 false 1 true true"   # RV32E baseline
    "16 true 1 true true"   # RV32E baseline
    
    # issueWidth=2 tests (always zba=true, regCount=32)
    "32 false 2 false true"    # w=2, single port, async
    "32 false 2 true true"     # w=2, single port, sync
    "32 true 2 false true"     # w=2, dual port, async
    "32 true 2 true true"      # w=2, dual port, sync
    
    # issueWidth=4 tests (always zba=true, regCount=32, dualPort=true)
    "32 true 4 false true"     # w=4, async
    "32 true 4 true true"      # w=4, sync
)

# Run all configurations
for cfg in "${configs[@]}"; do
    read -r regCount dualPort issueWidth syncRF zba <<< "$cfg"
    test_config "$regCount" "$dualPort" "$issueWidth" "$syncRF" "$zba"
done

# No need to restore config - using runtime args now

END_TIME=$(date +%s)
ELAPSED=$((END_TIME - START_TIME))

echo ""
echo "========================================"
echo "Results Summary:"
echo "========================================"
cat "$RESULTS"
echo ""
echo "Completed in ${ELAPSED}s"
echo "Results saved to:"
echo "  Text: $RESULTS"
echo "  CSV:  $CSV"
