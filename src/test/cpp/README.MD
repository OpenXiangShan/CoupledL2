# L2TSHRAlloc SV Testbench Makefile

This folder contains a simulator-agnostic Makefile for running the SystemVerilog testbench `L2TSHRAlloc_tb.sv` with either Verilator or VCS.

## Files

- `Makefile`: Build/run entry points for Verilator and VCS.
- `L2TSHRAlloc_tb.sv`: SystemVerilog testbench.

## Prerequisites

1. Generated RTL exists under `build/` (for example, `build/TestTop_L2TSHRAlloc.sv`, `build/L2TSHRAlloc.sv`, etc.).
2. One or both simulators installed:
   - Verilator (default flow)
   - Synopsys VCS

## Quick Start

From this directory (`src/test/cpp`):

```sh
make build SIM=verilator
make run   SIM=verilator
```

With VCS:

```sh
make build SIM=vcs
make run   SIM=vcs
```

## Common Targets

- `make all`: same as `make build`
- `make build SIM=<verilator|vcs>`: compile the testbench
- `make run SIM=<verilator|vcs>`: compile and run
- `make clean`: remove generated simulator artifacts
- `make help`: print usage examples

## Important Variables

- `SIM`: simulator selector (`verilator` default, or `vcs`)
- `RTL_SRCS`: RTL source list used by both simulators
  - Default: all `build/*.v` and `build/*.sv`
- `TB`: testbench top module (default `L2TSHRAlloc_tb`)

### Verilator-specific

- `VERILATOR`: verilator binary name/path
- `VERILATOR_TIMING`: timing mode (`--timing` default)
- `VERILATOR_FLAGS`: additional Verilator flags
- `VERILATOR_CFLAGS`: C++ compile flags (default `-std=gnu++20`)
- `VERILATOR_MAKEFLAGS`: forwarded to Verilator-generated make
  - Useful to choose compiler, e.g. `CXX=clang++`
- `VERILATOR_OUT`: output build directory
- `VERILATOR_BIN`: generated executable name

### VCS-specific

- `VCS`: vcs binary name/path
- `VCS_FLAGS`: compile flags passed to VCS
- `VCS_EXE`: generated executable name

## Examples

Use a custom RTL source list:

```sh
make run SIM=verilator RTL_SRCS='../../../build/*.sv'
```

Force clang++ for Verilator-generated C++ build:

```sh
make run SIM=verilator VERILATOR_MAKEFLAGS='CXX=clang++'
```

Override timing mode:

```sh
make run SIM=verilator VERILATOR_TIMING=--timing
```

## Troubleshooting

1. Build cannot find generated RTL files:
   - Ensure RTL has been generated into `build/` before running this Makefile.
2. Verilator C++ build errors related to standard library/toolchain:
   - Try `VERILATOR_MAKEFLAGS='CXX=clang++'`.
   - Keep `VERILATOR_CFLAGS` at a C++20-capable setting.
3. Unsupported simulator value:
   - Use `SIM=verilator` or `SIM=vcs`.
