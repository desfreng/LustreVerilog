# LustreVerilog

A simple and flexible compiler for translating Lustre code to Verilog. This project was developed as part of the **Models and languages for programming reactive systems** course taught by **Timothy Bourke** and **Marc Pouzet** at the ENS during the 2024-2025 academic year.

## Overview

LustreVerilog aims to provide a straightforward way to convert Lustre specifications into synthesizable Verilog code, facilitating hardware implementation and simulation.

## Requirements

### System Requirements

*   **Haskell Toolchain:** Cabal and GHC (Glasgow Haskell Compiler)
*   **Build System:** CMake
*   **Verilog Simulator:** Verilator
*   **Waveform Viewer:** GTKWave

### Installation

1.  **Build the Compiler:** Navigate to the project directory in your terminal and run `cabal build`.

2.  **Run the Compiler:** You can execute the compiler directly using `cabal run LustreVerilog -- <options>` (see the Command-Line Interface section for details).

3.  **Install the Compiler (Optional):** For easier access, install the compiler into your Cabal environment with `cabal install`.  This will make the `LustreVerilog` executable available in your system's PATH.

## Command-Line Interface

```
LustreVerilog [options] <input_file.lus>

Options:
  -o, --output <output_file.v>  Specify the output Verilog file (default: out.v).
  -d, --debug                   Enable debug mode.
  --int-size <INT-SIZE>         Bit size of a Lustre Integer (default: 32).
  --reset-low                   Reset is active low.
  --reset-high                  Reset is active high (default: Active Low).
  --stdlib-dir                  Print the standard library directory.
  --parse-only                  Stop after the parsing stage.
  --type-only                   Stop after the typing stage.
  --export-compiled             Stop before exporting to Verilog.
  --main-node <NODE>            Main node of the design (required for compilation).
  -h, --help                    Display this help message.

```

*Example Usage:*

```bash
# Compile my_design.lus to my_design.v, using node 'main' as the main node:
LustreVerilog -o my_design.v --main-node main my_design.lus

# Print the standard library directory:
LustreVerilog --stdlib-dir

# Parse only and print the AST:
LustreVerilog --parse-only -d my_design.lus

# Type check only and print the typed AST:
LustreVerilog --type-only -d my_design.lus

# Compile and stop before Verilog export:
LustreVerilog --transform-only my_design.lus

# Compile with active high reset:
LustreVerilog --reset-high -o my_design.v --main-node main my_design.lus

# Compile from stdin:
cat my_design.lus | LustreVerilog -o my_design.v --main-node main -
```

## Running the Examples

The project includes examples in the `test/` directory. To run these examples:

1.  **Generate Build Files:** Use CMake to generate the necessary build files:

    ```bash
    cmake -S. -Bbuild
    cd build
    ```

2.  **Build and Run Examples:**  Within the `build` directory, CMake creates targets for each subdirectory in `test/`.  For a directory named `add` (as an example):

    *   `make add`: Builds the executable that simulates the design specified in the `add` directory using a C++ wrapper.
    *   `make add-waveform`: Executes the `add` executable and generates a waveform file.
    *   `make add-gtkwave`: Opens GTKWave and displays the generated waveform.

    You can also use `make -j<number_of_cores>` to speed up the compilation process.  For example, `make -j4 add` will use 4 cores.

3.  **Clean Build Files:** To remove the generated build files, run `make clean` in the `build` directory.

## Project Structure

```
LustreVerilog/
├── app/                 # Main application entry point (Main.hs)
├── src/                 # Source code of the compiler
│   ├── Commons/             # Common data structures and utilities
│   ├── Compiling/           # Compilation logic
│   ├── LustreVerilog.hs     # Main compiler module
│   ├── Parsing/             # Lustre parsing logic
│   ├── Typing/              # Type checking logic
│   └── Verilog/             # Verilog-related utilities
├── stdlib/              # Standard library of Lustre nodes
│   └── internal/            # Internal Verilog implementations
├── test/                # Example Lustre files and testbenches
│   ├── add/                 # Example: Adder design
│   │   ├── main.lus             # Lustre specification
│   │   └── top.c                # C++ testbench wrapper
│   └── ...                  # Other test examples (and, concat, eq, etc.)
├── CMakeLists.txt       # CMake build configuration file
├── LICENSE              # License file
├── LustreVerilog.cabal  # Cabal package description file
├── README.md            # This file
└── slides/              # Slides for the project presentation
```

## License
This project is licensed under the GPL3 license. See the `LICENSE` file for details.