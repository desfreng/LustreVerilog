# LustreVerilog

A simple compiler for translating template of Lustre code to Verilog. 

## Overview

LustreVerilog aims to provide a straightforward way to convert template of
Lustre specifications into synthesizable Verilog code, 
facilitating hardware implementation and simulation.

## Requirements

### System Requirements

*   **Haskell Toolchain:** Cabal and GHC (version 9.12.2)
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
│   ├── Solver/              # ILP solver for constraints checking
│   ├── Typing/              # Type checking logic
│   └── Verilog/             # Verilog-related utilities
├── stdlib/              # Standard library of Lustre nodes
│   └── internal/            # Internal Verilog implementations
├── test/                # Example Lustre files and testbenches
│   ├── add/                 # Example: Adder design
│   │   ├── main.lus             # Lustre specification
│   │   └── top.cpp              # C++ testbench wrapper
│   └── ...                  # Other test examples (and, concat, eq, etc.)
├── CMakeLists.txt       # CMake build configuration file
├── LICENSE              # License file
├── LustreVerilog.cabal  # Cabal package description file
├── README.md            # This file
└── slides/              # Slides for the project presentation
```

## Input Language Format

The compiler accepts a synchronous dataflow language inspired by **Lustre**. 
The program consists of a list of `node` definitions.

### 1. Node Structure

A node is defined by its interface (inputs/outputs), optional parametric size
constraints, local variables, and a body of equations.

```lustre
node nodeName(input1: type; input2: type) returns (output1: type)
  size N
  where N > 0;  // Optional parametric size declaration
  var
    local_var: type;      // Local variables
  let
    local_var = expression;
    output1 = local_var + input1;
  tel
```

### 2. Data Types

The language supports standard primitives and explicit bit-sized integers:

| Type | Syntax | Description |
| --- | --- | --- |
| **Boolean** | `bool` | True/False logic. |
| **Integer** | `int` | Compile time sized integer (by default 32 bits) |
| **Unsigned** | `u<N>` | Unsigned integer of width `N` (e.g., `u<8>`). |
| **Signed** | `i<N>` | Signed integer of width `N` (e.g., `i<16>`). |
| **Raw** | `r<N>` | Raw bit-vector of width `N`. |

### 3. Operators & Expressions

* **Arithmetic:** `+`, `-`, `*`, `/`
* **Logic:** `and`, `or`, `not`
* **Comparison:** `==`, `<>`, `>=`, `<=`, `>`, `<`
* **Temporal:**
* `fby`: "Followed by" operator (e.g., `init fby next`).


* **Bitwise/Arrays:**
* `++`: Concatenation.
* `x[i]`: Indexing.
* `x[i:j]`: Slicing/Range selection from `i` inclusive to `j` exclusive.


* **Type Casting:** `raw`, `signed`, `unsigned` (used as unary prefixes).
* **Control Flow:** `if condition then expr else expr`.

### 4. Advanced Features

#### Parametric Sizes

You can define nodes that accept generic size parameters.
Constraints on these sizes can be enforced using the `where` clause.
Size expression are affine expression of the declared size variables
(ex. `N + 3*M + 1`).

```lustre
// A node handling parametric bit-widths
node parametricAdd(x: u<N>; y: u<N>) returns (z: u<N>)
  size N
  where
    N > 0;
    N <= 64
  let
    z = x + y;
  tel
```

#### Structural Branching

The node body can be split based on size criteria using `when` and `otherwise`:

```lustre
node conditionalBody(x: u<N>) returns (y: u<N>)
  size n
  when n == 1
    let
      y = x;
    tel
  otherwise
    let
      y = x + 1
    tel
```

See `test/` for multiple examples and `src/Parsing/Grammar.y` for
a yacc like grammar definition.

## License
This project is licensed under the GPL3 license. See the `LICENSE` file for details.
