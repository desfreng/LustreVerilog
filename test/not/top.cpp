#include <cstddef>
#include <cstdint>
#include <iostream>
#include <verilated.h>
#include <verilated_vcd_c.h>

#include "Vnot.h"

#ifndef WAVEFORM_FILE
#define WAVEFORM_FILE "waveform.vcd"
#endif

#define ADD_TEST(arg) arg, !arg

constexpr std::size_t TEST_SIZE = 2;

constexpr bool DATA[] = {ADD_TEST(true), ADD_TEST(false)};

constexpr std::size_t NB_TEST = sizeof(DATA) / TEST_SIZE;

int main(int argc, char **argv, char **env) {

  Verilated::commandArgs(argc, argv);

  vluint64_t sim_time = 0;
  Vnot dut = Vnot();

  Verilated::traceEverOn(true);
  VerilatedVcdC trace = VerilatedVcdC();

  dut.trace(&trace, 5);
  trace.open(WAVEFORM_FILE);

  dut.reset_n = 1; // Reset is Active Low

  for (size_t i = 0; i < NB_TEST; i++) {
    dut.clock = 1;
    dut.var_x = DATA[TEST_SIZE * i];

    dut.eval();
    trace.dump(2 * i);

    if ((bool)(dut.var_res) != DATA[TEST_SIZE * i + 1]) {
      std::cerr << "Error at cycle " << i
                << " (expected: " << DATA[TEST_SIZE * i + 2]
                << " found: " << (bool)(dut.var_res) << ")" << std::endl;
    }

    dut.clock = 0;
    dut.eval();
    trace.dump(2 * i + 1);
  }

  trace.close();

  return 0;
}
