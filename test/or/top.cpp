#include <cstdint>
#include <iostream>
#include <verilated.h>
#include <verilated_vcd_c.h>

#include "Vor.h"

#ifndef WAVEFORM_FILE
#define WAVEFORM_FILE "waveform.vcd"
#endif

#define CAST(x) (std::uint8_t)(x)

#define ADD_TEST(lhs, rhs) CAST(lhs), CAST(rhs), CAST(CAST(lhs) | CAST(rhs))

constexpr std::size_t TEST_SIZE = 3;

constexpr bool DATA[] = {ADD_TEST(false, false), ADD_TEST(false, true),
                         ADD_TEST(true, false), ADD_TEST(true, true)};

constexpr std::size_t NB_TEST = sizeof(DATA) / TEST_SIZE;

int main(int argc, char **argv, char **env) {

  Verilated::commandArgs(argc, argv);

  vluint64_t sim_time = 0;
  Vor dut = Vor();

  Verilated::traceEverOn(true);
  VerilatedVcdC trace = VerilatedVcdC();

  dut.trace(&trace, 5);
  trace.open(WAVEFORM_FILE);

  dut.reset_n = 1; // Reset is Active Low

  for (size_t i = 0; i < NB_TEST; i++) {
    dut.clock = 1;
    dut.var_x = DATA[TEST_SIZE * i];
    dut.var_y = DATA[TEST_SIZE * i + 1];

    dut.eval();
    trace.dump(2 * i);

    if (CAST(dut.var_res) != CAST(DATA[TEST_SIZE * i + 2])) {
      std::cerr << "Error at cycle " << i
                << " (expected: " << (int)(DATA[TEST_SIZE * i + 2])
                << " found: " << (int)(dut.var_res) << ")" << std::endl;
    }

    dut.clock = 0;
    dut.eval();
    trace.dump(2 * i + 1);
  }

  trace.close();

  return 0;
}
