#include <cstddef>
#include <cstdint>
#include <iostream>
#include <verilated.h>
#include <verilated_vcd_c.h>

#include "Vneg.h"

#ifndef WAVEFORM_FILE
#define WAVEFORM_FILE "waveform.vcd"
#endif

#define CAST(x) (std::int8_t)(x)

#define ADD_TEST(arg) CAST(arg), CAST(-arg)

constexpr std::size_t TEST_SIZE = 2;

constexpr std::uint8_t DATA[] = {ADD_TEST(0),  ADD_TEST(1),   ADD_TEST(100),
                                 ADD_TEST(50), ADD_TEST(120), ADD_TEST(127),
                                 ADD_TEST(-1), ADD_TEST(-20), ADD_TEST(-127)};

constexpr std::size_t NB_TEST = sizeof(DATA) / TEST_SIZE;

int main(int argc, char **argv, char **env) {

  Verilated::commandArgs(argc, argv);

  vluint64_t sim_time = 0;
  Vneg dut = Vneg();

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

    if (CAST(dut.var_res) != CAST(DATA[TEST_SIZE * i + 1])) {
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
