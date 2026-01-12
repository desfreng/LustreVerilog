#include "Vpopcount.h"
#include <bit>
#include <bitset>
#include <cstdint>
#include <iostream>
#include <random>
#include <verilated.h>
#include <verilated_vcd_c.h>

#ifndef WAVEFORM_FILE
#define WAVEFORM_FILE "waveform.vcd"
#endif

constexpr size_t SIM_TIME = 100;

int main(int argc, char **argv, char **env) {

  Verilated::commandArgs(argc, argv);

  vluint64_t sim_time = 0;
  Vpopcount dut = Vpopcount();

  Verilated::traceEverOn(true);
  VerilatedVcdC trace = VerilatedVcdC();

  dut.trace(&trace, 5);
  trace.open(WAVEFORM_FILE);

  dut.reset_n = 1; // Reset is Active Low

  std::random_device rd;
  std::uniform_int_distribution<std::uint16_t> dist(
      0, std::numeric_limits<uint16_t>::max());

  for (size_t i = 0; i < SIM_TIME; i++) {
    const uint16_t x = dist(rd);

    dut.clock = 1;
    dut.var_x = x;

    dut.eval();
    trace.dump(2 * i);

    dut.clock = 0;
    dut.eval();
    trace.dump(2 * i + 1);
  }

  trace.close();

  return 0;
}
