#include <cstdint>
#include <iostream>
#include <random>
#include <verilated.h>
#include <verilated_vcd_c.h>

#include "Vucmp.h"

#ifndef WAVEFORM_FILE
#define WAVEFORM_FILE "waveform.vcd"
#endif

constexpr size_t SIM_TIME = 100;

int main(int argc, char **argv, char **env) {

  Verilated::commandArgs(argc, argv);

  vluint64_t sim_time = 0;
  Vucmp dut = Vucmp();

  Verilated::traceEverOn(true);
  VerilatedVcdC trace = VerilatedVcdC();

  dut.trace(&trace, 5);
  trace.open(WAVEFORM_FILE);

  dut.reset_n = 1; // Reset is Active Low

  std::random_device rd;
  std::uniform_int_distribution<uint8_t> dist(0, 255);
  std::uniform_int_distribution<int> bool_dist(0, 2);

  for (size_t i = 0; i < SIM_TIME; i++) {
    const uint8_t x = dist(rd);
    const uint8_t y = bool_dist(rd) == 0 ? x : dist(rd);

    dut.clock = 1;
    dut.var_x = x;
    dut.var_y = y;

    dut.eval();
    trace.dump(2 * i);

    if (dut.var_eq != (uint8_t)(x == y) || dut.var_neq != (uint8_t)(x != y) ||
        dut.var_lt != (uint8_t)(x < y) || dut.var_le != (uint8_t)(x <= y) ||
        dut.var_ge != (uint8_t)(x >= y) || dut.var_gt != (uint8_t)(x > y)) {

      std::cerr << "Error at cycle " << i << " (expected: ";
      std::cerr << (int)(x == y) << ", " << (int)(x != y) << ", "
                << (int)(x > y) << ", " << (int)(x <= y) << ", "
                << (int)(x >= y) << ", " << (int)(x > y);
      std::cerr << ", found: ";
      std::cerr << (int)(dut.var_eq) << ", " << (int)(dut.var_neq) << ", "
                << (int)(dut.var_lt) << ", " << (int)(dut.var_le) << ", "
                << (int)(dut.var_ge) << ", " << (int)(dut.var_gt) << ")"
                << std::endl;
    }

    dut.clock = 0;
    dut.eval();
    trace.dump(2 * i + 1);
  }

  trace.close();

  return 0;
}
