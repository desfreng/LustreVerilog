#include <cstddef>
#include <cstdint>
#include <iostream>
#include <random>
#include <verilated.h>
#include <verilated_vcd_c.h>

#include "Vfby.h"

#ifndef WAVEFORM_FILE
#define WAVEFORM_FILE "waveform.vcd"
#endif

constexpr size_t SIM_TIME = 100;

int main(int argc, char **argv, char **env) {
  Verilated::commandArgs(argc, argv);

  vluint64_t sim_time = 0;
  Vfby dut = Vfby();

  std::random_device rd;
  std::uniform_int_distribution<size_t> dist(0, SIM_TIME / 2);
  std::uniform_int_distribution<uint8_t> bool_dist(0, 1);
  const size_t cut_off = dist(rd);
  bool must_be_true = false;

  Verilated::traceEverOn(true);
  VerilatedVcdC trace = VerilatedVcdC();

  dut.trace(&trace, 5);
  trace.open(WAVEFORM_FILE);

  dut.reset_n = 1; // Reset is Active Low

  for (size_t i = 0; i < SIM_TIME; i++) {
    dut.clock = 1;
    if (i > cut_off) {
      dut.var_a = bool_dist(rd);
    }

    must_be_true = dut.var_a > 0 || must_be_true;

    dut.eval();
    trace.dump(2 * i);

    if ((bool)(dut.var_x) != must_be_true) {
      std::cerr << "Error at cycle " << i
                << " (expected: " << (int)(must_be_true)
                << " found: " << (int)(dut.var_x) << ")" << std::endl;
    }

    dut.clock = 0;
    dut.eval();
    trace.dump(2 * i + 1);
  }

  trace.close();

  return 0;
}
