#include <cstddef>
#include <cstdint>
#include <iostream>
#include <random>
#include <verilated.h>
#include <verilated_vcd_c.h>

#include "Vif.h"

#ifndef WAVEFORM_FILE
#define WAVEFORM_FILE "waveform.vcd"
#endif

constexpr size_t SIM_TIME = 100;

int main(int argc, char **argv, char **env) {
  Verilated::commandArgs(argc, argv);

  vluint64_t sim_time = 0;
  Vif dut = Vif();

  std::random_device rd;
  std::uniform_int_distribution<uint8_t> dist(0, 255);
  std::uniform_int_distribution<uint8_t> bool_dist(0, 1);
  bool must_be_true = false;

  Verilated::traceEverOn(true);
  VerilatedVcdC trace = VerilatedVcdC();

  dut.trace(&trace, 5);
  trace.open(WAVEFORM_FILE);

  dut.reset_n = 1; // Reset is Active Low

  for (size_t i = 0; i < SIM_TIME; i++) {
    dut.clock = 1;
    dut.var_a = bool_dist(rd);
    dut.var_t = dist(rd);
    dut.var_f = dist(rd);

    dut.eval();
    trace.dump(2 * i);

    const uint8_t res = dut.var_a > 0 ? dut.var_t : dut.var_f;

    if (dut.var_res != res) {
      std::cerr << "Error at cycle " << i << " (expected: " << (int)(res)
                << " found: " << (int)(dut.var_res) << ")" << std::endl;
    }

    dut.clock = 0;
    dut.eval();
    trace.dump(2 * i + 1);
  }

  trace.close();

  return 0;
}
