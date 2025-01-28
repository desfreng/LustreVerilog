#include <cstddef>
#include <verilated.h>
#include <verilated_vcd_c.h>

#include "Vmain.h"

#ifndef WAVEFORM_FILE
#define WAVEFORM_FILE "waveform.vcd"
#endif

constexpr size_t SIM_TIME = 100;

int main(int argc, char **argv, char **env) {
  Verilated::commandArgs(argc, argv);

  vluint64_t sim_time = 0;
  Vmain dut = Vmain();

  Verilated::traceEverOn(true);
  VerilatedVcdC trace = VerilatedVcdC();

  dut.trace(&trace, 5);
  trace.open(WAVEFORM_FILE);

  dut.reset_n = 1; // Reset is Active Low

  for (size_t i = 0; i < SIM_TIME; i++) {
    dut.clock ^= 1;
    dut.eval();
    trace.dump(i);
  }

  trace.close();

  return 0;
}
