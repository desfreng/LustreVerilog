#include <cstddef>
#include <random>
#include <verilated.h>
#include <verilated_vcd_c.h>

#include "Vslides_ex3.h"

#ifndef WAVEFORM_FILE
#define WAVEFORM_FILE "waveform.vcd"
#endif

constexpr size_t SIM_TIME = 100;

int main(int argc, char **argv, char **env) {
  Verilated::commandArgs(argc, argv);

  vluint64_t sim_time = 0;
  Vslides_ex3 dut = Vslides_ex3();

  Verilated::traceEverOn(true);
  VerilatedVcdC trace = VerilatedVcdC();

  dut.trace(&trace, 5);
  trace.open(WAVEFORM_FILE);

  dut.reset_n = 1; // Reset is Active Low

  std::random_device rd;
  std::uniform_int_distribution<size_t> dist(0, SIM_TIME / 3);
  std::uniform_int_distribution<uint8_t> bool_dist(0, 1);
  const size_t cut_off = dist(rd);

  for (size_t i = 0; i < SIM_TIME; i++) {
    dut.clock = 1;

    if (i > cut_off) {
      dut.var_x = bool_dist(rd);
    }

    const bool do_reset = (2 * i > SIM_TIME) && (i % 10 == 0);
    dut.var_reset = do_reset;

    dut.eval();
    trace.dump(2 * i);

    dut.clock = 0;

    dut.eval();
    trace.dump(2 * i + 1);
  }

  trace.close();

  return 0;
}
