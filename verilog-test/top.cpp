#include <verilated.h>
#include <verilated_vcd_c.h>
#include "Vtop.h"

#define MAX_SIM_TIME 800

int main(int argc, char** argv, char** env) {
    Verilated::commandArgs(argc, argv);

    vluint64_t sim_time = 0;
    Vtop *dut = new Vtop();

    Verilated::traceEverOn(true);
    VerilatedVcdC *trace = new VerilatedVcdC();

    dut->trace(trace, 5);
    trace->open("waveform.vcd");


    while (sim_time < MAX_SIM_TIME) {
        if (14 <= sim_time && sim_time <= 15) {
            dut->reset_n = 0;
        } else {
            dut->reset_n = 1;
        }

        dut->clk ^= 1;
        dut->eval();
        trace->dump(sim_time);
        sim_time++;
    }

    trace->close();
    delete dut;

    return 0;
}
