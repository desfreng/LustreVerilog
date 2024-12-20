MODULE=test
TOP=top

ALL: build

sim: waveform.vcd
verilate: .stamp.verilate
build: obj_dir/V$(MODULE)

wave: waveform.vcd
	gtkwave waveform.vcd

waveform.vcd: ./obj_dir/V$(TOP)
	@./obj_dir/V$(TOP) +verilator+rand+reset+2

./obj_dir/V$(TOP): .stamp.verilate
	make -C obj_dir -f V$(TOP).mk V$(TOP)

.stamp.verilate: $(TOP).v $(MODULE).v $(TOP).cpp
	verilator --trace --x-assign unique --x-initial unique --top-module $(TOP) -cc $(TOP).v --exe $(TOP).cpp --timing
	@touch .stamp.verilate

clean:
	rm -rf .stamp.*;
	rm -rf ./obj_dir
	rm -rf waveform.vcd

.PHONY: sim verilate build wave lint clean
