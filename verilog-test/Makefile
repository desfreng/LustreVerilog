TOP=top
TOP_MODULE=main
VERILOG_ARGS=--Mdir "obj_dir" --trace --x-assign unique --x-initial unique --timing

# ALL: build

# sim-%: $*/wafeform
# %-verilate: .stamp.verilate
build-%: V%
	@echo $@

# %-wave: waveform.vcd
# gtkwave waveform.vcd
V%:
	@echo $@


# waveform.vcd: ./obj_dir/V$(MODULE)
# 	@./obj_dir/V$(MODULE) +verilator+rand+reset+2

# ./obj_dir/V%: .stamp.verilate.%
# 	make -C obj_dir -f V$*.mk V$*

# .stamp.verilate.%: %.v $(TOP).cpp
# 	verilator $(VERILOG_ARGS) --prefix $* --top-module $(TOP_MODULE) -cc $*.v $(TOP).cpp
# 	@touch .stamp.verilate.$*

# clean:
# 	rm -rf .stamp.*;
# 	rm -rf ./obj_dir
# 	rm -rf waveform.vcd

# .PHONY: sim verilate build wave lint clean
