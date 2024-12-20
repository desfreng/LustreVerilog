`include "test.v"

module top (
    input clk,
    input reset_n
);

  reg init;

  initial begin
    init = 1'b1;
  end

  always @(posedge clk) begin
    if (!reset_n) init <= 1'b1;
    else init <= 1'b0;
  end

  test test_thing (
      .clk(clk),
      .init(init),
      .x()
  );
endmodule
