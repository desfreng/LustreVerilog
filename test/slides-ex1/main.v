module main (
    input wire clock,
    input wire reset,
    input wire x,
    output reg [7:0] y
);
  always @(posedge clock) begin
    if (reset) y <= 8'd0;
    else if (x || y > 0) y <= y + 8'd1;
  end
endmodule
