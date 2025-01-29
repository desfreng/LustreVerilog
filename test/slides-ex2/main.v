module main (
    input  wire clock,
    input  wire reset,
    input  wire x,
    output reg  y
);
  wire new_y, reset_n, x_or_y;

  not (reset_n, reset);
  or (x_or_y, x, y);
  and (new_y, reset_n, x_or_y);

  always @(posedge clock) begin
    y <= new_y;
  end
endmodule
