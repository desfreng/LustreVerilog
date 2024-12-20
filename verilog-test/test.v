module test (
    input clk,
    input init,
    output wire [7:0] x
);

  wire [7:0] cpt;
  wire init_x;

  countByOne cpt_count (
      .clk(clk),
      .init(init),
      .x(cpt)
  );

  assign init_x = init | cpt == 8'd20;

  countByOne x_count (
      .clk(clk),
      .init(init_x),
      .x(x)
  );

endmodule


/*

node countByOne () returns (x: i8);
let
  x = 0 fby x + 1;
tel

*/

module countByOne (
    input clk,
    input init,
    output [7:0] x
);
  wire [7:0] old_x;
  wire [7:0] add_old_x;

  pre #(
      .N(7)
  ) pre_x (
      .clk(clk),
      .val(x),
      .old_val(old_x)
  );

  mergeBool #(
      .N(7)
  ) x_init (
      .boolClock(init),
      .trueVal(0),
      .falseVal(add_old_x),
      .res(x)
  );

  add #(
      .N(7)
  ) add_x (
      .lhs(old_x),
      .rhs(1),
      .res(add_old_x)
  );
endmodule


module mergeBool #(
    parameter N = 0
) (
    input wire boolClock,
    input wire [N:0] trueVal,
    input wire [N:0] falseVal,
    output reg [N:0] res
);

  always @(boolClock, trueVal, falseVal) begin
    case (boolClock)
      1'b0: res = falseVal;
      1'b1: res = trueVal;
    endcase
  end

endmodule

module pre #(
    parameter N = 0
) (
    input wire clk,
    input wire [N:0] val,
    output reg [N:0] old_val
);

  always @(posedge clk) begin
    old_val <= val;
  end
endmodule


module add #(
    parameter N = 0
) (
    input  wire [N:0] lhs,
    input  wire [N:0] rhs,
    output reg  [N:0] res
);

  always @(lhs, rhs) begin
    res = lhs + rhs;
  end
endmodule
