`include "internal/lustre_adder.v"

module lustre_add #(
    parameter N = 1
) (
    input  wire [N-1:0] lhs,
    input  wire [N-1:0] rhs,
    output wire [N-1:0] res
);

  internal_lustre_adder #(
      .N(N)
  ) adder (
      .lhs(lhs),
      .rhs(rhs),
      .carry_in(1'd0),
      .res(res),
      .flag_Z(),
      .flag_N(),
      .flag_C(),
      .flag_V()
  );
endmodule
