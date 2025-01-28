`include "internal/lustre_adder.v"

module lustre_sub #(
    parameter N = 1
) (
    input  wire [N-1:0] lhs,
    input  wire [N-1:0] rhs,
    output wire [N-1:0] res
);

  wire [N-1:0] new_rhs;
  not (new_rhs, rhs);

  internal_lustre_adder #(
      .N(N)
  ) adder (
      .lhs(lhs),
      .rhs(new_rhs),
      .carry_in(1'd1),
      .res(res),
      .flag_Z(),
      .flag_N(),
      .flag_C(),
      .flag_V()
  );
endmodule
