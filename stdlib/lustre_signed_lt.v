`include "internal/lustre_adder.v"

module lustre_signed_lt #(
    parameter N = 1
) (
    input wire [N-1:0] lhs,
    input wire [N-1:0] rhs,
    output wire res
);
  wire [N-1:0] new_rhs;
  not (new_rhs, rhs);

  wire flag_V;
  wire flag_N;

  internal_lustre_adder #(
      .N(N)
  ) adder (
      .lhs(lhs),
      .rhs(new_rhs),
      .carry_in(1'd1),
      .res(),
      .flag_Z(),
      .flag_N(flag_N),
      .flag_C(),
      .flag_V(flag_V)
  );

  xor (res, flag_N, flag_V);
endmodule
