`include "internal/lustre_adder.v"

module lustre_neg #(
    parameter N = 1
) (
    input  wire [N-1:0] arg,
    output wire [N-1:0] res
);
  wire [N-1:0] inverted_arg;

  not (inverted_arg, arg);

  internal_lustre_adder #(
      .N(N)
  ) adder (
      .lhs(inverted_arg),
      .rhs({N{1'b0}}),
      .carry_in(1'd1),
      .res(res),
      .flag_Z(),
      .flag_N(),
      .flag_C(),
      .flag_V()
  );
endmodule
