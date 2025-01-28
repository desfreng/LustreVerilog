`include "lustre_bit_adder.v"

module internal_lustre_adder #(
    parameter N = 1
) (
    input  [N-1:0] lhs,
    input  [N-1:0] rhs,
    input          carry_in,
    output [N-1:0] res,
    output         flag_Z,
    output         flag_N,
    output         flag_C,
    output         flag_V
);
  wire [N:0] carry;
  assign carry[0] = carry_in;

  genvar i;
  generate
    for (i = 0; i < N; i = i + 1) begin : full_adder_gen
      internal_lustre_bit_adder single_bit_adder (
          .a(lhs[i]),
          .b(rhs[i]),
          .carry_in(carry[i]),
          .res(res[i]),
          .carry_out(carry[i+1])
      );
    end
  endgenerate

  assign flag_Z = ~(|res);
  assign flag_N = res[N-1];
  assign flag_C = carry[N];
  xor (flag_V, carry[N], carry[N-1]);
endmodule
