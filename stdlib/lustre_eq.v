module lustre_eq #(
    parameter N = 1
) (
    input wire [N-1:0] lhs,
    input wire [N-1:0] rhs,
    output wire res
);
  wire [N-1:0] xor_res;

  xor (xor_res, lhs, rhs);
  assign res = ~(|xor_res);
endmodule
