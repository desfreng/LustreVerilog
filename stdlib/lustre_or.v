module lustre_or #(
    parameter N = 1
) (
    input  wire [N-1:0] lhs,
    input  wire [N-1:0] rhs,
    output wire [N-1:0] res
);
  or (res, lhs, rhs);
endmodule
