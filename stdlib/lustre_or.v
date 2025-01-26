module lustre_or #(
    parameter N = 1
) (
    input  wire unsigned [N-1:0] lhs,
    input  wire unsigned [N-1:0] rhs,
    output wire unsigned [N-1:0] res
);
  or (res, lhs, rhs);
endmodule
