module lustre_concat #(
    parameter M = 1,
    parameter N = 1
) (
    input  wire [  M-1:0] lhs,
    input  wire [  N-1:0] rhs,
    output wire [M+N-1:0] res
);
  assign res = {lhs, rhs};
endmodule
