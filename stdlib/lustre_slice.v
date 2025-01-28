module lustre_slice #(
    parameter N = 1,
    parameter I = 0,
    parameter J = 1
) (
    input  wire [  N-1:0] arg,
    output wire [J-I-1:0] res
);
  assign res = arg[J-1:I];
endmodule
