module lustre_not #(
    parameter N = 1
) (
    input  wire [N-1:0] arg,
    output wire [N-1:0] res
);
  not (res, arg);
endmodule
