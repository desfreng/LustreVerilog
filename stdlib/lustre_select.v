module lustre_select #(
    parameter N = 1,
    parameter I = 0
) (
    input wire [N-1:0] arg,
    output wire res
);
  assign res = arg[I];
endmodule
