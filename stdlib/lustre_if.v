module lustre_if #(
    parameter N = 1
) (
    input wire cond,
    input wire [N-1:0] true_branch,
    input wire [N-1:0] false_branch,
    output reg [N-1:0] res
);
  always @(cond, true_branch, false_branch) begin
    case (cond)
      1'd1: res = true_branch;
      1'd0: res = false_branch;
    endcase
  end
endmodule
