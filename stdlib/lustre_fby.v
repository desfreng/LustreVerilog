module lustre_fby #(
    parameter N = 1
) (
    input wire unsigned clock,
    input wire unsigned init,
    input wire unsigned [N-1:0] init_val,
    input wire unsigned [N-1:0] next_val,
    output reg unsigned [N-1:0] res
);
  reg unsigned [N-1:0] old_val;

  always @(posedge clock) begin
    old_val <= next_val;
    case (init)
      1'd1: res = init_val;
      1'd0: res = old_val;
    endcase
  end
endmodule
