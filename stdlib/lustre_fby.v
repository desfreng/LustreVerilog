module lustre_fby #(
    parameter N = 1
) (
    input wire clock,
    input wire init,
    input wire [N-1:0] init_val,
    input wire [N-1:0] next_val,
    output reg [N-1:0] res
);
  reg [N-1:0] old_val;

  always @(posedge clock) begin
    old_val <= next_val;
  end

  always @(init, init_val, old_val) begin
    case (init)
      1'd1: res = init_val;
      1'd0: res = old_val;
    endcase
  end
endmodule
