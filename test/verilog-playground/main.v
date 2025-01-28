`include <lustre_not.v>
`include <lustre_fby.v>

module main (
    input  wire unsigned clock,
    input  wire unsigned reset_n,
    output wire unsigned var_z
);
  reg unsigned init;

  initial begin
    init = 1'd1;
  end

  always @(posedge clock) begin
    if (!reset_n) init <= 1'd1;
    else init <= 1'd0;
  end

  node_main call_node_main_0 (
      .clock(clock),
      .init (init),
      .var_z(var_z)
  );
endmodule

module node_main (
    input  wire unsigned clock,
    input  wire unsigned init,
    output wire unsigned var_z
);
  wire unsigned not_var_z;

  lustre_not #(
      .N(1)
  ) call_lustre_not_1 (
      .arg(var_z),
      .res(not_var_z)
  );
  lustre_fby #(
      .N(1)
  ) call_lustre_fby_2 (
      .clock(clock),
      .init(init),
      .init_val(1'd0),
      .next_val(not_var_z),
      .res(var_z)
  );
endmodule
