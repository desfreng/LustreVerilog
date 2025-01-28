module internal_lustre_bit_adder (
    input  a,
    input  b,
    input  carry_in,
    output res,
    output carry_out
);
  wire a_and_b;
  wire a_and_carry_in;
  wire b_and_carry_in;

  and (a_and_b, a, b);
  and (a_and_carry_in, a, carry_in);
  and (b_and_carry_in, b, carry_in);

  xor (res, a, b, carry_in);
  or (carry_out, a_and_b, a_and_carry_in, b_and_carry_in);
endmodule
