/* () */
module mod1  (y, w1, w2);
  output wire [(((1'h1) + (5'h14)) + (5'h16)):(((1'h0) + (1'h0)) + (1'h0))] y;
  input wire [(4'h8):(1'h0)] w1;
  input wire [(5'h1f):(1'h0)] w2;
  wire [(5'h14):(1'h0)] w3;
  wire [(5'h16):(1'h0)] w4;
  assign w3 = $signed($unsigned({$unsigned((w2 ? (8'had) : w2)), w1, (8'ha5)}));
  assign w4 = $signed(w2);
  assign y = {w3, w4, (1'h0)};
endmodule
