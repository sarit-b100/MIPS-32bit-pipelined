module tb_MIPS_pipeline();
reg clk;
reg reset;
wire [31:0] writedata, dataadr;
wire memwrite;
// instantiate device to be tested
top_pipeline dut (clk, reset, writedata, dataadr, memwrite);
// initialize test
initial
begin
reset=1; # 22; reset=0;
end
// generate clock to sequence tests
always
begin
clk=1; # 5; clk=0; # 5;
end

endmodule
