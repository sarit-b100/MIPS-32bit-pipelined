module mips(input clk,reset,
            output [31:0] pc,
            input [31:0] instr,
            output memwritem,
            output [31:0] aluout,writedata,
            input [31:0] readdata);
          
 wire memwrited,memtoreg,regwrite,regdst,alusrc,branch,equald,pcsrc;
 wire [2:0] alucontrol;
 wire [31:0] instrd;
 controller c(instrd[31:26],instrd[5:0],equald,memtoreg,memwrited,pcsrc,alusrc,regdst,regwrite,branch,alucontrol);
 datapath dp(clk,reset,branch,memwrited,memtoreg,pcsrc,alusrc,regdst,regwrite,alucontrol,equald,memwritem,pc,instr,aluout,writedata,instrd,readdata);
endmodule

module controller(input [5:0] op,funct,input equald,
                  output memtoreg,memwrite,pcsrc,alusrc,regdst,regwrite,branch,output [2:0] alucontrol);
 wire [1:0] aluop;

 maindec md(op,memtoreg,memwrite,branch,alusrc,regdst,regwrite,aluop);
 aludec ad(funct,aluop,alucontrol);

 assign pcsrc=branch & equald;
endmodule

module maindec(input [5:0] op,
               output memtoreg,memwrite,branch,alusrc,regdst,regwrite,output [1:0] aluop);

 reg [7:0] controls=0;

 assign {regwrite, regdst, alusrc,
         branch, memwrite,
         memtoreg, aluop}=controls;
 always@(*)
 case(op)
  6'b000000: controls <= 8'b11000010; //R-type
  6'b100011: controls <= 8'b10100100; //LW
  6'b101011: controls <= 8'b00101000; //SW
  6'b000100: controls <= 8'b00010001; //BEQ
  6'b001000: controls <= 8'b10100000; //ADDI
  //default: controls <= 8'bxxxxxxxx; 
 endcase
endmodule

module aludec(input [5:0] funct,input [1:0] aluop,
              output reg [2:0] alucontrol);
 always@(*)
 case(aluop)
  2'b00:alucontrol<=3'b010;//add
  2'b01:alucontrol<=3'b110;//sub
  default:case(funct)
       6'b100000: alucontrol <= 3'b010; // ADD
       6'b100010: alucontrol <= 3'b110; // SUB
       6'b100100: alucontrol <= 3'b000; // AND
       6'b100101: alucontrol <= 3'b001; // OR
       6'b101010: alucontrol <= 3'b111; // SLT
       default: alucontrol <= 3'bxxx; // ???
      endcase
 endcase
endmodule

module datapath(input clk,reset,branchd,memwrited,memtoregd,pcsrcd,alusrcd,regdstd,regwrited,
                input [2:0] alucontrold,
                output equald,memwritem,output [31:0] pcf,
                input [31:0] rd,
                output [31:0] aluoutm,writedatam,instrd,
                input [31:0] readdatam);
 wire memwritee,memtorege,memtoregm,memtoregw,alusrce,regdste,regwritee,regwritem,regwritew;
 wire stallf,stalld,forwardad,forwardbd,flushe;
 wire [1:0] forwardae,forwardbe;
 wire [2:0] alucontrole;
 wire [31:0] aluoute,aluoutw,writedatae,writedataw,readdataw;
 wire [4:0] writerege,writeregm,writeregw;
 wire [31:0] pcnext,pcplus4f,pcplus4d,pcbranchd;
 wire [31:0] signimmd,signimme,signimmshd;//,instrd;
 wire [31:0] srcae,srcbe;
 wire [31:0] rd1d,rd2d,rd1e,rd2e,eq1,eq2,resultw;
 wire [4:0] rse,rte,rde;

 //pipeline registers
 pipereg2 instance2(clk,~stalld,pcsrcd,rd,pcplus4f,instrd,pcplus4d);
 pipereg3 instance3(clk,flushe,regwrited,memtoregd,memwrited,alucontrold,alusrcd,regdstd,
                    rd1d,rd2d,signimmd,instrd[25:21],instrd[20:16],instrd[15:11],
                    regwritee,memtorege,memwritee,alucontrole,alusrce,regdste,
                    rd1e,rd2e,signimme,
                    rse,rte,rde);
 pipereg4 instance4(clk,regwritee,memtorege,memwritee,aluoute,writedatae,writerege,
                    regwritem,memtoregm,memwritem,aluoutm,writedatam,writeregm);
 pipereg5 instance5(clk,regwritem,memtoregm,readdatam,aluoutm,writeregm,
                    regwritew,memtoregw,readdataw,aluoutw,writeregw);

 hazard instance6(instrd[25:21],instrd[20:16],rse,rte,
                  writerege,writeregm, writeregw,
                  regwritee, regwritem,regwritew,
                  memtorege, memtoregm,
                  branchd,
                  forwardad, forwardbd,forwardae, forwardbe,stallf, stalld,flushe);
 //next PC logic
 flopr #(32) pcreg(clk,reset,~stallf,pcnext,pcf);
 adder pcadd1(pcf,32'b100,pcplus4f);
 s12 immsh(signimmd,signimmshd);
 adder pcadd2(pcplus4d,signimmshd,pcbranchd);
 mux2 #(32) pcbrmux(pcplus4f,pcbranchd,pcsrcd,pcnext);

 
 //register file logic
 regfile rf(clk,regwritew,instrd[25:21],instrd[20:16],writeregw,resultw,rd1d,rd2d);
 mux2 #(5) wrmux(rte,rde,regdste,writerege);
 mux2 #(32) resmux(aluoutw,readdataw,memtoregw,resultw);
 signext se(instrd[15:0],signimmd);

 //ALU logic
 mux2 #(32) srcbmux(writedatae,signimme,alusrce,srcbe);
 alu alu(srcae,srcbe,alucontrole,aluoute,zero);

 //Hazard handling datapath
 mux2 #(32) hh1(rd1d,aluoutm,forwardad,eq1);
 mux2 #(32) hh2(rd2d,aluoutm,forwardbd,eq2);
 comparator c1(eq1,eq2,equald);
 assign pcsrcd=branchd & equald;
 mux3 hh3(rd1e,resultw,aluoutm,forwardae,srcae);
 mux3 hh4(rd2e,resultw,aluoutm,forwardbe,writedatae);

endmodule

module regfile(input clk,we3,input [4:0] ra1,ra2,wa3,input [31:0] wd3,
               output [31:0] rd1,rd2);

 reg [31:0] rf[31:0];//DOUBTDOUBT

 // three ported register file
 // read two ports combinationally
 // write third port on rising edge of clock
 // register 0 hardwired to 0
 
 always@(negedge clk)
 begin
  if(we3==1)
   rf[wa3]=wd3;
 end
 
  assign rd1=(ra1!=0)?rf[ra1]:0;
  assign rd2=(ra2!=0)?rf[ra2]:0;
 
endmodule

module comparator(input [31:0] A,B,output E);
 assign E=(A==B)?1:0;
endmodule

module alu(input [31:0] A,B,input [2:0] alucontrol,
           output reg [31:0] aluout,output reg zero);
 always@(*)
 case(alucontrol)
  3'b000:aluout=A & B;
  3'b001:aluout=A | B;
  3'b010:aluout=A+B;
  3'b100:aluout=A & ~B;
  3'b101:aluout=A | ~B;
  3'b110:aluout=A-B;
  3'b111:aluout=(A<B)?1:0;
 endcase
 always@(*)
 begin 
  if(A==B)
   zero=1;
  else
   zero=0;
 end
endmodule
 
module adder (input [31:0] a, b,
              output [31:0] y);
assign y=a+b;
endmodule

module s12(input [31:0] a,
           output [31:0] y);
 assign y={a[29:0],2'b00};
endmodule

module signext(input [15:0] a,
               output [31:0] y);
 assign y={{16{a[15]}},a};
endmodule

module flopr #(parameter WIDTH=8)
              (input clk,reset,en,input [WIDTH-1:0] d,
               output reg [WIDTH-1:0] q);
   always@(posedge clk,posedge reset)
    if(reset) q<=0;
    else if(en==1) q<=d;
endmodule

module mux2 #(parameter WIDTH=8)
             (input [WIDTH-1:0] d0,d1,input s,
              output [WIDTH-1:0] y);
 assign y=s?d1:d0;
endmodule

module mux3(input [31:0] A,B,C,input [1:0] sel,output reg [31:0] O);
 always@(*)
  case(sel)
   2'b00:O<=A;
   2'b01:O<=B;
   2'b10:O<=C;
   2'b11:O<=32'bx;
  endcase
endmodule

module top_pipeline(input clk, reset,
output [31:0] writedata, dataadr,
output memwrite);
wire [31:0] pc, instr, readdata;
// instantiate processor and memories
mips mips (clk, reset, pc, instr, memwrite, dataadr,
writedata, readdata);
imem imem (pc[7:2], instr);
dmem dmem (clk, memwrite, dataadr, writedata,
readdata);
endmodule

module dmem (input clk, we,
input [31:0] a, wd,
output [31:0] rd);
reg [31:0] RAM[63:0];//64 registers of 32 bit each 
assign rd = RAM[a[31:2]]; // word aligned
always @ (posedge clk)
if (we)
RAM[a[31:2]] <= wd;
endmodule 

module imem (input [5:0] a,
output [31:0] rd);
reg [31:0] RAM[63:0];
initial
begin
$readmemh ("memfile.dat",RAM);
end
assign rd=RAM[a]; // word aligned
endmodule   


module pipereg2(input clk,en,clr,input [31:0] rd,pcplus4f,output reg [31:0] instrd=0,pcplus4d);
 always@(posedge clk)
 begin 
  if(clr==1)
   {instrd,pcplus4d}<=0;
  else if(en==1)
   {instrd,pcplus4d}<={rd,pcplus4f};
 end
endmodule

module pipereg3(input clk,clr,regwrited,memtoregd,memwrited,input [2:0] alucontrold,input alusrcd,regdstd,
                input [31:0] rd1d,rd2d,signimmd,input [4:0] rsd,rtd,rdd,
                output reg regwritee=0,memtorege=0,memwritee=0,output reg [2:0] alucontrole=0,output reg alusrce=0,regdste=0,
                output reg [31:0] rd1e=0,rd2e=0,signimme=0,
                output reg [4:0] rse=0,rte=0,rde=0);
 always@(posedge clk)
 begin
  if(clr==1)
   {regwritee,memtorege,memwritee,alucontrole,alusrce,regdste,rd1e,rd2e,rse,rte,rde,signimme}<=0;
  else
   {regwritee,memtorege,memwritee,alucontrole,alusrce,regdste,rd1e,rd2e,rse,rte,rde,signimme}<={regwrited,memtoregd,memwrited,alucontrold,alusrcd,regdstd,rd1d,rd2d,rsd,rtd,rdd,signimmd};
 end
endmodule 

module pipereg4(input clk,regwritee,memtorege,memwritee,input [31:0] aluoute,writedatae,input [4:0] writerege,
                output reg regwritem=0,memtoregm=0,memwritem=0,output reg [31:0] aluoutm=0,writedatam=0,output reg [4:0] writeregm=0);
 always@(posedge clk)
 begin
  {regwritem,memtoregm,memwritem,aluoutm,writedatam,writeregm}<={regwritee,memtorege,memwritee,aluoute,writedatae,writerege};
 end
endmodule

module pipereg5(input clk,regwritem,memtoregm,input [31:0] readdatam,aluoutm,input [4:0] writeregm,
                output reg regwritew=0,memtoregw=0,output reg [31:0] readdataw=0,aluoutw=0,output reg [4:0] writeregw=0);
 always@(posedge clk)
 begin
  {regwritew,memtoregw,readdataw,aluoutw,writeregw}<={regwritem,memtoregm,readdatam,aluoutm,writeregm};
 end 
endmodule

module hazard(input [4:0] rsD, rtD, rsE, rtE,
input [4:0] writeregE,
writeregM, writeregW,
input regwriteE, regwriteM,
regwriteW,
input memtoregE, memtoregM,
input branchD,
output forwardaD, forwardbD,
output reg [1:0] forwardaE, forwardbE,
output stallF, stallD,flushE);

wire lwstallD, branchstallD;
// forwarding sources to D stage (branch equality)
assign forwardaD = (rsD !=0 & rsD == writeregM &
regwriteM);
assign forwardbD = (rtD !=0 & rtD == writeregM &
regwriteM);
// forwarding sources to E stage (ALU)
always@(*)
begin
forwardaE = 2'b00; forwardbE = 2'b00;
if (rsE != 0)
if (rsE == writeregM & regwriteM)
forwardaE = 2'b10;
else if (rsE == writeregW & regwriteW)
forwardaE = 2'b01;
if (rtE != 0)
if (rtE == writeregM & regwriteM)
forwardbE = 2'b10;
else if (rtE == writeregW & regwriteW)
forwardbE = 2'b01;
end
// stalls
assign lwstallD = memtoregE &
(rtE == rsD | rtE == rtD);
assign branchstallD = branchD &
(regwriteE &
(writeregE == rsD | writeregE == rtD) |
memtoregM &
(writeregM == rsD | writeregM == rtD));
assign stallD = lwstallD | branchstallD;
assign stallF = stallD;
// stalling D stalls all previous stages
assign flushE = stallD;

endmodule
                           