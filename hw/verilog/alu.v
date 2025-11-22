module adder (
	op,
	src_a,
	src_b,
	out,
	cout
);
	localparam signed [31:0] WIDTH = 32;
	input wire [2:0] op;
	input wire signed [31:0] src_a;
	input wire signed [31:0] src_b;
	output reg [31:0] out;
	output wire cout;
	reg is_sub;
	reg is_signed_sub;
	always @(*)
		case (op)
			3'b010: begin
				is_sub = 1'b0;
				is_signed_sub = 1'b0;
			end
			3'b100, 3'b101: begin
				is_sub = 1'b1;
				is_signed_sub = 1'b1;
			end
			3'b011, 3'b110, 3'b111: begin
				is_sub = 1'b1;
				is_signed_sub = 1'b0;
			end
			default: begin
				is_sub = 1'bx;
				is_signed_sub = 1'bx;
			end
		endcase
	reg [33:0] adder_a;
	reg [33:0] adder_b;
	wire signed [31:0] b_tmp = (is_sub ? ~src_b : src_b);
	function automatic signed [32:0] sv2v_cast_33_signed;
		input reg signed [32:0] inp;
		sv2v_cast_33_signed = inp;
	endfunction
	always @(*) begin
		adder_a = {sv2v_cast_33_signed(src_a), 1'b1};
		adder_b = {sv2v_cast_33_signed(b_tmp), is_sub};
		if (!is_signed_sub) begin
			adder_a[33] = 1'b0;
			adder_b[33] = 1'b1;
		end
	end
	wire [33:0] adder_out = adder_a + adder_b;
	assign cout = adder_out[33];
	wire eq = src_a == src_b;
	always @(*)
		case (op)
			3'b000: out = {31'b0000000000000000000000000000000, eq};
			3'b001: out = {31'b0000000000000000000000000000000, ~eq};
			3'b100, 3'b110: out = {31'b0000000000000000000000000000000, adder_out[33]};
			3'b101, 3'b111: out = {31'b0000000000000000000000000000000, ~adder_out[33]};
			3'b010: out = adder_out[WIDTH:1];
			3'b011: out = adder_out[WIDTH:1];
			default: out = 1'sbx;
		endcase
endmodule
module shifter3 (
	clk,
	start,
	val_i,
	sham_i,
	right_shift,
	arith_shift,
	val_o,
	sham_o
);
	parameter signed [31:0] WIDTH = 32;
	localparam signed [31:0] WSHAM = $clog2(WIDTH);
	input wire clk;
	input wire start;
	input wire [WIDTH - 1:0] val_i;
	input wire [WSHAM - 1:0] sham_i;
	input wire right_shift;
	input wire arith_shift;
	output wire [WIDTH - 1:0] val_o;
	output wire [WSHAM - 1:0] sham_o;
	reg [1:0] shift_cur;
	wire signed [WIDTH:0] val_ext = {val_i, {(!start && arith_shift ? val_i[0] : 1'b0)}};
	function automatic [WSHAM - 1:0] sv2v_cast_F7DE1;
		input reg [WSHAM - 1:0] inp;
		sv2v_cast_F7DE1 = inp;
	endfunction
	assign sham_o = sham_i - sv2v_cast_F7DE1(shift_cur);
	function automatic [1:0] sv2v_cast_2;
		input reg [1:0] inp;
		sv2v_cast_2 = inp;
	endfunction
	always @(*) begin
		shift_cur = (sham_i > 3 ? 2'd3 : sv2v_cast_2(sham_i));
		if (start && right_shift)
			shift_cur = 0;
	end
	function automatic [(WIDTH >= 0 ? WIDTH + 1 : 1 - WIDTH) - 1:0] _sv2v_strm_val_ext_r;
		input reg [(0 + (WIDTH >= 0 ? WIDTH + 1 : 1 - WIDTH)) - 1:0] inp;
		reg [(0 + (WIDTH >= 0 ? WIDTH + 1 : 1 - WIDTH)) - 1:0] _sv2v_strm_55E18_inp;
		reg [(0 + (WIDTH >= 0 ? WIDTH + 1 : 1 - WIDTH)) - 1:0] _sv2v_strm_55E18_out;
		integer _sv2v_strm_55E18_idx;
		begin
			_sv2v_strm_55E18_inp = {inp};
			for (_sv2v_strm_55E18_idx = 0; _sv2v_strm_55E18_idx <= ((0 + (WIDTH >= 0 ? WIDTH + 1 : 1 - WIDTH)) - 1); _sv2v_strm_55E18_idx = _sv2v_strm_55E18_idx + 1)
				_sv2v_strm_55E18_out[((0 + (WIDTH >= 0 ? WIDTH + 1 : 1 - WIDTH)) - 1) - _sv2v_strm_55E18_idx-:1] = _sv2v_strm_55E18_inp[_sv2v_strm_55E18_idx+:1];
			_sv2v_strm_val_ext_r = ((0 + (WIDTH >= 0 ? WIDTH + 1 : 1 - WIDTH)) <= (WIDTH >= 0 ? WIDTH + 1 : 1 - WIDTH) ? _sv2v_strm_55E18_out << ((WIDTH >= 0 ? WIDTH + 1 : 1 - WIDTH) - (0 + (WIDTH >= 0 ? WIDTH + 1 : 1 - WIDTH))) : _sv2v_strm_55E18_out >> ((0 + (WIDTH >= 0 ? WIDTH + 1 : 1 - WIDTH)) - (WIDTH >= 0 ? WIDTH + 1 : 1 - WIDTH)));
		end
	endfunction
	wire [WIDTH:0] val_ext_r = _sv2v_strm_val_ext_r({val_ext});
	function automatic signed [WIDTH - 1:0] sv2v_cast_099BD_signed;
		input reg signed [WIDTH - 1:0] inp;
		sv2v_cast_099BD_signed = inp;
	endfunction
	wire [WIDTH - 1:0] reversed = sv2v_cast_099BD_signed($signed(val_ext_r) >>> shift_cur);
	function automatic [WIDTH - 1:0] _sv2v_strm_shifted;
		input reg [(0 + WIDTH) - 1:0] inp;
		reg [(0 + WIDTH) - 1:0] _sv2v_strm_55E18_inp;
		reg [(0 + WIDTH) - 1:0] _sv2v_strm_55E18_out;
		integer _sv2v_strm_55E18_idx;
		begin
			_sv2v_strm_55E18_inp = {inp};
			for (_sv2v_strm_55E18_idx = 0; _sv2v_strm_55E18_idx <= ((0 + WIDTH) - 1); _sv2v_strm_55E18_idx = _sv2v_strm_55E18_idx + 1)
				_sv2v_strm_55E18_out[((0 + WIDTH) - 1) - _sv2v_strm_55E18_idx-:1] = _sv2v_strm_55E18_inp[_sv2v_strm_55E18_idx+:1];
			_sv2v_strm_shifted = ((0 + WIDTH) <= WIDTH ? _sv2v_strm_55E18_out << (WIDTH - (0 + WIDTH)) : _sv2v_strm_55E18_out >> ((0 + WIDTH) - WIDTH));
		end
	endfunction
	wire [WIDTH - 1:0] shifted = _sv2v_strm_shifted({reversed});
	wire sel_reverse = (!start ? right_shift && (sham_o == 0) : (start && right_shift) && (sham_o != 0));
	assign val_o = (sel_reverse ? reversed : shifted);
endmodule
module ALU(
	clk,
	start,
	src_a,
	src_b,
	f3,
	arith_bit,
	shadd,
	branch,
	result,
	shamt_out,
	ready
);
	localparam signed [31:0] XLEN = 32;
	localparam signed [31:0] WSHAM = $clog2(XLEN);
	input wire clk;
	input wire start;
	input wire [XLEN - 1:0] src_a;
	input wire [XLEN - 1:0] src_b;
	input wire [2:0] f3;
	input wire arith_bit;
	input wire shadd;
	input wire branch;
	output reg [XLEN - 1:0] result;
	output wire [WSHAM - 1:0] shamt_out;
	output reg ready;
	reg is_pure_shift;
	reg is_right_shift;
	reg is_pure_alu;
	reg is_logical;
	always @(*) begin
		is_pure_alu = !shadd && !branch;
		is_pure_shift = is_pure_alu && ((f3 == 3'b001) || (f3 == 3'b101));
		is_right_shift = f3 == 3'b101;
		is_logical = is_pure_alu && (((f3 == 3'b111) || (f3 == 3'b110)) || (f3 == 3'b100));
	end
	reg [WSHAM - 1:0] shamt;
	wire [31:0] shifter_out;
	wire shifter_done;
	function automatic [WSHAM - 1:0] sv2v_cast_B7F95;
		input reg [WSHAM - 1:0] inp;
		sv2v_cast_B7F95 = inp;
	endfunction
	always @(*) begin
		shamt = 0;
		if (shadd)
			shamt = sv2v_cast_B7F95(f3[2:1]);
		else if (is_pure_shift)
			shamt = src_b[4:0];
	end
	shifter3 #(.WIDTH(XLEN)) S(
		.clk(clk),
		.start(start),
		.val_i(src_a),
		.sham_i(shamt),
		.right_shift(is_right_shift),
		.arith_shift(arith_bit),
		.val_o(shifter_out),
		.sham_o(shamt_out)
	);
	wire adder_cout;
	reg [2:0] adder_op;
	wire [31:0] adder_out;
	always @(*)
		if (shadd)
			adder_op = 3'b010;
		else if (branch)
			adder_op = f3;
		else
			case (f3)
				3'b000: adder_op = (arith_bit ? 3'b011 : 3'b010);
				3'b010: adder_op = 3'b100;
				3'b011: adder_op = 3'b110;
				default: adder_op = 3'bxxx;
			endcase
	adder A(
		.op(adder_op),
		.src_a(src_a),
		.src_b(src_b),
		.out(adder_out),
		.cout(adder_cout)
	);
	always @(*)
		case (1'b1)
			is_pure_shift: ready = shamt_out == 0;
			shadd: ready = !start;
			default: ready = 1'b1;
		endcase
	always @(*) begin
		result = adder_out;
		if (is_pure_shift | (shadd & start))
			result = shifter_out;
		if (is_logical)
			case (f3)
				3'b111: result = src_a & src_b;
				3'b110: result = src_a | src_b;
				3'b100: result = src_a ^ src_b;
				default: result = adder_out;
			endcase
	end
endmodule
