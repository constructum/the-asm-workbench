//////////////////////////////////////////////////
//
// G. Del Castillo, W. Hardt
// Fast dynamic analysis of instruction sets
// (CODES/CASHE'98, GI/ITG/GMM)
//
//   program.asm
//

//------------------------------------------------
// example program: MATRIX MULTIPLICATION
//
// 1  // initialize dimensions, e.g.
// 2  for i := 1 to m
// 3    for j := 1 to n
// 4      C [i, j] := 0
// 5      for k := 1 to p
// 6        C [i, j] := C [i, j] + A [i, k] * B [k, j]
// 7      end for k
// 8    end for j
// 9  end for i
//


// Note: R(0) is used to hold the constant 0, R(1) for 1

static function R0 == R(0)
static function R1 == R(1)


// registers used for storing variables

static function i_reg == R(4)
static function j_reg == R(5)
static function k_reg == R(6)

static function m_reg == R(8)
static function p_reg == R(9)
static function n_reg == R(10)

// registers used for storing intermediate results

static function temp1_reg == R(12)
static function temp2_reg == R(13)
static function temp3_reg == R(14)
static function temp4_reg == R(15)
static function temp5_reg == R(16)
static function temp6_reg == R(17)
static function temp7_reg == R(18)
static function temp8_reg == R(19)
static function temp9_reg == R(20)
static function temp10_reg == R(21)
static function temp11_reg == R(22)
static function temp12_reg == R(23)


// little trick: how to move things around in the register file
//   (assuming that register 0 contains 0)

static function MOVE (dest_reg, src_reg) ==
  ADD (dest_reg, src_reg, R0)


//
// ... and finally the program !
//

static function prog == {
// prologue
  0 -> ZERO (R0),
  1 -> LDA (R1, R0, Disp(1)),

// line 1 (initialize dimensions)
  2 -> LDA (m_reg, R0, Disp(A_rows)),
  3 -> LDA (p_reg, R0, Disp(A_cols)),
  4 -> LDA (n_reg, R0, Disp(B_cols)),

// line 2
  5 -> LDA (i_reg, R0, Disp(1)),

  6 -> CMP (i_reg, m_reg),
  7 -> JUMP (R0, Disp(51), GT),

// line 3
  8 -> LDA (j_reg, R0, Disp(1)),

  9 -> CMP (j_reg, n_reg),
  10 -> JUMP (R0, Disp(49), GT),

// line 4
// first compute the address: C_addr + (i - 1) * n + (j - 1) ...
  11 -> LDA (temp1_reg, R0, C_disp),
  12 -> SUB (temp2_reg, i_reg, R1),
  13 -> MUL (temp2_reg, temp2_reg, n_reg),
  14 -> ADD (temp1_reg, temp1_reg, temp2_reg),
  15 -> SUB (temp3_reg, j_reg, R1),
  16 -> ADD (temp1_reg, temp1_reg, temp3_reg),
// ... and then store the value
  17 -> ST (R0, temp1_reg, Disp(0)),

// line 5
  18 -> LDA (k_reg, R0, Disp(1)),

  19 -> CMP (k_reg, p_reg),
  20 -> JUMP (R0, Disp(47), GT),

// line 6
// address of C [i, j]
  21 -> LDA (temp1_reg, R0, C_disp),
  22 -> SUB (temp2_reg, i_reg, R1),
  23 -> MUL (temp2_reg, temp2_reg, n_reg),
  24 -> ADD (temp1_reg, temp1_reg, temp2_reg),
  25 -> SUB (temp3_reg, j_reg, R1),
  26 -> ADD (temp1_reg, temp1_reg, temp3_reg),
// address of A [i, k]
  27 -> LDA (temp4_reg, R0, A_disp),
  28 -> SUB (temp5_reg, i_reg, R1),
  29 -> MUL (temp5_reg, temp5_reg, n_reg),
  30 -> ADD (temp4_reg, temp4_reg, temp5_reg),
  31 -> SUB (temp6_reg, k_reg, R1),
  32 -> ADD (temp4_reg, temp4_reg, temp6_reg),
// address of B [k, j]
  33 -> LDA (temp7_reg, R0, B_disp),
  34 -> SUB (temp8_reg, k_reg, R1),
  35 -> MUL (temp8_reg, temp8_reg, n_reg),
  36 -> ADD (temp7_reg, temp7_reg, temp8_reg),
  37 -> SUB (temp9_reg, j_reg, R1),
  38 -> ADD (temp7_reg, temp7_reg, temp9_reg),
// compute and store the result
  39 -> LD (temp10_reg, temp4_reg, Disp(0)),
  40 -> LD (temp11_reg, temp7_reg, Disp(0)),
  41 -> MUL (temp12_reg, temp10_reg, temp11_reg),
  42 -> LD (temp10_reg, temp1_reg, Disp(0)),
  43 -> ADD (temp10_reg, temp10_reg, temp12_reg),
  44 -> ST (temp10_reg, temp1_reg, Disp(0)),

// line 7
  45 -> ADD (k_reg, k_reg, R1),
  46 -> JUMP (R0, Disp(19), TRUE),

// line 8
  47 -> ADD (j_reg, j_reg, R1),
  48 -> JUMP (R0, Disp(9), TRUE),
  
// line 9
  49 -> ADD (i_reg, i_reg, R1),
  50 -> JUMP (R0, Disp(6), TRUE),

  51 -> HALT
}
