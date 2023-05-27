//////////////////////////////////////////////////
//
// G. Del Castillo, W. Hardt
// Fast dynamic analysis of instruction sets
// (CODES/CASHE'98, GI/ITG/GMM)
//
//   data.asm
//

// addresses where the matrices are located

static function A_addr == 100
static function B_addr == 200
static function C_addr == 300   // result

// ... and corresponding displacements

static function A_disp == Disp(A_addr)
static function B_disp == Disp(B_addr)
static function C_disp == Disp(C_addr)


// sizes of the matrices

static function A_rows == 2
static function A_cols == 2

static function B_rows == 2
static function B_cols == 2


// the matrices themselves

static function A_Matrix == {
  0 -> 1, 1 -> 2,
  2 -> 2, 3 -> 1
}

static function B_Matrix == {
  0 -> 0, 1 -> 2,
  2 -> 2, 3 -> 0
}
