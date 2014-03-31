BeginPackage["Utils`"];

Unprotect @@ Names["Utils`*"];
ClearAll @@ Names["Utils`*"];

SquareMatrixQ::usage = "gives True if expr is a list of lists or a 
two-dimensional SparseArray object that can represent a matrix whose two
dimensions are of the same order, and gives false otherwise";

MarkovMatrixQ::usage = "gives True if expr is a square matrix and the sum
of each row equals 1, and gives false otherwise";

NonSingularMatrixQ::usage = "gives True is the expr is a list of lists or a
two-dimensional SparseArray object that can represent a matrix whose determinant
is non-zero";

J::usage = "Compute the Jacobian of a system n equations by n variables";

SpectralBound::usage = "Compute the spectral bound of a square matrix (i.e. 
maximum real part of all eigenvalues)";

SpectralRadius::usage = "Compute the spectral radius of a square matrix (i.e.
the maximum absolute value of all eigenvalues)"

Begin["`Private`"]


SquareMatrixQ[expr_] := 
  MatrixQ[expr] && Length[Union[Dimensions[expr]]] == 1;


MarkovMatrixQ[expr_] :=
  SquareMatrixQ[expr] && MatchQ[Total /@ expr, {(1 | 1.) ..}];


NonSingularMatrixQ[expr_] := 
  MatrixQ[expr] && Det[expr] != 0;


J[eqs_?ListQ, vars_?ListQ] := 
  Outer[D, eqs, vars] /; Length[eqs] == Length[vars];


SpectralBound[mat_?SquareMatrixQ] := 
  Max[Re[Eigenvalues[mat]]];


SpectralRadius[mat_?SquareMatrixQ] :=
  Max[Abs[Eigenvalues[mat]]];


End[ ]

Protect @@ Names["Utils`*"];

EndPackage[ ];


