BeginPackage["Utils`"];

Unprotect @@ Names["Utils`*"];
ClearAll @@ Names["Utils`*"];

J::usage = "Compute the Jacobian of a system n equations by n variables";

Begin["`Private`"]

J[eqs_: ListQ, vars_: ListQ] := 
 Outer[D, eqs, vars] /; Length[eqs] == Length[vars];

End[ ]

Protect @@ Names["Utils`*"];

EndPackage[ ];


