#!/Applications/Mathematica.app/Contents/MacOS/MathematicaScript

(* this script is run where the only argument is the file path of a single p(r) curve:
   $ elongation_ratio.wls p_r.out
and returns a single value corresponding to the elongation ratio (ER)
according to Putnam, C.D. (2016) J. Appl. Crystallogr. 49, 1412-1419. *)

out = ToString[$ScriptCommandLine[[2]]];
er[out_] := 
 Module[
  {data, l, max, er, pr, firsthalf, secondhalf, interpolate},

  data = Import[out, "Table"];

(*starts after header that is found in gnom.out files*)
  l = Position[data, {"R", "P(R)", "ERROR"}] // Flatten // # + 2 &; 
  pr = data[[FromDigits@l ;; All, {1, 2}]];

(* locates point where to split curve (distance with max frequency) *)
  max = Cases[pr[[All, {1, 2}]], {_, Max[pr[[All, 2]]]}][[1, 1]]; 
  interpolate = Interpolation[pr];

(* integrate first half of the p(r) curve *)
  firsthalf = 
   NIntegrate[interpolate[x], {x, 0, max}, Method -> "Trapezoidal"]; 

(* integrate the second half 	of the	p(r) curve *)
  secondhalf = 
   NIntegrate[interpolate[x], {x, max, Max@pr[[All, {1, 2}]]}, 
    Method -> "Trapezoidal"];		
				
(* ER calculation *)
  er = secondhalf/firsthalf; 						
  Print@er
  ]

er[out]
