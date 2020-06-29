#!/Applications/Mathematica.app/Contents/MacOS/MathematicaScript

out = ToString[$ScriptCommandLine[[2]]];

er[out_] := 
 Module[
  {data, l, max, ef, pr, firsthalf, secondhalf, interpolate},
  data = Import[out, "Table"];
  l = Position[data, {"R", "P(R)", "ERROR"}] // Flatten // # + 2 &;
  pr = data[[FromDigits@l ;; All, {1, 2}]];
  max = Cases[pr[[All, {1, 2}]], {_, Max[pr[[All, 2]]]}][[1, 1]];
  interpolate = Interpolation[pr];
  firsthalf = 
   NIntegrate[interpolate[x], {x, 0, max}, Method -> "Trapezoidal"];
  secondhalf = 
   NIntegrate[interpolate[x], {x, max, Max@pr[[All, {1, 2}]]}, 
    Method -> "Trapezoidal"];
  ef = secondhalf/firsthalf;
  Print@ef
  ]

er[out]
