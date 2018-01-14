#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Code:: *)
(*Quit[]*)


(* ::Input::Initialization:: *)
num=ToExpression[$ScriptCommandLine[[2]]];
Once[LaunchKernels[num]];
ParallelEvaluate[Off[General::stop,NIntegrate::slwcon]];
ParallelEvaluate@SetOptions[NIntegrate,MaxRecursion->100,AccuracyGoal->12];
(*SetDirectory[NotebookDirectory[]];*)
(*AppendTo[$Path,NotebookDirectory[]];*)
(*Print[$Path];*)
SetDirectory[If[$InputFileName == "", NotebookDirectory[], Directory[]]];


(* ::Input::Initialization:: *)
Get["OneFlavour`"];
Clear[\[Phi]x,m2,\[Phi],vals,M1,M2,M3,\[Phi]1,\[Phi]2,\[Phi]3];
m=4.233;m1=4.233;m2=0.749;
MC[m_]:=Which[ToString[m]=="c",4.233,ToString[m]=="s",0.749,ToString[m]=="u",0.045,ToString[m]=="chiralu",0]


(* ::Input::Initialization:: *)
(*Solvet[m,m,Method->"BSW",MatrixSize->500,Force->True]*)


(* ::Input::Initialization:: *)
Solvet[m1,m2,Method->"'t Hooft",MatrixSize->11,Force->True]
Print["End"];



(* ::Code:: *)
(*(*DynamicSetting[(ma=ToExpression[Setting[#]];Setting[#])&,SetterBar["4.233",{"4.233","4.23","0.749"}]]; Shift+Ctrl+Enter*)*)


(* ::Code:: *)
(*(*DynamicSetting[SetterBar["acceigenstate",{"acceigenstate","eigenstate"}]]*)*)


(* ::Code:: *)
(*(*DynamicSetting[(mb=MC[Setting[#]];Setting[#])&,SetterBar["c",{"c","s","u","chiralu"}]]*)*)


(* ::Input:: *)
(*{ValsA,\[Phi]xA}=Import["D:/Documents/2-d-data/acceigenstate_m-0.749.wdx"];*)


(* ::Input:: *)
(*Table[\[Phi]xA[[i]]=(-1)^i \[Phi]xA[[i]],{i,1,Length@\[Phi]xA}];*)


(* ::Input:: *)
(*{ValsB,\[Phi]xB}=Import["../2-d-data/acceigenstate_m-c.wdx"];*)


(* ::Input:: *)
(*{ValsB,\[Phi]xB}=Import["../2-d-data/acceigenstate_m-s.wdx"];*)


(* ::Input:: *)
(*{ValsC,\[Phi]xC}=Import["D:/Documents/2-d-data/eigenstate_m-0.749.wdx"];*)


(* ::Input:: *)
(*{ValsC,\[Phi]xC}=Import["../2-d-data/acceigenstate_m1-4.23_m2-0.749.wdx"];*)


(* ::Input:: *)
(*{ValsA,\[Phi]xA}=Import["../2-d-data/"<>\!\(\**)
(*TagBox[*)
(*TagBox[*)
(*DynamicModuleBox[{BoxForm`var$$ = "acceigenstate"}, *)
(*InterpretationBox[*)
(*InterpretationBox[*)
(*StyleBox[GridBox[{*)
(*{*)
(*SetterBox[Dynamic[BoxForm`var$$], {"acceigenstate"}, "\"\<acceigenstate\>\""], *)
(*SetterBox[Dynamic[BoxForm`var$$], {"eigenstate"}, "\"\<eigenstate\>\""]}*)
(*},*)
(*BaselinePosition->{1, 1},*)
(*GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}}],*)
(*Deployed->True],*)
(*SetterBar[Dynamic[BoxForm`var$$], {"acceigenstate", "eigenstate"}]],*)
(*SetterBar[BoxForm`var$$, {"acceigenstate", "eigenstate"}]],*)
(*DynamicModuleValues:>{}],*)
(*Setting[#, {0}]& ],*)
(*Setting]\)<>"_m-"<>\!\(\**)
(*TagBox[*)
(*TagBox[*)
(*DynamicModuleBox[{BoxForm`var$$ = "4.233"}, *)
(*InterpretationBox[*)
(*InterpretationBox[*)
(*StyleBox[GridBox[{*)
(*{*)
(*SetterBox[Dynamic[BoxForm`var$$], {"4.233"}, "\"\<4.233\>\""], *)
(*SetterBox[Dynamic[BoxForm`var$$], {"4.23"}, "\"\<4.23\>\""], *)
(*SetterBox[Dynamic[BoxForm`var$$], {"0.749"}, "\"\<0.749\>\""]}*)
(*},*)
(*BaselinePosition->{1, 1},*)
(*GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}}],*)
(*Deployed->True],*)
(*SetterBar[Dynamic[BoxForm`var$$], {"4.233", "4.23", "0.749"}]],*)
(*SetterBar[BoxForm`var$$, {"4.233", "4.23", "0.749"}]],*)
(*DynamicModuleValues:>{}],*)
(*Setting[#, {0}]& ],*)
(*($CellContext`ma = ToExpression[Setting[#]]; Setting[#])& ]\)<>".wdx"];*)
(*{ValsB,\[Phi]xB}=Import["../2-d-data/"<>\!\(\**)
(*TagBox[*)
(*TagBox[*)
(*DynamicModuleBox[{BoxForm`var$$ = "acceigenstate"}, *)
(*InterpretationBox[*)
(*InterpretationBox[*)
(*StyleBox[GridBox[{*)
(*{*)
(*SetterBox[Dynamic[BoxForm`var$$], {"acceigenstate"}, "\"\<acceigenstate\>\""], *)
(*SetterBox[Dynamic[BoxForm`var$$], {"eigenstate"}, "\"\<eigenstate\>\""]}*)
(*},*)
(*BaselinePosition->{1, 1},*)
(*GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}}],*)
(*Deployed->True],*)
(*SetterBar[Dynamic[BoxForm`var$$], {"acceigenstate", "eigenstate"}]],*)
(*SetterBar[BoxForm`var$$, {"acceigenstate", "eigenstate"}]],*)
(*DynamicModuleValues:>{}],*)
(*Setting[#, {0}]& ],*)
(*Setting]\)<>"_m-"<>\!\(\**)
(*TagBox[*)
(*TagBox[*)
(*DynamicModuleBox[{BoxForm`var$$ = "c"}, *)
(*InterpretationBox[*)
(*InterpretationBox[*)
(*StyleBox[GridBox[{*)
(*{*)
(*SetterBox[Dynamic[BoxForm`var$$], {"c"}, "\"\<c\>\""], *)
(*SetterBox[Dynamic[BoxForm`var$$], {"s"}, "\"\<s\>\""], *)
(*SetterBox[Dynamic[BoxForm`var$$], {"u"}, "\"\<u\>\""], *)
(*SetterBox[Dynamic[BoxForm`var$$], {"chiralu"}, "\"\<chiralu\>\""]}*)
(*},*)
(*BaselinePosition->{1, 1},*)
(*GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}}],*)
(*Deployed->True],*)
(*SetterBar[Dynamic[BoxForm`var$$], {"c", "s", "u", "chiralu"}]],*)
(*SetterBar[BoxForm`var$$, {"c", "s", "u", "chiralu"}]],*)
(*DynamicModuleValues:>{}],*)
(*Setting[#, {0}]& ],*)
(*($CellContext`mb = $CellContext`MC[Setting[#]]; Setting[#])& ]\)<>".wdx"];*)
(*{ValsC,\[Phi]xC}=Import["../2-d-data/"<>\!\(\**)
(*TagBox[*)
(*TagBox[*)
(*DynamicModuleBox[{BoxForm`var$$ = "acceigenstate"}, *)
(*InterpretationBox[*)
(*InterpretationBox[*)
(*StyleBox[GridBox[{*)
(*{*)
(*SetterBox[Dynamic[BoxForm`var$$], {"acceigenstate"}, "\"\<acceigenstate\>\""], *)
(*SetterBox[Dynamic[BoxForm`var$$], {"eigenstate"}, "\"\<eigenstate\>\""]}*)
(*},*)
(*BaselinePosition->{1, 1},*)
(*GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}}],*)
(*Deployed->True],*)
(*SetterBar[Dynamic[BoxForm`var$$], {"acceigenstate", "eigenstate"}]],*)
(*SetterBar[BoxForm`var$$, {"acceigenstate", "eigenstate"}]],*)
(*DynamicModuleValues:>{}],*)
(*Setting[#, {0}]& ],*)
(*Setting]\)<>"_m1-"<>\!\(\**)
(*TagBox[*)
(*TagBox[*)
(*DynamicModuleBox[{BoxForm`var$$ = "4.23"}, *)
(*InterpretationBox[*)
(*InterpretationBox[*)
(*StyleBox[GridBox[{*)
(*{*)
(*SetterBox[Dynamic[BoxForm`var$$], {"4.233"}, "\"\<4.233\>\""], *)
(*SetterBox[Dynamic[BoxForm`var$$], {"4.23"}, "\"\<4.23\>\""], *)
(*SetterBox[Dynamic[BoxForm`var$$], {"0.749"}, "\"\<0.749\>\""]}*)
(*},*)
(*BaselinePosition->{1, 1},*)
(*GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}}],*)
(*Deployed->True],*)
(*SetterBar[Dynamic[BoxForm`var$$], {"4.233", "4.23", "0.749"}]],*)
(*SetterBar[BoxForm`var$$, {"4.233", "4.23", "0.749"}]],*)
(*DynamicModuleValues:>{}],*)
(*Setting[#, {0}]& ],*)
(*($CellContext`mc1 = ToExpression[Setting[#]]; Setting[#])& ]\)<>"_m2-"<>\!\(\**)
(*TagBox[*)
(*TagBox[*)
(*DynamicModuleBox[{BoxForm`var$$ = "0.749"}, *)
(*InterpretationBox[*)
(*InterpretationBox[*)
(*StyleBox[GridBox[{*)
(*{*)
(*SetterBox[Dynamic[BoxForm`var$$], {"4.233"}, "\"\<4.233\>\""], *)
(*SetterBox[Dynamic[BoxForm`var$$], {"4.23"}, "\"\<4.23\>\""], *)
(*SetterBox[Dynamic[BoxForm`var$$], {"0.749"}, "\"\<0.749\>\""]}*)
(*},*)
(*BaselinePosition->{1, 1},*)
(*GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}}],*)
(*Deployed->True],*)
(*SetterBar[Dynamic[BoxForm`var$$], {"4.233", "4.23", "0.749"}]],*)
(*SetterBar[BoxForm`var$$, {"4.233", "4.23", "0.749"}]],*)
(*DynamicModuleValues:>{}],*)
(*Setting[#, {0}]& ],*)
(*($CellContext`mc2 = ToExpression[Setting[#]]; Setting[#])& ]\)<>".wdx"];*)
(*Clear[\[CapitalPhi]A,\[CapitalPhi]B,\[CapitalPhi]C];*)
(*Set@@{\[CapitalPhi]A[Global`x_],Boole[0<=Global`x<=1]\[Phi]xA};*)
(*Set@@{\[CapitalPhi]B[Global`x_],Boole[0<=Global`x<=1]\[Phi]xB};*)
(*Set@@{\[CapitalPhi]C[Global`x_],Boole[0<=Global`x<=1]\[Phi]xC};*)


(* ::Input:: *)
(*{ValsD,\[Phi]xD}=Import["https://github.com/Turgon-Aran-Gondolin/2-d-data/raw/master/acceigenstate_m1-4.23_m2-0.749.wdx"];*)


(* ::Input:: *)
(*{ValsD,\[Phi]xD}=Import["../2-d-data/eigenstate_m-4.23.wdx"];*)


(* ::Input:: *)
(*Clear@\[CapitalPhi]D;*)
(*Set@@{\[CapitalPhi]D[Global`x_],Boole[0<=Global`x<=1]\[Phi]xD};*)


(* ::Input:: *)
(*ValsD*)


(* ::Input:: *)
(*\[Phi]xD[[9]]*)


(* ::Input:: *)
(*\[Lambda]=10^-6;*)


(* ::PageBreak:: *)
(**)


(* ::Input:: *)
(*ListPlot[{ParallelTable[(ValsA[[1]]^2-(ma^2-1)/x-(ma^2-1)/(1-x))\[Phi]xA[[1]]+NIntegrate[\[CapitalPhi]A[y][[1]]/(x-y)^2,{y,0,x-\[Lambda]}]+NIntegrate[\[CapitalPhi]A[y][[1]]/(x-y)^2,{y,x+\[Lambda],1}]-2/\[Lambda] \[CapitalPhi]A[x][[1]],{x,\[Lambda],1-\[Lambda],0.02}],ParallelTable[(ValsA[[2]]^2-(ma^2-1)/x-(ma^2-1)/(1-x))\[Phi]xA[[2]]+NIntegrate[\[CapitalPhi]A[y][[2]]/(x-y)^2,{y,0,x-\[Lambda]}]+NIntegrate[\[CapitalPhi]A[y][[2]]/(x-y)^2,{y,x+\[Lambda],1}]-2/\[Lambda] \[CapitalPhi]A[x][[2]],{x,\[Lambda],1-\[Lambda],0.02}]},Filling->Axis]*)


(* ::Input:: *)
(*ListPlot[{ParallelTable[(ValsB[[1]]^2-(mb^2-1)/x-(mb^2-1)/(1-x)) \[CapitalPhi]B[x][[1]]+NIntegrate[\[CapitalPhi]B[y][[1]]/(x-y)^2,{y,0,x-\[Lambda]}]+NIntegrate[\[CapitalPhi]B[y][[1]]/(x-y)^2,{y,x+\[Lambda],1}]-2/\[Lambda] \[CapitalPhi]B[x][[1]],{x,\[Lambda],1-\[Lambda],0.02}],ParallelTable[(ValsB[[2]]^2-(mb^2-1)/x-(mb^2-1)/(1-x))\[Phi]xB[[2]]+NIntegrate[\[CapitalPhi]B[y][[2]]/(x-y)^2,{y,0,x-\[Lambda]}]+NIntegrate[\[CapitalPhi]B[y][[2]]/(x-y)^2,{y,x+\[Lambda],1}]-2/\[Lambda] \[CapitalPhi]B[x][[2]],{x,\[Lambda],1-\[Lambda],0.02}]},Filling->Axis]*)


(* ::Input:: *)
(*ListPlot[ParallelTable[(ValsC[[1]]^2-(mc1^2-1)/x-(mc2^2-1)/(1-x))\[Phi]xC[[1]]+NIntegrate[\[CapitalPhi]C[y][[1]]/(x-y)^2,{y,0,x-\[Lambda]}]+NIntegrate[\[CapitalPhi]C[y][[1]]/(x-y)^2,{y,x+\[Lambda],1}]-2/\[Lambda] \[CapitalPhi]C[x][[1]],{x,\[Lambda],1-\[Lambda],0.02}],Filling->Axis]*)


(* ::Input:: *)
(*ListPlot[ParallelTable[(ValsD[[3]]^2-(m1^2-1)/x-(m1^2-1)/(1-x))\[Phi]xD[[3]]+NIntegrate[\[CapitalPhi]D[y][[3]]/(x-y)^2,{y,0,x-\[Lambda]}]+NIntegrate[\[CapitalPhi]D[y][[3]]/(x-y)^2,{y,x+\[Lambda],1}]-2/\[Lambda] \[CapitalPhi]D[x][[3]],{x,\[Lambda],1-\[Lambda],0.02}]]*)


(* ::Input:: *)
(*ListPlot[{ParallelTable[(ValsA[[3]]^2-(m^2-1)/x-(m^2-1)/(1-x))\[Phi]xA[[3]]+NIntegrate[\[CapitalPhi]A[y][[3]]/(x-y)^2,{y,0,x-\[Lambda]}]+NIntegrate[\[CapitalPhi]A[y][[3]]/(x-y)^2,{y,x+\[Lambda],1}]-2/\[Lambda] \[CapitalPhi]A[x][[3]],{x,\[Lambda],1-\[Lambda],0.02}]},Filling->Axis]*)


(* ::PageBreak:: *)
(**)


(* ::Input:: *)
(*ListPlot[ParallelTable[(ValsA[[2]]^2-(m^2-1)/x-(m^2-1)/(1-x))\[Phi]xA[[2]]+NIntegrate[\[CapitalPhi]A[y][[2]]/(x-y)^2,{y,0,x-\[Lambda]}]+NIntegrate[\[CapitalPhi]A[y][[2]]/(x-y)^2,{y,x+\[Lambda],1}]-2/\[Lambda] \[CapitalPhi]A[x][[2]],{x,\[Lambda],1-\[Lambda],0.02}]]*)


(* ::Input:: *)
(*ListPlot[ParallelTable[(ValsB[[2]]^2-(m^2-1)/x-(m^2-1)/(1-x))\[Phi]xB[[2]]+NIntegrate[\[CapitalPhi]B[y][[2]]/(x-y)^2,{y,0,x-\[Lambda]}]+NIntegrate[\[CapitalPhi]B[y][[2]]/(x-y)^2,{y,x+\[Lambda],1}]-2/\[Lambda] \[CapitalPhi]B[x][[2]],{x,\[Lambda],1-\[Lambda],0.02}]]*)


(* ::Input:: *)
(*Plot[Evaluate[Take[\[Phi]xA,2]-Take[\[Phi]xB,2]],{x,0,1},PlotStyle->Automatic]*)


(* ::Input:: *)
(*Plot[Evaluate@Take[\[Phi]xB,2],{x,0,1},PlotStyle->Automatic]*)


(* ::Input:: *)
(*\[Phi]xA[[1]]//N*)


(* ::Input:: *)
(*\[Phi]xB[[1]]//N*)


(* ::Code:: *)
(*\[CapitalPhi]B[y][[1]]*)


(* ::Code:: *)
(*Import["D:/Documents/2-d-data/eigenstate_m-u.wdx"]*)


(* ::Input:: *)
(*-1/3561061996595798 (-((49159279 (1-x)^(32187/20000) x^(7813/20000))/73274507)+(32482433 (1-x)^(146983/100000) x^(53017/100000))/192501132+(19570064 (1-x)^(53017/100000) x^(146983/100000))/116515385-(105469081 (1-x)^(7813/20000) x^(32187/20000))/157288971)//N*)


(* ::Input:: *)
(*Manipulate[Plot[x*\[Pi]*Cot[\[Pi]*x]-(1-m1^2),{x,0,2}],{m1,0.1,10}]*)
