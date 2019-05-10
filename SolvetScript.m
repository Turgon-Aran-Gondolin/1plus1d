(* ::Package:: *)

(* ::Code:: *)
(*Quit[]*)


(* ::Input::Initialization:: *)
<<MMARemoteSSH`
Parallel`Settings`$MathLinkTimeout=30;
(*LaunchRemoteKernels[];*)
LaunchKernels[4];
Kernels[]






(* ::Input::Initialization:: *)
(*If[$InputFileName == "",Once[LaunchKernels[16]],num=ToExpression[$ScriptCommandLine[[2]]];Once[LaunchKernels[num]]];*)
ParallelEvaluate[Off[General::stop,NIntegrate::slwcon]];
ParallelEvaluate@SetOptions[NIntegrate,MaxRecursion->100,AccuracyGoal->12];
(*SetDirectory[NotebookDirectory[]];*)
(*AppendTo[$Path,NotebookDirectory[]];*)
(*Print[$Path];*)


(* ::Input::Initialization:: *)
SetDirectory[If[$InputFileName == "", NotebookDirectory[], Directory[]]];
Get["OneFlavour`"];
Clear[\[Phi]x,m2,\[Phi],vals,M1,M2,M3,\[Phi]1,\[Phi]2,\[Phi]3];
m=4.19022;m1=4.19022;m2=0.09;
MC[m_]:=Which[ToString[m]=="c",4.233,ToString[m]=="s",0.749,ToString[m]=="u",0.045,ToString[m]=="chiralu",0]


(* ::Input::Initialization:: *)
(*Bottomonium and b quark mass*)

(*m=13.5565;
Solvet[m,m,SolveMethod->"BSW",MatrixSize\[Rule]500,Force->True]
Print["End"];*)



(*Charmonium and c quark mass*)

(*m=4.19022;
Solvet[m,m,SolveMethod->"BSW",MatrixSize\[Rule]100,Force->True]
Print["End"];*)


(* ::Section::Closed:: *)
(*Evaluate*)


(* ::Input::Initialization:: *)
Solvet[m1,m2,SolveMethod->"'t Hooft",MatrixSize->15,Force->True]
Print["End"];



Solvet[4.19022,0.045,SolveMethod->"'t Hooft",MatrixSize->15]
Print["End"];


Solvet[0.749,0.045,SolveMethod->"'t Hooft",MatrixSize->15]
Print["End"];


Solvet[13.5565,0.045,SolveMethod->"'t Hooft",MatrixSize->15]
Print["End"];


Solvet[4.19022,0.045,SolveMethod->"'t Hooft",MatrixSize->15]
Print["End"];


Solvet[0.749,0.5,SolveMethod->"'t Hooft",MatrixSize->15,Force->True]
Print["End"];


(* ::Input::Initialization:: *)
Solvet[6,4.19022,SolveMethod->"'t Hooft",MatrixSize->15,Force->True]
Print["End"];


(* ::Input::Initialization:: *)
Solvet[6,0.5,SolveMethod->"'t Hooft",MatrixSize->15,Force->True]
Print["End"];


(* ::Code:: *)
(*(*DynamicSetting[(ma=ToExpression[Setting[#]];Setting[#])&,SetterBar["4.233",{"4.233","4.23","0.749"}]]; Shift+Ctrl+Enter*)*)


(* ::Code:: *)
(*(*DynamicSetting[SetterBar["acceigenstate",{"acceigenstate","eigenstate"}]]*)*)


(* ::Code:: *)
(*(*DynamicSetting[(mb=MC[Setting[#]];Setting[#])&,SetterBar["c",{"c","s","u","chiralu"}]]*)*)


(* ::Section::Closed:: *)
(*Verify*)


(* ::Input:: *)
(*{ValsA,\[Phi]xA}=Import["D:/Documents/2-d-data/acceigenstate_m-4.233.wdx"];*)


(* ::Input:: *)
(*Table[\[Phi]xA[[i]]=(-1)^i \[Phi]xA[[i]],{i,1,Length@\[Phi]xA}];*)


(* ::Input:: *)
(*{ValsB,\[Phi]xB}=Import["../2-d-data/acceigenstate_m-c.wdx"];*)


(* ::Input:: *)
(*{ValsB,\[Phi]xB}=Import["../2-d-data/acceigenstate_m-s.wdx"];*)


(* ::Input:: *)
(*{ValsC,\[Phi]xC}=Import["D:/Documents/2-d-data/eigenstate_m-0.749.wdx"];*)


(* ::Input:: *)
(*{ValsC,\[Phi]xC}=Import["../2-d-data/acceigenstate_m1-0.749_m2-0.5.wdx"];*)


(* ::Code:: *)
(*Clear[\[CapitalPhi]A,\[CapitalPhi]B,\[CapitalPhi]C];*)
(*Set@@{\[CapitalPhi]A[Global`x_],Boole[0<=Global`x<=1]\[Phi]xA};*)
(*Set@@{\[CapitalPhi]B[Global`x_],Boole[0<=Global`x<=1]\[Phi]xB};*)
(*Set@@{\[CapitalPhi]C[Global`x_],Boole[0<=Global`x<=1]\[Phi]xC};*)


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
(*SetterBox[Dynamic[BoxForm`var$$], {"4.19022"}, "\"\<4.19022\>\""], *)
(*SetterBox[Dynamic[BoxForm`var$$], {"0.749"}, "\"\<0.749\>\""]}*)
(*},*)
(*BaselinePosition->{1, 1},*)
(*GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}}],*)
(*Deployed->True],*)
(*SetterBar[Dynamic[BoxForm`var$$], {"4.233", "4.23", "4.19022", "0.749"}]],*)
(*SetterBar[BoxForm`var$$, {"4.233", "4.23", "4.19022", "0.749"}]],*)
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
(*DynamicModuleBox[{BoxForm`var$$ = "4.19022"}, *)
(*InterpretationBox[*)
(*InterpretationBox[*)
(*StyleBox[GridBox[{*)
(*{*)
(*SetterBox[Dynamic[BoxForm`var$$], {"4.233"}, "\"\<4.233\>\""], *)
(*SetterBox[Dynamic[BoxForm`var$$], {"4.23"}, "\"\<4.23\>\""], *)
(*SetterBox[Dynamic[BoxForm`var$$], {"4.19022"}, "\"\<4.19022\>\""], *)
(*SetterBox[Dynamic[BoxForm`var$$], {"0.749"}, "\"\<0.749\>\""]}*)
(*},*)
(*BaselinePosition->{1, 1},*)
(*GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}}],*)
(*Deployed->True],*)
(*SetterBar[Dynamic[BoxForm`var$$], {"4.233", "4.23", "4.19022", "0.749"}]],*)
(*SetterBar[BoxForm`var$$, {"4.233", "4.23", "4.19022", "0.749"}]],*)
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
(*SetterBox[Dynamic[BoxForm`var$$], {"4.19022"}, "\"\<4.19022\>\""], *)
(*SetterBox[Dynamic[BoxForm`var$$], {"0.749"}, "\"\<0.749\>\""]}*)
(*},*)
(*BaselinePosition->{1, 1},*)
(*GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}}],*)
(*Deployed->True],*)
(*SetterBar[Dynamic[BoxForm`var$$], {"4.233", "4.23", "4.19022", "0.749"}]],*)
(*SetterBar[BoxForm`var$$, {"4.233", "4.23", "4.19022", "0.749"}]],*)
(*DynamicModuleValues:>{}],*)
(*Setting[#, {0}]& ],*)
(*($CellContext`mc2 = ToExpression[Setting[#]]; Setting[#])& ]\)<>".wdx"];*)
(*Clear[\[CapitalPhi]A,\[CapitalPhi]B,\[CapitalPhi]C];*)
(*Set@@{\[CapitalPhi]A[Global`x_],Boole[0<=Global`x<=1]\[Phi]xA};*)
(*Set@@{\[CapitalPhi]B[Global`x_],Boole[0<=Global`x<=1]\[Phi]xB};*)
(*Set@@{\[CapitalPhi]C[Global`x_],Boole[0<=Global`x<=1]\[Phi]xC};*)


ValsA


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
(*\[Lambda]=10^-6;mc2=0.5;mc1=0.749;*)


(* ::PageBreak:: *)
(**)


(* ::Input:: *)
(*ListPlot[{ParallelTable[(ValsA[[1]]^2-(ma^2-1)/x-(ma^2-1)/(1-x))\[Phi]xA[[1]]+NIntegrate[\[CapitalPhi]A[y][[1]]/(x-y)^2,{y,0,x-\[Lambda]}]+NIntegrate[\[CapitalPhi]A[y][[1]]/(x-y)^2,{y,x+\[Lambda],1}]-2/\[Lambda] \[CapitalPhi]A[x][[1]],{x,\[Lambda],1-\[Lambda],0.02}],ParallelTable[(ValsA[[2]]^2-(ma^2-1)/x-(ma^2-1)/(1-x))\[Phi]xA[[2]]+NIntegrate[\[CapitalPhi]A[y][[2]]/(x-y)^2,{y,0,x-\[Lambda]}]+NIntegrate[\[CapitalPhi]A[y][[2]]/(x-y)^2,{y,x+\[Lambda],1}]-2/\[Lambda] \[CapitalPhi]A[x][[2]],{x,\[Lambda],1-\[Lambda],0.02}]},Filling->Axis]*)


(* ::Input:: *)
(*ListPlot[{ParallelTable[(ValsB[[1]]^2-(mb^2-1)/x-(mb^2-1)/(1-x)) \[CapitalPhi]B[x][[1]]+NIntegrate[\[CapitalPhi]B[y][[1]]/(x-y)^2,{y,0,x-\[Lambda]}]+NIntegrate[\[CapitalPhi]B[y][[1]]/(x-y)^2,{y,x+\[Lambda],1}]-2/\[Lambda] \[CapitalPhi]B[x][[1]],{x,\[Lambda],1-\[Lambda],0.02}],ParallelTable[(ValsB[[2]]^2-(mb^2-1)/x-(mb^2-1)/(1-x))\[Phi]xB[[2]]+NIntegrate[\[CapitalPhi]B[y][[2]]/(x-y)^2,{y,0,x-\[Lambda]}]+NIntegrate[\[CapitalPhi]B[y][[2]]/(x-y)^2,{y,x+\[Lambda],1}]-2/\[Lambda] \[CapitalPhi]B[x][[2]],{x,\[Lambda],1-\[Lambda],0.02}]},Filling->Axis]*)


(* ::Input:: *)
(*ListPlot[ParallelTable[(ValsC[[#]]^2-(mc1^2-1)/x-(mc2^2-1)/(1-x))\[Phi]xC[[#]]+NIntegrate[\[CapitalPhi]C[y][[#]]/(x-y)^2,{y,0,x-\[Lambda]}]+NIntegrate[\[CapitalPhi]C[y][[#]]/(x-y)^2,{y,x+\[Lambda],1}]-2/\[Lambda] \[CapitalPhi]C[x][[#]],{x,\[Lambda],1-\[Lambda],0.02}],Filling->Axis]&@1*)


(* ::Input:: *)
(*ListPlot[ParallelTable[(1/(ValsC[[#]]^2\[Phi]xC[[#]]))((ValsC[[#]]^2-(mc1^2-1)/x-(mc2^2-1)/(1-x))\[Phi]xC[[#]]+NIntegrate[\[CapitalPhi]C[y][[#]]/(x-y)^2,{y,0,x-\[Lambda]}]+NIntegrate[\[CapitalPhi]C[y][[#]]/(x-y)^2,{y,x+\[Lambda],1}]-2/\[Lambda] \[CapitalPhi]C[x][[#]]),{x,\[Lambda],1-\[Lambda],0.02}],Filling->Axis]&@3*)


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


(* ::Section:: *)
(*Plot*)


Flavour[val_]:=Switch[val,4.19022,"c",0.749,"s",13.5565,"b",0.09,"d",0.045,"u"];
FM[val_]:=Switch[val,"c",4.19022,"s",0.749,"b",13.5565,"d",0.09,"u",0.045];
StringOverbar[s_]:="\!\(\*OverscriptBox[\("<>s<>"\), \(_\)]\)";
Legend[m1_,m2_,pos_]:=Placed[Framed(*Panel*)[Flavour[m1]<>StringOverbar[Flavour[m2]]],{Corner@pos}];
Corner[num_]:=Switch[num,1,{Left,Top},2,{Right,Top},3,{Left,Bottom},4,{Right,Bottom}];


m1=FM@"b";m2=FM@"u";
{Val,\[Phi][x_]}=Solvet[m1,m2,SolveMethod->"'t Hooft",MatrixSize->15];


SetOptions[$FrontEnd, PrintingStyleEnvironment -> "Working"]


(* ::Input::Initialization:: *)
fig=Plot[\[Phi][x][[1]],{x,0,1},PlotRange->All,Frame->True,PlotStyle->None,MaxRecursion->15,Filling->Axis,FillingStyle->Gray]


(* ::Input::Initialization:: *)
fig=Plot[\[Phi][x][[1]],{x,0,1},PlotRange->All,Frame->True,PlotStyle->Black,FrameLabel->{"x","\[Phi](x)"}(*,LabelStyle\[Rule]Medium*),ImageSize->200 (*,FrameTicks\[Rule]{{Range[0,2,0.2],Automatic}, Automatic}*) (*,MaxRecursion\[Rule]15,WorkingPrecision\[Rule]30,PlotPoints\[Rule]10^5*)]


(* ::Input::Initialization:: *)
fig=Labeled[Plot[\[Phi][x][[1]],{x,0,1},PlotRange->All,Frame->True,PlotStyle->Black(*,FrameLabel\[Rule]{"x","\[Phi](x)"}*) ,ImageSize->200,PlotLegends->Legend[m1,m2,3]],{"x","\[Phi](x)"},Reverse/@{{Bottom,Right},{Left,Top}},RotateLabel->False,LabelStyle->{FontFamily->"Arial"}]


(* ::Input::Initialization:: *)
fig=Labeled[ListLinePlot[Table[{x,\[Phi][x][[1]]},{x,Range[0,0.99,0.01]~Join~Range[0.99,1,10^-4]}],PlotRange->All,Frame->True,PlotStyle->Black,ImageSize->200,PlotLegends->Legend[m1,m2,1]],{"x","\[Phi](x)"},Reverse/@{{Bottom,Right},{Left,Top}},RotateLabel->False,LabelStyle->{FontFamily->"Arial"}]


(* ::Input::Initialization:: *)
Export["~/Github/2DScattering/NumFig/wf-"<>StringJoin@Map[Flavour,{m1,m2}]<>".pdf",fig,"PDF"]
Import[%,"PDF"]


(* ::Input::Initialization:: *)
Export["~/Github/2DScattering/NumFig/wf-"<>StringJoin@Map[Flavour,{m1,m2}]<>".png",fig]
Import[%]


DeleteFile["~/Github/2DScattering/NumFig/wf-cc.png"]
