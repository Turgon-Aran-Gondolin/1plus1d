#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Code:: *)
(*Quit[]*)


(* ::Input::Initialization:: *)
Once[LaunchKernels[32]];
ParallelEvaluate[Off[General::stop,NIntegrate::slwcon]];
ParallelEvaluate@SetOptions[NIntegrate,MaxRecursion->100,AccuracyGoal->12];
(*SetDirectory[NotebookDirectory[]];*)
(*AppendTo[$Path,NotebookDirectory[]];*)
(*Print[$Path];*)
SetDirectory[If[$InputFileName == "", NotebookDirectory[], Directory[]]];


(* ::Input::Initialization:: *)
Get["OneFlavour`"];
Clear[\[Phi]x,m2,\[Phi],vals,M1,M2,M3,\[Phi]1,\[Phi]2,\[Phi]3];
m=0.749;m1=4.23;m2=0.749;


(* ::Input::Initialization:: *)
(*Solvet[m,m,Method->"BSW",MatrixSize->500,Force->True]*)


(* ::Input::Initialization:: *)
Solvet[m1,m2,Method->"'t Hooft",MatrixSize->11,Force->True]
Print["End"];



(* ::Input:: *)
(*{ValsA,\[Phi]xA}=Import["D:/Documents/2-d-data/acceigenstate_m-0.749.wdx"];*)


(* ::Input:: *)
(*Table[\[Phi]xA[[i]]=-(-1)^i \[Phi]xA[[i]],{i,1,Length@\[Phi]xA}];*)


(* ::Input:: *)
(*{ValsA,\[Phi]xA}=Import["E:/Downloads/acceigenstate_m-0.749.wdx"];*)


(* ::Input:: *)
(*{ValsA,\[Phi]xA}=Import["E:/Downloads/acceigenstate_m-0.749(1).wdx"];*)


(* ::Input:: *)
(*{ValsB,\[Phi]xB}=Import["D:/Documents/2-d-data/acceigenstate_m-s.wdx"];*)


(* ::Input:: *)
(*{ValsC,\[Phi]xC}=Import["D:/Documents/2-d-data/eigenstate_m-0.749.wdx"];*)


(* ::Input:: *)
(*Clear[\[CapitalPhi]A,\[CapitalPhi]B,\[CapitalPhi]C];*)
(*Set@@{\[CapitalPhi]A[Global`x_],Boole[0<=Global`x<=1]\[Phi]xA};*)
(*Set@@{\[CapitalPhi]B[Global`x_],Boole[0<=Global`x<=1]\[Phi]xB};*)
(*Set@@{\[CapitalPhi]C[Global`x_],Boole[0<=Global`x<=1]\[Phi]xC};*)


(* ::Input:: *)
(*{ValsD,\[Phi]xD}=Import["https://github.com/Turgon-Aran-Gondolin/2-d-data/raw/master/acceigenstate_m1-4.23_m2-0.749.wdx"];*)
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
(*ListPlot[{ParallelTable[(ValsA[[1]]^2-(m^2-1)/x-(m^2-1)/(1-x))\[Phi]xA[[1]]+NIntegrate[\[CapitalPhi]A[y][[1]]/(x-y)^2,{y,0,x-\[Lambda]}]+NIntegrate[\[CapitalPhi]A[y][[1]]/(x-y)^2,{y,x+\[Lambda],1}]-2/\[Lambda] \[CapitalPhi]A[x][[1]],{x,\[Lambda],1-\[Lambda],0.02}],ParallelTable[(ValsA[[2]]^2-(m^2-1)/x-(m^2-1)/(1-x))\[Phi]xA[[2]]+NIntegrate[\[CapitalPhi]A[y][[2]]/(x-y)^2,{y,0,x-\[Lambda]}]+NIntegrate[\[CapitalPhi]A[y][[2]]/(x-y)^2,{y,x+\[Lambda],1}]-2/\[Lambda] \[CapitalPhi]A[x][[2]],{x,\[Lambda],1-\[Lambda],0.02}]},Filling->Axis]*)


(* ::Input:: *)
(*ListPlot[{ParallelTable[(ValsB[[1]]^2-(m^2-1)/x-(m^2-1)/(1-x))\[Phi]xB[[1]]+NIntegrate[\[CapitalPhi]B[y][[1]]/(x-y)^2,{y,0,x-\[Lambda]}]+NIntegrate[\[CapitalPhi]B[y][[1]]/(x-y)^2,{y,x+\[Lambda],1}]-2/\[Lambda] \[CapitalPhi]B[x][[1]],{x,\[Lambda],1-\[Lambda],0.02}],ParallelTable[(ValsB[[2]]^2-(m^2-1)/x-(m^2-1)/(1-x))\[Phi]xB[[2]]+NIntegrate[\[CapitalPhi]B[y][[2]]/(x-y)^2,{y,0,x-\[Lambda]}]+NIntegrate[\[CapitalPhi]B[y][[2]]/(x-y)^2,{y,x+\[Lambda],1}]-2/\[Lambda] \[CapitalPhi]B[x][[2]],{x,\[Lambda],1-\[Lambda],0.02}]},Filling->Axis]*)


(* ::Input:: *)
(*ListPlot[ParallelTable[(ValsC[[1]]^2-(m^2-1)/x-(m^2-1)/(1-x))\[Phi]xC[[1]]+NIntegrate[\[CapitalPhi]C[y][[1]]/(x-y)^2,{y,0,x-\[Lambda]}]+NIntegrate[\[CapitalPhi]C[y][[1]]/(x-y)^2,{y,x+\[Lambda],1}]-2/\[Lambda] \[CapitalPhi]C[x][[1]],{x,\[Lambda],1-\[Lambda],0.02}]]*)


(* ::Input:: *)
(*ListPlot[ParallelTable[(ValsD[[9]]^2-(m1^2-1)/x-(m2^2-1)/(1-x))\[Phi]xD[[9]]+NIntegrate[\[CapitalPhi]D[y][[9]]/(x-y)^2,{y,0,x-\[Lambda]}]+NIntegrate[\[CapitalPhi]D[y][[9]]/(x-y)^2,{y,x+\[Lambda],1}]-2/\[Lambda] \[CapitalPhi]D[x][[9]],{x,\[Lambda],1-\[Lambda],0.02}]]*)


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
