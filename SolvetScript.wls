#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Code:: *)
(*Quit[]*)


(* ::Input::Initialization:: *)
Once[LaunchKernels[48]];
ParallelEvaluate[Off[General::stop,NIntegrate::slwcon]];
ParallelEvaluate@SetOptions[NIntegrate,MaxRecursion->100,AccuracyGoal->12];
(*SetDirectory[NotebookDirectory[]];*)
(*AppendTo[$Path,NotebookDirectory[]];*)
(*Print[$Path];*)
SetDirectory[If[$InputFileName == "", NotebookDirectory[], Directory[]]];


(* ::Input::Initialization:: *)
Get["OneFlavour`"];
Clear[\[Phi]x,m2,\[Phi],vals,M1,M2,M3,\[Phi]1,\[Phi]2,\[Phi]3];
m=0.749;


(* ::Input::Initialization:: *)
(*Solvet[m,m,Method->"BSW",MatrixSize->500,Force->True]*)


(* ::Input::Initialization:: *)
Solvet[m,m,Method->"'t Hooft",MatrixSize->15,Force->True]


(* ::Input:: *)
(*{ValsA,\[Phi]xA}=Import["D:/Documents/2-d-data/acceigenstate_m-0.749.wdx"];*)


(* ::Input:: *)
(*{ValsB,\[Phi]xB}=Import["D:/Documents/2-d-data/acceigenstate_m-s.wdx"];*)


(* ::Input:: *)
(*{ValsC,\[Phi]xC}=Import["D:/Documents/2-d-data/eigenstate_m-0.749.wdx"];*)


(* ::Code:: *)
(*Clear[\[CapitalPhi]A,\[CapitalPhi]B];*)
(*Set@@{\[CapitalPhi]A[Global`x_],Boole[0<=Global`x<=1]\[Phi]xA};*)
(*Set@@{\[CapitalPhi]B[Global`x_],Boole[0<=Global`x<=1]\[Phi]xB};*)
(*Set@@{\[CapitalPhi]C[Global`x_],Boole[0<=Global`x<=1]\[Phi]xC};*)


\[Lambda]=10^-6;


(* ::Input:: *)
(*ListPlot[ParallelTable[(ValsA[[1]]^2-(m^2-1)/x-(m^2-1)/(1-x))\[Phi]xA[[1]]+NIntegrate[\[CapitalPhi]A[y][[1]]/(x-y)^2,{y,0,x-\[Lambda]}]+NIntegrate[\[CapitalPhi]A[y][[1]]/(x-y)^2,{y,x+\[Lambda],1}]-2/\[Lambda] \[CapitalPhi]A[x][[1]],{x,\[Lambda],1-\[Lambda],0.02}]]*)


(* ::Input:: *)
(*ListPlot[ParallelTable[(ValsB[[1]]^2-(m^2-1)/x-(m^2-1)/(1-x))\[Phi]xB[[1]]+NIntegrate[\[CapitalPhi]B[y][[1]]/(x-y)^2,{y,0,x-\[Lambda]}]+NIntegrate[\[CapitalPhi]B[y][[1]]/(x-y)^2,{y,x+\[Lambda],1}]-2/\[Lambda] \[CapitalPhi]B[x][[1]],{x,\[Lambda],1-\[Lambda],0.02}]]*)


(* ::Input:: *)
(*ListPlot[ParallelTable[(ValsC[[1]]^2-(m^2-1)/x-(m^2-1)/(1-x))\[Phi]xC[[1]]+NIntegrate[\[CapitalPhi]C[y][[1]]/(x-y)^2,{y,0,x-\[Lambda]}]+NIntegrate[\[CapitalPhi]C[y][[1]]/(x-y)^2,{y,x+\[Lambda],1}]-2/\[Lambda] \[CapitalPhi]C[x][[1]],{x,\[Lambda],1-\[Lambda],0.02}]]*)


(* ::Input:: *)
(*ListPlot[ParallelTable[(ValsA[[2]]^2-(m^2-1)/x-(m^2-1)/(1-x))\[Phi]xA[[2]]+NIntegrate[\[CapitalPhi]A[y][[2]]/(x-y)^2,{y,0,x-\[Lambda]}]+NIntegrate[\[CapitalPhi]A[y][[2]]/(x-y)^2,{y,x+\[Lambda],1}]-2/\[Lambda] \[CapitalPhi]A[x][[2]],{x,\[Lambda],1-\[Lambda],0.02}]]*)


(* ::Input:: *)
(*ListPlot[ParallelTable[(ValsB[[2]]^2-(m^2-1)/x-(m^2-1)/(1-x))\[Phi]xB[[2]]+NIntegrate[\[CapitalPhi]B[y][[2]]/(x-y)^2,{y,0,x-\[Lambda]}]+NIntegrate[\[CapitalPhi]B[y][[2]]/(x-y)^2,{y,x+\[Lambda],1}]-2/\[Lambda] \[CapitalPhi]B[x][[2]],{x,\[Lambda],1-\[Lambda],0.02}]]*)


(* ::Input:: *)
(*Plot[Evaluate[Take[\[Phi]xA,2]-Take[\[Phi]xB,2]],{x,0,1},PlotStyle->Automatic]*)


(* ::Input:: *)
(*Plot[Evaluate@Take[\[Phi]xB,2],{x,0,1},PlotStyle->Automatic]*)


(* ::Input:: *)
(*\[Phi]xA[[1]]//Expand//N*)


(* ::Input:: *)
(*\[Phi]xB[[1]]//Expand//N*)


(* ::Code:: *)
(*\[CapitalPhi]B[y]*)


(* ::Code:: *)
(*Import["D:/Documents/2-d-data/eigenstate_m-u.wdx"]*)
