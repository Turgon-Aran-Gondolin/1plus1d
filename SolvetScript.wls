#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Code:: *)
(*Quit[]*)


(* ::Input::Initialization:: *)
Once[LaunchKernels[32]];
ParallelEvaluate[Off[General::stop,NIntegrate::slwcon]];
ParallelEvaluate@SetOptions[NIntegrate,MaxRecursion->100,AccuracyGoal->10];
(*SetDirectory[NotebookDirectory[]];*)
(*AppendTo[$Path,NotebookDirectory[]];*)
(*Print[$Path];*)
SetDirectory[If[$InputFileName == "", NotebookDirectory[], Directory[]]];



(* ::Input::Initialization:: *)
Get["OneFlavour`"];
Clear[\[Phi]x,m2,\[Phi],vals,M1,M2,M3,\[Phi]1,\[Phi]2,\[Phi]3];
m=0.749;
Solvet[m,m,Method->"'t Hooft",MatrixSize->9,Force->True]


(* ::Code:: *)
(*{ValsB,\[Phi]xB}=Import["D:/Documents/2-d-data/acceigenstate_m-u.wdx"];*)


(* ::Code:: *)
(*Clear[\[CapitalPhi]B];*)
(*Set@@{\[CapitalPhi]B[Global`x_],Boole[0<=Global`x<=1]\[Phi]xB};*)


(* ::Code:: *)
(*\[CapitalPhi]B[y]*)


(* ::Code:: *)
(*Import["D:/Documents/2-d-data/eigenstate_m-u.wdx"]*)
