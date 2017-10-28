#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Code:: *)
(*Quit*)


(* ::Input::Initialization:: *)
Once[LaunchKernels[32]]
ParallelEvaluate[Off[General::stop,NIntegrate::slwcon]];
ParallelEvaluate@SetOptions[NIntegrate,MaxRecursion->100,AccuracyGoal->15];
SetOptions[$Output, FormatType->OutputForm];
(*AppendTo[$Path,NotebookDirectory[]];
Print[$Path];*)
(*AppendTo[$Path,DirectoryName@$InputFileName];*)
SetDirectory[If[$InputFileName=="", NotebookDirectory[], Directory[]]];


(* ::Input::Initialization:: *)
Get["OneFlavour`"]
Clear[\[Phi]x,m2,\[Phi],vals,M1,M2,M3,\[Phi]1,\[Phi]2,\[Phi]3];
m=0.749;
{n1,n2,n3,n4}={0,0,0,0};
Msumdat=Msum[m,{n1,n2,n3,n4},SRange->{10^-3,3,0.02},Lambda->10^-5,Method->"'t Hooft",MatrixSize->9];
Put[Msumdat,"data/Msumdat_m-"<>ToString[m]<>"-n-("<>StringDrop[StringJoin@((ToString[#]<>"-")&/@{n1,n2,n3,n4}),-1]<>").dat"]
Print["End"];
