#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Code:: *)
(*Quit*)


(* ::Input::Initialization:: *)
Once[LaunchKernels[8]]
ParallelEvaluate[Off[General::stop,NIntegrate::slwcon,NIntegrate::precw]];
ParallelEvaluate@SetOptions[NIntegrate(*,MaxRecursion->200,AccuracyGoal->15*)(*,WorkingPrecision->30*),Method->{Automatic,"SymbolicProcessing"->False}];
SetOptions[$Output, FormatType->OutputForm];
(*AppendTo[$Path,NotebookDirectory[]];
Print[$Path];*)
(*AppendTo[$Path,DirectoryName@$InputFileName];*)
SetDirectory[If[$InputFileName=="", NotebookDirectory[], Directory[]]];


(* ::Input:: *)
(*ParallelEvaluate[Off[NIntegrate::ncvb]];*)


(* ::Input::Initialization:: *)
Get["OneFlavour`"]
Clear[\[Phi]x,m2,\[Phi],vals,M1,M2,M3,\[Phi]1,\[Phi]2,\[Phi]3];
m=4.23;
{n1,n2,n3,n4}={0,0,0,0};
Msumdat=Msum[m,{n1,n2,n3,n4},SRange->{10^-3,1.9,0.03},Lambda->10^-6,Method->"'t Hooft",MatrixSize->9];
Put[Msumdat,"data/Msumdat_m-"<>ToString[m]<>"-n-("<>StringDrop[StringJoin@((ToString[#]<>"-")&/@{n1,n2,n3,n4}),-1]<>").dat"]
Print["End"];



