#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Code:: *)
(*Quit*)


(* ::Input::Initialization:: *)
Once[LaunchKernels[8]]
ParallelEvaluate[Off[General::stop,NIntegrate::slwcon,NIntegrate::precw]];
ParallelEvaluate@SetOptions[NIntegrate,MaxRecursion->200,AccuracyGoal->15,PrecisionGoal->8(*,WorkingPrecision->25*)];
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
time=SessionTime[];
Msumdat=Msum[m,{n1,n2,n3,n4},SRange->{10^-3,1.9,0.2},Lambda->5.*10^-7,Method->"'t Hooft",MatrixSize->9,Sequence@@Map[#->{Method->(*"ClenshawCurtisRule"*)"LevinRule"}&,{I1Option,I2Option,I3Option}]];
Print[SessionTime[]-time];
Put[Msumdat,"data/Msumdat_m-"<>ToString[m]<>"-n-("<>StringDrop[StringJoin@((ToString[#]<>"-")&/@{n1,n2,n3,n4}),-1]<>").dat"]
Print["End"];



