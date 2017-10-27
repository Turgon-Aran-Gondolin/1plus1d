#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Input::Initialization:: *)
Once[LaunchKernels[32]]
ParallelEvaluate[Off[General::stop,NIntegrate::slwcon]];
ParallelEvaluate@SetOptions[NIntegrate,MaxRecursion->100,AccuracyGoal->10];
(*AppendTo[$Path,NotebookDirectory[]];
Print[$Path];*)
Get["OneFlavour`"]
Clear[\[Phi]x,m2,\[Phi],vals,M1,M2,M3,\[Phi]1,\[Phi]2,\[Phi]3];
m=4.23;
{n1,n2,n3,n4}={2,2,1,1};
Msumdat=Msum[m,{n1,n2,n3,n4},SRange->{10^-3,1,0.03},Lambda->10^-6];
Put[Msumdat,"data/Msumdat_m-"<>ToString[m]<>"-n-("<>StringDrop[StringJoin@((ToString[#]<>"-")&/@{n1,n2,n3,n4}),-1]<>").dat"]
