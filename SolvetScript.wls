#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Input::Initialization:: *)
Once[LaunchKernels[32]]
ParallelEvaluate[Off[General::stop,NIntegrate::slwcon]];
ParallelEvaluate@SetOptions[NIntegrate,MaxRecursion->100,AccuracyGoal->10];
(*SetDirectory[NotebookDirectory[]];*)
(*AppendTo[$Path,NotebookDirectory[]];*)
(*Print[$Path];*)
Get["OneFlavour`"];
Clear[\[Phi]x,m2,\[Phi],vals,M1,M2,M3,\[Phi]1,\[Phi]2,\[Phi]3];
m=4.23;
Solvet[m,m,Method->"'t Hooft",MatrixSize->9];
