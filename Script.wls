#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Code:: *)
(*Quit*)


(* ::Input::Initialization:: *)
SetDirectory[If[MatchQ[$ScriptCommandLine,{}], NotebookDirectory[], Directory[]]];
If[MatchQ[$ScriptCommandLine,{}],Once[LaunchKernels[Input["Kernel Number",16,WindowSize->Small]]],num=ToExpression[$ScriptCommandLine[[2]]];Once[LaunchKernels[num]]];
ParallelEvaluate[Off[General::stop,NIntegrate::slwcon,NIntegrate::precw]];
ParallelEvaluate@SetOptions[NIntegrate(*,MaxRecursion->200,AccuracyGoal->15*)(*,WorkingPrecision->30*),Method->{Automatic,"SymbolicProcessing"->False}];
SetOptions[$Output, FormatType->OutputForm];
(*AppendTo[$Path,NotebookDirectory[]];
Print[$Path];*)
(*AppendTo[$Path,DirectoryName@$InputFileName];*)
(*SetDirectory[If[$InputFileName=="", NotebookDirectory[], Directory[]]];*)


(* ::Input:: *)
(*ParallelEvaluate[Off[NIntegrate::ncvb]];*)


(* ::Input::Initialization:: *)
Get["OneFlavour`"]
Clear[\[Phi]x,m2,\[Phi],vals,M1,M2,M3,\[Phi]1,\[Phi]2,\[Phi]3];
m=4.233; m1=4.233; m2=0.749;
{n1,n2,n3,n4}={2,2,2,2};
Switch[Evaluate@ChoiceDialog["Pick one",{"One Flavour"->1,"Two Flavour"->2,"Three Flavour"->3,"Four Flavour"->4}],1,
Msumdat=Msum[m,{n1,n2,n3,n4},SRange->{10^(-3),7,0.05},Lambda->10^-6,SolveMethod->"'t Hooft"(*,MatrixSize->9*)];
(*Msumdat[[2]]=(<<"data/Msumdat_m-4.233-n-(2-2-1-1).dat")[[2]]~Join~Msumdat[[2]];*)
Put[Msumdat,"data/Msumdat_m-"<>ToString[m]<>"-n-("<>StringDrop[StringJoin@((ToString[#]<>"-")&/@{n1,n2,n3,n4}),-1]<>").dat"],2,
Msumdat=Msum2[{m1,m2},{n1,n2,n3,n4},SRange->{10^-3,19.9,0.03},Lambda->10^-6,SolveMethod->"'t Hooft"(*,MatrixSize->9*)];
Put[Msumdat,"data/Msumdat_m1-"<>ToString[m1]<>"-m2-"<>ToString[m2]<>"-n-("<>StringDrop[StringJoin@((ToString[#]<>"-")&/@{n1,n2,n3,n4}),-1]<>").dat"],3,,4,]
Print["End"];















