#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Code:: *)
(*Quit*)


(* ::Input::Initialization:: *)
(*<< MMARemoteSSH`
LaunchRemoteKernels[];*)
LaunchKernels[4];
Kernels[]


(* ::Input::Initialization:: *)
SetDirectory[If[MatchQ[$ScriptCommandLine,{}], NotebookDirectory[], Directory[]]];
(*If[MatchQ[$ScriptCommandLine,{}],Once[LaunchKernels[Input["Kernel Number",16,WindowSize->Small]]],num=ToExpression[$ScriptCommandLine[[2]]];Once[LaunchKernels[num]]];*)
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
Clear[ϕx,m2,ϕ,vals,M1,M2,M3,ϕ1,ϕ2,ϕ3];
m=13.5565; m1=4.19022; m2=0.749;
{n1,n2,n3,n4}={2,2,0,0};
Switch[Evaluate@ChoiceDialog["Pick one",{"One Flavour"->1,"Two Flavour"->2,"Three Flavour"->3,"Four Flavour"->4}],1,
Msumdat=Msum[m,{n1,n2,n3,n4},SRange->{10^(-3),8,0.1},Lambda->10^-6(*,SolveMethod->"'t Hooft",MatrixSize->9*)];
If[FailureQ[#],Null,Msumdat[[2]]=Sort[#[[2]]~Join~Msumdat[[2]]]]&[(Get@("data/Msumdat_m-13.5565-n-("<>ToString[n1]<>"-"<>ToString[n2]<>"-"<>ToString[n3]<>"-"<>ToString[n4]<>").dat"))];
Put[Msumdat,"data/Msumdat_m-"<>ToString[m]<>"-n-("<>StringDrop[StringJoin@((ToString[#]<>"-")&/@{n1,n2,n3,n4}),-1]<>").dat"],2,
Msumdat=Msum2[{m1,m2},{n1,n2,n3,n4},SRange->{20,1000,10},Lambda->10^-6,SolveMethod->"'t Hooft"(*,MatrixSize->9*)];
If[FailureQ[#],Null,Msumdat[[2]]=Sort[#[[2]]~Join~Msumdat[[2]]]]&[(Get@("data/Msumdat_m1-4.19022-m2-0.749-n-("<>ToString[n1]<>"-"<>ToString[n2]<>"-"<>ToString[n3]<>"-"<>ToString[n4]<>").dat"))];
Put[Msumdat,"data/Msumdat_m1-"<>ToString[m1]<>"-m2-"<>ToString[m2]<>"-n-("<>StringDrop[StringJoin@((ToString[#]<>"-")&/@{n1,n2,n3,n4}),-1]<>").dat"],3,Null,4,Null]
Print["End"];
(*SendMail[
  "To" -> "huangys@ihep.ac.cn",
  "Subject" -> "Computation finished",
  "Body" -> "Computation finished"
  ];*)


















