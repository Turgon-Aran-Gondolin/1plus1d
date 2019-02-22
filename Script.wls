#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Code:: *)
(*Quit*)


(* ::Input::Initialization:: *)
<< MMARemoteSSH`
(*LaunchRemoteKernels[];*)
LaunchKernels[8];
Kernels[]


(* ::Input::Initialization:: *)
SetDirectory[If[MatchQ[$ScriptCommandLine,{}], NotebookDirectory[], Directory[]]];
(*If[MatchQ[$ScriptCommandLine,{}],Once[LaunchKernels[Input["Kernel Number",16,WindowSize->Small]]],num=ToExpression[$ScriptCommandLine[[2]]];Once[LaunchKernels[num]]];*)
ParallelEvaluate[Off[General::stop,NIntegrate::slwcon,NIntegrate::precw]];
ParallelEvaluate@SetOptions[NIntegrate,MaxRecursion->200(*,AccuracyGoal->15*)(*,WorkingPrecision->30*)];
SetOptions[$Output, FormatType->OutputForm];
(*AppendTo[$Path,NotebookDirectory[]];
Print[$Path];*)
(*AppendTo[$Path,DirectoryName@$InputFileName];*)
(*SetDirectory[If[$InputFileName=="", NotebookDirectory[], Directory[]]];*)


(* ::Input:: *)
(*ParallelEvaluate[Off[NIntegrate::ncvb]];*)


(* ::Input::Initialization:: *)
Get["OneFlavour`"]
If[$PVM=="Differential",(ParallelEvaluate[#];#)&@Unevaluated[SetOptions[NIntegrate,Method->{Automatic,"SymbolicProcessing"->False}]],(ParallelEvaluate[#];#)&@Unevaluated[SetOptions[NIntegrate,Method->{Automatic,"SymbolicProcessing"->True}]]];
Clear[\[Phi]x,m2,\[Phi],vals,M1,M2,M3,\[Phi]1,\[Phi]2,\[Phi]3];
m=4.19022; mQ=13.5565; mq=0.045;md=0.09;
{n1,n2,n3,n4}={1,1,1,1};
Switch[Evaluate@ChoiceDialog["Pick one",{"One Flavour"->1,"Two Flavour"->2,"Three Flavour"->3,"Four Flavour"->4}],1,
Msumdat=Msum[m,{n1,n2,n3,n4},SRange->{2 10^(-3),1,0.03},Lambda->10^-6,SolveMethod->"'t Hooft"(*,MatrixSize->9*)];
If[FailureQ[#],,If[ChoiceDialog["Discard old result or not? ",{"Yes"->False,"No"->True}],Msumdat[[2]]=DeleteDuplicatesBy[Sort[#[[2]]~Join~Msumdat[[2]]],First],Null]]&[(<<("data/Msumdat_m-4.19022-n-("<>ToString[n1]<>"-"<>ToString[n2]<>"-"<>ToString[n3]<>"-"<>ToString[n4]<>").dat"))];
Put[Msumdat,"data/Msumdat_m-"<>ToString[m]<>"-n-("<>StringDrop[StringJoin@((ToString[#]<>"-")&/@{n1,n2,n3,n4}),-1]<>").dat"],
2,
Msumdat=Msum2[{mQ,mq},{n1,n2,n3,n4},SRange->{10^(-3),1000,10},Lambda->10^-6,SolveMethod->"'t Hooft"(*,MatrixSize->9*)];
If[FailureQ[#],,If[ChoiceDialog["Discard old result or not? ",{"Yes"->False,"No"->True}],Msumdat[[2]]=Sort[#[[2]]~Join~Msumdat[[2]]],Null]]&[(<<("data/Msumdat_m1-4.19022-m2-0.749-n-("<>ToString[n1]<>"-"<>ToString[n2]<>"-"<>ToString[n3]<>"-"<>ToString[n4]<>").dat"))];
Put[Msumdat,"data/Msumdat_m1-"<>ToString[mQ]<>"-m2-"<>ToString[mq]<>"-n-("<>StringDrop[StringJoin@((ToString[#]<>"-")&/@{n1,n2,n3,n4}),-1]<>").dat"],
3,Msumdat=Msum3[{mQ,mq,md},{n1,n2,n3,n4},SRange->{20,1000,10},Lambda->10^-6,SolveMethod->"'t Hooft"(*,MatrixSize->9*),ProcessType->({{a,c},{a,b}}->{{a,c},{a,b}}),AssignQuark->{a->mQ,b->md,c->mq}];
If[FailureQ[#],,If[ChoiceDialog["Discard old result or not? ",{"Yes"->False,"No"->True}],Msumdat[[2]]=Sort[#[[2]]~Join~Msumdat[[2]]],Null]]&[(<<("data/Msumdat_m1-13.5565-m2-0.045-m3-0.09"<>"-n-("<>ToString[n1]<>"-"<>ToString[n2]<>"-"<>ToString[n3]<>"-"<>ToString[n4]<>").dat"))];
Put[Msumdat,"data/Msumdat_m1-"<>ToString[mQ]<>"-m2-"<>ToString[mq]<>"-m3-"<>ToString[md]<>"-n-("<>StringDrop[StringJoin@((ToString[#]<>"-")&/@{n1,n2,n3,n4}),-1]<>").dat"],
4,Null]
Print["End"];







































