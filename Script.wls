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
CheckDup[file_]:=If[FailureQ[file],Null,If[ChoiceDialog["Discard old result or not? ",{"Yes"->False,"No"->True,"Abort"->"Abort"}],Msumdat[[2]]=DeleteDuplicatesBy[Sort[file[[2]]~Join~Msumdat[[2]]],First],Null,Abort[]]];
Clear[\[Phi]x,m2,\[Phi],vals,M1,M2,M3,\[Phi]1,\[Phi]2,\[Phi]3];

m=4.19022; mQ=4.19022; mq=0.749;md=0.09;
{n1,n2,n3,n4}={2,2,2,2};
range={1 10^(-3),100,0.5};\[Lambda]=10^-6;method="'t Hooft";

Switch[Evaluate@ChoiceDialog["How many flavour is requested? ",{"1"->1,"2"->2,"3"->3,"4"->4}],1,
Msumdat=Msum[m,{n1,n2,n3,n4},SRange->range,Lambda->\[Lambda],SolveMethod->method(*,MatrixSize->9*)];
CheckDup[Get["data/Msumdat_m-4.19022-n-("<>ToString[n1]<>"-"<>ToString[n2]<>"-"<>ToString[n3]<>"-"<>ToString[n4]<>").dat"]];
Put[Msumdat,"data/Msumdat_m-"<>ToString[m]<>"-n-("<>StringDrop[StringJoin@((ToString[#]<>"-")&/@{n1,n2,n3,n4}),-1]<>").dat"],
2,
Msumdat=Msum2[{mQ,mq},{n1,n2,n3,n4},SRange->range,Lambda->\[Lambda],SolveMethod->method(*,MatrixSize->9*)];
CheckDup[Get["data/Msumdat_m1-4.19022-m2-0.749-n-("<>ToString[n1]<>"-"<>ToString[n2]<>"-"<>ToString[n3]<>"-"<>ToString[n4]<>").dat"]];
Put[Msumdat,"data/Msumdat_m1-"<>ToString[mQ]<>"-m2-"<>ToString[mq]<>"-n-("<>StringDrop[StringJoin@((ToString[#]<>"-")&/@{n1,n2,n3,n4}),-1]<>").dat"],
3,
Msumdat=Msum3[{mQ,mq,md},{n1,n2,n3,n4},SRange->range,Lambda->\[Lambda],SolveMethod->method(*,MatrixSize->9*),ProcessType->({{a,c},{a,b}}->{{a,c},{a,b}}),AssignQuark->{a->mQ,b->md,c->mq}];
CheckDup[Get["data/Msumdat_m1-13.5565-m2-0.045-m3-0.09"<>"-n-("<>ToString[n1]<>"-"<>ToString[n2]<>"-"<>ToString[n3]<>"-"<>ToString[n4]<>").dat"]];
Put[Msumdat,"data/Msumdat_m1-"<>ToString[mQ]<>"-m2-"<>ToString[mq]<>"-m3-"<>ToString[md]<>"-n-("<>StringDrop[StringJoin@((ToString[#]<>"-")&/@{n1,n2,n3,n4}),-1]<>").dat"],
4,Null]
Print["End"];







































