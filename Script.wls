#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Code:: *)
(*Quit*)


(* ::Input::Initialization:: *)
(*<< MMARemoteSSH`*)
(*LaunchRemoteKernels[];*)
Kernels[]
LaunchKernels[];
Kernels[]


(* ::Input::Initialization:: *)
SetDirectory[If[MatchQ[$ScriptCommandLine,{}], NotebookDirectory[], Directory[]]];
(*If[MatchQ[$ScriptCommandLine,{}],Once[LaunchKernels[Input["Kernel Number",16,WindowSize->Small]]],num=ToExpression[$ScriptCommandLine[[2]]];Once[LaunchKernels[num]]];*)
ParallelEvaluate[Off[General::stop,NIntegrate::slwcon,NIntegrate::precw(*,NIntegrate::zeroregion*)]];
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
CheckDup[file_]:=If[MatchQ[file,$Failed],Print["Creating new entry..."],If[ChoiceDialog["Discard old result or not? ",{"Yes"->False,"No"->True,"Abort"->"Abort"}],Print["Data merge confirmed..."];Msumdat[[2]]=DeleteDuplicatesBy[Sort[file[[2]]~Join~Msumdat[[2]]],First],Print["Overwrite confirmed..."],Abort[]]];
Clear[\[Phi]x,m2,\[Phi],vals,M1,M2,M3,\[Phi]1,\[Phi]2,\[Phi]3];

m=4.19022; mq=0.749; mQ=4.19022;md=13.5565;
{n1,n2,n3,n4}={2,2,0,0};
range={2 10^(-2),10,0.05};\[Lambda]=10^-6;method="'t Hooft";size=12;
(*mq=0.749; mQ=4.19022;
type=({{a,b},{a,b}}->{{a,b},{a,b}});
assign={a->mQ,b->mq};*)
mQ=0.749; mq=4.19022;md=13.5565;
type=({{a,b},{c,a}}->{{a,b},{c,a}});
assign={a->mQ,b->md,c->mq};

filenamepartn="-n-("<>StringDrop[StringJoin@((ToString[#]<>"-")&/@{n1,n2,n3,n4}),-1]<>").dat";
filenamepart="-n-("<>ToString[n1]<>"-"<>ToString[n2]<>"-"<>ToString[n3]<>"-"<>ToString[n4]<>")-type-"<>StringDelete[StringReplace[ToString[Apply[Plus,Map[StringJoin,Map[ToString,type,{3}],{2}],{1}]],"->"->"-"]," "]<>".dat";

Switch[Evaluate@ChoiceDialog["How many flavour is requested? ",{"1"->1,"2"->2,"3"->3,"4"->4}],1,
Msumdat=Msum[m,{n1,n2,n3,n4},SRange->range,Lambda->\[Lambda],SolveMethod->method,MatrixSize->size];
CheckDup[Get["data/Msumdat_m-"<>ToString[m]<>filenamepartn]];
Put[Msumdat,"data/Msumdat_m-"<>ToString[m]<>filenamepartn],
2,
Msumdat=Msum2[{mQ,mq},{n1,n2,n3,n4},SRange->range,Lambda->\[Lambda],SolveMethod->method,MatrixSize->size,ProcessType->type,AssignQuark->assign];
CheckDup[Get["data/Msumdat_m1-"<>ToString[mQ]<>"-m2-"<>ToString[mq]<>filenamepart]];
Put[Msumdat,"data/Msumdat_m1-"<>ToString[mQ]<>"-m2-"<>ToString[mq]<>filenamepart],
3,
Msumdat=Msum3[{mQ,mq,md},{n1,n2,n3,n4},SRange->range,Lambda->\[Lambda],SolveMethod->method,MatrixSize->size,ProcessType->type,AssignQuark->assign(*{a->mQ,b->md,c->mq}*)];
CheckDup[Get["data/Msumdat_m1-"<>ToString[mQ]<>"-m2-"<>ToString[mq]<>"-m3-"<>ToString[md]<>filenamepart]];
Put[Msumdat,"data/Msumdat_m1-"<>ToString[mQ]<>"-m2-"<>ToString[mq]<>"-m3-"<>ToString[md]<>filenamepart],
4,Null]
Print["End"];



