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
(ParallelEvaluate[#];#)&@Unevaluated[Off[General::stop,NIntegrate::slwcon,NIntegrate::precw,NIntegrate::zeroregion]];
(ParallelEvaluate[#];#)&@Unevaluated[SetOptions[NIntegrate,MaxRecursion->200,AccuracyGoal->6(*,WorkingPrecision\[Rule]50*)(*,Method->"QuasiMonteCarlo",MaxPoints->5 10^7*)(*,Method\[Rule]{"LocalAdaptive","SingularityHandler"\[Rule]None}*)(*,Method\[Rule]{"DuffyCoordinates","Corners"\[Rule]{{0,0},{0,1},{1,0},{1,1}}}*)]];
(*SetOptions[$Output, FormatType->OutputForm];*)
(*AppendTo[$Path,NotebookDirectory[]];
Print[$Path];*)
(*AppendTo[$Path,DirectoryName@$InputFileName];*)
(*SetDirectory[If[$InputFileName=="", NotebookDirectory[], Directory[]]];*)


(* ::Code:: *)
(*Options[NIntegrate]*)


(* ::Input:: *)
(*ParallelEvaluate[Off[NIntegrate::ncvb]];*)


(* ::Code:: *)
(*ParallelEvaluate[Information@OneFlavour`Private`\[ScriptCapitalI]1];*)


(* ::Input::Initialization:: *)
Clear[\[Phi]x,m2,\[Phi],vals,M1,M2,M3,\[Phi]1,\[Phi]2,\[Phi]3];
Get["OneFlavour`"]
(*If[$PVM=="Differential",(ParallelEvaluate[#];#)&@Unevaluated[SetOptions[NIntegrate,Method->{Automatic,"SymbolicProcessing"->False}]],(ParallelEvaluate[#];#)&@Unevaluated[SetOptions[NIntegrate,Method->{Automatic,"SymbolicProcessing"->True}]]];*)
CheckDup[file_]:=If[Print["Time consumed: ",StringJoin[{If[#>=100,ToString@#,IntegerString[#,10,2]]&@Floor[#/3600],":",IntegerString[Floor[Mod[#,3600]/60],10,2],":",IntegerString[Mod[#,60],10,2]}&@Round[#]]&[AbsoluteTime[]-time1]];MatchQ[file,$Failed],Print["Creating new entry..."],If[ChoiceDialog["Discard old result or not? ",{"Yes"->False,"No"->True,"Abort"->"Abort"}],Print["Data merge confirmed..."];Msumdat[[2]]=DeleteDuplicatesBy[Sort[file[[2]]~Join~Msumdat[[2]]],First],Print["Overwrite confirmed..."],Print["DO NOT SAVE, ABORTING..."];Abort[]]];
time1=AbsoluteTime[];

m=4.19022; (*mq=0.749; mQ=4.19022;md=13.5565;*)
{n1,n2,n3,n4}={1,1,0,0};
range={0.001,20,0.1};
\[Lambda]=10^-3;method="'t Hooft";size=12;
(*mq=4.19022; mQ=13.5565;
type=({{a,b},{a,b}}->{{a,b},{a,b}});
assign={a->mQ,b->mq};*)
(*mq=0.749; mQ=4.19022;md=13.5565;*)
(*mQ=0.749; mq=4.19022;md=0.09;*)
(*mQ=0.749; mq=0.09;md=4.19022;*)
(*mQ=4.19022; mq=0.09;md=0.045;
type=(*({{a,b},{c,a}}->{{a,b},{c,a}})*)(*({{a,c},{b,b}}->{{b,c},{a,b}})*)(*({{a,c},{c,b}}->{{c,c},{a,b}})*)({{a,c},{a,b}}->{{a,c},{a,b}});
assign={a->mQ,b->mq,c->md};*)
mq=0.749; mQ=4.19022;md=13.5565;mp=0.09;
(*mq=4.19022; mQ=0.749;md=6;mp=0.5;*)
type=({{a,d},{c,b}}->{{c,d},{a,b}})(*({{a,d},{b,a}}->{{c,d},{b,c}})*);
assign={a->mQ,b->mq,c->md,d->mp};
alterkin=(*True*) False;

kin=If[alterkin,"-ak",""];
filenamepartn="-n-("<>StringDrop[StringJoin@((ToString[#]<>"-")&/@{n1,n2,n3,n4}),-1]<>").dat";
filenamepart="-n-("<>ToString[n1]<>"-"<>ToString[n2]<>"-"<>ToString[n3]<>"-"<>ToString[n4]<>")-type-"<>StringDelete[StringReplace[ToString[Apply[StringJoin[#1,"+",#2]&,Map[StringJoin,Map[ToString,type,{3}],{2}],{1}]],"->"->"-"]," "]<>kin<>".dat";

Switch[Evaluate@ChoiceDialog["How many flavour is requested? ",{"1"->1,"2"->2,"3"->3,"4"->4,"Abort"->5}],1,
Msumdat=Msum[m,{n1,n2,n3,n4},SRange->range,Lambda->\[Lambda],SolveMethod->method,MatrixSize->size];Message[Msum::done];
CheckDup[Get["data/Msumdat_m-"<>ToString[m]<>filenamepartn]];
Put[Msumdat,"data/Msumdat_m-"<>ToString[m]<>filenamepartn],
2,
Msumdat=Msum2[{mQ,mq},{n1,n2,n3,n4},SRange->range,Lambda->\[Lambda],SolveMethod->method,MatrixSize->size,ProcessType->type,AssignQuark->assign,AnotherKinematics->alterkin];Message[Msum::done];
CheckDup[Get["data/Msumdat_m1-"<>ToString[mQ]<>"-m2-"<>ToString[mq]<>filenamepart]];
Put[Msumdat,"data/Msumdat_m1-"<>ToString[mQ]<>"-m2-"<>ToString[mq]<>filenamepart],
3,
Msumdat=Msum3[{mQ,mq,md},{n1,n2,n3,n4},SRange->range,Lambda->\[Lambda],SolveMethod->method,MatrixSize->size,ProcessType->type,AssignQuark->assign(*{a->mQ,b->md,c->mq}*),AnotherKinematics->alterkin];Message[Msum::done];
CheckDup[Get["data/Msumdat_m1-"<>ToString[mQ]<>"-m2-"<>ToString[mq]<>"-m3-"<>ToString[md]<>filenamepart]];
Put[Msumdat,"data/Msumdat_m1-"<>ToString[mQ]<>"-m2-"<>ToString[mq]<>"-m3-"<>ToString[md]<>filenamepart],
4,Msumdat=Msum4[{mQ,mq,md,mp},{n1,n2,n3,n4},SRange->range,Lambda->\[Lambda],SolveMethod->method,MatrixSize->size,ProcessType->type,AssignQuark->assign(*{a->mQ,b->md,c->mq}*),AnotherKinematics->alterkin];Message[Msum::done];
CheckDup[Get["data/Msumdat_m1-"<>ToString[mQ]<>"-m2-"<>ToString[mq]<>"-m3-"<>ToString[md]<>"-m4-"<>ToString[mp]<>filenamepart]];
Put[Msumdat,"data/Msumdat_m1-"<>ToString[mQ]<>"-m2-"<>ToString[mq]<>"-m3-"<>ToString[md]<>"-m4-"<>ToString[mp]<>filenamepart],5,Abort[]]
Print["End"];




















Return
