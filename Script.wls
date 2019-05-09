#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Code:: *)
(*Quit*)


(* ::Input::Initialization:: *)
(*<< MMARemoteSSH`*)
(*LaunchRemoteKernels[];*)
(*Kernels[]*)
SetSystemOptions["ParallelOptions"->"MathLinkTimeout"->20.];
<<MMARemoteSSH`
LaunchKernels[8];
LaunchRemoteKernels[3,7];
LaunchRemoteKernels[4,7];
LaunchRemoteKernels[5,8];
(*LaunchRemoteKernels[6,24];
LaunchRemoteKernels[6,24];*)
(*LaunchRemoteKernels[6,8];*)
Kernels[]


(* ::Input::Initialization:: *)
SetDirectory[If[MatchQ[$ScriptCommandLine,{}], NotebookDirectory[], Directory[]]];
(*If[MatchQ[$ScriptCommandLine,{}],Once[LaunchKernels[Input["Kernel Number",16,WindowSize->Small]]],num=ToExpression[$ScriptCommandLine[[2]]];Once[LaunchKernels[num]]];*)
(ParallelEvaluate[#];#)&@Unevaluated[Off[General::stop,NIntegrate::slwcon,NIntegrate::precw,NIntegrate::zeroregion,CompiledFunction::cfsa]];
(ParallelEvaluate[#];#)&@Unevaluated[SetOptions[NIntegrate,MaxRecursion->200,AccuracyGoal->6(*,WorkingPrecision\[Rule]50*)(*,Method->"QuasiMonteCarlo",MaxPoints-> 10^9*)(*,Method\[Rule]{"LocalAdaptive","SingularityHandler"\[Rule]None}*)(*,Method\[Rule]{"DuffyCoordinates","Corners"\[Rule]{{0,0},{0,1},{1,0},{1,1}}}*)]];
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


Clear[\[Phi]x,m2,\[Phi],vals,M1,M2,M3,\[Phi]1,\[Phi]2,\[Phi]3];
Get["OneFlavour`"]

DistributeDefinitions["OneFlavour`"];
(ParallelEvaluate[#];#)&@Unevaluated[SetOptions[OneFlavour`Private`\[ScriptCapitalI]3,Method->Automatic]];

Print["Load Complete\n"];
(*OneFlavour`Private`\[ScriptCapitalI]1[\[Omega]1_,\[Omega]2_,opt:OptionsPattern[]][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,M1_,M2_,M3_,M4_]:= 0;
OneFlavour`Private`\[ScriptCapitalI]2[\[Omega]1_,\[Omega]2_,opt:OptionsPattern[]][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,M1_,M2_,M3_,M4_]:= 0;
OneFlavour`Private`\[ScriptCapitalI]3[\[Omega]1_,\[Omega]2_,opt:OptionsPattern[{OneFlavour`Private`Op->"I"}]][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][ml_?ListQ,Ma_,Mb_,Mc_,Md_]:= (*0;*)
Module[{m1,m2,m3,m4,op},op=OptionValue[OneFlavour`Private`Op];
{m1,m2,m3,m4}=ml[[Which[op=="I",{1,2,3,4},op=="Q",{2,1,3,4},op=="C",{1,2,4,3},op=="P",{2,1,4,3},op=="RQ",{2,1,3,4},op=="RC",{1,2,4,3},op=="RP",{2,1,4,3},op=="R",{1,2,3,4}]]];
(*Print[(Mc^2+Md^2/\[Omega]2+(m1^2-2 \[Beta]^2)/(x-\[Omega]1)+(m2^2-2 \[Beta]^2)/(x-1)-(m3^2-2 \[Beta]^2)/(x-\[Omega]1+\[Omega]2)-(m4^2-2 \[Beta]^2)/x) ];*)(-((4 \[Pi])/OneFlavour`Private`Nc))(NIntegrate[(Mc^2+Md^2/\[Omega]2
(*+(m1^2-2 OneFlavour`Private`\[Beta]^2)/(x-\[Omega]1)*)
+(m2^2-2 OneFlavour`Private`\[Beta]^2)/(x-1)
(*-(m3^2-2 OneFlavour`Private`\[Beta]^2)/(x-\[Omega]1+\[Omega]2)*)
(*-(m4^2-2 OneFlavour`Private`\[Beta]^2)/x*)) \[Phi]1[(x-\[Omega]1+\[Omega]2)/(1+\[Omega]2-\[Omega]1)] \[Phi]2[x/\[Omega]1] \[Phi]3[x] \[Phi]4[(x-\[Omega]1+\[Omega]2)/\[Omega]2],{x,0,1},Evaluate[FilterRules[{opt},Options[NIntegrate]]]]//OneFlavour`Private`UsrReap)];
OneFlavour`Private`\[ScriptCapitalM]1[\[Omega]1_,\[Omega]2_,opt:OptionsPattern[]][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,Ma_,Mb_,Mc_,Md_]:=(*(1-\[Omega]1+\[Omega]2)/\[Omega]2*)(*\[Omega]1*)\[Omega]2/(1-\[Omega]1+\[Omega]2) *)(*\[Omega]1/(1-\[Omega]1+\[Omega]2)*)(*OneFlavour`Private`\[ScriptCapitalI]3[(1-\[Omega]1+\[Omega]2)/\[Omega]2,1/\[Omega]2,Op->"P",(Evaluate[If[ListQ[#1],FilterRules[#1,Options[NIntegrate]],{}]]&)[OptionValue[I3Option]]][\[Phi]2,\[Phi]1,\[Phi]4,\[Phi]3][m,Mb,Ma,Md,Mc]*)
(*If[\[Omega]1>1,OneFlavour`Private`\[ScriptCapitalI]1[1/\[Omega]1,(1-\[Omega]1+\[Omega]2)/\[Omega]1,(Evaluate[If[ListQ[#1],FilterRules[#1,Options[NIntegrate]],{}]]&)[OptionValue[I1Option]]][\[Phi]4,\[Phi]3,\[Phi]2,\[Phi]1][m,Md,Mc,Mb,Ma],OneFlavour`Private`\[ScriptCapitalI]1[\[Omega]1,\[Omega]2,(Evaluate[If[ListQ[#1],FilterRules[#1,Options[NIntegrate]],{}]]&)[OptionValue[I1Option]]][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][m,Ma,Mb,Mc,Md]]+If[\[Omega]2>\[Omega]1,OneFlavour`Private`\[ScriptCapitalI]2[\[Omega]1,\[Omega]2,(Evaluate[If[ListQ[#1],FilterRules[#1,Options[NIntegrate]],{}]]&)[OptionValue[I2Option]]][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][m,Ma,Mb,Mc,Md],OneFlavour`Private`\[ScriptCapitalI]2[\[Omega]2/(1-\[Omega]1+\[Omega]2),\[Omega]1/(1-\[Omega]1+\[Omega]2),(Evaluate[If[ListQ[#1],FilterRules[#1,Options[NIntegrate]],{}]]&)[OptionValue[I2Option]]][\[Phi]3,\[Phi]4,\[Phi]1,\[Phi]2][m,Mc,Md,Ma,Mb]]+Which[0<\[Omega]1<1&&\[Omega]2>=\[Omega]1,OneFlavour`Private`\[ScriptCapitalI]3[1/\[Omega]1,(1-\[Omega]1+\[Omega]2)/\[Omega]1,Op->"Q",(Evaluate[If[ListQ[#1],FilterRules[#1,Options[NIntegrate]],{}]]&)[OptionValue[I3Option]]][\[Phi]4,\[Phi]3,\[Phi]2,\[Phi]1][m,Md,Mc,Mb,Ma],\[Omega]2>=\[Omega]1&&\[Omega]1>=1,OneFlavour`Private`\[ScriptCapitalI]3[\[Omega]1,\[Omega]2,(Evaluate[If[ListQ[#1],FilterRules[#1,Options[NIntegrate]],{}]]&)[OptionValue[I3Option]]][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][m,Ma,Mb,Mc,Md],0<\[Omega]1<1&&\[Omega]1>\[Omega]2,OneFlavour`Private`\[ScriptCapitalI]3[(1-\[Omega]1+\[Omega]2)/\[Omega]2,1/\[Omega]2,Op->"P",(Evaluate[If[ListQ[#1],FilterRules[#1,Options[NIntegrate]],{}]]&)[OptionValue[I3Option]]][\[Phi]2,\[Phi]1,\[Phi]4,\[Phi]3][m,Mb,Ma,Md,Mc],\[Omega]1>=1&&\[Omega]1>\[Omega]2,OneFlavour`Private`\[ScriptCapitalI]3[\[Omega]2/(1-\[Omega]1+\[Omega]2),\[Omega]1/(1-\[Omega]1+\[Omega]2),Op->"C",(Evaluate[If[ListQ[#1],FilterRules[#1,Options[NIntegrate]],{}]]&)[OptionValue[I3Option]]][\[Phi]3,\[Phi]4,\[Phi]1,\[Phi]2][m,Mc,Md,Ma,Mb]];*)(*Which[0<\[Omega]1<1&&\[Omega]2>=\[Omega]1,Print["Q"],\[Omega]2>=\[Omega]1&&\[Omega]1>=1,Print["I"],0<\[Omega]1<1&&\[Omega]1>\[Omega]2,Print["P"],\[Omega]1>=1&&\[Omega]1>\[Omega]2,Print["C"]];*)

(*If[$PVM=="Differential",(ParallelEvaluate[#];#)&@Unevaluated[SetOptions[NIntegrate,Method->{Automatic,"SymbolicProcessing"->False}]],(ParallelEvaluate[#];#)&@Unevaluated[SetOptions[NIntegrate,Method->{Automatic,"SymbolicProcessing"->True}]]];*)
CheckDup[file_]:=If[Print["Time consumed: ",StringJoin[{If[#>=100,ToString@#,IntegerString[#,10,2]]&@Floor[#/3600],":",IntegerString[Floor[Mod[#,3600]/60],10,2],":",IntegerString[Mod[#,60],10,2]}&@Round[#]]&[AbsoluteTime[]-time1]];MatchQ[file,$Failed],Print["Creating new entry..."],If[ChoiceDialog["Discard old result or not? ",{"Yes"->False,"Merge"->True,"Abort"->"Abort"}],Print["Data merge confirmed..."];Msumdat[[2]]=DeleteDuplicatesBy[Sort[file[[2]]~Join~Msumdat[[2]]],First],Print["Overwrite confirmed..."],Print["DO NOT SAVE, ABORTING..."];Abort[]]];
time1=AbsoluteTime[];

m=0.749; (*mq=0.749; mQ=4.19022;md=13.5565;*)
{n1,n2,n3,n4}={0,0,0,0};
(*range={0.001,1,0.01};*)
range={0.001,20,0.1};
(*range={0.001,20,0.5};*)
\[Lambda]=10^-4;method="'t Hooft";size=12;
(*mQ=13.5565;mq=4.19022;*) 
(*mQ=4.19022;mq=0.749;
type=({{a,b},{a,b}}->{{a,b},{a,b}}) (*({{a,a},{b,a}}->{{a,a},{b,a}})*);
assign={a->mQ,b->mq};*)
(*mq=0.749; mQ=4.19022;md=13.5565;*)
(*mQ=0.749; mq=4.19022;md=0.09;*)
(*mQ=4.19022; mq=0.749;md=0.09;*)
(*mQ=0.749; mq=0.09;md=4.19022;*)
(*mQ=4.19022; mq=0.09;md=0.045;*)
mQ=13.5565; mq=0.09;md=0.045;
type=(*({{a,b},{c,a}}->{{a,b},{c,a}})*) (*({{a,c},{b,b}}->{{b,c},{a,b}})*)(*({{a,c},{c,b}}->{{c,c},{a,b}})*) ({{a,c},{a,b}}->{{a,c},{a,b}});
assign={a->mQ,b->mq,c->md};
(*mq=0.749; mQ=4.19022;md=13.5565;mp=0.09;
(*mq=4.19022; mQ=0.749;md=6;mp=0.5;*)
type=({{a,d},{c,b}}->{{c,d},{a,b}})(*({{a,d},{b,a}}->{{c,d},{b,c}})*);
assign={a->mQ,b->mq,c->md,d->mp};*)
alterkin=True (*False*);

kin=If[alterkin,"-ak",""];
filenamepartn="-n-("<>StringDrop[StringJoin@((ToString[#]<>"-")&/@{n1,n2,n3,n4}),-1]<>")"<>kin<>".dat";
filenamepart="-n-("<>ToString[n1]<>"-"<>ToString[n2]<>"-"<>ToString[n3]<>"-"<>ToString[n4]<>")-type-"<>StringDelete[StringReplace[ToString[Apply[StringJoin[#1,"+",#2]&,Map[StringJoin,Map[ToString,type,{3}],{2}],{1}]],"->"->"-"]," "]<>kin<>".dat";

Switch[Evaluate@ChoiceDialog["How many flavour is requested? ",{"Length"->Length[assign],"1"->1,"2"->2,"3"->3,"4"->4,"Abort"->5}],1,
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
Put[Msumdat,"data/Msumdat_m1-"<>ToString[mQ]<>"-m2-"<>ToString[mq]<>"-m3-"<>ToString[md]<>"-m4-"<>ToString[mp]<>filenamepart],
5,Abort[]]
Print["End"];


Return
