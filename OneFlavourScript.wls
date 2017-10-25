#!/usr/bin/env wolframscript
(* ::Package:: *)

Once[LaunchKernels[8]]
ParallelEvaluate[Off[General::stop,NIntegrate::slwcon]];
ParallelEvaluate@SetOptions[NIntegrate,MaxRecursion->100,AccuracyGoal->10];
AppendTo[$Path,DirectoryName@$InputFileName];
Get["OneFlavour`"];
Print[$ScriptCommandLine];
(*Print[FullForm@ToExpression[$ScriptCommandLine[[4]]]];
Print[$KernelCount];
Abort[];*)
If[ContainsAny[{ToExpression[$ScriptCommandLine]},{$Failed}],Quit[]];
m=ToExpression[$ScriptCommandLine[[2]]];
(*{mQ,mq}=ToExpression[$ScriptCommandLine[[2]]];*)
{n1,n2,n3,n4}=ToExpression[$ScriptCommandLine[[3]]];
Msumdat=Msum[m,{n1,n2,n3,n4},SRange->ToExpression[$ScriptCommandLine[[4]]](*{10^-3,1,0.01}*),Lambda->10^-5];
(*Msumdat=Msum2[{mQ,mq},{n1,n2,n3,n4},SRange->ToExpression[$ScriptCommandLine[[4]]](*{10^-3,1,0.01}*),Lambda->10^-5];*)
Export["data/Msumdat_m-"<>ToString[m]<>"-n-("<>StringDrop[StringJoin@((ToString[#]<>"-")&/@{n1,n2,n3,n4}),-1]<>").dat",Msumdat,"CSV"]
