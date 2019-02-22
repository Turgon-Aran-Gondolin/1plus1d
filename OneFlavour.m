(* ::Package:: *)

BeginPackage["OneFlavour`"];


(* ::Input::Initialization:: *)
Solvet::usage="Solvet[m1,m2] is used to solve 't Hooft equation or pull out results.
Option: SolveMethod. SolveMethod->\"BSW\": use BSW SolveMethod; SolveMethod->\"'t Hooft\": use the SolveMethod 't Hooft suggested in his original paper."
Msum::usage=
"Msum[mQ,{n1,n2,n3,n4},Options] is the total sum of amplitudes. The result is of the form {{{n1,n2,n3,n4},{M1,M2,M3,M4},{mq}},{{Sqrt[S],amp},...}}.\n Option: SRange->{a,b,d}, the real range of centre-of-mass energy square S is {M1+M2+a,M1+M2+b,d} where d is the interval.\n Option: Lambda->\[Lambda], the cutoff of principal value integral.\n Option: SolveMethod. SolveMethod->\"BSW\": use BSW SolveMethod; SolveMethod->\"'t Hooft\": use the SolveMethod 't Hooft suggested in his original paper.\n Option: DataDir->\"Data desination\". e.g. \"D:/data\"./n Option: I1Option->Options[NIntegrate],I2Option->Options[NIntegrate],I3Option->Options[NIntegrate].\n Option: gvalue.
"
Msum2::usage=
"Msum2[{mQ,mq},{n1,n2,n3,n4},Options] is the total sum of two-flavour amplitudes. The result is of the form {{{n1,n2,n3,n4},{M1,M2,M3,M4},{mQ,mq}},{{Sqrt[S],amp},...}}.\n Option: SRange->{a,b,d}, the real range of centre-of-mass energy square S is {M1+M2+a,M1+M2+b,d} where d is the interval.\n Option: Lambda->\[Lambda], the cutoff of principal value integral.\n Option: SolveMethod. SolveMethod->\"BSW\": use BSW SolveMethod; SolveMethod->\"'t Hooft\": use the SolveMethod 't Hooft suggested in his original paper.\n Option: DataDir->\"Data desination\". e.g. \"D:/data\".\n Option: gvalue.
"
Msum3::usage=
"Msum3[{mQ,mq,md},{n1,n2,n3,n4},Options] is the total sum of three-flavour amplitudes. The result is of the form {{{n1,n2,n3,n4},{M1,M2,M3,M4},{mQ,mq,md}},{{Sqrt[S],amp},...}}. Option: SRange->{a,b,d}, the real range of centre-of-mass energy square S is {M1+M2+a,M1+M2+b,d} where d is the interval. Option: Lambda->\[Lambda], the cutoff of principal value integral. Option: SolveMethod. SolveMethod->\"BSW\": use BSW SolveMethod; SolveMethod->\"'t Hooft\": use the SolveMethod 't Hooft suggested in his original paper. Option: DataDir->\"Data desination\". e.g. \"D:/data\".\n Option: gvalue.
"
Msum4::usage=""
displayfunction1::usage="displayfunction1[Msumdat,{min,max}], amplitude plot, min/max is the spare space in both ends. ";
displayfunction2::usage="displayfunction2[Msumdat,{min,max}], amplitude square plot, min/max is the spare space in both ends. ";
displayfunctionboth::usage="displayfunctionboth[Msumdat,{min,max}], amplitude square plot, min/max is the spare space in both ends. ";
SRange::usage=
"Option for Msum family. Indicate the energy region as {start,end,interval}";
SolveMethod::usage=
"\"'t Hooft\" and \"BSW\"";
Lambda::usage=
"Option of Msum function: Cutoff of Cauchy principle value integral";
MatrixSize::usage=
    "Matrix size for \"'t Hooft\" and \"BSW\" methods. "
Force::usage=
    "Option for Solvet, force solving even if solution exists. "
ProcessType::usage=
"({{a,b},{c,a}}->{{a,b},{c,a}})";
AssignQuark::usage=
"{a->m1,b->m2}";
PVInt;
PVInt::met="None of Subtraction or Differential method is employed.";
PVMethod;
$PVM;

\[Phi]n;
Mn;

\[Omega]1S;
\[Omega]2S;
\[Omega]1So;
\[Omega]2So;
test;


(*ParallelEvaluate[Print[$KernelID]];*)


Begin["`Private`"]



(* ::Section:: *)
(*PV*)


(* ::Subsection:: *)
(*PV Definition*)


(* ::DisplayFormula:: *)
(*Prescription for PV integral:P \!\( *)
(*\*SubsuperscriptBox[\(\[Integral]\), \(0\), \(1\)]\( *)
(*\*FractionBox[\(F[k]\), *)
(*SuperscriptBox[\((k - p)\), \(2\)]] \[DifferentialD]k\)\)=\!\( *)
(*\*SubsuperscriptBox[\(\[Integral]\), \(0\), \(p - \[Lambda]\)]\( *)
(*\*FractionBox[\(F[k]\), *)
(*SuperscriptBox[\((k - p)\), \(2\)]] \[DifferentialD]k\)\)+\!\( *)
(*\*SubsuperscriptBox[\(\[Integral]\), \(p + \[Lambda]\), \(1\)]\( *)
(*\*FractionBox[\(F[k]\), *)
(*SuperscriptBox[\((k - p)\), \(2\)]] \[DifferentialD]k\)\)-2/\[Lambda]F[p](Li's)*)
(*P \!\( *)
(*\*SubsuperscriptBox[\(\[Integral]\), \(0\), \(1\)]\( *)
(*\*FractionBox[\(F[k]\), *)
(*SuperscriptBox[\((k - p)\), \(2\)]] \[DifferentialD]k\)\)=\!\( *)
(*\*SubsuperscriptBox[\(\[Integral]\), \(0\), \(1\)]\( *)
(*\*FractionBox[\(F[k] - F[p] - \((k - p)\) \(F'\)[p]\), *)
(*SuperscriptBox[\((k - p)\), \(2\)]] \[DifferentialD]k \((hep - ph/0111225)\)\)\)+F[p]/(p(p-1))(differed from Li's by a constant)*)
(*If it's a 2-d integral,\!\( *)
(*\*SubsuperscriptBox[\(\[Integral]\), \(0\), \(1\)]\(\[DifferentialD]xP \( *)
(*\*SubsuperscriptBox[\(\[Integral]\), \(0\), \(1\)]\[DifferentialD]k *)
(*\*FractionBox[\(F[k]\), *)
(*SuperscriptBox[\((k - p[x])\), \(2\)]]\)\)\)=\!\( *)
(*\*SubsuperscriptBox[\(\[Integral]\), \(0\), \(1\)]\(\[DifferentialD]x \( *)
(*\*SubsuperscriptBox[\(\[Integral]\), \(0\), \(p - \[Lambda]\)]*)
(*\*FractionBox[\(F[k]\), *)
(*SuperscriptBox[\((k - p[x])\), \(2\)]] \[DifferentialD]k\)\)\)+\!\( *)
(*\*SubsuperscriptBox[\(\[Integral]\), \(0\), \(1\)]\(\[DifferentialD]x \( *)
(*\*SubsuperscriptBox[\(\[Integral]\), \(p + \[Lambda]\), \(1\)]*)
(*\*FractionBox[\(F[k]\), *)
(*SuperscriptBox[\((k - p[x])\), \(2\)]] \[DifferentialD]k\)\)\)-2/\[Lambda]\!\( *)
(*\*SubsuperscriptBox[\(\[Integral]\), \(\[Lambda]\), \(1 - \[Lambda]\)]\(\[DifferentialD]xF[p[x]]\)\)-1/\[Lambda] \!\( *)
(*\*SubsuperscriptBox[\(\[Integral]\), \(0\), \(\[Lambda]\)]\(\[DifferentialD]xF[p[x]]\)\)-1/\[Lambda] \!\( *)
(*\*SubsuperscriptBox[\(\[Integral]\), \(1 - \[Lambda]\), \(1\)]\(\[DifferentialD]xF[p[x]] \((Li' s)\)\)\)*)
(*\!\( *)
(*\*SubsuperscriptBox[\(\[Integral]\), \(0\), \(1\)]\(\[DifferentialD]xP \( *)
(*\*SubsuperscriptBox[\(\[Integral]\), \(0\), \(1\)]\[DifferentialD]k *)
(*\*FractionBox[\(F[k]\), *)
(*SuperscriptBox[\((k - p[x])\), \(2\)]]\)\)\)=\!\( *)
(*\*SubsuperscriptBox[\(\[Integral]\), \(0\), \(1\)]\(\[DifferentialD]x \( *)
(*\*SubsuperscriptBox[\(\[Integral]\), \(0\), \(1\)]\[DifferentialD]k *)
(*\*FractionBox[\(F[k] - F[p] - \((k - p)\) \(F'\)[p]\), *)
(*SuperscriptBox[\((k - p)\), \(2\)]]\)\)\)+\!\( *)
(*\*SubsuperscriptBox[\(\[Integral]\), \(0\), \(1 - \[Lambda]\)]\(\[DifferentialD]x *)
(*\*FractionBox[\(F[p]\), \(p - 1\)]\)\)-\!\( *)
(*\*SubsuperscriptBox[\(\[Integral]\), \(\[Lambda]\), \(1\)]\(\[DifferentialD]x *)
(*\*FractionBox[\(F[p]\), \(p\)]\)\)-\!\( *)
(*\*SubsuperscriptBox[\(\[Integral]\), \(\[Lambda]\), \(1 - \[Lambda]\)]\(\[DifferentialD]\(xF'\)[p] Log[p/\((1 - p)\)]\)\)-\!\( *)
(*\*SubsuperscriptBox[\(\[Integral]\), \(0\), \(\[Lambda]\)]\(\[DifferentialD]\(xF'\)[p] Log[1/\((1 - p)\)]\)\)-\!\( *)
(*\*SubsuperscriptBox[\(\[Integral]\), \(1 - \[Lambda]\), \(1\)]\(\[DifferentialD]\(xF'\)[p] Log[p]\)\)*)


(* ::Subsection:: *)
(*PV Operator*)


PVInt[intg_,var_,pole_,opt:OptionsPattern[{PVMethod->"Differential"}]]:=Module[{F,dF,v},(*Print[intg (var-pole)^2/.var\[Rule]v];*)
	Set@@{F[v_],Simplify[intg (var-pole)^2/.var->v]};
	(*Print[F[var]];*)
	Which[
		OptionValue[PVMethod]=="Subtraction",
		intg Boole[0<var<(pole-\[Lambda])||1>var>(pole+\[Lambda])]-2/\[Lambda] F[pole]Boole[\[Lambda]<pole<(1-\[Lambda])] -1/\[Lambda] F[pole]Boole[0<pole<\[Lambda]]-1/\[Lambda] F[pole]Boole[(1-\[Lambda])<pole<1],
		OptionValue[PVMethod]=="Differential",
		intg -F[pole]/(var-pole)^2-F'[pole]/(var-pole)+F[pole]/(pole-1) Boole[0<=pole<1-\[Lambda]]-F[pole]/pole Boole[\[Lambda]<pole<=1]
		-F'[pole]Log[pole/(1-pole)]Boole[\[Lambda]<pole<1-\[Lambda]]
		-F'[pole]Log[1/(1-pole)]Boole[0<pole<\[Lambda]]-F'[pole]Log[pole]Boole[1-\[Lambda]<pole<1],
		True,Message[PVInt::met]
	]
];


(* ::Section:: *)
(*Amplitude*)


(* ::Input::Initialization:: *)
If[$OperatingSystem=="Windows",{dirglo="D:/Documents/2-d-data";},{dirglo="~/Documents/2-d-data";}];
\[ScriptCapitalC][expr_[\[Omega]__][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,M1_,M2_,M3_,M4_]]:=expr[\[Omega]][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][m,M1,M2,M3,M4]/. {\[Omega]1->\[Omega]2/(1+\[Omega]2-\[Omega]1),\[Omega]2->\[Omega]1/(1+\[Omega]2-\[Omega]1),\[Phi]1->\[Phi]3,\[Phi]3->\[Phi]1,\[Phi]2->\[Phi]4,\[Phi]4->\[Phi]2,M1->M3,M3->M1,M2->M4,M4->M2};
\[ScriptCapitalP][expr_[\[Omega]__][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,M1_,M2_,M3_,M4_]]:=expr[\[Omega]][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][m,M1,M2,M3,M4]/. {\[Omega]1->(1+\[Omega]2-\[Omega]1)/\[Omega]2,\[Omega]2->1/\[Omega]2,\[Phi]1->\[Phi]2,\[Phi]2->\[Phi]1,\[Phi]3->\[Phi]4,\[Phi]4->\[Phi]3,M1->M2,M2->M1,M3->M4,M4->M3};
\[ScriptCapitalR][expr_[\[Omega]__][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,M1_,M2_,M3_,M4_]]:=expr[\[Omega]][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][m,M1,M2,M3,M4]/. {\[Omega]1->\[Omega]1/\[Omega]2,\[Omega]2->1/\[Omega]2,\[Phi]3->\[Phi]4,\[Phi]4->\[Phi]3,M3->M4,M4->M3};
\[ScriptCapitalS][expr_[\[Omega]__][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,M1_,M2_,M3_,M4_]]:=expr[\[Omega]][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][m,M1,M2,M3,M4]/. {\[Omega]1->1/\[Omega]1,\[Omega]2->(1+\[Omega]2-\[Omega]1)/\[Omega]1,\[Phi]1->\[Phi]4,\[Phi]4->\[Phi]1,\[Phi]2->\[Phi]3,\[Phi]3->\[Phi]2,M1->M4,M4->M1,M2->M3,M3->M2};
MVG[\[Omega]1_,\[Omega]2_,opt:OptionsPattern[]][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_]:=If[\[Omega]2>\[Omega]1,4 g^2 \[Omega]1 NIntegrate[(\[Phi]1[x/(1+\[Omega]2-\[Omega]1)] \[Phi]2[y] \[Phi]3[x] \[Phi]4[1+((y-1) \[Omega]1)/\[Omega]2])/(1+y \[Omega]1-x+\[Omega]2-\[Omega]1)^2,{x,0,1},{y,0,1},Evaluate[FilterRules[{opt},Options[NIntegrate]]]],4 g^2 \[Omega]2 (1+\[Omega]2-\[Omega]1) NIntegrate[(\[Phi]1[x] \[Phi]2[1+((y-1) \[Omega]2)/\[Omega]1] \[Phi]3[x (1+\[Omega]2-\[Omega]1)] \[Phi]4[y])/(1+y \[Omega]2+x (\[Omega]1-\[Omega]2-1))^2,{x,0,1},{y,0,1},Evaluate[FilterRules[{opt},Options[NIntegrate]]]]];
MHG[\[Omega]1_,\[Omega]2_,opt:OptionsPattern[]][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_]:=If[\[Omega]2>\[Omega]1,4 g^2 \[Omega]1 NIntegrate[(\[Phi]1[(\[Omega]2-\[Omega]1+x)/(\[Omega]2-\[Omega]1+1)] \[Phi]2[y] \[Phi]3[x] \[Phi]4[(y \[Omega]1)/\[Omega]2])/(y \[Omega]1-\[Omega]2-x)^2,{x,0,1},{y,0,1},Evaluate[FilterRules[{opt},Options[NIntegrate]]]],4 g^2 \[Omega]2 (1+\[Omega]2-\[Omega]1) NIntegrate[(\[Phi]1[x] \[Phi]2[(y \[Omega]2)/\[Omega]1] \[Phi]3[\[Omega]1-\[Omega]2+(1-\[Omega]1+\[Omega]2) x] \[Phi]4[y])/(y \[Omega]2-x (1+\[Omega]2-\[Omega]1)-\[Omega]1)^2,{x,0,1},{y,0,1},Evaluate[FilterRules[{opt},Options[NIntegrate]]]]];
EXPR[\[Omega]1_,\[Omega]2_,opt:OptionsPattern[{I1Option->OptionsPattern[],I2Option->OptionsPattern[],I3Option->OptionsPattern[]}]][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,Ma_,Mb_,Mc_,Md_]:=If[\[Omega]1>1,\[ScriptCapitalM]0[1-\[Omega]1+\[Omega]2,\[Omega]2][\[Phi]2,\[Phi]1,\[Phi]3,\[Phi]4][m,Mb,Ma,Mc,Md]+\[ScriptCapitalM]0[1/(1-\[Omega]1+\[Omega]2),\[Omega]1/(1-\[Omega]1+\[Omega]2)][\[Phi]4,\[Phi]3,\[Phi]1,\[Phi]2][m,Md,Mc,Ma,Mb]+\[ScriptCapitalI]1[1/\[Omega]1,(1-\[Omega]1+\[Omega]2)/\[Omega]1,(Evaluate[If[ListQ[#1],FilterRules[#1,Options[NIntegrate]],{}]]&)[OptionValue[I1Option]]][\[Phi]4,\[Phi]3,\[Phi]2,\[Phi]1][m,Md,Mc,Mb,Ma]+\[ScriptCapitalI]2[1/((1+1/\[Omega]2-\[Omega]1/\[Omega]2) \[Omega]2),\[Omega]1/((1+1/\[Omega]2-\[Omega]1/\[Omega]2) \[Omega]2),(Evaluate[If[ListQ[#1],FilterRules[#1,Options[NIntegrate]],{}]]&)[OptionValue[I2Option]]][\[Phi]4,\[Phi]3,\[Phi]1,\[Phi]2][m,Md,Mc,Ma,Mb],\[ScriptCapitalM]0[\[Omega]2/\[Omega]1,(1-\[Omega]1+\[Omega]2)/\[Omega]1][\[Phi]3,\[Phi]4,\[Phi]2,\[Phi]1][m,Mc,Md,Mb,Ma]+\[ScriptCapitalM]0[\[Omega]1/\[Omega]2,1/\[Omega]2][\[Phi]1,\[Phi]2,\[Phi]4,\[Phi]3][m,Ma,Mb,Md,Mc]+\[ScriptCapitalI]1[\[Omega]1,\[Omega]2,(Evaluate[If[ListQ[#1],FilterRules[#1,Options[NIntegrate]],{}]]&)[OptionValue[I1Option]]][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][m,Ma,Mb,Mc,Md]+\[ScriptCapitalI]2[\[Omega]1/\[Omega]2,1/\[Omega]2,(Evaluate[If[ListQ[#1],FilterRules[#1,Options[NIntegrate]],{}]]&)[OptionValue[I2Option]]][\[Phi]1,\[Phi]2,\[Phi]4,\[Phi]3][m,Ma,Mb,Md,Mc]]+If[\[Omega]2>\[Omega]1,\[ScriptCapitalM]0[\[Omega]1,\[Omega]2][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][m,Ma,Mb,Mc,Md]+\[ScriptCapitalM]0[1/\[Omega]1,(1-\[Omega]1+\[Omega]2)/\[Omega]1][\[Phi]4,\[Phi]3,\[Phi]2,\[Phi]1][m,Md,Mc,Mb,Ma]+\[ScriptCapitalI]1[\[Omega]1/\[Omega]2,1/\[Omega]2,(Evaluate[If[ListQ[#1],FilterRules[#1,Options[NIntegrate]],{}]]&)[OptionValue[I1Option]]][\[Phi]1,\[Phi]2,\[Phi]4,\[Phi]3][m,Ma,Mb,Md,Mc]+\[ScriptCapitalI]2[\[Omega]1,\[Omega]2,(Evaluate[If[ListQ[#1],FilterRules[#1,Options[NIntegrate]],{}]]&)[OptionValue[I2Option]]][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][m,Ma,Mb,Mc,Md],\[ScriptCapitalM]0[(1-\[Omega]1+\[Omega]2)/\[Omega]2,1/\[Omega]2][\[Phi]2,\[Phi]1,\[Phi]4,\[Phi]3][m,Mb,Ma,Md,Mc]+\[ScriptCapitalM]0[\[Omega]2/(1-\[Omega]1+\[Omega]2),\[Omega]1/(1-\[Omega]1+\[Omega]2)][\[Phi]3,\[Phi]4,\[Phi]1,\[Phi]2][m,Mc,Md,Ma,Mb]+\[ScriptCapitalI]1[\[Omega]2/\[Omega]1,((1+1/\[Omega]2-\[Omega]1/\[Omega]2) \[Omega]2)/\[Omega]1,(Evaluate[If[ListQ[#1],FilterRules[#1,Options[NIntegrate]],{}]]&)[OptionValue[I1Option]]][\[Phi]3,\[Phi]4,\[Phi]2,\[Phi]1][m,Mc,Md,Mb,Ma]+\[ScriptCapitalI]2[\[Omega]2/(1-\[Omega]1+\[Omega]2),\[Omega]1/(1-\[Omega]1+\[Omega]2),(Evaluate[If[ListQ[#1],FilterRules[#1,Options[NIntegrate]],{}]]&)[OptionValue[I2Option]]][\[Phi]3,\[Phi]4,\[Phi]1,\[Phi]2][m,Mc,Md,Ma,Mb]]+Which[0<\[Omega]1<1&&\[Omega]2>=\[Omega]1,\[ScriptCapitalI]3[1/\[Omega]1,(1-\[Omega]1+\[Omega]2)/\[Omega]1,(Evaluate[If[ListQ[#1],FilterRules[#1,Options[NIntegrate]],{}]]&)[OptionValue[I3Option]]][\[Phi]4,\[Phi]3,\[Phi]2,\[Phi]1][m,Md,Mc,Mb,Ma]+\[ScriptCapitalI]3[\[Omega]2/\[Omega]1,((1+1/\[Omega]2-\[Omega]1/\[Omega]2) \[Omega]2)/\[Omega]1,(Evaluate[If[ListQ[#1],FilterRules[#1,Options[NIntegrate]],{}]]&)[OptionValue[I3Option]]][\[Phi]3,\[Phi]4,\[Phi]2,\[Phi]1][m,Mc,Md,Mb,Ma],\[Omega]2>=\[Omega]1&&\[Omega]1>=1,\[ScriptCapitalI]3[\[Omega]1,\[Omega]2,(Evaluate[If[ListQ[#1],FilterRules[#1,Options[NIntegrate]],{}]]&)[OptionValue[I3Option]]][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][m,Ma,Mb,Mc,Md]+\[ScriptCapitalI]3[(1+1/\[Omega]2-\[Omega]1/\[Omega]2) \[Omega]2,\[Omega]2,(Evaluate[If[ListQ[#1],FilterRules[#1,Options[NIntegrate]],{}]]&)[OptionValue[I3Option]]][\[Phi]2,\[Phi]1,\[Phi]3,\[Phi]4][m,Mb,Ma,Mc,Md],0<\[Omega]1<1&&\[Omega]1>\[Omega]2,\[ScriptCapitalI]3[\[Omega]1/\[Omega]2,1/\[Omega]2,(Evaluate[If[ListQ[#1],FilterRules[#1,Options[NIntegrate]],{}]]&)[OptionValue[I3Option]]][\[Phi]1,\[Phi]2,\[Phi]4,\[Phi]3][m,Ma,Mb,Md,Mc]+\[ScriptCapitalI]3[(1-\[Omega]1+\[Omega]2)/\[Omega]2,1/\[Omega]2,(Evaluate[If[ListQ[#1],FilterRules[#1,Options[NIntegrate]],{}]]&)[OptionValue[I3Option]]][\[Phi]2,\[Phi]1,\[Phi]4,\[Phi]3][m,Mb,Ma,Md,Mc],\[Omega]1>=1&&\[Omega]1>\[Omega]2,\[ScriptCapitalI]3[1/((1+1/\[Omega]2-\[Omega]1/\[Omega]2) \[Omega]2),\[Omega]1/((1+1/\[Omega]2-\[Omega]1/\[Omega]2) \[Omega]2),(Evaluate[If[ListQ[#1],FilterRules[#1,Options[NIntegrate]],{}]]&)[OptionValue[I3Option]]][\[Phi]4,\[Phi]3,\[Phi]1,\[Phi]2][m,Md,Mc,Ma,Mb]+\[ScriptCapitalI]3[\[Omega]2/(1-\[Omega]1+\[Omega]2),\[Omega]1/(1-\[Omega]1+\[Omega]2),(Evaluate[If[ListQ[#1],FilterRules[#1,Options[NIntegrate]],{}]]&)[OptionValue[I3Option]]][\[Phi]3,\[Phi]4,\[Phi]1,\[Phi]2][m,Mc,Md,Ma,Mb]];


(* ::Input::Initialization:: *)
\[ScriptCapitalM]0[\[Omega]1_,\[Omega]2_][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,Ma_,Mb_,Mc_,Md_]:=4 g^2 \[Omega]1 NIntegrate[(\[Phi]1[(\[Omega]2-\[Omega]1+x)/(\[Omega]2-\[Omega]1+1)] \[Phi]2[y] \[Phi]3[x] \[Phi]4[(y \[Omega]1)/\[Omega]2])/(y \[Omega]1-\[Omega]2-x)^2,{x,0,1},{y,0,1}];


(* ::Code:: *)
(*\[ScriptCapitalM]0[\[Omega]1_,\[Omega]2_][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,Ma_,Mb_,Mc_,Md_]:=0;*)


(* ::Input::Initialization:: *)
\[ScriptCapitalM]1[\[Omega]1_,\[Omega]2_,opt:OptionsPattern[{I1Option->OptionsPattern[],I2Option->OptionsPattern[],I3Option->OptionsPattern[]}]][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,Ma_,Mb_,Mc_,Md_]:=If[\[Omega]1>1,\[ScriptCapitalI]1[1/\[Omega]1,(1-\[Omega]1+\[Omega]2)/\[Omega]1,(Evaluate[If[ListQ[#1],FilterRules[#1,Options[NIntegrate]],{}]]&)[OptionValue[I1Option]]][\[Phi]4,\[Phi]3,\[Phi]2,\[Phi]1][m,Md,Mc,Mb,Ma],\[ScriptCapitalI]1[\[Omega]1,\[Omega]2,(Evaluate[If[ListQ[#1],FilterRules[#1,Options[NIntegrate]],{}]]&)[OptionValue[I1Option]]][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][m,Ma,Mb,Mc,Md]]+If[\[Omega]2>\[Omega]1,\[ScriptCapitalI]2[\[Omega]1,\[Omega]2,(Evaluate[If[ListQ[#1],FilterRules[#1,Options[NIntegrate]],{}]]&)[OptionValue[I2Option]]][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][m,Ma,Mb,Mc,Md],\[ScriptCapitalI]2[\[Omega]2/(1-\[Omega]1+\[Omega]2),\[Omega]1/(1-\[Omega]1+\[Omega]2),(Evaluate[If[ListQ[#1],FilterRules[#1,Options[NIntegrate]],{}]]&)[OptionValue[I2Option]]][\[Phi]3,\[Phi]4,\[Phi]1,\[Phi]2][m,Mc,Md,Ma,Mb]]+Which[0<\[Omega]1<1&&\[Omega]2>=\[Omega]1,\[ScriptCapitalI]3[1/\[Omega]1,(1-\[Omega]1+\[Omega]2)/\[Omega]1,(Evaluate[If[ListQ[#1],FilterRules[#1,Options[NIntegrate]],{}]]&)[OptionValue[I3Option]]][\[Phi]4,\[Phi]3,\[Phi]2,\[Phi]1][m,Md,Mc,Mb,Ma],\[Omega]2>=\[Omega]1&&\[Omega]1>=1,\[ScriptCapitalI]3[\[Omega]1,\[Omega]2,(Evaluate[If[ListQ[#1],FilterRules[#1,Options[NIntegrate]],{}]]&)[OptionValue[I3Option]]][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][m,Ma,Mb,Mc,Md],0<\[Omega]1<1&&\[Omega]1>\[Omega]2,\[ScriptCapitalI]3[(1-\[Omega]1+\[Omega]2)/\[Omega]2,1/\[Omega]2,(Evaluate[If[ListQ[#1],FilterRules[#1,Options[NIntegrate]],{}]]&)[OptionValue[I3Option]]][\[Phi]2,\[Phi]1,\[Phi]4,\[Phi]3][m,Mb,Ma,Md,Mc],\[Omega]1>=1&&\[Omega]1>\[Omega]2,\[ScriptCapitalI]3[\[Omega]2/(1-\[Omega]1+\[Omega]2),\[Omega]1/(1-\[Omega]1+\[Omega]2),(Evaluate[If[ListQ[#1],FilterRules[#1,Options[NIntegrate]],{}]]&)[OptionValue[I3Option]]][\[Phi]3,\[Phi]4,\[Phi]1,\[Phi]2][m,Mc,Md,Ma,Mb]];


(*General PV*)


(* ::Code:: *)
(*$PVM="Subtraction"(*"Differential"*);*)


$PVM="Differential";


Unevaluated@(\[ScriptCapitalI]1[\[Omega]1_,\[Omega]2_,opt:OptionsPattern[]][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,M1_,M2_,M3_,M4_]:=
	-4 g^2 NIntegrate[expr,{x,0,1(*-(1./10^7)*)},{y,0,1},Evaluate[FilterRules[{opt},Options[NIntegrate]]]]
	)/.expr->PVInt[1/((y-1) \[Omega]1+(1-x) \[Omega]2)^2 \[Omega]1 \[Omega]2 \[Phi]1[(x \[Omega]2)/(1+\[Omega]2-\[Omega]1)] \[Phi]4[x]\[Phi]2[y] \[Phi]3[y \[Omega]1] ,y,(\[Omega]1-(1-x) \[Omega]2)/\[Omega]1,PVMethod->$PVM];
Unevaluated@(\[ScriptCapitalI]2[\[Omega]1_,\[Omega]2_,opt:OptionsPattern[]][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,M1_,M2_,M3_,M4_]:=
	-4 g^2 NIntegrate[expr,{x,0,1(*-(1./10^7)*)},{y,0,1-1./10^7},Evaluate[FilterRules[{opt},Options[NIntegrate]]]]
	)/.expr->PVInt[1/(y \[Omega]1-x)^2 \[Omega]1 \[Phi]1[(x+\[Omega]2-\[Omega]1)/(1+\[Omega]2-\[Omega]1)] \[Phi]3[x] \[Phi]2[y] \[Phi]4[((y-1) \[Omega]1+\[Omega]2)/\[Omega]2] ,y,x/\[Omega]1,PVMethod->$PVM];
(*DO NOT CHANGE THIS LIMIT!!! Related to NIntegrate::errprec error. *)


(* ::Input:: *)
(*(*Differential PV Method: *)*)
(*\[ScriptCapitalI]1[\[Omega]1_,\[Omega]2_,opt:OptionsPattern[]][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,M1_,M2_,M3_,M4_]:=-4 g^2 NIntegrate[(1/(((y-1) \[Omega]1+(1-x) \[Omega]2)^2))\[Omega]1 \[Omega]2 (\[Phi]1[(x \[Omega]2)/(1+\[Omega]2-\[Omega]1)] \[Phi]4[x] (\[Phi]2[y] \[Phi]3[y \[Omega]1]-\[Phi]2[(\[Omega]1-(1-x) \[Omega]2)/\[Omega]1] \[Phi]3[\[Omega]1-(1-x) \[Omega]2]-(y-(\[Omega]1-(1-x) \[Omega]2)/\[Omega]1) (\[Phi]3[\[Omega]1-(1-x) \[Omega]2] Derivative[1][\[Phi]2][(\[Omega]1-(1-x) \[Omega]2)/\[Omega]1]+\[Omega]1 \[Phi]2[(\[Omega]1-(1-x) \[Omega]2)/\[Omega]1] Derivative[1][\[Phi]3][\[Omega]1-(1-x) \[Omega]2])))+(\[Omega]1 \[Omega]2 (\[Phi]1[(x \[Omega]2)/(1+\[Omega]2-\[Omega]1)] \[Phi]4[x] \[Phi]2[(\[Omega]1-(1-x) \[Omega]2)/\[Omega]1] \[Phi]3[\[Omega]1-(1-x) \[Omega]2]))/((\[Omega]1^2 (\[Omega]1-(1-x) \[Omega]2) ((\[Omega]1-(1-x) \[Omega]2)/\[Omega]1-1))/\[Omega]1),{x,0,1(*-(1./10^10)*)},{y,0,1},Evaluate[FilterRules[{opt},Options[NIntegrate]]]];*)
(*\[ScriptCapitalI]2[\[Omega]1_,\[Omega]2_,opt:OptionsPattern[]][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,M1_,M2_,M3_,M4_]:=-4 g^2 NIntegrate[(1/((y \[Omega]1-x)^2))\[Omega]1 (\[Phi]1[(x+\[Omega]2-\[Omega]1)/(1+\[Omega]2-\[Omega]1)] \[Phi]3[x] (\[Phi]2[y] \[Phi]4[((y-1) \[Omega]1+\[Omega]2)/\[Omega]2]-\[Phi]2[x/\[Omega]1] \[Phi]4[(x-\[Omega]1+\[Omega]2)/\[Omega]2]-(y-x/\[Omega]1) (\[Phi]4[((-1+x/\[Omega]1) \[Omega]1+\[Omega]2)/\[Omega]2] Derivative[1][\[Phi]2][x/\[Omega]1]+(\[Omega]1 \[Phi]2[x/\[Omega]1] Derivative[1][\[Phi]4][((-1+x/\[Omega]1) \[Omega]1+\[Omega]2)/\[Omega]2])/\[Omega]2)))+(\[Phi]1[(x+\[Omega]2-\[Omega]1)/(1+\[Omega]2-\[Omega]1)] \[Phi]3[x] \[Phi]2[x/\[Omega]1] \[Phi]4[(x-\[Omega]1+\[Omega]2)/\[Omega]2])/((\[Omega]1 x (x/\[Omega]1-1))/\[Omega]1),{x,0,1},{y,0(*1./10^10*),1-1./10^7},Evaluate[FilterRules[{opt},Options[NIntegrate]]]];*)


(* ::Input:: *)
(*(*Subtraction PV Method: *)*)
(*\[ScriptCapitalI]1[\[Omega]1_,\[Omega]2_,opt:OptionsPattern[]][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,M1_,M2_,M3_,M4_]:=-4 g^2 ( *)
(*NIntegrate[(\[Omega]1 \[Omega]2 \[Phi]1[(x \[Omega]2)/(1+\[Omega]2-\[Omega]1)] \[Phi]4[x] \[Phi]2[y] \[Phi]3[y \[Omega]1])/((y-1) \[Omega]1+(1-x) \[Omega]2)^2,{x,0,1},{y,0,(\[Omega]1-(1-x) \[Omega]2)/\[Omega]1-\[Lambda]},Evaluate[FilterRules[{opt},Options[NIntegrate]]]]+NIntegrate[(\[Omega]1 \[Omega]2 \[Phi]1[(x \[Omega]2)/(1+\[Omega]2-\[Omega]1)] \[Phi]4[x] \[Phi]2[y] \[Phi]3[y \[Omega]1])/((y-1) \[Omega]1+(1-x) \[Omega]2)^2,{x,0,1},{y,(\[Omega]1-(1-x) \[Omega]2)/\[Omega]1+\[Lambda],1},Evaluate[FilterRules[{opt},Options[NIntegrate]]]]-2/\[Lambda] NIntegrate[(\[Omega]1 \[Omega]2)/\[Omega]1^2 \[Phi]1[(x \[Omega]2)/(1+\[Omega]2-\[Omega]1)] \[Phi]4[x] \[Phi]2[(\[Omega]1-\[Omega]2+x \[Omega]2)/\[Omega]1] \[Phi]3[(\[Omega]1-\[Omega]2+x \[Omega]2)/\[Omega]1 \[Omega]1],{x,0,1},Evaluate[FilterRules[{opt},Options[NIntegrate]]]]);*)
(*\[ScriptCapitalI]2[\[Omega]1_,\[Omega]2_,opt:OptionsPattern[]][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,M1_,M2_,M3_,M4_]:=-4 g^2 NIntegrate[1/(y \[Omega]1-x)^2 \[Omega]1 (\[Phi]1[(x+\[Omega]2-\[Omega]1)/(1+\[Omega]2-\[Omega]1)] \[Phi]3[x] (\[Phi]2[y] \[Phi]4[((y-1) \[Omega]1+\[Omega]2)/\[Omega]2]-\[Phi]2[x/\[Omega]1] \[Phi]4[(x-\[Omega]1+\[Omega]2)/\[Omega]2]-(y-x/\[Omega]1) (\[Phi]4[((-1+x/\[Omega]1) \[Omega]1+\[Omega]2)/\[Omega]2] Derivative[1][\[Phi]2][x/\[Omega]1]+(\[Omega]1 \[Phi]2[x/\[Omega]1] Derivative[1][\[Phi]4][((-1+x/\[Omega]1) \[Omega]1+\[Omega]2)/\[Omega]2])/\[Omega]2)))(*Boundary*)+(\[Phi]1[(x+\[Omega]2-\[Omega]1)/(1+\[Omega]2-\[Omega]1)] \[Phi]3[x] \[Phi]2[x/\[Omega]1] \[Phi]4[(x-\[Omega]1+\[Omega]2)/\[Omega]2])/(\[Omega]1 x/\[Omega]1 (x/\[Omega]1-1)),{x,0,1},{y,0(*1./10^10*),1-1./10^7},Evaluate[FilterRules[{opt},Options[NIntegrate]]]];*)


(*Default \[Lambda] value*)
\[Lambda]=10^-6;


(* ::Input::Initialization:: *)
\[ScriptCapitalI]3[\[Omega]1_,\[Omega]2_,opt:OptionsPattern[]][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,Ma_,Mb_,Mc_,Md_]:=(-((4 \[Pi])/Nc))NIntegrate[(Mc^2+Md^2/\[Omega]2+(If[ListQ[m],m[[1]],m]^2-2\[Beta]^2)/(x-\[Omega]1)+(If[ListQ[m],m[[2]],m]^2-2\[Beta]^2)/(x-1)-(If[ListQ[m],m[[3]],m]^2-2\[Beta]^2)/(x-\[Omega]1+\[Omega]2)-(If[ListQ[m],m[[4]],m]^2-2\[Beta]^2)/x) \[Phi]1[(x-\[Omega]1+\[Omega]2)/(1+\[Omega]2-\[Omega]1)] \[Phi]2[x/\[Omega]1] \[Phi]3[x] \[Phi]4[(x-\[Omega]1+\[Omega]2)/\[Omega]2],{x,0,1},Evaluate[FilterRules[{opt},Options[NIntegrate]]]];


(* ::Section:: *)
(*Solve 't Hooft eqn*)


Clear[m1,m2,\[Beta],Nx]; (*\[Beta]=1 unit*)
Nx=500;(*the size of the working matrix*)
\[Beta]=1; (*the mass unit,definition \[Beta]^2=(g^2 Nc)/(2 \[Pi])*)
(*m1=3.2*\[Beta];(*m1 and m2 are the bare masses of the quark and the anti-quark.*)
m2=3.2*\[Beta];*)
gglo=1;Nc=(\[Beta]^2 2\[Pi])/g^2;(*\[DoubleStruckCapitalC] denotes the interchange of final states.*)
vMatx[n_,m_]:=(vMatx[n-1,m-1] m/(m-1)+(8m)/(n+m-1) ((1+(-1)^(n+m))/2));
vMatx[1,m_]:=4(1+(-1)^(m+1));
vMatx[n_,1]:=4/n (1+(-1)^(n+1));
Determine\[Phi]x[m1_,m2_,\[Beta]_,Nx_]:=
Module[{HMatx,VMatx,vals,vecs,g,\[Phi]x,\[Phi]},
SetSharedVariable[vecs,m1,m2,\[Beta],g];
SetSharedFunction[g];
HMatx=ParallelTable[4Min[n,m]((-1)^(n+m) (m1^2-\[Beta]^2)+(m2^2-\[Beta]^2)),{n,1,Nx},{m,1,Nx},DistributedContexts->{"OneFlavour`Private`"}];
VMatx=ParallelTable[vMatx[n,m],{n,1,Nx},{m,1,Nx},DistributedContexts->{"OneFlavour`Private`"}];
{vals,vecs}=Eigensystem[N[HMatx+VMatx]];
Do[g[j]=Dot[ParallelTable[Sin[i ArcCos[2Global`x-1]],{i,1,Nx},DistributedContexts->{"OneFlavour`Private`"}],vecs[[j]]],{j,1,Nx}];
\[Phi]x=ParallelTable[Quiet[g[Nx-n]/Sqrt[NIntegrate[g[Nx-n]^2,{Global`x,0,1}]]],{n,0,20},DistributedContexts->{"OneFlavour`Private`"}];
Clear[g];
{Sqrt[Reverse[vals]]\[Beta],\[Phi]x}
];
accDetermine\[Phi][m1_,m2_,beta_,Nb_]:=If[MatchQ[m1,m2],accDetermine\[Phi]1[m1,m2,beta,Nb],accDetermine\[Phi]2[m1,m2,beta,Nb]];
accDetermine\[Phi]1[m1_,m2_,beta_,Nb_]:=
Module[{psi,\[Epsilon]=10^-6,Kernel1,Kernel2,hMT,sMT,sMatrix,hMTUp,hMT1,hMatrix,eg,vals,vecs,func,\[Mu],\[Mu]s,nfunc,\[Beta],\[Beta]start=1.1,norm},
(*\[Beta]=If[m1\[GreaterEqual]Sqrt[2],First@#,#]&[x/.FindInstance[x*\[Pi]*Cot[\[Pi]*x]-(1-m1^2)==0&&0<x<2,x,Reals,2]];*)
\[Beta]=x/.FindInstance[x*\[Pi]*Cot[\[Pi]*x]-(1-m1^2)==0&&0<x<2,x,Reals,2];
(*While[Chop[\[Beta]*\[Pi]*Cot[\[Pi]*\[Beta]]-(m1^2-1)]!=0,\[Beta]=x/.FindRoot[x*\[Pi]*Cot[\[Pi]*x]-(m1^2-1)==0,{x,\[Beta]start}];Print["\[Beta]=",\[Beta],"   ",\[Beta]*\[Pi]*Cot[\[Pi]*\[Beta]]-(m1^2-1)];\[Beta]start=\[Beta]start+0.1];*)
Print["\[Beta]=",\[Beta],"   ",\[Beta]*\[Pi]*Cot[\[Pi]*\[Beta]]-(1-m1^2)];
Kernel1[n_][x_?NumberQ]:=NIntegrate[psi[n,y]/(x-y)^2,{y,0,x-\[Epsilon]}];
Kernel2[n_][x_?NumberQ]:=NIntegrate[psi[n,y]/(x-y)^2,{y,x+\[Epsilon],1}];
(*psi[n_,x_]:=psi[n,x]=If[m1\[GreaterEqual]Sqrt[2],Which[n==0,x^(2-\[Beta])*(1-x)^\[Beta],n==1,(1-x)^(2-\[Beta])*x^\[Beta],n>=2,Sin[(n-1)*\[Pi]*x]],Which[n==0,x^(2-\[Beta][[1]])*(1-x)^\[Beta][[1]],n==1,(1-x)^(2-\[Beta][[1]])*x^\[Beta][[1]],n\[Equal]2,x^(2-\[Beta][[2]])*(1-x)^\[Beta][[2]],n\[Equal]3,(1-x)^(2-\[Beta][[2]])*x^\[Beta][[2]],n\[GreaterEqual]4,Sin[(n-3)*\[Pi]*x]]];*)
psi[n_,x_]:=psi[n,x]=Which[n==0,x^(2-\[Beta][[1]])*(1-x)^\[Beta][[1]],n==1,(1-x)^(2-\[Beta][[1]])*x^\[Beta][[1]],n==2,x^(2-\[Beta][[2]])*(1-x)^\[Beta][[2]],n==3,(1-x)^(2-\[Beta][[2]])*x^\[Beta][[2]],n>=4,Sin[(n-3)*\[Pi]*x]];
hMT[m_,n_]:=hMT[m,n]=NIntegrate[psi[m,x]((m1^2-1)/(x*(1-x))*psi[n,x]-Kernel1[n][x]-Kernel2[n][x]+(2psi[n,x])/\[Epsilon]),{x,\[Epsilon],1-\[Epsilon]},WorkingPrecision->20];
sMT[m_,n_]:=sMT[m,n]=NIntegrate[psi[m,x]psi[n,x],{x,0,1}];
sMatrix=ParallelTable[Quiet[sMT[m,n]],{m,0,Nb},{n,0,Nb},DistributedContexts->{"OneFlavour`Private`"}]//Chop;
(*Print["Half"];*)
hMTUp=PadLeft[#,Nb+1]&/@(Normal@SparseArray[ParallelMap[(#+1)->Quiet[hMT@@#]&,Flatten[Table[{m,n},{m,0,Nb},{n,m,Nb}],1],DistributedContexts->{"OneFlavour`Private`"}]]);
(*Print[hMTUp];*)
hMatrix=Transpose[hMTUp]+hMTUp-DiagonalMatrix[Diagonal[hMTUp]];
(*Print[MatrixForm[hMatrix]];*)
{\[Mu]s,vecs}=Transpose[SortBy[Transpose[Eigensystem[Inverse[sMatrix].hMatrix]],First]];
\[Mu]=Sqrt[\[Mu]s];
(*eg[\[Mu]_]:=Det[hMatrix-\[Mu]^2*sMatrix];
\[Mu]=Sort@DeleteDuplicatesBy[Table[\[Mu]/.FindRoot[eg[\[Mu]]==0,{\[Mu],a,a+0.2}],{a,0,Nb^2,0.2}],SetPrecision[#,2]&];*)
Print[\[Mu]];
(*vecs=(Flatten@NullSpace[hMatrix-#^2 sMatrix,Tolerance->0.001])&/@\[Mu];*)
Print[vecs];Print[Dimensions@vecs];
func=ParallelTable[Table[psi[i,Global`x],{i,0,Nb}].vecs[[j]],{j,1,Length[vecs]},DistributedContexts->{"OneFlavour`Private`"}];
norm=ParallelTable[Sqrt[NIntegrate[func[[n]]^2,{Global`x,0,1}]],{n,1,Length[func]},DistributedContexts->{"OneFlavour`Private`"}];
nfunc=ParallelTable[func[[n]]/norm[[n]],{n,1,Length[func]},DistributedContexts->{"OneFlavour`Private`"}];
Clear[func,vecs];
{\[Mu],nfunc}
];
accDetermine\[Phi]2[m1_,m2_,beta_,Nb_]:=
Module[{psi,\[Epsilon]=10^-6,Kernel1,Kernel2,hMT,sMT,sMatrix,hMTUp,hMT1,hMatrix,eg,vals,vecs,func,\[Mu],\[Mu]s,nfunc,\[Beta],\[Beta]start=1.1,norm},
\[Beta]=x/.{FindInstance[x*\[Pi]*Cot[\[Pi]*x]-(1-m1^2)==0&&0<x<2,x,Reals,2],FindInstance[x*\[Pi]*Cot[\[Pi]*x]-(1-m2^2)==0&&0<x<2,x,Reals,2]};
(*\[Beta]=Flatten[x/.{FindInstance[x*\[Pi]*Cot[\[Pi]*x]-(1-m1^2)==0&&0<x<1,x,Reals,1],FindInstance[x*\[Pi]*Cot[\[Pi]*x]-(1-m2^2)==0&&0<x<1,x,Reals,1]}];*)
(*While[Chop[\[Beta]*\[Pi]*Cot[\[Pi]*\[Beta]]-(m1^2-1)]!=0,\[Beta]=x/.FindRoot[x*\[Pi]*Cot[\[Pi]*x]-(m1^2-1)==0,{x,\[Beta]start}];Print["\[Beta]=",\[Beta],"   ",\[Beta]*\[Pi]*Cot[\[Pi]*\[Beta]]-(m1^2-1)];\[Beta]start=\[Beta]start+0.1];*)
Print["\[Beta]=",\[Beta],"   ",(#*\[Pi]*Cot[\[Pi]*#]-(1-m1^2))&@\[Beta][[1]],"   ",(#*\[Pi]*Cot[\[Pi]*#]-(1-m2^2))&@\[Beta][[2]]];
Kernel1[n_][x_?NumberQ]:=NIntegrate[psi[n,y]/(x-y)^2,{y,0,x-\[Epsilon]}];
Kernel2[n_][x_?NumberQ]:=NIntegrate[psi[n,y]/(x-y)^2,{y,x+\[Epsilon],1}];
(*psi[n_,x_]:=psi[n,x]=Which[n==0,(1-x)^(2-\[Beta][[1]])*x^\[Beta][[1]],n==1,x^(2-\[Beta][[2]])*(1-x)^\[Beta][[2]],n\[GreaterEqual]2,Sin[(n-1)*\[Pi]*x]];*)
psi[n_,x_]:=psi[n,x]=Which[n==0,x^(\[Beta][[1,1]])*(1-x)^(2-\[Beta][[1,1]]),n==1,x^(\[Beta][[1,2]])*(1-x)^(2-\[Beta][[1,2]]),n==2,x^(2-\[Beta][[2,1]])*(1-x)^\[Beta][[2,1]],n==3,x^(2-\[Beta][[2,2]])*(1-x)^\[Beta][[2,2]],n>=4,Sin[(n-3)*\[Pi]*x]];
hMT[m_,n_]:=hMT[m,n]=NIntegrate[psi[m,x](((m1^2-1)/x+(m2^2-1)/(1-x))*psi[n,x]-Kernel1[n][x]-Kernel2[n][x]+(2psi[n,x])/\[Epsilon]),{x,\[Epsilon],1-\[Epsilon]},WorkingPrecision->20];
sMT[m_,n_]:=sMT[m,n]=NIntegrate[psi[m,x]psi[n,x],{x,0,1}];
sMatrix=ParallelTable[Quiet[sMT[m,n]],{m,0,Nb},{n,0,Nb},DistributedContexts->{"OneFlavour`Private`"}]//Chop;
(*Print["Half"];*)
hMTUp=Normal@SparseArray[ParallelMap[(#+1)->Quiet[hMT@@#]&,Flatten[Table[{m,n},{m,0,Nb},{n,0,Nb}],1],DistributedContexts->{"OneFlavour`Private`"}]];
(*Print[hMTUp];*)
hMatrix=(*Transpose[hMTUp]+*)hMTUp(*-DiagonalMatrix[Diagonal[hMTUp]]*);
(*Print[MatrixForm[hMatrix]];*)
{\[Mu]s,vecs}=Transpose[SortBy[Transpose[Eigensystem[Inverse[sMatrix].hMatrix]],First]];
\[Mu]=Sqrt[\[Mu]s];
(*eg[\[Mu]_]:=Det[hMatrix-\[Mu]^2*sMatrix];
\[Mu]=Sort@DeleteDuplicatesBy[Table[\[Mu]/.FindRoot[eg[\[Mu]]==0,{\[Mu],a,a+0.2}],{a,0,Nb^2,0.2}],SetPrecision[#,2]&];*)
Print[\[Mu]];
(*vecs=(Flatten@NullSpace[hMatrix-#^2 sMatrix,Tolerance->0.001])&/@\[Mu];*)
Print[vecs];Print[Dimensions@vecs];
func=ParallelTable[Table[psi[i,Global`x],{i,0,Nb}].vecs[[j]],{j,1,Length[vecs]},DistributedContexts->{"OneFlavour`Private`"}];
norm=ParallelTable[Sqrt[NIntegrate[func[[n]]^2,{Global`x,0,1}]],{n,1,Length[func]},DistributedContexts->{"OneFlavour`Private`"}];
nfunc=ParallelTable[func[[n]]/norm[[n]],{n,1,Length[func]},DistributedContexts->{"OneFlavour`Private`"}];
Clear[func,vecs];
{\[Mu],nfunc}
];


Solvet[m1_,m2_,opt:OptionsPattern[{SolveMethod->"BSW",DataDir->dirglo,MatrixSize->500,Force->False}]]:=Module[{filename,filenameacc,ValsB,\[Phi]xB,\[CapitalPhi]B},
dir=OptionValue[DataDir];Nx=OptionValue[MatrixSize];
filename=If[m1==m2,"/eigenstate_m-"<>ToString[m1],"/eigenstate_m1-"<>ToString[m1]<>"_m2-"<>ToString[m2]]<>".wdx";
filenameacc=If[m1==m2,"/acceigenstate_m-"<>ToString[m1],"/acceigenstate_m1-"<>ToString[m1]<>"_m2-"<>ToString[m2]]<>".wdx";
(*If[FileNames[filenameacc,dir,Infinity]=={},*)(*If[ChoiceDialog["Choose to use BSW SolveMethod or to use the original solution suggested by 't Hooft, ",{"BSW SolveMethod"\[Rule]True,"Brute force integration"\[Rule]False}],Determine=Determine\[Phi]x,filename=filenameacc;Determine=accDetermine\[Phi]]];*)
Which[OptionValue[SolveMethod]=="BSW",Determine=Determine\[Phi]x,OptionValue[SolveMethod]=="'t Hooft",Determine=accDetermine\[Phi];filename=filenameacc];
(*Print[dir<>filename];Print[filenameacc];Print[OptionValue[SolveMethod]=="'t Hooft"];*)
If[FileNames[filename,dir,Infinity]=={}||OptionValue[Force],
{
{ValsB,\[Phi]xB}=Determine[m1,m2,\[Beta],Nx];
Set@@{\[CapitalPhi]B[Global`x_],Boole[0<=Global`x<=1]\[Phi]xB};
Export[dir<>filename,{ValsB,\[Phi]xB}]
},
{
{ValsB,\[Phi]xB}=Import[dir<>filename];
(*Print[{ValsB,\[Phi]xB}];*)
Set@@{\[CapitalPhi]B[Global`x_],Boole[0<=Global`x<=1]\[Phi]xB}
(*Set@@{\[CapitalPhi]B[x_],Boole[0<=x<=1]\[Phi]xB};*)
}
];
{ValsB,\[CapitalPhi]B[Global`x]}]


(* ::Section:: *)
(*Kinematics*)


\[Omega]1S[s_][M1_,M2_,M3_,M4_]:=(-M1^2+M2^2+s-Sqrt[M1^4+(M2^2-s)^2-2 M1^2 (M2^2+s)])/(M3^2-M4^2+s+Sqrt[M3^4+(M4^2-s)^2-2 M3^2 (M4^2+s)]);
\[Omega]2S[s_][M1_,M2_,M3_,M4_]:=(-M3^2+M4^2+s-Sqrt[M3^4+(M4^2-s)^2-2 M3^2 (M4^2+s)])/(M3^2-M4^2+s+Sqrt[M3^4+(M4^2-s)^2-2 M3^2 (M4^2+s)]);
\[Omega]1So[s_][M1_,M2_,M3_,M4_]:=-((M2^2 M3^2+M2^2 M4^2-M2^2 s+M3^2 s+M4^2 s-s^2-M2^2 Sqrt[M3^4+(M4^2-s)^2-2 M3^2 (M4^2+s)]-s Sqrt[M3^4+(M4^2-s)^2-2 M3^2 (M4^2+s)]+M1^2 (-M3^2-M4^2+s+Sqrt[M3^4+(M4^2-s)^2-2 M3^2 (M4^2+s)])+Sqrt[2] \[Sqrt]((M1^4+(M2^2-s)^2-2 M1^2 (M2^2+s)) (M3^4-(M4^2-s) (-M4^2+s+Sqrt[M3^4+(M4^2-s)^2-2 M3^2 (M4^2+s)])-M3^2 (2 s+Sqrt[M3^4+(M4^2-s)^2-2 M3^2 (M4^2+s)]))))/(2 M3^2 (-M3^2+M4^2+s+Sqrt[M3^4+(M4^2-s)^2-2 M3^2 (M4^2+s)])));
\[Omega]2So[s_][M1_,M2_,M3_,M4_]:=(-M3^2-M4^2+s+Sqrt[-4 M3^2 M4^2+(-M3^2-M4^2+s)^2])/(2 M3^2);


test[s_,m1_,m2_,m3_,m4_]:=\[Omega]1S[s][m1,m2,m3,m4]


(* ::Section:: *)
(*Calculate Amplitude*)


\[Phi]n[n_][\[Phi]_][x_]:=\[Phi][x][[n+1]];
Mn[n_][vals_]:=vals[[n+1]];


(* ::Subsection:: *)
(*Single Flavour*)


Msum[mQ_,{n1_?IntegerQ,n2_?IntegerQ,n3_?IntegerQ,n4_?IntegerQ},opt:OptionsPattern[{SRange->{10^-3,2,0.01},Lambda->10^-6,SolveMethod->"BSW",MatrixSize->500,DataDir->dirglo,gvalue->gglo,I1Option->OptionsPattern[],I2Option->OptionsPattern[],I3Option->OptionsPattern[]}]]:=
Module[{\[Phi]xB,\[CapitalPhi]B,ValsB,\[Phi]x\[Pi],\[CapitalPhi]\[Pi],Vals\[Pi],M1,M2,M3,\[Phi]1,\[Phi]2,\[Phi]3,Ares,filename,\[Omega]now,m1,m2,M4,\[Phi]4,\[Omega]1,\[Omega]2,Sen,filenameacc,Determine,Mseq,\[CapitalPhi]Bi,Si,dir,\[Phi]1a,\[Phi]2a,\[Phi]3a,\[Phi]4a},
(*SetSharedVariable[m2,m1];*)
m1=mQ;m2=mQ;\[Lambda]=OptionValue[Lambda];g=OptionValue[gvalue];dir=OptionValue[DataDir];Nx=OptionValue[MatrixSize];
filename=If[m1==m2,"/eigenstate_m-"<>ToString[m1],"/eigenstate_m1-"<>ToString[m1]<>"_m2-"<>ToString[m2]]<>".wdx";
filenameacc=If[m1==m2,"/acceigenstate_m-"<>ToString[m1],"/acceigenstate_m1-"<>ToString[m1]<>"_m2-"<>ToString[m2]]<>".wdx";
(*If[FileNames[filenameacc,dir,Infinity]=={},*)(*If[ChoiceDialog["Choose to use BSW SolveMethod or to use the original solution suggested by 't Hooft, ",{"BSW SolveMethod"\[Rule]True,"Brute force integration"\[Rule]False}],Determine=Determine\[Phi]x,filename=filenameacc;Determine=accDetermine\[Phi]]];*)
Which[OptionValue[SolveMethod]=="BSW",Determine=Determine\[Phi]x,filename=filenameacc;OptionValue[SolveMethod]=="'t Hooft",Determine=accDetermine\[Phi]];
If[FileNames[filename,dir,Infinity]=={},
{
{ValsB,\[Phi]xB}=Determine[m1,m2,\[Beta],Nx];
Set@@{\[CapitalPhi]B[Global`x_],Boole[0<=Global`x<=1]\[Phi]xB};
Export[dir<>filename,{ValsB,\[Phi]xB}]
},
{
{ValsB,\[Phi]xB}=Import[dir<>filename];
Set@@{\[CapitalPhi]B[Global`x_],Boole[0<=Global`x<=1]\[Phi]xB};
}
];
(*Set@@{\[CapitalPhi]B[x_?NumberQ],If[0<x<1,\[CapitalPhi]Bi[x],Table[0,{n,Length@\[Phi]xB}]]};*)
Print["Wavefunction build complete."];
(*Print[\[Phi]n[#][\[CapitalPhi]B][x]&[1]];*)
{M1,\[Phi]1a}={Mn[#][ValsB],\[Phi]n[#][\[CapitalPhi]B]}&[n1];
{M2,\[Phi]2a}={Mn[#][ValsB],\[Phi]n[#][\[CapitalPhi]B]}&[n2];
{M3,\[Phi]3a}={Mn[#][ValsB],\[Phi]n[#][\[CapitalPhi]B]}&[n3];
{M4,\[Phi]4a}={Mn[#][ValsB],\[Phi]n[#][\[CapitalPhi]B]}&[n4];
Set@@{\[Phi]1[x_],\[Phi]1a[x]};
Set@@{\[Phi]2[x_],\[Phi]2a[1-x]};
Set@@{\[Phi]3[x_],\[Phi]3a[x]};
Set@@{\[Phi]4[x_],\[Phi]4a[1-x]};
(*Set[\[Phi]1[x_?NumberQ],Piecewise[{{\[Phi]1a[x],0<x<1},{\[Phi]1a[z]/(x-z)^2/((m1^2-1)/x+(m2^2-1)/(1-x)-M1^2),x>1||x<0}},0]];
Set[\[Phi]2[x_?NumberQ],Piecewise[{{\[Phi]2a[x],0<x<1},{\[Phi]2a[z]/(x-z)^2/((m1^2-1)/x+(m2^2-1)/(1-x)-M2^2),x>1||x<0}},0]];
Set[\[Phi]3[x_?NumberQ],Piecewise[{{\[Phi]3a[x],0<x<1},{\[Phi]3a[z]/(x-z)^2/((m1^2-1)/x+(m2^2-1)/(1-x)-M3^2),x>1||x<0}},0]];
Set[\[Phi]4[x_?NumberQ],Piecewise[{{\[Phi]4a[x],0<x<1},{\[Phi]4a[z]/(x-z)^2/((m1^2-1)/x+(m2^2-1)/(1-x)-M4^2),x>1||x<0}},0]];*)
(*Print[\[Phi]1[x]];*)
Mseq=Sequence[M1,M2,M3,M4];Si=If[n1+n2>=n3+n4,M1+M2,M3+M4];
Print["M1=",M1,"  M2=",M2,"  M3=",M3,"  M4=",M4];
(*Print[$Context];*)
(*DistributeDefinitions[M1,M2,M3,M4,\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4,EXPR,\[ScriptCapitalM]0,\[ScriptCapitalI]1,\[ScriptCapitalI]2,\[ScriptCapitalI]3,\[Omega]1S,\[Omega]2S];*)
Which[MatchQ[m1,Global`u], m1 =(*Global`u= *)0.045,MatchQ[m1,Global`c], m1=(*Global`c =*) 4.19022,MatchQ[m1,Global`s], m1 =(*Global`s=*)0.749];
Print["m1=",m1];
{{{n1,n2,n3,n4},{M1,M2,M3,M4},{m1}},ParallelTable[Sen=Ssqur^2;\[Omega]1=\[Omega]1S[Sen][M1,M2,M3,M4];\[Omega]2=\[Omega]2S[Sen][M1,M2,M3,M4];(*Print[\[ScriptCapitalI]2[\[Omega]1,\[Omega]2][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][m1,M1,M2,M3,M4]];*)
{Ssqur,EXPR[\[Omega]1,\[Omega]2,I1Option->OptionValue[I1Option],I2Option->OptionValue[I2Option],I3Option->OptionValue[I3Option](*,Evaluate@FilterRules[{opt},Options[NIntegrate]]*)][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][m1,M1,M2,M3,M4]}
,{Ssqur,Si+OptionValue[SRange][[1]],Si+OptionValue[SRange][[2]],OptionValue[SRange][[3]]},DistributedContexts->{"OneFlavour`Private`","OneFlavour`"}]}
];




(* ::Subsection:: *)
(*Double Flavours*)


Msum2[{mQ_,mq_},{n1_?IntegerQ,n2_?IntegerQ,n3_?IntegerQ,n4_?IntegerQ},opt:OptionsPattern[{SRange->{10^-3,2,0.01},Lambda->10^-6,SolveMethod->"BSW",DataDir->dirglo,gvalue->gglo,ProcessType->({{a,b},{b,a}}->{{a,b},{b,a}}),AssignQuark->{a->m1,b->m2},I1Option->OptionsPattern[],I2Option->OptionsPattern[],I3Option->OptionsPattern[],AnotherKinematics->False}]]:=
Module[{\[Phi]x1,\[CapitalPhi]1,Vals1,\[Phi]x2,\[CapitalPhi]2,Vals2,\[Phi]x3,\[CapitalPhi]3,Vals3,\[Phi]x4,\[CapitalPhi]4,Vals4,M1,M2,M3,M4,\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4,Ares,filename,\[Omega]now,m1,m2,\[Omega]1,\[Omega]2,Sen,filenameacc,Determine,Mseq,\[CapitalPhi]temp,dir,Si,ParA,ParB,ParC,ParD,\[ScriptCapitalM],\[ScriptCapitalM]0\[ScriptCapitalC]t,\[ScriptCapitalM]1t,Eigenlist,Masslist,\[CapitalPhi]list,\[Omega]1o,\[Omega]2o,\[Phi]1t,\[Phi]2t,\[Phi]3t,\[Phi]4t},
(*SetSharedVariable[m2,m1];*)
m1=mQ;m2=mq;\[Lambda]=OptionValue[Lambda];g=OptionValue[gvalue];dir=OptionValue[DataDir];
filename[m1_,m2_]:=Which[m1==m2,"/eigenstate_m-"<>ToString[m1],m1>m2,"/eigenstate_m1-"<>ToString[m1]<>"_m2-"<>ToString[m2],m1<m2,"/eigenstate_m1-"<>ToString[m2]<>"_m2-"<>ToString[m1]]<>".wdx";
filenameacc[m1_,m2_]:=Which[m1==m2,"/acceigenstate_m-"<>ToString[m1],m1>m2,"/acceigenstate_m1-"<>ToString[m1]<>"_m2-"<>ToString[m2],m1<m2,"/acceigenstate_m1-"<>ToString[m2]<>"_m2-"<>ToString[m1]]<>".wdx";
Which[OptionValue[SolveMethod]=="BSW",Determine=Determine\[Phi]x,filename=filenameacc;OptionValue[SolveMethod]=="'t Hooft",Determine=accDetermine\[Phi]];
{ParA,ParB}=Keys[OptionValue[ProcessType]];{ParC,ParD}=Values[OptionValue[ProcessType]];
If[SubsetQ[Flatten@{ParA,ParB,ParC,ParD},Keys[OptionValue[AssignQuark]]],Print["Convention checked out"],Abort[]];
Eigenlist={{Vals1,\[Phi]x1},{Vals2,\[Phi]x2},{Vals3,\[Phi]x3},{Vals4,\[Phi]x4}};
Masslist={ParA,ParB,ParC,ParD}/.OptionValue[AssignQuark];
\[CapitalPhi]list={\[CapitalPhi]1,\[CapitalPhi]2,\[CapitalPhi]3,\[CapitalPhi]4};
Which[
(Thread[MatchQ[#,{{a_,a_},{b_,a_}}]&[{ParA,ParB}],And])&&(Thread[MatchQ[#,{{b_,a_},{b_,b_}}]&[{ParC,ParD}],And])||(Thread[MatchQ[#,{{a_,a_},{a_,a_}}]&[{ParA,ParB}],And])&&(Thread[MatchQ[#,{{b_,a_},{a_,b_}}]&[{ParC,ParD}],And]),
\[ScriptCapitalM][\[Omega]1_,\[Omega]2_,opts__][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,M1_,M2_,M3_,M4_]:=\[ScriptCapitalM]0\[ScriptCapitalC]t[\[Omega]1,\[Omega]2,opts][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][m,M1,M2,M3,M4],
(Thread[MatchQ[#,{{a_,a_},{b_,a_}}]&[{ParA,ParB}],And])&&(Thread[MatchQ[#,{{b_,a_},{b_,b_}}]&[{ParC,ParD}],And]),
\[ScriptCapitalM][\[Omega]1_,\[Omega]2_,opts__][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,M1_,M2_,M3_,M4_]:=Evaluate[(#+\[ScriptCapitalR][#])&[\[ScriptCapitalM]0\[ScriptCapitalC]t[\[Omega]1,\[Omega]2,opts][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][m,M1,M2,M3,M4]]],
(Thread[MatchQ[#,{{a_,b_},{b_,a_}}]&[{ParA,ParB}],And])&&(Thread[MatchQ[#,{{a_,b_},{b_,a_}}]&[{ParC,ParD}],And]),
\[ScriptCapitalM][\[Omega]1_,\[Omega]2_,opts__][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,M1_,M2_,M3_,M4_]:=MVG[\[Omega]1,\[Omega]2,(*Evaluate@FilterRules[{opts},Options[NIntegrate]]*)opts][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4]+MHG[\[Omega]1,\[Omega]2,(*Evaluate@FilterRules[{opts},Options[NIntegrate]]*)opts][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4],
(Thread[MatchQ[#,{{a_,a_},{b_,b_}}]&[{ParA,ParB}],And])&&(Thread[MatchQ[#,{{b_,a_},{a_,b_}}]&[{ParC,ParD}],And]),
\[ScriptCapitalM][\[Omega]1_,\[Omega]2_,opts__][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,M1_,M2_,M3_,M4_]:=\[ScriptCapitalM]1[\[Omega]1,\[Omega]2,FilterRules[{opts},Options[\[ScriptCapitalM]1]]][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][{ParA[[1]],ParB[[1]],ParB[[1]],ParA[[1]]}/.OptionValue[AssignQuark],M1,M2,M3,M4],
(Thread[MatchQ[#,{{a_,b_},{b_,a_}}]&[{ParA,ParB}],And])&&(Thread[MatchQ[#,{{b_,b_},{a_,a_}}]&[{ParC,ParD}],And]),
\[ScriptCapitalM][\[Omega]1_,\[Omega]2_,opts__][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,M1_,M2_,M3_,M4_]:=\[ScriptCapitalM]1[\[Omega]1,\[Omega]2,FilterRules[{opts},Options[\[ScriptCapitalM]1]]][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][{ParA[[1]],ParA[[1]],ParB[[1]],ParB[[1]]}/.OptionValue[AssignQuark],M1,M2,M3,M4],
(Thread[MatchQ[#,{{a_,b_},{a_,b_}}]&[{ParA,ParB}],And])&&(Thread[MatchQ[#,{{a_,b_},{a_,b_}}]&[{ParC,ParD}],And]),
\[ScriptCapitalM][\[Omega]1_,\[Omega]2_,opts__][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,M1_,M2_,M3_,M4_]:=Evaluate[(#+\[ScriptCapitalR][#])&[\[ScriptCapitalM]1t[\[Omega]1,\[Omega]2,opts][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][{ParA[[1]],ParB[[2]],ParA[[1]],ParB[[2]]}/.OptionValue[AssignQuark],M1,M2,M3,M4]]],
(Thread[MatchQ[#,{{a_,b_},{b_,a_}}]&[{ParA,ParB}],And])&&(Thread[MatchQ[#,{{a_,b_},{b_,a_}}]&[{ParC,ParD}],And]),
\[ScriptCapitalM][\[Omega]1_,\[Omega]2_,opts__][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,M1_,M2_,M3_,M4_]:=Evaluate[\[ScriptCapitalM]0\[ScriptCapitalC]t[\[Omega]1,\[Omega]2,opts][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][m,M1,M2,M3,M4]+\[ScriptCapitalR][\[ScriptCapitalM]1t[\[Omega]1,\[Omega]2,opts][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][{ParA[[1]],ParA[[1]],ParB[[2]],ParA[[1]]}/.OptionValue[AssignQuark],M1,M2,M3,M4]]]
];
Do[
If[FileNames[filename[Sequence@@Masslist[[i]]],dir,Infinity]=={},
{
Evaluate[Eigenlist[[i]]]=Determine[Sequence@@Masslist[[i]],\[Beta],Nx];
Set@@{Evaluate[\[CapitalPhi]list[[i]]][Global`x_],Boole[0<=Global`x<=1]Eigenlist[[i,2]]};
(*\[CapitalPhi]list[[i]]=\[CapitalPhi]temp;*)
Export[dir<>filename[Sequence@@Masslist[[i]]],Eigenlist[[i]]];
},
{
Evaluate[Eigenlist[[i]]]=Import[dir<>filename[Sequence@@Masslist[[i]]]];
(*Print[Vals1];*)
Set@@{Evaluate[\[CapitalPhi]list[[i]]][Global`x_],Boole[0<=Global`x<=1]Eigenlist[[i,2]]};
(*\[CapitalPhi]list[[i]]=\[CapitalPhi]temp;*)
}
],{i,4}
];
(*Print[\[CapitalPhi]1];*)
(*Set@@{\[CapitalPhi]B[x_?NumberQ],If[0<x<1,\[CapitalPhi]Bi[x],Table[0,{n,Length@\[Phi]xB}]]};*)
Print["Wavefunction build complete."];
(*Print[\[Phi]n[#][\[CapitalPhi]B][x]&[1]];*)
{M1,\[Phi]1t}={Mn[#][Vals1],\[Phi]n[#][\[CapitalPhi]1]}&[n1];
{M2,\[Phi]2t}={Mn[#][Vals2],\[Phi]n[#][\[CapitalPhi]2]}&[n2];
{M3,\[Phi]3t}={Mn[#][Vals3],\[Phi]n[#][\[CapitalPhi]3]}&[n3];
{M4,\[Phi]4t}={Mn[#][Vals4],\[Phi]n[#][\[CapitalPhi]4]}&[n4];
Set@@{\[Phi]1[x_],\[Phi]1t[x/.x/;(!OrderedQ[ParA])->1-x]};
Set@@{\[Phi]2[x_],\[Phi]2t[x/.x/;(!OrderedQ[ParB])->1-x]};
Set@@{\[Phi]3[x_],\[Phi]3t[x/.x/;(!OrderedQ[ParC])->1-x]};
Set@@{\[Phi]4[x_],\[Phi]4t[x/.x/;(!OrderedQ[ParD])->1-x]};
(*Print[\[Phi]1[y]];*)
Mseq=Sequence[M1,M2,M3,M4];Si=If[n1+n2>=n3+n4,M1+M2,M3+M4];
Print["M1=",M1,"  M2=",M2,"  M3=",M3,"  M4=",M4];
\[ScriptCapitalM]0\[ScriptCapitalC]t[\[Omega]1_,\[Omega]2_,opts__][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,M1_,M2_,M3_,M4_]:=MHG[\[Omega]1,\[Omega]2,(*Evaluate@FilterRules[{opts},Options[NIntegrate]]*)opts][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4];
\[ScriptCapitalM]1t[\[Omega]1_,\[Omega]2_,opts__][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,M1_,M2_,M3_,M4_]:=\[ScriptCapitalM]1[\[Omega]1,\[Omega]2,FilterRules[{opts},Options[\[ScriptCapitalM]1]]][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][m,M1,M2,M3,M4];
(*Print[$Context];*)
(*DistributeDefinitions[M1,M2,M3,M4,\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4,EXPR,\[ScriptCapitalM]0,\[ScriptCapitalI]1,\[ScriptCapitalI]2,\[ScriptCapitalI]3,\[Omega]1S,\[Omega]2S];*)
{{{n1,n2,n3,n4},{M1,M2,M3,M4},{m1,m2}},ParallelTable[Sen=Ssqur^2;\[Omega]1=(\[Omega]1S/.\[Omega]1S/;OptionValue[AnotherKinematics]->\[Omega]1So)[Sen][M1,M2,M3,M4];\[Omega]2=(\[Omega]2S/.\[Omega]2S/;OptionValue[AnotherKinematics]->\[Omega]2So)[Sen][M1,M2,M3,M4];(*Print[$KernelID];*)(*Print[{Ssqur,\[ScriptCapitalM]0[\[Omega]1,\[Omega]2][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][m,M1,M2,M3,M4]}];*)
{Ssqur,\[ScriptCapitalM][\[Omega]1,\[Omega]2,opt][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][m,M1,M2,M3,M4]},{Ssqur,Si+OptionValue[SRange][[1]],Si+OptionValue[SRange][[2]],OptionValue[SRange][[3]]},DistributedContexts->{"OneFlavour`Private`","OneFlavour`"}]}
];




(* ::Subsection::Closed:: *)
(*Triple Flavours*)


Msum3[{mQ_,mq_,md_},{n1_?IntegerQ,n2_?IntegerQ,n3_?IntegerQ,n4_?IntegerQ},opt:OptionsPattern[{SRange->{10^-3,2,0.01},Lambda->10^-6,SolveMethod->"BSW",DataDir->dirglo,gvalue->gglo,ProcessType->({{a,b},{c,a}}->{{a,b},{c,a}}),AssignQuark->{a->m1,b->m2,c->m3},I1Option->OptionsPattern[],I2Option->OptionsPattern[],I3Option->OptionsPattern[]}]]:=
Module[{\[Phi]x1,\[CapitalPhi]1,Vals1,\[Phi]x2,\[CapitalPhi]2,Vals2,\[Phi]x3,\[CapitalPhi]3,Vals3,\[Phi]x4,\[CapitalPhi]4,Vals4,M1,M2,M3,M4,\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4,Ares,filename,\[Omega]now,m1,m2,m3,\[Omega]1,\[Omega]2,Sen,filenameacc,Determine,Mseq,dir,Si,\[ScriptCapitalM],\[ScriptCapitalM]0\[ScriptCapitalC]t,\[ScriptCapitalM]1t,Eigenlist,Masslist,ParA,ParB,ParC,ParD,\[CapitalPhi]list,\[Phi]1t,\[Phi]2t,\[Phi]3t,\[Phi]4t},
(*SetSharedVariable[m2,m1];*)
m1=mQ;m2=mq;m3=md;\[Lambda]=OptionValue[Lambda];g=OptionValue[gvalue];dir=OptionValue[DataDir];
filename[m1_,m2_]:=Which[m1==m2,"/eigenstate_m-"<>ToString[m1],m1>m2,"/eigenstate_m1-"<>ToString[m1]<>"_m2-"<>ToString[m2],m1<m2,"/eigenstate_m1-"<>ToString[m2]<>"_m2-"<>ToString[m1]]<>".wdx";
filenameacc[m1_,m2_]:=Which[m1==m2,"/acceigenstate_m-"<>ToString[m1],m1>m2,"/acceigenstate_m1-"<>ToString[m1]<>"_m2-"<>ToString[m2],m1<m2,"/acceigenstate_m1-"<>ToString[m2]<>"_m2-"<>ToString[m1]]<>".wdx";
Which[OptionValue[SolveMethod]=="BSW",Determine=Determine\[Phi]x,filename=filenameacc;OptionValue[SolveMethod]=="'t Hooft",Determine=accDetermine\[Phi]];
{ParA,ParB}=Keys[OptionValue[ProcessType]];{ParC,ParD}=Values[OptionValue[ProcessType]];
If[SubsetQ[Flatten@{ParA,ParB,ParC,ParD},Keys[OptionValue[AssignQuark]]],Print["Convention checked out"],Abort[]];
Which[(*check classification*)
MatchQ[{ParA,ParB},{{a_,c_},{b_,b_}}]&&MatchQ[{ParC,ParD},{{b_,c_},{a_,b_}}],
\[ScriptCapitalM][\[Omega]1_,\[Omega]2_,opts__][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,M1_,M2_,M3_,M4_]:=\[ScriptCapitalM]1[\[Omega]1,\[Omega]2,FilterRules[{opts},Options[\[ScriptCapitalM]1]]][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][{ParA[[1]],ParB[[1]],ParB[[1]],ParA[[2]]}/.OptionValue[AssignQuark],M1,M2,M3,M4],
MatchQ[{ParA,ParB},{{a_,c_},{c_,b_}}]&&MatchQ[{ParC,ParD},{{c_,c_},{a_,b_}}],
\[ScriptCapitalM][\[Omega]1_,\[Omega]2_,opts__][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,M1_,M2_,M3_,M4_]:=\[ScriptCapitalM]1[\[Omega]1,\[Omega]2,FilterRules[{opts},Options[\[ScriptCapitalM]1]]][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][{ParA[[1]],ParB[[2]],ParB[[1]],ParB[[1]]}/.OptionValue[AssignQuark],M1,M2,M3,M4],
MatchQ[{ParA,ParB},{{a_,c_},{a_,b_}}]&&MatchQ[{ParC,ParD},{{a_,c_},{a_,b_}}],
\[ScriptCapitalM][\[Omega]1_,\[Omega]2_,opts__][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,M1_,M2_,M3_,M4_]:=\[ScriptCapitalM]1[\[Omega]1,\[Omega]2,FilterRules[{opts},Options[\[ScriptCapitalM]1]]][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][{ParA[[1]],ParB[[2]],ParA[[1]],ParA[[2]]}/.OptionValue[AssignQuark],M1,M2,M3,M4],
True,
\[ScriptCapitalM][\[Omega]1_,\[Omega]2_,opts__][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,M1_,M2_,M3_,M4_]:=\[ScriptCapitalM]0\[ScriptCapitalC]t[\[Omega]1,\[Omega]2,opts][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][m,M1,M2,M3,M4]
];
Eigenlist={{Vals1,\[Phi]x1},{Vals2,\[Phi]x2},{Vals3,\[Phi]x3},{Vals4,\[Phi]x4}};
Masslist={ParA,ParB,ParC,ParD}/.OptionValue[AssignQuark];
\[CapitalPhi]list={\[CapitalPhi]1,\[CapitalPhi]2,\[CapitalPhi]3,\[CapitalPhi]4};
Do[
If[FileNames[filename[Sequence@@Masslist[[i]]],dir,Infinity]=={},
{
Evaluate[Eigenlist[[i]]]=Determine[Sequence@@Masslist[[i]],\[Beta],Nx];
Set@@{Evaluate[\[CapitalPhi]list[[i]]][Global`x_],Boole[0<=Global`x<=1]Eigenlist[[i,2]]};
Export[dir<>filename[Sequence@@Masslist[[i]]],Eigenlist[[i]]]
},
{
Evaluate[Eigenlist[[i]]]=Import[dir<>filename[Sequence@@Masslist[[i]]]];
Set@@{Evaluate[\[CapitalPhi]list[[i]]][Global`x_],Boole[0<=Global`x<=1]Eigenlist[[i,2]]};
}
],{i,4}
];
(*Set@@{\[CapitalPhi]B[x_?NumberQ],If[0<x<1,\[CapitalPhi]Bi[x],Table[0,{n,Length@\[Phi]xB}]]};*)
Print["Wavefunction build complete."];
(*Print[\[Phi]n[#][\[CapitalPhi]B][x]&[1]];*)
{M1,\[Phi]1t}={Mn[#][Vals1],\[Phi]n[#][\[CapitalPhi]1]}&[n1];
{M2,\[Phi]2t}={Mn[#][Vals2],\[Phi]n[#][\[CapitalPhi]2]}&[n2];
{M3,\[Phi]3t}={Mn[#][Vals3],\[Phi]n[#][\[CapitalPhi]3]}&[n3];
{M4,\[Phi]4t}={Mn[#][Vals4],\[Phi]n[#][\[CapitalPhi]4]}&[n4];
Set@@{\[Phi]1[x_],\[Phi]1t[x/.x/;(!OrderedQ[ParA])->1-x]};
Set@@{\[Phi]2[x_],\[Phi]2t[x/.x/;(!OrderedQ[ParB])->1-x]};
Set@@{\[Phi]3[x_],\[Phi]3t[x/.x/;(!OrderedQ[ParC])->1-x]};
Set@@{\[Phi]4[x_],\[Phi]4t[x/.x/;(!OrderedQ[ParD])->1-x]};
Mseq=Sequence[M1,M2,M3,M4];Si=If[n1+n2>=n3+n4,M1+M2,M3+M4];
Print["M1=",M1,"  M2=",M2,"  M3=",M3,"  M4=",M4];
\[ScriptCapitalM]0\[ScriptCapitalC]t[\[Omega]1_,\[Omega]2_,opts__][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,M1_,M2_,M3_,M4_]:=MHG[\[Omega]1,\[Omega]2,Evaluate@FilterRules[{opts},Options[NIntegrate]]][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4];
\[ScriptCapitalM]1t[\[Omega]1_,\[Omega]2_,opts__][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,M1_,M2_,M3_,M4_]:=\[ScriptCapitalM]1[\[Omega]1,\[Omega]2,FilterRules[{opts},Options[\[ScriptCapitalM]1]]][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][m,M1,M2,M3,M4];
(*Print[$Context];*)
(*DistributeDefinitions[M1,M2,M3,M4,\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4,EXPR,\[ScriptCapitalM]0,\[ScriptCapitalI]1,\[ScriptCapitalI]2,\[ScriptCapitalI]3,\[Omega]1S,\[Omega]2S];*)
{{{n1,n2,n3,n4},{M1,M2,M3,M4},{m1,m2,m3}},ParallelTable[Sen=Ssqur^2;\[Omega]1=\[Omega]1S[Sen][M1,M2,M3,M4];\[Omega]2=\[Omega]2S[Sen][M1,M2,M3,M4];
{Ssqur,\[ScriptCapitalM][\[Omega]1,\[Omega]2,opt][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][m,M1,M2,M3,M4]},{Ssqur,Si+OptionValue[SRange][[1]],Si+OptionValue[SRange][[2]],OptionValue[SRange][[3]]},DistributedContexts->{"OneFlavour`Private`"}]}
];




(* ::Subsection::Closed:: *)
(*Tetra Flavours*)


Msum4[{mQ_,mq_,mP_,mp_},{n1_?IntegerQ,n2_?IntegerQ,n3_?IntegerQ,n4_?IntegerQ},opt:OptionsPattern[{SRange->{10^-3,2,0.01},Lambda->10^-6,SolveMethod->"BSW",DataDir->dirglo,gvalue->gglo,ProcessType->({{a,d},{c,b}}->{{c,d},{a,b}}),AssignQuark->{a->m1,b->m2,c->m3,d->m4},I1Option->OptionsPattern[],I2Option->OptionsPattern[],I3Option->OptionsPattern[]}]]:=
Module[{\[Phi]x1,\[CapitalPhi]1,Vals1,\[Phi]x2,\[CapitalPhi]2,Vals2,\[Phi]x3,\[CapitalPhi]3,Vals3,\[Phi]x4,\[CapitalPhi]4,Vals4,M1,M2,M3,M4,\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4,Ares,filename,\[Omega]now,m1,m2,m3,m4,\[Omega]1,\[Omega]2,Sen,filenameacc,Determine,Mseq,\[CapitalPhi]Bi,dir,Si,ParA,ParB,ParC,ParD,\[ScriptCapitalM],\[ScriptCapitalM]0\[ScriptCapitalC]t,\[ScriptCapitalM]1t,Eigenlist,Masslist,\[CapitalPhi]list},
(*SetSharedVariable[m2,m1];*)
m1=mQ;m2=mq;m3=mP;m4=mp;\[Lambda]=OptionValue[Lambda];g=OptionValue[gvalue];dir=OptionValue[DataDir];
filename[m1_,m2_]:=Which[m1==m2,"/eigenstate_m-"<>ToString[m1],m1>m2,"/eigenstate_m1-"<>ToString[m1]<>"_m2-"<>ToString[m2],m1<m2,"/eigenstate_m1-"<>ToString[m2]<>"_m2-"<>ToString[m1]]<>".wdx";
filenameacc[m1_,m2_]:=Which[m1==m2,"/acceigenstate_m-"<>ToString[m1],m1>m2,"/acceigenstate_m1-"<>ToString[m1]<>"_m2-"<>ToString[m2],m1<m2,"/acceigenstate_m1-"<>ToString[m2]<>"_m2-"<>ToString[m1]]<>".wdx";
Which[OptionValue[SolveMethod]=="BSW",Determine=Determine\[Phi]x,filename=filenameacc;OptionValue[SolveMethod]=="'t Hooft",Determine=accDetermine\[Phi]];
{ParA,ParB}=Keys[OptionValue[ProcessType]];{ParC,ParD}=Values[OptionValue[ProcessType]];
If[SubsetQ[Flatten@{ParA,ParB,ParC,ParD},Keys[OptionValue[AssignQuark]]],Print["Convention checked out"],Abort[]];
If[
DuplicateFreeQ[ParA,ParB],
\[ScriptCapitalM][\[Omega]1_,\[Omega]2_,opts__][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,M1_,M2_,M3_,M4_]:=\[ScriptCapitalM]1[\[Omega]1,\[Omega]2,(*FilterRules[{opts},Options[NIntegrate]]*)FilterRules[{opts},Options[\[ScriptCapitalM]1]]][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][{ParA[[1]],ParB[[2]],ParB[[1]],ParA[[2]]}/.OptionValue[AssignQuark],M1,M2,M3,M4],
\[ScriptCapitalM][\[Omega]1_,\[Omega]2_,opts__][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,M1_,M2_,M3_,M4_]:=\[ScriptCapitalM]0\[ScriptCapitalC]t[\[Omega]1,\[Omega]2,opts][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][m,M1,M2,M3,M4]
];
Eigenlist={{Vals1,\[Phi]x1},{Vals2,\[Phi]x2},{Vals3,\[Phi]x3},{Vals4,\[Phi]x4}};
Masslist={ParA,ParB,ParC,ParD}/.OptionValue[AssignQuark];
\[CapitalPhi]list={\[CapitalPhi]1,\[CapitalPhi]2,\[CapitalPhi]3,\[CapitalPhi]4};
Do[
If[FileNames[filename[Sequence@@Masslist[[i]]],dir,Infinity]=={},
{
Evaluate[Eigenlist[[i]]]=Determine[Sequence@@Masslist[[i]],\[Beta],Nx];
Set@@{Evaluate[\[CapitalPhi]list[[i]]][Global`x_],Boole[0<=Global`x<=1]Eigenlist[[i,2]]};
Export[dir<>filename[Sequence@@Masslist[[i]]],Eigenlist[[i]]]
},
{
Evaluate[Eigenlist[[i]]]=Import[dir<>filename[Sequence@@Masslist[[i]]]];
Set@@{Evaluate[\[CapitalPhi]list[[i]]][Global`x_],Boole[0<=Global`x<=1]Eigenlist[[i,2]]};
}
],{i,4}
];
Print["Wavefunction build complete."];
(*Print[\[Phi]n[#][\[CapitalPhi]B][x]&[1]];*)
{M1,\[Phi]1}={Mn[#][Vals1],\[Phi]n[#][\[CapitalPhi]1]}&[n1];
{M2,\[Phi]2}={Mn[#][Vals2],\[Phi]n[#][\[CapitalPhi]2]}&[n2];
{M3,\[Phi]3}={Mn[#][Vals3],\[Phi]n[#][\[CapitalPhi]3]}&[n3];
{M4,\[Phi]4}={Mn[#][Vals4],\[Phi]n[#][\[CapitalPhi]4]}&[n4];
Mseq=Sequence[M1,M2,M3,M4];Si=If[n1+n2>=n3+n4,M1+M2,M3+M4];
Print["M1=",M1,"  M2=",M2,"  M3=",M3,"  M4=",M4];
\[ScriptCapitalM]0\[ScriptCapitalC]t[\[Omega]1_,\[Omega]2_,opts__][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,M1_,M2_,M3_,M4_]:=MHG[\[Omega]1,\[Omega]2,Evaluate@FilterRules[{opts},Options[NIntegrate]]][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4];
\[ScriptCapitalM]1t[\[Omega]1_,\[Omega]2_,opts__][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,M1_,M2_,M3_,M4_]:=\[ScriptCapitalM]1[\[Omega]1,\[Omega]2,opts][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][m,M1,M2,M3,M4];
(*Print[$Context];*)
(*DistributeDefinitions[M1,M2,M3,M4,\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4,EXPR,\[ScriptCapitalM]0,\[ScriptCapitalI]1,\[ScriptCapitalI]2,\[ScriptCapitalI]3,\[Omega]1S,\[Omega]2S];*)
{{{n1,n2,n3,n4},{M1,M2,M3,M4},{m1,m2}},ParallelTable[Sen=Ssqur^2;\[Omega]1=\[Omega]1S[Sen][M1,M2,M3,M4];\[Omega]2=\[Omega]2S[Sen][M1,M2,M3,M4];
{Ssqur,\[ScriptCapitalM][\[Omega]1,\[Omega]2,opt][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][m,M1,M2,M3,M4]},{Ssqur,Si+OptionValue[SRange][[1]],Si+OptionValue[SRange][[2]],OptionValue[SRange][[3]]},DistributedContexts->{"OneFlavour`Private`"}]}
];
(*$DistributedContexts:=$Context;*)




(* ::Section::Closed:: *)
(*Display Functions*)


displayfunction1[Msumdat_,{min_,max_},opt:OptionsPattern[{Joined->True,LegendMargins->3,ImageSize->Medium}]]:=ListPlot[{Select[Re@Msumdat[[2]],#\[Element]Reals&],{{If[Msumdat[[1,1,1]]+Msumdat[[1,1,2]]>=Msumdat[[1,1,3]]+Msumdat[[1,1,4]],Msumdat[[1,2,1]]+Msumdat[[1,2,2]],Msumdat[[1,2,3]]+Msumdat[[1,2,4]]],Last[Msumdat[[2]]][[2]]},{If[Msumdat[[1,1,1]]+Msumdat[[1,1,2]]>=Msumdat[[1,1,3]]+Msumdat[[1,1,4]],Msumdat[[1,2,1]]+Msumdat[[1,2,2]],Msumdat[[1,2,3]]+Msumdat[[1,2,4]]],First[Msumdat[[2]]][[2]]}}},PlotRange->{{First[Msumdat[[2]]][[1]]+min,Last[Msumdat[[2]]][[1]]+max},All},Joined->{OptionValue[Joined],True},Evaluate@FilterRules[{opt},Options[ListPlot]],PlotLabel->"Amp: Threshold: "<>ToString[If[Msumdat[[1,1,1]]+Msumdat[[1,1,2]]>=Msumdat[[1,1,3]]+Msumdat[[1,1,4]],Msumdat[[1,2,1]]+Msumdat[[1,2,2]],Msumdat[[1,2,3]]+Msumdat[[1,2,4]]]]<>" \nQuark mass: "<>ReplaceAll[ToString[#]<>" "&/@Msumdat[[1,3]],List->StringJoin]<>"\nMeson mass: "<>ReplaceAll[ToString[#]<>" "&/@Msumdat[[1,2]],List->StringJoin]<>"",Joined->OptionValue[Joined],ImageSize->OptionValue[ImageSize],PlotLegends->Placed[LineLegend[{ToString[Msumdat[[1,1,1]]]<>"+"<>ToString[Msumdat[[1,1,2]]]<>"\[Rule]"<>ToString[Msumdat[[1,1,3]]]<>"+"<>ToString[Msumdat[[1,1,4]]]},LegendLayout->{"Column",1}(*,LegendMarkerSize\[Rule]20*),LegendMargins->OptionValue[LegendMargins],LegendFunction->"Frame"],{Right,Top}],Frame->True,FrameLabel->{"","\[ScriptCapitalM]"},PlotStyle->{{Automatic},{Thick,Dotted}}];
displayfunction2[Msumdat_,{min_,max_},opt:OptionsPattern[{Joined->True,LegendMargins->3,ImageSize->Medium}]]:=ListPlot[{Transpose@{Transpose[Msumdat[[2]]][[1]],(Transpose[Msumdat[[2]]][[2]])^2},{{If[Msumdat[[1,1,1]]+Msumdat[[1,1,2]]>=Msumdat[[1,1,3]]+Msumdat[[1,1,4]],Msumdat[[1,2,1]]+Msumdat[[1,2,2]],Msumdat[[1,2,3]]+Msumdat[[1,2,4]]],Last[Msumdat[[2]]][[2]]^2},{If[Msumdat[[1,1,1]]+Msumdat[[1,1,2]]>=Msumdat[[1,1,3]]+Msumdat[[1,1,4]],Msumdat[[1,2,1]]+Msumdat[[1,2,2]],Msumdat[[1,2,3]]+Msumdat[[1,2,4]]],First[Msumdat[[2]]][[2]]^2}}},PlotRange->{{First[Msumdat[[2]]][[1]]+min,Last[Msumdat[[2]]][[1]]+max},All},Joined->{OptionValue[Joined],True},Evaluate@FilterRules[{opt},Options[ListPlot]],PlotLabel->"Amp square: Threshold: "<>ToString[If[Msumdat[[1,1,1]]+Msumdat[[1,1,2]]>=Msumdat[[1,1,3]]+Msumdat[[1,1,4]],Msumdat[[1,2,1]]+Msumdat[[1,2,2]],Msumdat[[1,2,3]]+Msumdat[[1,2,4]]]]<>" \nQuark mass: "<>ReplaceAll[ToString[#]<>" "&/@Msumdat[[1,3]],List->StringJoin]<>"\nMeson mass: "<>ReplaceAll[ToString[#]<>" "&/@Msumdat[[1,2]],List->StringJoin]<>"",Joined->OptionValue[Joined], ImageSize->OptionValue[ImageSize],PlotLegends->Placed[LineLegend[{ToString[Msumdat[[1,1,1]]]<>"+"<>ToString[Msumdat[[1,1,2]]]<>"\[Rule]"<>ToString[Msumdat[[1,1,3]]]<>"+"<>ToString[Msumdat[[1,1,4]]]},LegendLayout->{"Column",1}(*,LegendMarkerSize\[Rule]20*),LegendMargins->OptionValue[LegendMargins],LegendFunction->"Frame"],{Right,Top}],Frame->True,FrameLabel->{"","|\[ScriptCapitalM]\!\(\*SuperscriptBox[\(|\), \(2\)]\)"},PlotStyle->{{Automatic},{Thick,Dotted}}];
displayfunctionboth[Msumdat_,{min_,max_},opt:OptionsPattern[{Joined->True,LegendMargins->3,ImageSize->Medium}]]:=GraphicsRow[{ListPlot[{Select[Re@Msumdat[[2]],#\[Element]Reals&],{{If[Msumdat[[1,1,1]]+Msumdat[[1,1,2]]>=Msumdat[[1,1,3]]+Msumdat[[1,1,4]],Msumdat[[1,2,1]]+Msumdat[[1,2,2]],Msumdat[[1,2,3]]+Msumdat[[1,2,4]]],Last[Msumdat[[2]]][[2]]},{If[Msumdat[[1,1,1]]+Msumdat[[1,1,2]]>=Msumdat[[1,1,3]]+Msumdat[[1,1,4]],Msumdat[[1,2,1]]+Msumdat[[1,2,2]],Msumdat[[1,2,3]]+Msumdat[[1,2,4]]],First[Msumdat[[2]]][[2]]}}},PlotRange->{{First[Msumdat[[2]]][[1]]+min,Last[Msumdat[[2]]][[1]]+max},All},Joined->{OptionValue[Joined],True},Evaluate@FilterRules[{opt},Options[ListPlot]],ImageSize->OptionValue[ImageSize],PlotLegends->Placed[LineLegend[{ToString[Msumdat[[1,1,1]]]<>"+"<>ToString[Msumdat[[1,1,2]]]<>"\[Rule]"<>ToString[Msumdat[[1,1,3]]]<>"+"<>ToString[Msumdat[[1,1,4]]]},LegendLayout->{"Column",1}(*,LegendMarkerSize\[Rule]20*),LegendMargins->OptionValue[LegendMargins],LegendFunction->"Frame"],{Right,Top}],Frame->True,FrameLabel->{"","\[ScriptCapitalM]"},PlotStyle->{{Automatic},{Thick,Dotted}}],ListPlot[{Transpose@{Transpose[Msumdat[[2]]][[1]],(Transpose[Msumdat[[2]]][[2]])^2},{{If[Msumdat[[1,1,1]]+Msumdat[[1,1,2]]>=Msumdat[[1,1,3]]+Msumdat[[1,1,4]],Msumdat[[1,2,1]]+Msumdat[[1,2,2]],Msumdat[[1,2,3]]+Msumdat[[1,2,4]]],Last[Msumdat[[2]]][[2]]^2},{If[Msumdat[[1,1,1]]+Msumdat[[1,1,2]]>=Msumdat[[1,1,3]]+Msumdat[[1,1,4]],Msumdat[[1,2,1]]+Msumdat[[1,2,2]],Msumdat[[1,2,3]]+Msumdat[[1,2,4]]],First[Msumdat[[2]]][[2]]^2}}},PlotRange->{{First[Msumdat[[2]]][[1]]+min,Last[Msumdat[[2]]][[1]]+max},All},Evaluate@FilterRules[{opt},Options[ListPlot]],Joined->OptionValue[Joined], ImageSize->OptionValue[ImageSize],PlotLegends->Placed[LineLegend[{ToString[Msumdat[[1,1,1]]]<>"+"<>ToString[Msumdat[[1,1,2]]]<>"\[Rule]"<>ToString[Msumdat[[1,1,3]]]<>"+"<>ToString[Msumdat[[1,1,4]]]},LegendLayout->{"Column",1}(*,LegendMarkerSize\[Rule]20*),LegendMargins->OptionValue[LegendMargins],LegendFunction->"Frame"],{Right,Top}],Frame->True,FrameLabel->{"","|\[ScriptCapitalM]\!\(\*SuperscriptBox[\(|\), \(2\)]\)"},PlotStyle->{{Automatic},{Thick,Dotted}}]},PlotLabel->"Amp square: Threshold: "<>ToString[If[Msumdat[[1,1,1]]+Msumdat[[1,1,2]]>=Msumdat[[1,1,3]]+Msumdat[[1,1,4]],Msumdat[[1,2,1]]+Msumdat[[1,2,2]],Msumdat[[1,2,3]]+Msumdat[[1,2,4]]]]<>" \nQuark mass: "<>ReplaceAll[ToString[#]<>" "&/@Msumdat[[1,3]],List->StringJoin]<>"\nMeson mass: "<>ReplaceAll[ToString[#]<>" "&/@Msumdat[[1,2]],List->StringJoin]<>"",ImageSize->Large,Frame->True];


End[]


EndPackage[]


 
