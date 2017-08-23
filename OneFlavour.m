(* ::Package:: *)

BeginPackage["OneFlavour`"];


(* ::Input::Initialization:: *)
Msum::usage=
"Msum[mQ,{n1,n2,n3,n4},Options] is the total sum of amplitudes. The result is of the form {{{n1,n2,n3,n4},{M1,M2,M3,M4}},{{Sqrt[S],amp},...}}.\n Option: SRange->{a,b,d}, the real range of centre-of-mass energy square S is {M1+M2+a,M1+M2+b,d} where d is the interval.\n Option: Lambda->\[Lambda], the cutoff of principal value integral.\n Option: Method. Method->\"BSW\": use BSW method; Method->\"'t Hooft\": use the method 't Hooft suggested in his original paper.\n Option: DataDir->\"Data desination\". e.g. \"D:/data\".
"


(*ParallelEvaluate[Print[$KernelID]];*)


Begin["`Private`"]


(* ::Input::Initialization:: *)
 If[$OperatingSystem=="Windows",{dirglo="D:/Documents/2-d-data";},{dirglo="~/Documents/2-d-data";}];
(*$DistributedContexts="OneFlavour`Private`";*)
EXPR[\[Omega]1_,\[Omega]2_][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,Ma_,Mb_,Mc_,Md_]:=If[\[Omega]1>1,\[ScriptCapitalM]0[1-\[Omega]1+\[Omega]2,\[Omega]2][\[Phi]2,\[Phi]1,\[Phi]3,\[Phi]4][m,Mb,Ma,Mc,Md]+\[ScriptCapitalM]0[1/(1-\[Omega]1+\[Omega]2),\[Omega]1/(1-\[Omega]1+\[Omega]2)][\[Phi]4,\[Phi]3,\[Phi]1,\[Phi]2][m,Md,Mc,Ma,Mb]+\[ScriptCapitalI]1[1/\[Omega]1,(1-\[Omega]1+\[Omega]2)/\[Omega]1][\[Phi]4,\[Phi]3,\[Phi]2,\[Phi]1][m,Md,Mc,Mb,Ma]+\[ScriptCapitalI]2[1/((1+1/\[Omega]2-\[Omega]1/\[Omega]2) \[Omega]2),\[Omega]1/((1+1/\[Omega]2-\[Omega]1/\[Omega]2) \[Omega]2)][\[Phi]4,\[Phi]3,\[Phi]1,\[Phi]2][m,Md,Mc,Ma,Mb],\[ScriptCapitalM]0[\[Omega]2/\[Omega]1,(1-\[Omega]1+\[Omega]2)/\[Omega]1][\[Phi]3,\[Phi]4,\[Phi]2,\[Phi]1][m,Mc,Md,Mb,Ma]+\[ScriptCapitalM]0[\[Omega]1/\[Omega]2,1/\[Omega]2][\[Phi]1,\[Phi]2,\[Phi]4,\[Phi]3][m,Ma,Mb,Md,Mc]+\[ScriptCapitalI]1[\[Omega]1,\[Omega]2][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][m,Ma,Mb,Mc,Md]+\[ScriptCapitalI]2[\[Omega]1/\[Omega]2,1/\[Omega]2][\[Phi]1,\[Phi]2,\[Phi]4,\[Phi]3][m,Ma,Mb,Md,Mc]]+If[\[Omega]2>\[Omega]1,\[ScriptCapitalM]0[\[Omega]1,\[Omega]2][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][m,Ma,Mb,Mc,Md]+\[ScriptCapitalM]0[1/\[Omega]1,(1-\[Omega]1+\[Omega]2)/\[Omega]1][\[Phi]4,\[Phi]3,\[Phi]2,\[Phi]1][m,Md,Mc,Mb,Ma]+\[ScriptCapitalI]1[\[Omega]1/\[Omega]2,1/\[Omega]2][\[Phi]1,\[Phi]2,\[Phi]4,\[Phi]3][m,Ma,Mb,Md,Mc]+\[ScriptCapitalI]2[\[Omega]1,\[Omega]2][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][m,Ma,Mb,Mc,Md],\[ScriptCapitalM]0[(1-\[Omega]1+\[Omega]2)/\[Omega]2,1/\[Omega]2][\[Phi]2,\[Phi]1,\[Phi]4,\[Phi]3][m,Mb,Ma,Md,Mc]+\[ScriptCapitalM]0[\[Omega]2/(1-\[Omega]1+\[Omega]2),\[Omega]1/(1-\[Omega]1+\[Omega]2)][\[Phi]3,\[Phi]4,\[Phi]1,\[Phi]2][m,Mc,Md,Ma,Mb]+\[ScriptCapitalI]1[\[Omega]2/\[Omega]1,((1+1/\[Omega]2-\[Omega]1/\[Omega]2) \[Omega]2)/\[Omega]1][\[Phi]3,\[Phi]4,\[Phi]2,\[Phi]1][m,Mc,Md,Mb,Ma]+\[ScriptCapitalI]2[\[Omega]2/(1-\[Omega]1+\[Omega]2),\[Omega]1/(1-\[Omega]1+\[Omega]2)][\[Phi]3,\[Phi]4,\[Phi]1,\[Phi]2][m,Mc,Md,Ma,Mb]]+Which[0<\[Omega]1<1&&\[Omega]2>=\[Omega]1,\[ScriptCapitalI]3[1/\[Omega]1,(1-\[Omega]1+\[Omega]2)/\[Omega]1][\[Phi]4,\[Phi]3,\[Phi]2,\[Phi]1][m,Md,Mc,Mb,Ma]+\[ScriptCapitalI]3[\[Omega]2/\[Omega]1,((1+1/\[Omega]2-\[Omega]1/\[Omega]2) \[Omega]2)/\[Omega]1][\[Phi]3,\[Phi]4,\[Phi]2,\[Phi]1][m,Mc,Md,Mb,Ma],\[Omega]2>=\[Omega]1&&\[Omega]1>=1,\[ScriptCapitalI]3[\[Omega]1,\[Omega]2][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][m,Ma,Mb,Mc,Md]+\[ScriptCapitalI]3[(1+1/\[Omega]2-\[Omega]1/\[Omega]2) \[Omega]2,\[Omega]2][\[Phi]2,\[Phi]1,\[Phi]3,\[Phi]4][m,Mb,Ma,Mc,Md],0<\[Omega]1<1&&\[Omega]1>\[Omega]2,\[ScriptCapitalI]3[\[Omega]1/\[Omega]2,1/\[Omega]2][\[Phi]1,\[Phi]2,\[Phi]4,\[Phi]3][m,Ma,Mb,Md,Mc]+\[ScriptCapitalI]3[(1-\[Omega]1+\[Omega]2)/\[Omega]2,1/\[Omega]2][\[Phi]2,\[Phi]1,\[Phi]4,\[Phi]3][m,Mb,Ma,Md,Mc],\[Omega]1>=1&&\[Omega]1>\[Omega]2,\[ScriptCapitalI]3[1/((1+1/\[Omega]2-\[Omega]1/\[Omega]2) \[Omega]2),\[Omega]1/((1+1/\[Omega]2-\[Omega]1/\[Omega]2) \[Omega]2)][\[Phi]4,\[Phi]3,\[Phi]1,\[Phi]2][m,Md,Mc,Ma,Mb]+\[ScriptCapitalI]3[\[Omega]2/(1-\[Omega]1+\[Omega]2),\[Omega]1/(1-\[Omega]1+\[Omega]2)][\[Phi]3,\[Phi]4,\[Phi]1,\[Phi]2][m,Mc,Md,Ma,Mb]];
\[ScriptCapitalM]0[\[Omega]1_,\[Omega]2_][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,Ma_,Mb_,Mc_,Md_]:=4 g^2 \[Omega]1 NIntegrate[(\[Phi]1[(\[Omega]2-\[Omega]1+x)/(\[Omega]2-\[Omega]1+1)] \[Phi]2[y] \[Phi]3[x] \[Phi]4[(y \[Omega]1)/\[Omega]2])/(y \[Omega]1-\[Omega]2-x)^2,{x,0,1},{y,0,1}];
\[ScriptCapitalI]1[\[Omega]1_,\[Omega]2_][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,M1_,M2_,M3_,M4_]:=-4 g^2 (NIntegrate[((\[Omega]1 \[Omega]2) \[Phi]1[(x \[Omega]2)/(1+\[Omega]2-\[Omega]1)] \[Phi]2[y] \[Phi]3[y \[Omega]1] \[Phi]4[x])/((y-1) \[Omega]1+(1-x) \[Omega]2)^2,{x,(If[#1>0,#1,0]&)[(-\[Omega]1+\[Lambda] \[Omega]1+\[Omega]2)/\[Omega]2],(If[#1<1,#1,1]&)[(-\[Lambda] \[Omega]1+\[Omega]2)/\[Omega]2]},{y,0,(\[Omega]1-\[Omega]2+x \[Omega]2)/\[Omega]1-\[Lambda]}]+NIntegrate[((\[Omega]1 \[Omega]2) \[Phi]1[(x \[Omega]2)/(1+\[Omega]2-\[Omega]1)] \[Phi]2[y] \[Phi]3[y \[Omega]1] \[Phi]4[x])/((y-1) \[Omega]1+(1-x) \[Omega]2)^2,{x,(If[#1>0,#1,0]&)[(-\[Omega]1+\[Lambda] \[Omega]1+\[Omega]2)/\[Omega]2],(If[#1<1,#1,1]&)[(-\[Lambda] \[Omega]1+\[Omega]2)/\[Omega]2]},{y,(\[Omega]1-\[Omega]2+x \[Omega]2)/\[Omega]1+\[Lambda],1}]-(2 NIntegrate[(\[Omega]2 \[Phi]1[(x \[Omega]2)/(1+\[Omega]2-\[Omega]1)] \[Phi]2[(\[Omega]1-\[Omega]2+x \[Omega]2)/\[Omega]1] \[Phi]3[((\[Omega]1-\[Omega]2+x \[Omega]2) \[Omega]1)/\[Omega]1] \[Phi]4[x])/\[Omega]1,{x,(If[#1>0,#1,0]&)[(-\[Omega]1+\[Lambda] \[Omega]1+\[Omega]2)/\[Omega]2],(If[#1<1,#1,1]&)[(-\[Lambda] \[Omega]1+\[Omega]2)/\[Omega]2]}])/\[Lambda]+If[(-\[Omega]1+\[Lambda] \[Omega]1+\[Omega]2)/\[Omega]2>0,NIntegrate[((\[Omega]1 \[Omega]2) \[Phi]1[(x \[Omega]2)/(1+\[Omega]2-\[Omega]1)] \[Phi]2[y] \[Phi]3[y \[Omega]1] \[Phi]4[x])/((y-1) \[Omega]1+(1-x) \[Omega]2)^2,{x,0,(-\[Omega]1+\[Lambda] \[Omega]1+\[Omega]2)/\[Omega]2},{y,0,1}],0]+If[(-\[Lambda] \[Omega]1+\[Omega]2)/\[Omega]2<1,NIntegrate[((\[Omega]1 \[Omega]2) \[Phi]1[(x \[Omega]2)/(1+\[Omega]2-\[Omega]1)] \[Phi]2[y] \[Phi]3[y \[Omega]1] \[Phi]4[x])/((y-1) \[Omega]1+(1-x) \[Omega]2)^2,{x,(-\[Lambda] \[Omega]1+\[Omega]2)/\[Omega]2,1},{y,0,1}],0]);
\[ScriptCapitalI]2[\[Omega]1_,\[Omega]2_][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,M1_,M2_,M3_,M4_]:=-4 g^2 (NIntegrate[(\[Omega]1 \[Phi]1[(x+\[Omega]2-\[Omega]1)/(1+\[Omega]2-\[Omega]1)] \[Phi]2[y] \[Phi]3[x] \[Phi]4[((y-1) \[Omega]1+\[Omega]2)/\[Omega]2])/(y \[Omega]1-x)^2,{x,(If[#1>0,#1,0]&)[\[Omega]1 \[Lambda]],(If[#1<1,#1,1]&)[\[Omega]1 (1-\[Lambda])]},{y,0,x/\[Omega]1-\[Lambda]}]+NIntegrate[(\[Omega]1 \[Phi]1[(x+\[Omega]2-\[Omega]1)/(1+\[Omega]2-\[Omega]1)] \[Phi]2[y] \[Phi]3[x] \[Phi]4[((y-1) \[Omega]1+\[Omega]2)/\[Omega]2])/(y \[Omega]1-x)^2,{x,(If[#1>0,#1,0]&)[\[Omega]1 \[Lambda]],(If[#1<1,#1,1]&)[\[Omega]1 (1-\[Lambda])]},{y,x/\[Omega]1+\[Lambda],1}]-(2 NIntegrate[(\[Phi]1[(x+\[Omega]2-\[Omega]1)/(1+\[Omega]2-\[Omega]1)] \[Phi]2[x/\[Omega]1] \[Phi]3[x] \[Phi]4[((x/\[Omega]1-1) \[Omega]1+\[Omega]2)/\[Omega]2])/\[Omega]1,{x,(If[#1>0,#1,0]&)[\[Omega]1 \[Lambda]],(If[#1<1,#1,1]&)[\[Omega]1 (1-\[Lambda])]}])/\[Lambda]+If[\[Omega]1 \[Lambda]>0,NIntegrate[(\[Omega]1 \[Phi]1[(x+\[Omega]2-\[Omega]1)/(1+\[Omega]2-\[Omega]1)] \[Phi]2[y] \[Phi]3[x] \[Phi]4[((y-1) \[Omega]1+\[Omega]2)/\[Omega]2])/(y \[Omega]1-x)^2,{x,0,\[Omega]1 \[Lambda]},{y,0,1}],0]+If[\[Omega]1 (1-\[Lambda])<1,NIntegrate[(\[Omega]1 \[Phi]1[(x+\[Omega]2-\[Omega]1)/(1+\[Omega]2-\[Omega]1)] \[Phi]2[y] \[Phi]3[x] \[Phi]4[((y-1) \[Omega]1+\[Omega]2)/\[Omega]2])/(y \[Omega]1-x)^2,{x,\[Omega]1 (1-\[Lambda]),1},{y,0,1}],0]);
\[ScriptCapitalI]3[\[Omega]1_,\[Omega]2_][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_][m_,Ma_,Mb_,Mc_,Md_]:=-((4 \[Pi])/Nc) NIntegrate[(Mc^2+Md^2/\[Omega]2+(m^2-\[Beta]^2)/(x-\[Omega]1)+(m^2-\[Beta]^2)/(x-1)-(m^2-\[Beta]^2)/(x-\[Omega]1+\[Omega]2)-(m^2-\[Beta]^2)/x) \[Phi]1[(x-\[Omega]1+\[Omega]2)/(1+\[Omega]2-\[Omega]1)] \[Phi]2[x/\[Omega]1] \[Phi]3[x] \[Phi]4[(x-\[Omega]1+\[Omega]2)/\[Omega]2],{x,0,1}];
Clear[m1,m2,\[Beta],Nx]; (*\[Beta]=1 unit*)
Nx=500;(*the size of the working matrix*)
\[Beta]=1; (*the mass unit,definition \[Beta]^2=g^2/(2Pi)Nc*)
(*m1=3.2*\[Beta];(*m1 and m2 are the bare masses of the quark and the anti-quark.*)
m2=3.2*\[Beta];*)
g=1;Nc=(\[Beta]^2 \[Pi])/g^2;(*\[DoubleStruckCapitalC] denotes the interchange of final states.*)
vMatx[n_,m_]:=(vMatx[n-1,m-1] m/(m-1)+(8m)/(n+m-1) ((1+(-1)^(n+m))/2))
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
accDetermine\[Phi][m1_,m2_,\[Beta]_,Nb_]:=
Module[{psi,\[Epsilon]=10^-6,Kernel1,Kernel2,hMT,sMT,sMatrix,hMTUp,hMT1,hMatrix,eg,vals,vecs,func,\[Mu],nfunc},
Kernel1[n_][x_?NumberQ]:=NIntegrate[psi[n,y]/(x-y)^2,{y,0,x-\[Epsilon]}];
Kernel2[n_][x_?NumberQ]:=NIntegrate[psi[n,y]/(x-y)^2,{y,x+\[Epsilon],1}];
psi[n_,x_]:=psi[n,x]=Which[n==0,x^(2-\[Beta])*(1-x)^\[Beta],n==1,(1-x)^(2-\[Beta])*x^\[Beta],n>=2,Sin[(n-1)*\[Pi]*x]];
hMT[m_,n_]:=hMT[m,n]=NIntegrate[psi[m,x]((m1^2-1)/(x*(1-x))*psi[n,x]-Kernel1[n][x]-Kernel2[n][x]+(2psi[n,x])/\[Epsilon]),{x,\[Epsilon],1-\[Epsilon]},WorkingPrecision->20];
sMT[m_,n_]:=sMT[m,n]=NIntegrate[psi[m,x]psi[n,x],{x,0,1}];
sMatrix=ParallelTable[Quiet[sMT[m,n]],{m,0,Nb},{n,0,Nb}]//Chop;hMTUp=PadLeft[#,Nb+1]&/@ParallelTable[Quiet[hMT[m,n]],{m,0,Nb},{n,m,Nb},DistributedContexts->{"OneFlavour`Private`"}];
(*Print[hMTUp];*)
hMatrix=Transpose[hMTUp]+hMTUp-DiagonalMatrix[Diagonal[hMTUp]];
(*Print[MatrixForm[hMatrix]];*)
eg[\[Mu]_]:=Det[hMatrix-\[Mu]^2*sMatrix];
\[Mu]=Sort@DeleteDuplicatesBy[Table[\[Mu]/.FindRoot[eg[\[Mu]]==0,{\[Mu],a,a+0.2}],{a,0,Nb^2,0.2}],SetPrecision[#,2]&];
Print[\[Mu]];
vecs=(Flatten@NullSpace[hMatrix-#^2 sMatrix,Tolerance->0.001])&/@\[Mu];
Print[vecs];Print[Dimensions@vecs];
func=ParallelTable[Table[psi[i,Global`x],{i,0,Nb}].vecs[[j]],{j,1,Length[vecs]},DistributedContexts->{"OneFlavour`Private`"}];
nfunc=ParallelTable[Quiet[func[[n]]/Sqrt[NIntegrate[func[[n]]^2,{x,0,1}]]],{n,1,20},DistributedContexts->{"OneFlavour`Private`"}];
Clear[func,vecs];
{\[Mu],nfunc}
];
\[Phi]n[n_][\[Phi]_][x_]:=\[Phi][x][[n+1]];
Mn[n_][vals_]:=vals[[n+1]];
\[Omega]1S[s_][M1_,M2_,M3_,M4_]:=(-M1^2+M2^2+s-Sqrt[s] Sqrt[(M1^4+(M2^2-s)^2-2 M1^2 (M2^2+s))/s])/(M3^2-M4^2+s+Sqrt[s] Sqrt[(M3^4+(M4^2-s)^2-2 M3^2 (M4^2+s))/s]);
\[Omega]2S[s_][M1_,M2_,M3_,M4_]:=(-M3^2+M4^2+s-Sqrt[s] Sqrt[(M3^4+(M4^2-s)^2-2 M3^2 (M4^2+s))/s])/(M3^2-M4^2+s+Sqrt[s] Sqrt[(M3^4+(M4^2-s)^2-2 M3^2 (M4^2+s))/s]);
Msum[mQ_,{n1_?IntegerQ,n2_?IntegerQ,n3_?IntegerQ,n4_?IntegerQ},OptionsPattern[{SRange->{10^-3,2,0.01},Lambda->10^-6,Method->"BSW",DataDir->dirglo}]]:=
Module[{\[Phi]xB,\[CapitalPhi]B,ValsB,\[Phi]x\[Pi],\[CapitalPhi]\[Pi],Vals\[Pi],M1,M2,M3,\[Phi]1,\[Phi]2,\[Phi]3,Ares,filename,\[Omega]now,m1,m2,M4,\[Phi]4,\[Omega]1,\[Omega]2,Sen,filenameacc,Determine,Mseq,\[CapitalPhi]Bi,Si,dir},
(*SetSharedVariable[m2,m1];*)
m1=mQ;m2=mQ;\[Lambda]=OptionValue[Lambda];dir=OptionValue[DataDir];
filename=If[m1==m2,"/eigenstate_m-"<>ToString[m1],"/eigenstate_m1-"<>ToString[m1]<>"_m2-"<>ToString[m2]]<>".wdx";
filenameacc=If[m1==m2,"/acceigenstate_m-"<>ToString[m1],"/acceigenstate_m1-"<>ToString[m1]<>"_m2-"<>ToString[m2]]<>".wdx";
If[FileNames[filenameacc,dir,Infinity]=={},(*If[ChoiceDialog["Choose to use BSW method or to use the original solution suggested by 't Hooft, ",{"BSW method"\[Rule]True,"Brute force integration"\[Rule]False}],Determine=Determine\[Phi]x,filename=filenameacc;Determine=accDetermine\[Phi]]];*)
Which[OptionValue[Method]=="BSW",Determine=Determine\[Phi]x,filename=filenameacc;OptionValue[Method]=="'t Hooft",Determine=accDetermine\[Phi]]];
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
{M1,\[Phi]1}={Mn[#][ValsB],\[Phi]n[#][\[CapitalPhi]B]}&[n1];
{M2,\[Phi]2}={Mn[#][ValsB],\[Phi]n[#][\[CapitalPhi]B]}&[n2];
{M3,\[Phi]3}={Mn[#][ValsB],\[Phi]n[#][\[CapitalPhi]B]}&[n3];
{M4,\[Phi]4}={Mn[#][ValsB],\[Phi]n[#][\[CapitalPhi]B]}&[n4];
Mseq=Sequence[M1,M2,M3,M4];Si=If[n1+n2>=n3+n4,M1+M2,M3+M4];
Print["M1=",M1,"  M2=",M2,"  M3=",M3,"  M4=",M4];
(*Print[$Context];*)
(*DistributeDefinitions[M1,M2,M3,M4,\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4,EXPR,\[ScriptCapitalM]0,\[ScriptCapitalI]1,\[ScriptCapitalI]2,\[ScriptCapitalI]3,\[Omega]1S,\[Omega]2S];*)
{{{n1,n2,n3,n4},{M1,M2,M3,M4}},ParallelTable[Sen=Ssqur^2;\[Omega]1=\[Omega]1S[Sen][M1,M2,M3,M4];\[Omega]2=\[Omega]2S[Sen][M1,M2,M3,M4];
{Ssqur,EXPR[\[Omega]1,\[Omega]2][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4][mQ,M1,M2,M3,M4]},{Ssqur,Si+OptionValue[SRange][[1]],Si+OptionValue[SRange][[2]],OptionValue[SRange][[3]]},DistributedContexts->{"OneFlavour`Private`"}]}
];
(*$DistributedContexts:=$Context;*)


End[]


EndPackage[]


 


(* ::Input:: *)
(*Msum[1,{0,0,0,0}]*)
