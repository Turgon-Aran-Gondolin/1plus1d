(* ::Package:: *)

BeginPackage["TwoFlavour`"];


Msum2::usage=
"Msum2[{mQ,mq,md},{n1,n2,n3,n4},Options] is the total sum of three-flavour amplitudes. The result is of the form {{{n1,n2,n3,n4},{M1,M2,M3,M4}},{{Sqrt[S],amp},...}}. Option: SRange->{a,b,d}, the real range of centre-of-mass energy square S is {M1+M2+a,M1+M2+b,d} where d is the interval. Option: Lambda->\[Lambda], the cutoff of principal value integral. Option: Method. Method->\"BSW\": use BSW method; Method->\"'t Hooft\": use the method 't Hooft suggested in his original paper.
"


(*ParallelEvaluate[Print[$KernelID]];*)


Begin["`Private`"]


(* ::Input::Initialization:: *)
 If[$OperatingSystem=="Windows",{dir="D:/Documents/2-d-data";},{dir="~/Documents/2-d-data";}];
(*$DistributedContexts="OneFlavour`Private`";*)
MHG[\[Omega]1_,\[Omega]2_][\[Phi]1_,\[Phi]2_,\[Phi]3_,\[Phi]4_]:=If[\[Omega]2>\[Omega]1,4 g^2 \[Omega]1 NIntegrate[(\[Phi]1[(\[Omega]2-\[Omega]1+x)/(\[Omega]2-\[Omega]1+1)] \[Phi]2[y] \[Phi]3[x] \[Phi]4[(y \[Omega]1)/\[Omega]2])/(y \[Omega]1-\[Omega]2-x)^2,{x,0,1},{y,0,1}],4 g^2 \[Omega]2(1+\[Omega]2-\[Omega]1) NIntegrate[(\[Phi]1[x] \[Phi]2[(y \[Omega]2)/\[Omega]1] \[Phi]3[\[Omega]1-\[Omega]2+(1-\[Omega]1+\[Omega]2) x] \[Phi]4[y])/(y \[Omega]2-x (1+\[Omega]2-\[Omega]1)-\[Omega]1)^2,{x,0,1},{y,0,1}]];
Clear[m1,m2,\[Beta],Nx]; (*\[Beta]=1 unit*)
Nx=500;(*the size of the working matrix*)
\[Beta]=1; (*the mass unit,definition \[Beta]^2=g^2/(2Pi)Nc*)
(*m1=3.2*\[Beta];(*m1 and m2 are the bare masses of the quark and the anti-quark.*)
m2=3.2*\[Beta];*)
g=1;Nc=(\[Beta]^2 \[Pi])/g^2;
vMatx[n_,m_]:=(vMatx[n-1,m-1] m/(m-1)+(8m)/(n+m-1) ((1+(-1)^(n+m))/2))
vMatx[1,m_]:=4(1+(-1)^(m+1));
vMatx[n_,1]:=4/n (1+(-1)^(n+1));
Determine\[Phi]x[m1_,m2_,\[Beta]_,Nx_]:=
Module[{HMatx,VMatx,vals,vecs,g,\[Phi]x,\[Phi]},
SetSharedVariable[vecs,m1,m2,\[Beta],g];
SetSharedFunction[g];
HMatx=ParallelTable[4Min[n,m]((-1)^(n+m) (m1^2-\[Beta]^2)+(m2^2-\[Beta]^2)),{n,1,Nx},{m,1,Nx},DistributedContexts->{"TwoFlavour`Private`"}];
VMatx=ParallelTable[vMatx[n,m],{n,1,Nx},{m,1,Nx},DistributedContexts->{"TwoFlavour`Private`"}];
{vals,vecs}=Eigensystem[N[HMatx+VMatx]];
Do[g[j]=Dot[ParallelTable[Sin[i ArcCos[2x-1]],{i,1,Nx},DistributedContexts->{"TwoFlavour`Private`"}],vecs[[j]]],{j,1,Nx}];
\[Phi]x=ParallelTable[Quiet[g[Nx-n]/Sqrt[NIntegrate[g[Nx-n]^2,{x,0,1}]]],{n,0,20},DistributedContexts->{"TwoFlavour`Private`"}];
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
sMatrix=ParallelTable[Quiet[sMT[m,n]],{m,0,Nb},{n,0,Nb}]//Chop;hMTUp=PadLeft[#,Nb+1]&/@ParallelTable[Quiet[hMT[m,n]],{m,0,Nb},{n,m,Nb},DistributedContexts->{"TwoFlavour`Private`"}];
(*Print[hMTUp];*)
hMatrix=Transpose[hMTUp]+hMTUp-DiagonalMatrix[Diagonal[hMTUp]];
(*Print[MatrixForm[hMatrix]];*)
eg[\[Mu]_]:=Det[hMatrix-\[Mu]^2*sMatrix];
\[Mu]=Sort@DeleteDuplicatesBy[Table[\[Mu]/.FindRoot[eg[\[Mu]]==0,{\[Mu],a,a+0.2}],{a,0,Nb^2,0.2}],SetPrecision[#,2]&];
Print[\[Mu]];
vecs=(Flatten@NullSpace[hMatrix-#^2 sMatrix,Tolerance->0.001])&/@\[Mu];
Print[vecs];Print[Dimensions@vecs];
func=ParallelTable[Table[psi[i,x],{i,0,Nb}].vecs[[j]],{j,1,Length[vecs]},DistributedContexts->{"TwoFlavour`Private`"}];
nfunc=ParallelTable[Quiet[func[[n]]/Sqrt[NIntegrate[func[[n]]^2,{x,0,1}]]],{n,1,20},DistributedContexts->{"TwoFlavour`Private`"}];
Clear[func,vecs];
{\[Mu],nfunc}
];
\[Phi]n[n_][\[Phi]_][x_]:=\[Phi][x][[n+1]];
Mn[n_][vals_]:=vals[[n+1]];
\[Omega]1S[s_][M1_,M2_,M3_,M4_]:=(-M1^2+M2^2+s-Sqrt[s] Sqrt[(M1^4+(M2^2-s)^2-2 M1^2 (M2^2+s))/s])/(M3^2-M4^2+s+Sqrt[s] Sqrt[(M3^4+(M4^2-s)^2-2 M3^2 (M4^2+s))/s]);
\[Omega]2S[s_][M1_,M2_,M3_,M4_]:=(-M3^2+M4^2+s-Sqrt[s] Sqrt[(M3^4+(M4^2-s)^2-2 M3^2 (M4^2+s))/s])/(M3^2-M4^2+s+Sqrt[s] Sqrt[(M3^4+(M4^2-s)^2-2 M3^2 (M4^2+s))/s]);
Msum3[{mQ_,mq_,md_},{n1_?IntegerQ,n2_?IntegerQ,n3_?IntegerQ,n4_?IntegerQ},OptionsPattern[{SRange->{10^-3,2,0.01},Lambda->10^-6,Method->"BSW"}]]:=
Module[{\[Phi]xab,\[CapitalPhi]ab,Valsab,\[Phi]xca,\[CapitalPhi]ca,Valsca,M1,M2,M3,\[Phi]1,\[Phi]2,\[Phi]3,Ares,filename,\[Omega]now,m1,m2,m3,M4,\[Phi]4,\[Omega]1,\[Omega]2,Sen,filenameacc,Determine,Mseq,\[CapitalPhi]Bi},
(*SetSharedVariable[m2,m1];*)
m1=mQ;m2=mq;m3=md;\[Lambda]=OptionValue[Lambda];
filename=Which[m1==m2,"/eigenstate_m-"<>ToString[m1],m1>m2,"/eigenstate_m1-"<>ToString[m1]<>"_m2-"<>ToString[m2],m1<m2,"/eigenstate_m1-"<>ToString[m2]<>"_m2-"<>ToString[m1]]<>".wdx";
filenameacc=Which[m1==m2,"/acceigenstate_m-"<>ToString[m1],m1>m2,"/acceigenstate_m1-"<>ToString[m1]<>"_m2-"<>ToString[m2],m1<m2,"/acceigenstate_m1-"<>ToString[m2]<>"_m2-"<>ToString[m1]]<>".wdx";
If[FileNames[filenameacc,dir,Infinity]=={},(*If[ChoiceDialog["Choose to use BSW method or to use the original solution suggested by 't Hooft, ",{"BSW method"\[Rule]True,"Brute force integration"\[Rule]False}],Determine=Determine\[Phi]x,filename=filenameacc;Determine=accDetermine\[Phi]]];*)
Which[OptionValue[Method]=="BSW",Determine=Determine\[Phi]x,filename=filenameacc;OptionValue[Method]=="'t Hooft",Determine=accDetermine\[Phi]]];
If[FileNames[filename,dir,Infinity]=={},
{
{Valsab,\[Phi]xab}=Determine[m1,m2,\[Beta],Nx];
Set@@{\[CapitalPhi]ab[Global`x_],Boole[0<=Global`x<=1]\[Phi]xab};
Export[dir<>filename,{Valsab,\[Phi]xab}]
},
{
{Valsab,\[Phi]xab}=Import[dir<>filename];
Set@@{\[CapitalPhi]ab[Global`x_],Boole[0<=Global`x<=1]\[Phi]xab};
}
];
filename=Which[m1==m3,"/eigenstate_m-"<>ToString[m1],m1>m3,"/eigenstate_m1-"<>ToString[m1]<>"_m2-"<>ToString[m3],m1<m3,"/eigenstate_m1-"<>ToString[m3]<>"_m2-"<>ToString[m1]]<>".wdx";
filenameacc=Which[m1==m3,"/acceigenstate_m-"<>ToString[m1],m1>m3,"/acceigenstate_m1-"<>ToString[m1]<>"_m2-"<>ToString[m3],m1<m3,"/acceigenstate_m1-"<>ToString[m3]<>"_m2-"<>ToString[m1]]<>".wdx";
If[FileNames[filenameacc,dir,Infinity]=={},(*If[ChoiceDialog["Choose to use BSW method or to use the original solution suggested by 't Hooft, ",{"BSW method"\[Rule]True,"Brute force integration"\[Rule]False}],Determine=Determine\[Phi]x,filename=filenameacc;Determine=accDetermine\[Phi]]];*)
Which[OptionValue[Method]=="BSW",Determine=Determine\[Phi]x,filename=filenameacc;OptionValue[Method]=="'t Hooft",Determine=accDetermine\[Phi]]];
If[FileNames[filename,dir,Infinity]=={},
{
{Valsca,\[Phi]xca}=Determine[m1,m3,\[Beta],Nx];
Set@@{\[CapitalPhi]ca[Global`x_],Boole[0<=Global`x<=1]\[Phi]xca};
Export[dir<>filename,{Valsca,\[Phi]xca}]
},
{
{Valsca,\[Phi]xca}=Import[dir<>filename];
Set@@{\[CapitalPhi]ca[Global`x_],Boole[0<=Global`x<=1]\[Phi]xca};
}
];
(*Set@@{\[CapitalPhi]B[x_?NumberQ],If[0<x<1,\[CapitalPhi]Bi[x],Table[0,{n,Length@\[Phi]xB}]]};*)
Print["Wavefunction build complete."];
(*Print[\[Phi]n[#][\[CapitalPhi]B][x]&[1]];*)
{M1,\[Phi]1}={Mn[#][Valsab],\[Phi]n[#][\[CapitalPhi]ab]}&[n1];
{M2,\[Phi]2}={Mn[#][Valsca],\[Phi]n[#][\[CapitalPhi]ca]}&[n2];
{M3,\[Phi]3}={Mn[#][Valsab],\[Phi]n[#][\[CapitalPhi]ab]}&[n3];
{M4,\[Phi]4}={Mn[#][Valsca],\[Phi]n[#][\[CapitalPhi]ca]}&[n4];
Mseq=Sequence[M1,M2,M3,M4];
Print["M1=",M1,"  M2=",M2,"  M3=",M3,"  M4=",M4];
(*Print[$Context];*)
(*DistributeDefinitions[M1,M2,M3,M4,\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4,EXPR,\[ScriptCapitalM]0,\[ScriptCapitalI]1,\[ScriptCapitalI]2,\[ScriptCapitalI]3,\[Omega]1S,\[Omega]2S];*)
{{{n1,n2,n3,n4},{M1,M2,M3,M4}},ParallelTable[Sen=Ssqur^2;\[Omega]1=\[Omega]1S[Sen][M1,M2,M3,M4];\[Omega]2=\[Omega]2S[Sen][M1,M2,M3,M4];
{Ssqur,MHG[\[Omega]1,\[Omega]2][\[Phi]1,\[Phi]2,\[Phi]3,\[Phi]4]},{Ssqur,M1+M2+OptionValue[SRange][[1]],M1+M2+OptionValue[SRange][[2]],OptionValue[SRange][[3]]},DistributedContexts->{"TwoFlavour`Private`"}]}
];
(*$DistributedContexts:=$Context;*)


End[]


EndPackage[]


 


(* ::Input:: *)
(*Msum[1,{0,0,0,0}]*)
