(* ::Package:: *)

(* ::Section:: *)
(*Init & Import*)


(* ::Input::Initialization:: *)
AppendTo[$Path,NotebookDirectory[]];
Get["OneFlavour`"]


SetDirectory[NotebookDirectory[]];


(* ::Subsection::Closed:: *)
(*c*)


Msumdat=<<"data/Msumdat_m-c-n-(0-0-0-0).dat";


Msumdat[1]=<<"https://github.com/Turgon-Aran-Gondolin/1plus1d/raw/master/data/Msumdat_m-4.19022-n-(0-0-0-0).dat";


Msumdat[1]=<<"data/Msumdat_m-4.19022-n-(0-0-0-0).dat";
Msumdat[2]=<<"data/Msumdat_m-4.19022-n-(1-1-1-1).dat";
Msumdat[3]=<<"data/Msumdat_m-4.19022-n-(0-0-1-1).dat";
Msumdat[4]=<<"data/Msumdat_m-4.19022-n-(2-2-0-0).dat";
Msumdat[5]=<<"data/Msumdat_m-4.19022-n-(2-2-1-1).dat";
Msumdat[6]=<<"data/Msumdat_m-4.19022-n-(2-2-2-2).dat";


Msumdat=<<"https://github.com/Turgon-Aran-Gondolin/1plus1d/raw/master/data/Msumdat_m-4.23-n-(0-0-0-0).dat";


Msumdat=<<"https://github.com/Turgon-Aran-Gondolin/1plus1d/raw/d7faab507a34a1cb0a9cb2b6ea2c90207212c7c6/data/Msumdat_m-4.23-n-(0-0-0-0).dat";


(* ::Subsection::Closed:: *)
(*s*)


Msumdat=<<"data/Msumdat_m-0.749-n-(0-0-0-0).dat";


Msumdat=<<"https://github.com/Turgon-Aran-Gondolin/1plus1d/raw/master/data/Msumdat_m-0.749-n-(0-0-0-0).dat";


Msumdat=<<"https://github.com/Turgon-Aran-Gondolin/1plus1d/raw/fc8da22f28f9743604026b5eccc8cbc955c83c6e/data/Msumdat_m-0.749-n-(0-0-0-0).dat";


(* ::Subsection::Closed:: *)
(*csbar*)


Msumdat[1]=<<"data/Msumdat_m1-4.19022-m2-0.749-n-(0-0-0-0).dat";
Msumdat[2]=<<"data/Msumdat_m1-4.19022-m2-0.749-n-(1-0-0-1).dat";
Msumdat[3]=<<"data/Msumdat_m1-4.19022-m2-0.749-n-(1-0-1-0).dat";
Msumdat[4]=<<"data/Msumdat_m1-4.19022-m2-0.749-n-(1-0-1-1).dat";
Msumdat[5]=<<"data/Msumdat_m1-4.19022-m2-0.749-n-(1-1-1-1).dat";
Msumdat[6]=<<"data/Msumdat_m1-4.19022-m2-0.749-n-(2-0-1-0).dat";
Msumdat[7]=<<"data/Msumdat_m1-4.19022-m2-0.749-n-(2-0-2-0).dat";
Msumdat[8]=<<"data/Msumdat_m1-4.19022-m2-0.749-n-(2-1-2-0).dat";
Msumdat[9]=<<"data/Msumdat_m1-4.19022-m2-0.749-n-(2-2-2-2).dat";


Msumdat[5]


Msumdat[2]={Msumdat[2][[1]],(List@@Msumdat[2][[2]])[[1]]}


(* ::Subsection::Closed:: *)
(*cs ab+ab-ab+ab*)


Msumdat[1]=<<"data/Msumdat_m1-4.19022-m2-0.749-n-(0-0-0-0)-type-ab+ab-ab+ab.dat";


(* ::Subsection::Closed:: *)
(*bc ab+ab-ab+ab*)


Msumdat[1]=<<"data/Msumdat_m1-13.5565-m2-4.19022-n-(0-0-0-0)-type-ab+ab-ab+ab.dat";
Msumdat[2]=<<"data/Msumdat_m1-13.5565-m2-4.19022-n-(1-1-0-0)-type-ab+ab-ab+ab.dat";
Msumdat[3]=<<"data/Msumdat_m1-13.5565-m2-4.19022-n-(0-0-1-1)-type-ab+ab-ab+ab.dat";
Msumdat[4]=<<"data/Msumdat_m1-13.5565-m2-4.19022-n-(1-1-1-1)-type-ab+ab-ab+ab.dat";


If[And@@Table[Msumdat[i][[1,3]]=={13.5565`,4.19022`},{i,4}],Print["OK"],Abort[]];
Msumdat[1]>>"data/Msumdat_m1-13.5565-m2-4.19022-n-(0-0-0-0)-type-ab+ab-ab+ab.dat";
Msumdat[2]>>"data/Msumdat_m1-13.5565-m2-4.19022-n-(1-1-0-0)-type-ab+ab-ab+ab.dat";
Msumdat[3]>>"data/Msumdat_m1-13.5565-m2-4.19022-n-(0-0-1-1)-type-ab+ab-ab+ab.dat";
Msumdat[4]>>"data/Msumdat_m1-13.5565-m2-4.19022-n-(1-1-1-1)-type-ab+ab-ab+ab.dat";


(* ::Subsection::Closed:: *)
(*bcsd*)


datstr1="_m1-4.19022-m2-0.749-m3-13.5565-m4-0.09";datstr2="-type-ad+cb-cd+ab";


Msumdat[1]=<<("data/Msumdat"<>datstr1<>"-n-(0-0-0-0)"<>datstr2<>".dat");
Msumdat[2]=<<("data/Msumdat"<>datstr1<>"-n-(1-1-0-0)"<>datstr2<>".dat");
Msumdat[3]=<<("data/Msumdat"<>datstr1<>"-n-(0-0-1-1)"<>datstr2<>".dat");
Msumdat[4]=<<("data/Msumdat"<>datstr1<>"-n-(1-1-1-1)"<>datstr2<>".dat");


Msumdat[5]=<<("data/Msumdat"<>datstr1<>"-n-(0-0-0-0)"<>datstr2<>"-ak.dat");
Msumdat[6]=<<("data/Msumdat"<>datstr1<>"-n-(1-1-0-0)"<>datstr2<>"-ak.dat");
Msumdat[7]=<<("data/Msumdat"<>datstr1<>"-n-(0-0-1-1)"<>datstr2<>"-ak.dat");
Msumdat[8]=<<("data/Msumdat"<>datstr1<>"-n-(1-1-1-1)"<>datstr2<>"-ak.dat");


If[And@@Table[Msumdat[i][[1,3]]=={4.19022`,0.749`,13.5565`,0.09`},{i,4}],Print["OK"],Abort[]];
Msumdat[1]>>("data/Msumdat"<>datstr1<>"-n-(0-0-0-0)"<>datstr2<>".dat");
Msumdat[2]>>("data/Msumdat"<>datstr1<>"-n-(1-1-0-0)"<>datstr2<>".dat");
Msumdat[3]>>("data/Msumdat"<>datstr1<>"-n-(0-0-1-1)"<>datstr2<>".dat");
Msumdat[4]>>("data/Msumdat"<>datstr1<>"-n-(1-1-1-1)"<>datstr2<>".dat");


If[And@@Table[Msumdat[i][[1,3]]=={4.19022`,0.749`,13.5565`,0.09`},{i,5,8}],Print["OK"],Abort[]];
Msumdat[5]>>("data/Msumdat"<>datstr1<>"-n-(0-0-0-0)"<>datstr2<>"-ak.dat");
Msumdat[6]>>("data/Msumdat"<>datstr1<>"-n-(1-1-0-0)"<>datstr2<>"-ak.dat");
Msumdat[7]>>("data/Msumdat"<>datstr1<>"-n-(0-0-1-1)"<>datstr2<>"-ak.dat");
Msumdat[8]>>("data/Msumdat"<>datstr1<>"-n-(1-1-1-1)"<>datstr2<>"-ak.dat");


(* ::Subsection::Closed:: *)
(*3F ac+bb-bc+ab*)


Msumdat[1]=<<"data/Msumdat_m1-0.749-m2-4.19022-m3-0.09-n-(0-0-0-0)-type-ac+bb-bc+ab.dat";
Msumdat[2]=<<"data/Msumdat_m1-0.749-m2-4.19022-m3-0.09-n-(1-1-0-0)-type-ac+bb-bc+ab.dat";
Msumdat[3]=<<"data/Msumdat_m1-0.749-m2-4.19022-m3-0.09-n-(0-0-1-1)-type-ac+bb-bc+ab.dat";
Msumdat[4]=<<"data/Msumdat_m1-0.749-m2-4.19022-m3-0.09-n-(1-1-1-1)-type-ac+bb-bc+ab.dat";


Msumdat[5]=<<"data/Msumdat_m1-0.749-m2-4.19022-m3-0.09-n-(0-0-0-0)-type-ac+bb-bc+ab-ak.dat";
Msumdat[6]=<<"data/Msumdat_m1-0.749-m2-4.19022-m3-0.09-n-(1-1-0-0)-type-ac+bb-bc+ab-ak.dat";
Msumdat[7]=<<"data/Msumdat_m1-0.749-m2-4.19022-m3-0.09-n-(0-0-1-1)-type-ac+bb-bc+ab-ak.dat";
Msumdat[8]=<<"data/Msumdat_m1-0.749-m2-4.19022-m3-0.09-n-(1-1-1-1)-type-ac+bb-bc+ab-ak.dat";


(* ::Subsection::Closed:: *)
(*3F ac+cb-cc+ab*)


Msumdat[1]=<<"data/Msumdat_m1-0.749-m2-0.09-m3-4.19022-n-(0-0-0-0)-type-ac+cb-cc+ab.dat";
Msumdat[2]=<<"data/Msumdat_m1-0.749-m2-0.09-m3-4.19022-n-(1-1-0-0)-type-ac+cb-cc+ab.dat";
Msumdat[3]=<<"data/Msumdat_m1-0.749-m2-0.09-m3-4.19022-n-(0-0-1-1)-type-ac+cb-cc+ab.dat";
Msumdat[4]=<<"data/Msumdat_m1-0.749-m2-0.09-m3-4.19022-n-(1-1-1-1)-type-ac+cb-cc+ab.dat";


Msumdat[5]=<<"data/Msumdat_m1-0.749-m2-0.09-m3-4.19022-n-(0-0-0-0)-type-ac+cb-cc+ab-ak.dat";
Msumdat[6]=<<"data/Msumdat_m1-0.749-m2-0.09-m3-4.19022-n-(1-1-0-0)-type-ac+cb-cc+ab-ak.dat";
Msumdat[7]=<<"data/Msumdat_m1-0.749-m2-0.09-m3-4.19022-n-(0-0-1-1)-type-ac+cb-cc+ab-ak.dat";
Msumdat[8]=<<"data/Msumdat_m1-0.749-m2-0.09-m3-4.19022-n-(1-1-1-1)-type-ac+cb-cc+ab-ak.dat";


(* ::Subsection::Closed:: *)
(*3F sd+sc-sd+sc*)


datstr1="_m1-0.749-m2-4.19022-m3-0.09";datstr2="-type-ac+ab-ac+ab";


Msumdat[1]=<<("data/Msumdat"<>datstr1<>"-n-(0-0-0-0)"<>datstr2<>".dat");
Msumdat[2]=<<("data/Msumdat"<>datstr1<>"-n-(1-1-0-0)"<>datstr2<>".dat");
Msumdat[3]=<<("data/Msumdat"<>datstr1<>"-n-(0-0-1-1)"<>datstr2<>".dat");
Msumdat[4]=<<("data/Msumdat"<>datstr1<>"-n-(1-1-1-1)"<>datstr2<>".dat");


Msumdat[5]=<<("data/Msumdat"<>datstr1<>"-n-(0-0-0-0)"<>datstr2<>"-ak.dat");
Msumdat[6]=<<("data/Msumdat"<>datstr1<>"-n-(1-1-0-0)"<>datstr2<>"-ak.dat");
Msumdat[7]=<<("data/Msumdat"<>datstr1<>"-n-(0-0-1-1)"<>datstr2<>"-ak.dat");
Msumdat[8]=<<("data/Msumdat"<>datstr1<>"-n-(1-1-1-1)"<>datstr2<>"-ak.dat");


If[And@@Table[Msumdat[i][[1,3]]=={0.749`,4.19022`,0.09`},{i,4}],Print["OK"],Abort[]];
Msumdat[1]>>("data/Msumdat"<>datstr1<>"-n-(0-0-0-0)"<>datstr2<>".dat");
Msumdat[2]>>("data/Msumdat"<>datstr1<>"-n-(1-1-0-0)"<>datstr2<>".dat");
Msumdat[3]>>("data/Msumdat"<>datstr1<>"-n-(0-0-1-1)"<>datstr2<>".dat");
Msumdat[4]>>("data/Msumdat"<>datstr1<>"-n-(1-1-1-1)"<>datstr2<>".dat");


If[And@@Table[Msumdat[i][[1,3]]=={0.749`,4.19022`,0.09`},{i,5,8}],Print["OK"],Abort[]];
Msumdat[5]>>("data/Msumdat"<>datstr1<>"-n-(0-0-0-0)"<>datstr2<>"-ak.dat");
Msumdat[6]>>("data/Msumdat"<>datstr1<>"-n-(1-1-0-0)"<>datstr2<>"-ak.dat");
Msumdat[7]>>("data/Msumdat"<>datstr1<>"-n-(0-0-1-1)"<>datstr2<>"-ak.dat");
Msumdat[8]>>("data/Msumdat"<>datstr1<>"-n-(1-1-1-1)"<>datstr2<>"-ak.dat");


(* ::Subsection:: *)
(*3F cd+cs-cd+cs*)


datstr1="_m1-4.19022-m2-0.749-m3-0.09";datstr2="-type-ac+ab-ac+ab";


Msumdat[1]=<<("data/Msumdat"<>datstr1<>"-n-(0-0-0-0)"<>datstr2<>".dat");
Msumdat[2]=<<("data/Msumdat"<>datstr1<>"-n-(1-1-0-0)"<>datstr2<>".dat");
Msumdat[3]=<<("data/Msumdat"<>datstr1<>"-n-(0-0-1-1)"<>datstr2<>".dat");
Msumdat[4]=<<("data/Msumdat"<>datstr1<>"-n-(1-1-1-1)"<>datstr2<>".dat");


Msumdat[5]=<<("data/Msumdat"<>datstr1<>"-n-(0-0-0-0)"<>datstr2<>"-ak.dat");
Msumdat[6]=<<("data/Msumdat"<>datstr1<>"-n-(1-1-0-0)"<>datstr2<>"-ak.dat");
Msumdat[7]=<<("data/Msumdat"<>datstr1<>"-n-(0-0-1-1)"<>datstr2<>"-ak.dat");
Msumdat[8]=<<("data/Msumdat"<>datstr1<>"-n-(1-1-1-1)"<>datstr2<>"-ak.dat");


If[And@@Table[Msumdat[i][[1,3]]=={4.19022`,0.749`,0.09`},{i,4}],Print["OK"],Abort[]];
Msumdat[1]>>("data/Msumdat"<>datstr1<>"-n-(0-0-0-0)"<>datstr2<>".dat");
Msumdat[2]>>("data/Msumdat"<>datstr1<>"-n-(1-1-0-0)"<>datstr2<>".dat");
Msumdat[3]>>("data/Msumdat"<>datstr1<>"-n-(0-0-1-1)"<>datstr2<>".dat");
Msumdat[4]>>("data/Msumdat"<>datstr1<>"-n-(1-1-1-1)"<>datstr2<>".dat");


If[And@@Table[Msumdat[i][[1,3]]=={4.19022`,0.749`,0.09`},{i,5,8}],Print["OK"],Abort[]];
Msumdat[5]>>("data/Msumdat"<>datstr1<>"-n-(0-0-0-0)"<>datstr2<>"-ak.dat");
Msumdat[6]>>("data/Msumdat"<>datstr1<>"-n-(1-1-0-0)"<>datstr2<>"-ak.dat");
Msumdat[7]>>("data/Msumdat"<>datstr1<>"-n-(0-0-1-1)"<>datstr2<>"-ak.dat");
Msumdat[8]>>("data/Msumdat"<>datstr1<>"-n-(1-1-1-1)"<>datstr2<>"-ak.dat");


(* ::Subsection::Closed:: *)
(*3F bu bd*)


Msumdat[1]=<<"data/Msumdat_m1-13.5565-m2-0.09-m3-0.045-n-(0-0-0-0)-type-ac+ab-ac+ab.dat";


Msumdat[5]=<<"data/Msumdat_m1-13.5565-m2-0.09-m3-0.045-n-(0-0-0-0)-type-ac+ab-ac+ab-ak.dat";


If[And@@Table[Msumdat[i][[1,3]]=={13.5565`,0.09`,0.045`},{i,4}],Print["OK"],Abort[]];
Msumdat[1]>>"data/Msumdat_m1-13.5565-m2-0.09-m3-0.045-n-(0-0-0-0)-type-ac+ab-ac+ab.dat";(*
Msumdat[2]>>"data/Msumdat_m1-13.5565-m2-0.09-m3-0.045-n-(1-1-0-0)-type-ac+ab-ac+ab.dat";
Msumdat[3]>>"data/Msumdat_m1-13.5565-m2-0.09-m3-0.045-n-(0-0-1-1)-type-ac+ab-ac+ab.dat";
Msumdat[4]>>"data/Msumdat_m1-13.5565-m2-0.09-m3-0.045-n-(1-1-1-1)-type-ac+ab-ac+ab.dat";*)


If[And@@Table[Msumdat[i][[1,3]]=={13.5565`,0.09`,0.045`},{i,5,8}],Print["OK"],Abort[]];
Msumdat[5]>>"data/Msumdat_m1-13.5565-m2-0.09-m3-0.045-n-(0-0-0-0)-type-ac+ab-ac+ab-ak.dat";(*
Msumdat[6]>>"data/Msumdat_m1-13.5565-m2-0.09-m3-0.045-n-(1-1-0-0)-type-ac+ab-ac+ab-ak.dat";
Msumdat[7]>>"data/Msumdat_m1-13.5565-m2-0.09-m3-0.045-n-(0-0-1-1)-type-ac+ab-ac+ab-ak.dat";
Msumdat[8]>>"data/Msumdat_m1-13.5565-m2-0.09-m3-0.045-n-(1-1-1-1)-type-ac+ab-ac+ab-ak.dat";*)


(* ::Subsection:: *)
(*data examine*)


Msumdat[#][[1]]&/@Range[8]


(* ::Section::Closed:: *)
(*Old*)


(* ::Code:: *)
(*Msumdat=Import["data\\Msumdat_m-4.23-n-(2-2-2-2).dat","CSV"];*)


(* ::Code:: *)
(*Msumdat=Msumdat//ToExpression;*)


(* ::Code:: *)
(*Msumdat[[1]]=Take[Msumdat[[1]],3]*)


(* ::Code:: *)
(*Msumdat*)


(* ::Section:: *)
(*Chop ErrorBar*)


CEB[data_]:={data[[1]],data[[2,All,1;;2]]};
CEB2[data_]:=data[[All,1;;2]];
FlattenDat[data_]:=MapAt[Map[Flatten[#]&,#]&,data,2];
REB[data_]:={data[[1]],Sort[PadRight[#,3,0]&/@data[[2]],#1[[1]]<#2[[1]]&]};


(* ::Section::Closed:: *)
(*Edit*)


iii=5;
Msumdattmp=Chop[Msumdat[iii],10^-4];
Msumdattmp[[2]]=Cases[Msumdattmp[[2]],_?(NumberQ[#[[2]]]&)];
Si=Max[Plus@@Msumdattmp[[1,2,1;;2]],Plus@@Msumdattmp[[1,2,3;;4]]];


Msumdattmp[[2]]=Cases[Msumdattmp[[2]],_?(Element[#[[2]],Reals]&)];


Msumdattmp[[2]]=Msumdattmp[[2]]//Re;


?Msumdattmp


(* ::Subsubsection:: *)
(*Delete by abs*)


Msumdattmp[[2]]=DeleteCases[Msumdattmp[[2]],_?(Abs[#[[2]]]>10^7&)];


(* ::Subsubsection:: *)
(*Delete by difference*)


(*Delete points with difference larger than 500/digit (recommand to do this first)*)
diff=1000;
Msumdattmp[[2]]=Delete[#,Position[Transpose[{#[[1;;All]],MovingMedian[#,1]}],_?(Abs[Subtract@@#[[2]]]>diff&),{1},Heads->False]]&[Msumdattmp[[2]]];


diff=500;
Msumdattmp[[2]]=Delete[#,Position[Transpose[{#[[1;;All]],MovingMedian[#,1]}],_?(Abs[Subtract@@#[[2]]]>diff&),{1},Heads->False]]&[Msumdattmp[[2]]];


(* ::Subsubsection:: *)
(*Manual point-searching*)


(*Manually find point exceed difference of 500/digit*)
(Position[Transpose[{#[[1;;All]],MovingMedian[#,1]}],_?(Abs[Subtract@@#[[2]]]>100&),{1},Heads->False])&[Msumdattmp[[2]]//CEB2]
Extract[Msumdattmp[[2]]//CEB2,%]


(*Drop point of position {9/digit} based on manual searching*)
Msumdattmp[[2]]=Drop[Msumdattmp[[2]],{-2}];


Msumdattmp[[2]]=DeleteCases[Msumdattmp[[2]],_?(#[[1]]==115.5356842582988` &)];


(* ::Subsubsection:: *)
(*QuantileRegression*)


(*Use QuantileRegression package to select outliers*)
Once@Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/QuantileRegression.m"]
qs={0.01,0.5,0.99};
{qs[[1]],1-qs[[-1]]}*Length[Msumdattmp[[2]]]
qfuncs=QuantileRegression[Msumdattmp[[2]]//CEB2,5,qs];
topOutliers=Select[Msumdattmp[[2]],qfuncs[[-1]][#[[1]]]<#[[2]]&]
bottomOutliers=Select[Msumdattmp[[2]],qfuncs[[1]][#[[1]]]>#[[2]]&]
Show[ListLinePlot[Msumdattmp[[2]]//CEB2,PlotRange->All],ListPlot[MapIndexed[Callout[#1,#2//First,Below,CalloutStyle->Red]&,topOutliers//CEB2],PlotMarkers->{Red,Tiny}],ListPlot[MapIndexed[Callout[#1,#2//First]&,bottomOutliers//CEB2],PlotMarkers->{Green,Tiny}]]


Msumdattmp[[2]]=Complement[Msumdattmp[[2]],Flatten[{topOutliers},{1}],Flatten[{bottomOutliers},{1}]];


Msumdattmp[[2]]=Complement[Msumdattmp[[2]],NestWhile[Flatten[#,1]&,{topOutliers[[1;;5]]},Depth@#>3&]];


Msumdattmp[[2]]=Complement[Msumdattmp[[2]],NestWhile[Flatten[#,1]&,{bottomOutliers[[1;;5]]},Depth@#>3&]];


Show[ListLinePlot[Msumdattmp[[2]],PlotRange->{{Si+0,Si+20},All}],ListPlot[MapIndexed[Callout[#1,#2//First,Below,CalloutStyle->Red]&,topOutliers],PlotMarkers->{Red,Tiny}],ListPlot[MapIndexed[Callout[#1,#2//First]&,bottomOutliers],PlotMarkers->{Green,Tiny}]]


ListPlot[Msumdattmp[[2]],PlotRange->{{Si+0,Si+5},All},PlotMarkers->{Automatic,Tiny}]


(* ::Subsubsection::Closed:: *)
(*Others*)


Msumdat[[2]]=DeleteCases[Msumdat[[2]],_?(#[[2]]<0&)];


Msumdat[[2]]=DeleteCases[Msumdat[[2]],_?(!NumberQ[#[[2]]]&)];


(*Manually select point by x-axis*)
Msumdattmp[[2]]=DeleteCases[Msumdattmp[[2]],_?(#[[1]]==23.730315927339234` &)];


Msumdat[[2]]=Delete[Msumdat[[2]],Drop[Position[PeakDetect[Msumdat[[2,All,2]]],1],1]];


Part[Msumdat[[2]],Flatten@Drop[Position[PeakDetect[Msumdat[[2,All,2]],0,3],1],1]]


(* ::Subsubsection:: *)
(*Reload and Save*)


Msumdat[iii]=Msumdattmp;


(* ::Section:: *)
(*Select Breaking Point*)


FindDiscontinuity[dat_,\[Sigma]_,s_]:=Partition[FindPeaks[Table[Abs[Divide@@Reverse[dat[[i+2]]-dat[[i+1]]]/Divide@@Reverse[dat[[i+1]]-dat[[i]]] Mean[{dat[[i+2]]-dat[[i+1]],dat[[i+1]]-dat[[i]]}][[2]]],{i,Length[dat]-2}],\[Sigma],s][[All,1]]+1,1];
FindDiscontinuity[dat_,\[Sigma]_]:=FindDiscontinuity[dat,\[Sigma],Automatic];
FindDiscontinuity[dat_]:=FindDiscontinuity[dat,Automatic,Automatic];


Clear@$DISPOS


iiii=8;
dispos=If[MatchQ[#,Null],Abort[],#]&@Catch[Do[If[#!={},Throw[#]]&@FindDiscontinuity[Msumdat[iiii][[2]]//Chop//CEB2,1,srp],{srp,70,1,-1}]]
dispoi=Check[Extract[Msumdat[iiii][[2]],dispos],"No $DISC"]
ListPlot[Msumdat[iiii][[2]]//Chop//CEB2,PlotRange->All,Epilog-> {Red,PointSize[Medium],Point[dispoi//Chop//CEB2]}]


$DISPOS[iiii]=dispos[[1]]


Put[FullDefinition@$DISPOS,"data/DISC"<>datstr1<>datstr2<>".dat"]


Get["data/DISC"<>datstr1<>datstr2<>".dat"];
?$DISPOS


(* ::Section:: *)
(*Display *)


SetOptions[$FrontEnd, 
 PrivateFontOptions -> {"OperatorSubstitution" -> False}]


DimensionConvertion[dat_]:={{dat[[1,1]],Sequence@@(0.34dat[[1,2;;3]])},MapThread[{0.34#1,#2}&,Transpose[dat[[2]]]]};
DimensionConvertionR[dat_]:={{dat[[1,1]],Sequence@@(0.34^-1 dat[[1,2;;3]])},MapThread[{0.34^-1 #1,#2}&,Transpose[dat[[2]]]]};
ColorList=ColorData[97,"ColorList"];
LineList=Table[Dashing[0.004 2^r],{r,1,3}]~Join~{DotDashed}~Join~Table[Dashing[{0.002 2^r,0.02-0.002 2^r}],{r,1,5}];
PTest[]:=Print["Halt"];


(* ::Input::Initialization:: *)
datlis={3}(*Reverse@*)(*Range[1,8]*);
Module[{min=.1,maxt=3.5,dat,thre,str,legendfun,length,datindex},length=Length@datlis;dat=DimensionConvertion/@(Chop[Msumdat[#]//CEB]&/@datlis);
thre=Max[#[[1,2,1]]+#[[1,2,2]],#[[1,2,3]]+#[[1,2,4]]]&@dat[[1]];
str=(ToString[#[[1,1,1]]]<>"+"<>ToString[#[[1,1,2]]]<>"\[Rule]"<>ToString[#[[1,1,3]]]<>"+"<>ToString[#[[1,1,4]]]&[DimensionConvertion@Chop[Msumdat[#]//CEB]])<>If[#>4," (back)",""]&/@datlis;Print[("Amp: Threshold: "<>ToString[thre]<>" GeV\nQuark mass: "<>ReplaceAll[ToString[#]<>" GeV "&/@#[[1,3]],List->StringJoin]<>" \nmass: "<>ReplaceAll[ToString[#]<>" GeV "&/@#[[1,2]],List->StringJoin]<>"")&@dat[[1]]];fig=Labeled[Legended[Show[MapIndexed[(datindex=First[#2];ListPlot[If[ListQ@$DISPOS[datlis[[datindex]]],{Re@#1[[2,;;$DISPOS[datlis[[datindex]]][[1]]]],Re@#1[[2,$DISPOS[datlis[[#2//First]]][[1]]+1;;]]},{Re@#1[[2]]}],Joined->True,(*FrameLabel->{Row[{Spacer@400,"GeV"}],"\[ScriptCapitalM]"},*)PlotStyle->PadRight[#,If[ListQ[$DISPOS[datlis[[datindex]]]],2,1],#]&@{{Black}~Join~LineList[[#2]]},
Epilog->{(*Thick,*)Dotted,Black(*#2*),Line[{{thre,(*Last[#1[[2]]][[2]]*)0},{thre,First[#1[[2]]][[2]]}}]}
])&,dat],ImageSize->300,Frame->True,PlotRange->{{thre-min,maxt thre},(*{-3,0.55}*){All,All}},AspectRatio->9/15(*,TargetUnits->{"GeV",""}*)],Placed[LineLegend[LineList[[;;length]],str,LegendLayout->{"Column",1}(*,LegendMarkerSize\[Rule]20*),LegendMargins->3,LegendFunction->"Frame"],{Right(*Left*),Top(*Bottom*)}]],
{"\[ScriptCapitalM]","\!\(\*SqrtBox[\(s\)]\)/GeV"(*"Sqrt[s]/\[Lambda]"*)},{Reverse@{Left,Top},Reverse@{Bottom,Right}}]
]


(* ::Input:: *)
(*Export[Which[$OperatingSystem=="Windows","NumFig/cs_4.19_0.749_zoom_2.eps",$OperatingSystem=="Unix","~/Github/2DScattering/NumFig/fig"<>datstr1<>"-pos-"<>StringJoin@@Riffle[ToString/@datlis,"-"]<>datstr2<>".pdf"],fig,ImageResolution->300]*)


(* ::Subsubsection::Closed:: *)
(*Zoom*)


(* ::Input:: *)
(*(Print[("Amp: Threshold: "<>ToString[If[#[[1,1,1]]+#[[1,1,2]]>=#[[1,1,3]]+#[[1,1,4]],#[[1,2,1]]+#[[1,2,2]],#[[1,2,3]]+#[[1,2,4]]]]<>" GeV\nQuark mass: "<>ReplaceAll[ToString[#]<>" GeV "&/@#[[1,3]],List->StringJoin]<>" \nmass: "<>ReplaceAll[ToString[#]<>" GeV "&/@#[[1,2]],List->StringJoin]<>"")&@#[[1]]];figin=ListPlot[Select[Re@#[[2]],#\[Element]Reals&]&/@#,PlotRange->{{Min[#1]-0.05,Max[#2]-310}&@(Sequence@@(Transpose[{First[#[[2]]][[1]]-0.1,Last[#[[2]]][[1]]}&/@#])),All},Joined->True,ImageSize->300,Frame->True,(*FrameLabel->{Row[{Spacer@400,"GeV"}],"\[ScriptCapitalM]"},*)PlotStyle->MapThread[{Black(*#1*),#2}&,{Take[ColorList,Length@#],Take[LineList,Length@#]}],AspectRatio->9/15,TargetUnits->{"GeV",""}*)
(*,Epilog->MapThread[{(*Thick,*)Dotted,Black(*#2*),Line[{{If[#1[[1,1,1]]+#1[[1,1,2]]>=#1[[1,1,3]]+#1[[1,1,4]],#1[[1,2,1]]+#1[[1,2,2]],#1[[1,2,3]]+#1[[1,2,4]]],(*Last[#1[[2]]][[2]]*)0},{If[#1[[1,1,1]]+#1[[1,1,2]]>=#1[[1,1,3]]+#1[[1,1,4]],#1[[1,2,1]]+#1[[1,2,2]],#1[[1,2,3]]+#1[[1,2,4]]],First[#1[[2]]][[2]]}}]}&,{#,Take[ColorList,Length@#]}]*)
(*]*)
(*)&@(DimensionConvertion/@(Msumdat[#]&/@(*Reverse@*)Range[1,3]))*)


(* ::Input:: *)
(*(Print[("Amp: Threshold: "<>ToString[If[#[[1,1,1]]+#[[1,1,2]]>=#[[1,1,3]]+#[[1,1,4]],#[[1,2,1]]+#[[1,2,2]],#[[1,2,3]]+#[[1,2,4]]]]<>" GeV\nQuark mass: "<>ReplaceAll[ToString[#]<>" GeV "&/@#[[1,3]],List->StringJoin]<>" \nmass: "<>ReplaceAll[ToString[#]<>" GeV "&/@#[[1,2]],List->StringJoin]<>"")&@#[[1]]];fig=Labeled[ListPlot[Select[Re@#[[2]],#\[Element]Reals&]&/@#,PlotRange->{{Min[#1]-0.05,Max[#2]}&@(Sequence@@(Transpose[{First[#[[2]]][[1]]-0.1,Last[#[[2]]][[1]]}&/@#])),All},Joined->True,ImageSize->300,PlotLegends->Placed[LineLegend[(ToString[#[[1,1,1]]]<>"+"<>ToString[#[[1,1,2]]]<>"\[Rule]"<>ToString[#[[1,1,3]]]<>"+"<>ToString[#[[1,1,4]]])&/@#,LegendLayout->{"Column",1},LegendMarkerSize->12,LegendMargins->3,LegendFunction->"Frame"],{Right,Top}],Frame->True,(*FrameLabel->{Row[{Spacer@400,"GeV"}],"\[ScriptCapitalM]"},*)PlotStyle->MapThread[{Black(*#1*),#2}&,{Take[ColorList,Length@#],Take[LineList,Length@#]}],AspectRatio->9/15,TargetUnits->{"GeV",""}*)
(*,*)
(*Epilog->Inset[figin,{Right,Top}(*Scaled[{0.4,0.8}]*),{Right,Top},270],Epilog->MapThread[{(*Thick,*)Dotted,Black(*#2*),Line[{{If[#1[[1,1,1]]+#1[[1,1,2]]>=#1[[1,1,3]]+#1[[1,1,4]],#1[[1,2,1]]+#1[[1,2,2]],#1[[1,2,3]]+#1[[1,2,4]]],(*Last[#1[[2]]][[2]]*)0},{If[#1[[1,1,1]]+#1[[1,1,2]]>=#1[[1,1,3]]+#1[[1,1,4]],#1[[1,2,1]]+#1[[1,2,2]],#1[[1,2,3]]+#1[[1,2,4]]],First[#1[[2]]][[2]]}}]}&,{#,Take[ColorList,Length@#]}]*)
(*],*)
(*{"\[ScriptCapitalM]","GeV"},{Reverse@{Left,Top},Reverse@{Bottom,Right}}]*)
(*)&@(DimensionConvertion/@(Msumdat[#]&/@(*Reverse@*)Range[1,3]))*)


(* ::Input:: *)
(*(#[[2,-1,1]]-Total[#[[1,2,1;;2]]])&@Msumdat[2] (*Range of data in unit of \[beta]*)*)


(* ::Input:: *)
(*(Print[("Amp: Threshold: "<>ToString[If[#[[1,1,1]]+#[[1,1,2]]>=#[[1,1,3]]+#[[1,1,4]],#[[1,2,1]]+#[[1,2,2]],#[[1,2,3]]+#[[1,2,4]]]]<>" GeV\nQuark mass: "<>ReplaceAll[ToString[#]<>" GeV "&/@#[[1,3]],List->StringJoin]<>" \nmass: "<>ReplaceAll[ToString[#]<>" GeV "&/@#[[1,2]],List->StringJoin]<>"")&@#[[1]]];fig=Labeled[ListPlot[Select[Re@#[[2]],#\[Element]Reals&]&/@#,PlotRange->{{Min[#1]-0.05,Max[#2]}&@(Sequence@@(Transpose[{First[#[[2]]][[1]]-0.1,Last[#[[2]]][[1]]}&/@#])),All},Joined->True,ImageSize->300,PlotLegends->Placed[LineLegend[(ToString[#[[1,1,1]]]<>"+"<>ToString[#[[1,1,2]]]<>"\[Rule]"<>ToString[#[[1,1,3]]]<>"+"<>ToString[#[[1,1,4]]])&/@#,LegendLayout->{"Column",1},LegendMarkerSize->12,LegendMargins->3,LegendFunction->"Frame"],Scaled[{0.85,0.8}](*{Left,Top}*)],Frame->True,(*FrameLabel->{Row[{Spacer@400,"GeV"}],"\[ScriptCapitalM]"},*)PlotStyle->MapThread[{Black(*#1*),#2}&,{Take[ColorList,Length@#],Take[LineList,Length@#]}],AspectRatio->9/15,TargetUnits->{"GeV",""}*)
(*,*)
(*Epilog->{Inset[figin,(*{Right,Top}*)Scaled[{0.7,1}],{Right,Top},200]},Epilog->MapThread[{(*Thick,*)Dotted,Black(*#2*),Line[{{If[#1[[1,1,1]]+#1[[1,1,2]]>=#1[[1,1,3]]+#1[[1,1,4]],#1[[1,2,1]]+#1[[1,2,2]],#1[[1,2,3]]+#1[[1,2,4]]],(*Last[#1[[2]]][[2]]*)0},{If[#1[[1,1,1]]+#1[[1,1,2]]>=#1[[1,1,3]]+#1[[1,1,4]],#1[[1,2,1]]+#1[[1,2,2]],#1[[1,2,3]]+#1[[1,2,4]]],First[#1[[2]]][[2]]}}]}&,{#,Take[ColorList,Length@#]}]*)
(*],*)
(*{"\[ScriptCapitalM]","GeV"},{Reverse@{Left,Top},Reverse@{Bottom,Right}}]*)
(*)&@(DimensionConvertion/@(Msumdat[#]&/@(*Reverse@*)Range[1,3]))*)


(* ::Subsubsection::Closed:: *)
(*Display-1*)


SS[s_]=Evaluate[FindFormula[Msumdat[[2]],s]]


GeneralizedLinearModelFit[Msumdat[[2]],{x,x^2,x^3,x^4,x^5,x^6},x,LinkFunction->Identity]


(* ::Input:: *)
(*glmresids=%104[{"FitResiduals","AnscombeResiduals"(*,"PearsonResiduals","StandardizedPearsonResiduals"*)}];*)
(*labels={"fit","Anscombe"(*,"Pearson","standardized Pearson"*)};*)
(*plots=MapThread[ListPlot[#1,Frame->True,PlotLabel->#2,Filling->0]&,{glmresids,labels}];*)


(* ::Input:: *)
(*GraphicsGrid[Partition[plots,2],ImageSize->400,PlotLabel->"Types of Residuals"]*)


Plot[%104[s],{s,21,25}]


ListPlot[Table[%104[Msumdat[[2,i,1]]]-Msumdat[[2,i,2]],{i,1,Length[Msumdat[[2]]]}]]


(* ::Input::Initialization:: *)
Row[{displayfunction1[Msumdat,{-0.1,0.1},Joined->\!\(\*
TagBox[
DynamicModuleBox[{$CellContext`x$$ = False}, 
InterpretationBox[
StyleBox[GridBox[{
{
SetterBox[Dynamic[$CellContext`x$$], {True}, "True"], 
SetterBox[Dynamic[$CellContext`x$$], {False}, "False"]}
},
BaselinePosition->{1, 1},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}}],
Deployed->True],
SetterBar[Dynamic[$CellContext`x$$], {True, False}]],
DynamicModuleValues:>{}],
Setting]\),LegendMargins->\!\(\*
TagBox[
TagBox[
DynamicModuleBox[{Typeset`i$$ = 3}, 
InputFieldBox[Dynamic[Typeset`i$$],
FieldSize->1],
DynamicModuleValues:>{}],
InputField[Setting[#], FieldSize -> 1]& ],
Setting]\),ImageSize->Medium],displayfunction2[Msumdat,{-0.2,0.1},Joined->\!\(\*
TagBox[
DynamicModuleBox[{$CellContext`x$$ = False}, 
InterpretationBox[
StyleBox[GridBox[{
{
SetterBox[Dynamic[$CellContext`x$$], {True}, "True"], 
SetterBox[Dynamic[$CellContext`x$$], {False}, "False"]}
},
BaselinePosition->{1, 1},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}}],
Deployed->True],
SetterBar[Dynamic[$CellContext`x$$], {True, False}]],
DynamicModuleValues:>{}],
Setting]\) ,LegendMargins->\!\(\*
TagBox[
TagBox[
DynamicModuleBox[{Typeset`i$$ = 3}, 
InputFieldBox[Dynamic[Typeset`i$$],
FieldSize->1],
DynamicModuleValues:>{}],
InputField[Setting[#], FieldSize -> 1]& ],
Setting]\),ImageSize->Medium]}]


(* ::Input:: *)
(*displayfunctionboth[Msumdat,{-0.1,0.1},Joined->\!\(\**)
(*TagBox[*)
(*DynamicModuleBox[{$CellContext`x$$ = True}, *)
(*InterpretationBox[*)
(*StyleBox[GridBox[{*)
(*{*)
(*SetterBox[Dynamic[$CellContext`x$$], {True}, "True"], *)
(*SetterBox[Dynamic[$CellContext`x$$], {False}, "False"]}*)
(*},*)
(*BaselinePosition->{1, 1},*)
(*GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}, "Items" -> {}, "ItemsIndexed" -> {}}],*)
(*Deployed->True],*)
(*SetterBar[Dynamic[$CellContext`x$$], {True, False}]],*)
(*DynamicModuleValues:>{}],*)
(*Setting]\),LegendMargins->\!\(\**)
(*TagBox[*)
(*TagBox[*)
(*DynamicModuleBox[{Typeset`i$$ = 3}, *)
(*InputFieldBox[Dynamic[Typeset`i$$],*)
(*FieldSize->1],*)
(*DynamicModuleValues:>{}],*)
(*InputField[Setting[#], FieldSize -> 1]& ],*)
(*Setting]\),ImageSize->Large]*)
