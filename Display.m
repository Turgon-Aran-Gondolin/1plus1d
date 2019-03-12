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
(*cs 2ab-2ab*)


Msumdat[1]=<<"data/Msumdat_m1-4.19022-m2-0.749-n-(0-0-0-0)-type-2ab-2ab.dat";


(* ::Subsection:: *)
(*bc 2 ab - 2 ab*)


Msumdat[1]=<<"data/Msumdat_m1-13.5565-m2-4.19022-n-(0-0-0-0)-type-2ab-2ab.dat";
Msumdat[2]=<<"data/Msumdat_m1-13.5565-m2-4.19022-n-(1-1-0-0)-type-2ab-2ab.dat";
Msumdat[3]=<<"data/Msumdat_m1-13.5565-m2-4.19022-n-(0-0-1-1)-type-2ab-2ab.dat";
Msumdat[4]=<<"data/Msumdat_m1-13.5565-m2-4.19022-n-(1-1-1-1)-type-2ab-2ab.dat";


If[And@@Table[Msumdat[i][[1,3]]=={13.5565`,4.19022`},{i,4}],Print["OK"],Abort[]];
Msumdat[1]>>"data/Msumdat_m1-13.5565-m2-4.19022-n-(0-0-0-0)-type-2ab-2ab.dat";
Msumdat[2]>>"data/Msumdat_m1-13.5565-m2-4.19022-n-(1-1-0-0)-type-2ab-2ab.dat";
Msumdat[3]>>"data/Msumdat_m1-13.5565-m2-4.19022-n-(0-0-1-1)-type-2ab-2ab.dat";
Msumdat[4]>>"data/Msumdat_m1-13.5565-m2-4.19022-n-(1-1-1-1)-type-2ab-2ab.dat";


(* ::Subsection:: *)
(*bcsd*)


Msumdat[1]=<<"data/Msumdat_m1-4.19022-m2-0.749-m3-13.5565-m4-0.09-n-(0-0-0-0)-type-ad+cb-ab+cd.dat";
Msumdat[2]=<<"data/Msumdat_m1-4.19022-m2-0.749-m3-13.5565-m4-0.09-n-(1-1-0-0)-type-ad+cb-ab+cd.dat";
Msumdat[3]=<<"data/Msumdat_m1-4.19022-m2-0.749-m3-13.5565-m4-0.09-n-(0-0-1-1)-type-ad+cb-ab+cd.dat";
Msumdat[4]=<<"data/Msumdat_m1-4.19022-m2-0.749-m3-13.5565-m4-0.09-n-(1-1-1-1)-type-ad+cb-ab+cd.dat";


(* ::Subsection:: *)
(*data examine*)


Msumdat[#][[1]]&/@Range[4]


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
(*Edit*)


iii=4;
Msumdattmp=Re@Chop[Msumdat[iii]];
Si=Max[Plus@@Msumdattmp[[1,2,1;;2]],Plus@@Msumdattmp[[1,2,3;;4]]];


(* ::Subsubsection:: *)
(*Delete by abs*)


Msumdattmp[[2]]=DeleteCases[Msumdattmp[[2]],_?(Abs[#[[2]]]>10000&)];


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
(Position[Transpose[{#[[1;;All]],MovingMedian[#,1]}],_?(Abs[Subtract@@#[[2]]]>100&),{1},Heads->False])&[Msumdattmp[[2]]]
Extract[Msumdattmp[[2]],%]


(*Drop point of position {9/digit} based on manual searching*)
Msumdattmp[[2]]=Drop[Msumdattmp[[2]],{-2}];


Msumdattmp[[2]]=DeleteCases[Msumdattmp[[2]],_?(#[[1]]==43.35068743894098` &)];


(* ::Subsubsection:: *)
(*QuantileRegression*)


(*Use QuantileRegression package to select outliers*)
Once@Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/QuantileRegression.m"]
qs={0.01,0.5,0.99};
{qs[[1]],1-qs[[-1]]}*Length[Msumdattmp[[2]]]
qfuncs=QuantileRegression[Msumdattmp[[2]],5,qs];
topOutliers=Select[Msumdattmp[[2]],qfuncs[[-1]][#[[1]]]<#[[2]]&]
bottomOutliers=Select[Msumdattmp[[2]],qfuncs[[1]][#[[1]]]>#[[2]]&]
Show[ListLinePlot[Msumdattmp[[2]],PlotRange->All],ListPlot[MapIndexed[Callout[#1,#2//First,Below,CalloutStyle->Red]&,topOutliers],PlotMarkers->{Red,Tiny}],ListPlot[MapIndexed[Callout[#1,#2//First]&,bottomOutliers],PlotMarkers->{Green,Tiny}]]


Msumdattmp[[2]]=Complement[Msumdattmp[[2]],Flatten[{topOutliers},{1}],Flatten[{bottomOutliers},{1}]];


Msumdattmp[[2]]=Complement[Msumdattmp[[2]],Flatten[{topOutliers[[1;;4]]},{1}]];


Msumdattmp[[2]]=Complement[Msumdattmp[[2]],Flatten[{bottomOutliers[[3]]},{1}]];


Show[ListLinePlot[Msumdattmp[[2]],PlotRange->{{Si+1,Si+10},All}],ListPlot[MapIndexed[Callout[#1,#2//First,Below,CalloutStyle->Red]&,topOutliers],PlotMarkers->{Red,Tiny}],ListPlot[MapIndexed[Callout[#1,#2//First]&,bottomOutliers],PlotMarkers->{Green,Tiny}]]


ListPlot[Msumdattmp[[2]],PlotRange->{{Si+4,Si+4.5},All}]


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
(*Display *)


SetOptions[$FrontEnd, 
 PrivateFontOptions -> {"OperatorSubstitution" -> False}]


DimensionConvertion[dat_]:={{dat[[1,1]],Sequence@@(0.34dat[[1,2;;3]])},MapThread[{0.34#1,#2}&,Transpose[dat[[2]]]]};
ColorList=ColorData[97,"ColorList"];
LineList=Table[Dashing[0.002 2^r],{r,1,3}]~Join~{DotDashed}~Join~Table[Dashing[{0.002 2^r,0.02-0.002 2^r}],{r,1,5}];


(* ::Input::Initialization:: *)
(Print[("Amp: Threshold: "<>ToString[If[#[[1,1,1]]+#[[1,1,2]]>=#[[1,1,3]]+#[[1,1,4]],#[[1,2,1]]+#[[1,2,2]],#[[1,2,3]]+#[[1,2,4]]]]<>" GeV\nQuark mass: "<>ReplaceAll[ToString[#]<>" GeV "&/@#[[1,3]],List->StringJoin]<>" \nmass: "<>ReplaceAll[ToString[#]<>" GeV "&/@#[[1,2]],List->StringJoin]<>"")&@#[[1]]];fig=Labeled[ListPlot[Select[Re@#[[2]],#\[Element]Reals&]&/@#,PlotRange->{{Min[#1]-.5,1.5Min[#1]}&@(Sequence@@(Transpose[{First[#[[2]]][[1]]-0.1,Last[#[[2]]][[1]]}&/@#])),{All,All}(*All*)},Joined->True,ImageSize->300,PlotLegends->Placed[LineLegend[(ToString[#[[1,1,1]]]<>"+"<>ToString[#[[1,1,2]]]<>"\[Rule]"<>ToString[#[[1,1,3]]]<>"+"<>ToString[#[[1,1,4]]])&/@#,LegendLayout->{"Column",1}(*,LegendMarkerSize\[Rule]20*),LegendMargins->3,LegendFunction->"Frame"],{Right,Top}],Frame->True,(*FrameLabel->{Row[{Spacer@400,"GeV"}],"\[ScriptCapitalM]"},*)PlotStyle->MapThread[{Black(*#1*),#2}&,{Take[ColorList,Length@#],Take[LineList,Length@#]}],AspectRatio->9/15,TargetUnits->{"GeV",""}
,Epilog->MapThread[{(*Thick,*)Dotted,Black(*#2*),Line[{{Max[#1[[1,2,1]]+#1[[1,2,2]],#1[[1,2,3]]+#1[[1,2,4]]],(*Last[#1[[2]]][[2]]*)0},{Max[#1[[1,2,1]]+#1[[1,2,2]],#1[[1,2,3]]+#1[[1,2,4]]],First[#1[[2]]][[2]]}}]}&,{#,Take[ColorList,Length@#]}]
],
{"\[ScriptCapitalM]","\!\(\*SqrtBox[\(s\)]\)/GeV"(*"Sqrt[s]/\[Lambda]"*)},{Reverse@{Left,Top},Reverse@{Bottom,Right}}]
)&@(DimensionConvertion/@(Chop[Msumdat[#]]&/@{2}(*Reverse@*)(*Range[1,8]*)))


ListPlot[Msumdat[2]//Last]


(* ::Input:: *)
(*Export[Which[$OperatingSystem=="Windows","NumFig/cs_4.19_0.749_zoom_2.eps",$OperatingSystem=="Unix","~/Github/2DScattering/NumFig/cs.eps"],fig,ImageResolution->300]*)


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
