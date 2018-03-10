(* ::Package:: *)

(* ::Subsubsection:: *)
(*Init & Import*)


(* ::Input::Initialization:: *)
AppendTo[$Path,NotebookDirectory[]];
Get["OneFlavour`"]


SetDirectory[NotebookDirectory[]];


Msumdat>>"data\\Msumdat_m-4.23-n-(2-2-2-2).dat"


Msumdat=<<"data\\Msumdat_m-s-n-(0-0-0-0).dat";


FileNames["*","data"]


Msumdat>>"data\\Msumdat_m-4.23-n-(2-2-1-1).dat";


Msumdat=<<"data\\Msumdat_m-4.23-n-(2-2-1-1).dat";


Msumdat=<<"data\\Msumdat_m-c-n-(0-0-0-0).dat";


Msumdat=<<"data\\Msumdat_m-4.23-n-(0-0-0-0).dat";


Msumdat=<<"data\\Msumdat_m-0.749-n-(0-0-0-0).dat";


(*c*)


Msumdat=<<"data/Msumdat_m-c-n-(0-0-0-0).dat";


Msumdat[1]=<<"data/Msumdat_m-4.233-n-(0-0-0-0).dat";
Msumdat[2]=<<"data/Msumdat_m-4.233-n-(1-1-1-1).dat";
Msumdat[3]=<<"data/Msumdat_m-4.233-n-(0-0-1-1).dat";
Msumdat[4]=<<"data/Msumdat_m-4.233-n-(2-2-0-0).dat";
Msumdat[5]=<<"data/Msumdat_m-4.23-n-(2-2-1-1).dat";
Msumdat[6]=<<"data/Msumdat_m-4.23-n-(2-2-2-2).dat";


Msumdat=<<"https://github.com/Turgon-Aran-Gondolin/1plus1d/raw/master/data/Msumdat_m-4.23-n-(0-0-0-0).dat";


Msumdat=<<"https://github.com/Turgon-Aran-Gondolin/1plus1d/raw/d7faab507a34a1cb0a9cb2b6ea2c90207212c7c6/data/Msumdat_m-4.23-n-(0-0-0-0).dat";


(*s*)


Msumdat=<<"data/Msumdat_m-0.749-n-(0-0-0-0).dat";


Msumdat=<<"https://github.com/Turgon-Aran-Gondolin/1plus1d/raw/master/data/Msumdat_m-0.749-n-(0-0-0-0).dat";


Msumdat=<<"https://github.com/Turgon-Aran-Gondolin/1plus1d/raw/fc8da22f28f9743604026b5eccc8cbc955c83c6e/data/Msumdat_m-0.749-n-(0-0-0-0).dat";


(*csbar*)


Msumdat=<<"data/Msumdat_m1-4.233-m2-0.749-n-(0-0-0-0).dat";


Msumdat=<<"data/Msumdat_m1-4.233-m2-0.749-n-(1-0-1-0).dat";


Msumdat=<<"data/Msumdat_m1-4.233-m2-0.749-n-(1-1-1-1).dat";


Msumdat[1]


(* ::Subsubsection::Closed:: *)
(*Old*)


(* ::Code:: *)
(*Msumdat=Import["data\\Msumdat_m-4.23-n-(2-2-2-2).dat","CSV"];*)


(* ::Code:: *)
(*Msumdat=Msumdat//ToExpression;*)


(* ::Code:: *)
(*Msumdat[[1]]=Take[Msumdat[[1]],3]*)


(* ::Code:: *)
(*Msumdat*)


(* ::Subsubsection::Closed:: *)
(*Edit*)


Msumdat[[2]]=Msumdat[[2]]//Chop;


Msumdat[[2]]=DeleteCases[Msumdat[[2]],_?(Abs[#[[2]]]^2>100000&)];


Msumdat[[2]]=DeleteCases[Msumdat[[2]],_?(#[[2]]<0&)];


Msumdat[[2]]=DeleteCases[Msumdat[[2]],_?(!NumberQ[#[[2]]]&)];


Nest


Msumdat[[2]]=Delete[Msumdat[[2]],Drop[Position[PeakDetect[Msumdat[[2,All,2]]],1],1]];


Part[Msumdat[[2]],Flatten@Drop[Position[PeakDetect[Msumdat[[2,All,2]],0,3],1],1]]


(* ::Subsubsection:: *)
(*Display *)


DimensionConvertion[dat_]:={{dat[[1,1]],Sequence@@(0.34dat[[1,2;;3]])},MapThread[{0.34#1,#2}&,Transpose[dat[[2]]]]};


ColorList=ColorData[97,"ColorList"];


(* ::Input::Initialization:: *)
(Print[("Amp: Threshold: "<>ToString[If[#[[1,1,1]]+#[[1,1,2]]>=#[[1,1,3]]+#[[1,1,4]],#[[1,2,1]]+#[[1,2,2]],#[[1,2,3]]+#[[1,2,4]]]]<>" GeV\nQuark mass: "<>ReplaceAll[ToString[#]<>" "&/@#[[1,3]],List->StringJoin]<>"GeV \nmass: "<>ReplaceAll[ToString[#]<>" GeV "&/@#[[1,2]],List->StringJoin]<>"")&@#[[1]]];Labeled[ListPlot[Select[Re@#[[2]],#\[Element]Reals&]&/@#,PlotRange->{{Min[#1]-0.1,Max[#2]-3}&@(Sequence@@(Transpose[{First[#[[2]]][[1]]-0.1,Last[#[[2]]][[1]]}&/@#])),All},Joined->True,ImageSize->500,PlotLegends->Placed[LineLegend[(ToString[#[[1,1,1]]]<>"+"<>ToString[#[[1,1,2]]]<>"\[Rule]"<>ToString[#[[1,1,3]]]<>"+"<>ToString[#[[1,1,4]]])&/@#,LegendLayout->{"Column",1}(*,LegendMarkerSize\[Rule]20*),LegendMargins->OptionValue[LegendMargins],LegendFunction->"Frame"],{Right,Top}],Frame->True,(*FrameLabel->{Row[{Spacer@400,"GeV"}],"\[ScriptCapitalM]"},*)PlotStyle->Take[ColorList,Length@#],AspectRatio->9/15,TargetUnits->{"GeV",""},
Epilog->MapThread[{Dashed,#2,Line[{{If[#1[[1,1,1]]+#1[[1,1,2]]>=#1[[1,1,3]]+#1[[1,1,4]],#1[[1,2,1]]+#1[[1,2,2]],#1[[1,2,3]]+#1[[1,2,4]]],(*Last[#1[[2]]][[2]]*)0},{If[#1[[1,1,1]]+#1[[1,1,2]]>=#1[[1,1,3]]+#1[[1,1,4]],#1[[1,2,1]]+#1[[1,2,2]],#1[[1,2,3]]+#1[[1,2,4]]],First[#1[[2]]][[2]]}}]}&,{#,Take[ColorList,Length@#]}]],
{"\[ScriptCapitalM]","GeV"},{Reverse@{Left,Top},Reverse@{Bottom,Right}}]
)&@(DimensionConvertion/@(Msumdat[#]&/@Range[1,6]))


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
