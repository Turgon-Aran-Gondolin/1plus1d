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


Msumdat=<<"data\\Msumdat_m-0.749-n-(0-0-0-0).dat";


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


(* ::Subsubsection:: *)
(*Display*)


Msumdat[[2]]


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
