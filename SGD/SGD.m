(* Mathematica Package *)

(* Created by the Wolfram Workbench 01-jun-2015 *)

BeginPackage["SGD`"]
SGDGetPoints::usage="SGDGetPoints[grid,dataset] returns the list of points defined by given grid and dataset in the format used by Interpolation.";
SGDSelectPoints::usage="SGDSelectPoints[points, coordinateList, valueList] returns those points (in the format of multidimensional Interpolation data) which have given values in given coordinates.";


SGDViewDataSet::usage="SGDViewDataSet[grid,dataset] returns a Manipulate object to explore given SGD.";


SGDGetAxis::usage="asdf";
SGDGetGrid::usage="asdf";
SGDGetDataset::usage="asdf";

SGDAxisQ::usage="SGDAxisQ[expr] returns True if expr matches a SDG axis (WITHOUT metadata), and False otherwise.";
SGDGridQ::usage="SGDGridQ[expr] returns True if expr matches a SDG axis (WITHOUT metadata (but perhaps in substructures)), and False otherwise.";
SGDDataQ::usage="SGDDataQ[expr] returns True if expr matches SDG data (WITHOUT metadata), and False otherwise.";
SGDDatasetQ::usage="SGDDatasetQ[expr] returns True if expr matches a SDG dataset (WITHOUT metadata (but perhaps in substructures)), and False otherwise.";

Begin["`Private`"]
(*Pattern Definition*)

(*A data of type checked bt typeQ is metadated if it's of the form:*)

Metadated[typeQ_] := {s_String, x_?typeQ}

MetadatedQ[element_, typeQ_] := MatchQ[element, Metadated[typeQ]]


GetData[element_, typeQ_] := 
 If[MetadatedQ[element, typeQ] && typeQ[element], Message[GetData::Ambiguity];
   element, If[MetadatedQ[element, typeQ], element[[2]], 
   If[typeQ[element], element, Message[GetData::NoMatch]]]]

GetData::Ambiguity = "Unable to decide whether there is or not metadata. Assuming there is NO \
metadata.";

GetData::NoMatch = "Data does not match to given arguments.";

GetMeta[element_, typeQ_] := 
 If[MetadatedQ[element, typeQ] && typeQ[element], Message[GetData::Ambiguity];
   "", If[MetadatedQ[element, typeQ], element[[1]], 
   If[typeQ[element], "", Message[GetData::NoMatch]; ""]]]

MetadataForm[element_, typeQ_] := 
 If[MetadatedQ[element, typeQ] && typeQ[element], 
  Message[MetadataForm::Ambiguity]; element, 
  If[MetadatedQ[element, typeQ], element, 
   If[typeQ[element], {"", element}, 
    Message[MetadataForm::NoMatch]; {"", element}]]]

MetadataForm::Ambiguity = 
  "Unable to decide whether there is or not metadata. Assuming there IS \
metadata.";
MetadataForm::NoMatch = 
  "Data does not match to given arguments. Empty metadata was added anyway.";


(*Structure definition*)

(*An Axis in the grid*)
pSGDAxis = x_?(VectorQ[#, NumericQ] &);
pSGDAxisMeta = {s_String, x_?(VectorQ[#, NumericQ] &)};
pSGDAxisGen = pSGDAxis | pSGDAxisMeta;
SGDAxisQ = MatchQ[#, pSGDAxis] &;
SGDAxisGenQ = MatchQ[#, pSGDAxisGen] &;

(*The Grid*)
pSGDGrid = x_?(VectorQ[#, (MatchQ[#, pSGDAxisGen] &)] &);
pSGDGridMeta = {s_String, x_?(VectorQ[#, (MatchQ[#, pSGDAxisGen] &)] &)};
pSGDGridGen = pSGDGrid | pSGDGridMeta;
SGDGridQ = MatchQ[#, pSGDGrid] &;
SGDGridGenQ = MatchQ[#, pSGDGridGen] &;

(*Some data (each part of the dataset)*)

pSGDData = x_?(ArrayQ[#, _, NumericQ] &);
pSGDDataMeta = {s_String, x_?(ArrayQ[#, _, NumericQ] &)};
pSGDDataGen = pSGDData | pSGDDataMeta;
SGDDataQ = MatchQ[#, pSGDData] &;
SGDDataGenQ = MatchQ[#, pSGDDataGen] &;

(*Dataset: a tuple of integers identifying the indexes of the grid + multidim arrays of data (optionally described)*)
pSGDDataset = {index_?(VectorQ[#, IntegerQ] &), dataList_?(VectorQ[#, (MatchQ[#, pSGDDataGen] &)] &)};
pSGDDatasetMeta = {s_String, {index_?(VectorQ[#, IntegerQ] &), dataList_?(VectorQ[#, (MatchQ[#, pSGDDataGen] &)] &)}};
pSGDDatasetGen = pSGDDataset|pSGDDatasetMeta;
SGDDatasetQ = MatchQ[#, pSGDDataset] &;
SGDDatasetGenQ = MatchQ[#, pSGDDatasetGen] &;

(*TODO: Check if dimessions are coherent*)

(*Basic functionality*)

SGDGetAxis[g_?SGDAxisGenQ] := GetData[g, SGDAxisQ]

SGDGetGrid[g_?SGDGridGenQ] := GetData[#, SGDAxisQ] & /@ GetData[g, SGDGridQ]

SGDGetDataset[g_?SGDDatasetGenQ] := 
 Block[{g2}, g2 = GetData[g, SGDDatasetQ]; 
  g2[[2]] = GetData[#, SGDDataQ] & /@ g2[[2]]; g2]

SGDGetPoints[grid_?SGDGridGenQ, dataset_?SGDDatasetGenQ] := Block[{grid2, dataset2},
  dataset2 = SGDGetDataset[dataset]; 
  grid2 = Part @@ {SGDGetGrid[grid], dataset2[[1]]}; 
  Map[Flatten[MapIndexed[({MapThread[Part, {grid2, #2}], #1} &), #, {-1}], 
     Length[grid2] - 1] &, dataset2[[2]]]]




SGDSelectPoints[pointList_, coordinate_List, value_List] := 
 Map[{Delete[#[[1]], List /@ coordinate], #[[2]]} &, 
  Select[pointList, #[[1, coordinate]] == value &]]
  

SGDViewDataSet[{grid_?SGDGridGenQ, dataset_?SGDDatasetGenQ}] := 
 With[{grid2 = GetData[#, SGDAxisQ] & /@ grid, 
   metagrid2 = GetMeta[#, SGDAxisQ] & /@ grid, 
   dataset2 = GetData[dataset, SGDDatasetQ],
   metadataset2 = GetMeta[#, SGDDataQ] & /@ GetData[dataset, SGDDatasetQ][[2]],
   metaTitle = GetMeta[dataset, SGDDatasetQ],
   points = SGDGetPoints[grid, dataset]},
  DynamicModule[{plotChoice = 1, 
    choices = 
     Table[grid2[[dataset2[[1, i]], 1]], {i, Length[dataset2[[1]]]}]}, 
   Column[{Row[{"Variable to plot", 
       Slider[Dynamic[plotChoice], {1, Length[dataset2[[1]]], 1}]}], 
     Grid[Table[
       With[{i = i}, {Dynamic[
          Style[metagrid2[[dataset2[[1, i]]]], 
           If[i == plotChoice, Red, Black]]], 
         Slider[Dynamic[choices[[i]]], {grid2[[dataset2[[1, i]]]]}], 
         Dynamic[Style[choices[[i]], 
           If[i == plotChoice, FontVariations -> {"StrikeThrough" -> True}, 
            Black]]]}], {i, Length[dataset2[[1]]]}]], 
     Dynamic[ListPlot[
       Map[Flatten, 
        SGDSelectPoints[#, Drop[Range[Length[dataset2[[1]]]], {plotChoice}], 
           Drop[choices, {plotChoice}]] & /@ points, {2}], 
       PlotLabel -> metaTitle, PlotLegends -> metadataset2, 
       AxesLabel -> {metagrid2[[dataset2[[1, plotChoice]]]], ""}]]}]]]



End[]

EndPackage[]

