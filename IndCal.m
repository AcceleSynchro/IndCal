(* ::Package:: *)

(*\:8fd9\:662f\:7528\:4e8e\:8ba1\:7b97\:5f20\:91cf\:6307\:6807\:7f29\:5e76\:7684\:7a0b\:5e8f\:5305*)


BeginPackage["IndCal`"]

Print["--IndCal Version 1.0 By T.A"]


(*\:4e00\:4e9b\:7c7b\:578b\:7b26\:53f7*)
Dim::usage="\:8868\:793a\:7ef4\:6570"
Indice::usage="\:8868\:793a\:4e00\:65cf\:6307\:6807\nIndice[i]"
Tensor::usage="\:8868\:793a\:4e00\:4e2a\:5f20\:91cf\nTensor[name,rank,{indices}]"


(*\:51fd\:6570\:7684\:58f0\:660e*)
DefIndice::usage="\:5b9a\:4e49\:4e00\:65cf\:6307\:6807\nDefIndice[indices]"
DefMetric::usage="\:5b9a\:4e49\:5ea6\:89c4\:5f20\:91cf\nDefMetric[metric]"
DefTensor::usage="\:5b9a\:4e49\:4e00\:4e2a\:5f20\:91cf\nDefTensor[name,rank]"
Initial::usage="\:521d\:59cb\:5316\:5b9a\:4e49\:5ea6\:89c4\:3001\:7ef4\:6570\:548c\:4e00\:65cf\:6307\:6807\nInitial[metric,dimension,{indices}]"
InsertIndice::usage="\:63d2\:5165\:4e00\:65cf\:6307\:6807\nInsertIndice[tensor,{indices}]"


Begin["`Private`"]


(*Indice*)
Format[Indice[i_]]:=Superscript["",i]
Format[Indice[-i_]]:=Subscript["",i]
Format[Indice[i__]]:=Row[Indice/@{i}]
Indice/:-Indice[i_]:=Indice[-i]
Indice/:p_Tensor[i__Indice]:=InsertIndice[p,{i}]
Indice/:x_Plus[i__Indice]:=InsertIndice[x,{i}]

(*Tensor*)
Format[Tensor[name_String,rank_Integer,{}]]:=name
Format[Tensor[name_String,rank_Integer,{i__Indice}]]:=Row[{name,i}]


(*\:51fd\:6570\:7684\:5b9a\:4e49*)
(*DefIndice*)
DefIndice[i__]:=DefIndice/@{i}
DefIndice[i_]:=Module[{iString},
iString=ToString[i];
i=Indice[iString];
Print[" \:6307\:6807 ",iString," \:5df2\:5b9a\:4e49 "];
]

(*DefMetric*)
DefMetric[g_]:=Module[{gString},
gString=ToString[g];
DefTensor[g,2];
Tensor/:Tensor[p_,r_,{a___,Indice[i_],b___}]Tensor[gString,2,{c___,Indice[-i_],d___}]:=Tensor[p,r,{a,c,d,b}];
Tensor/:Tensor[p_,r_,{a___,Indice[i_],b___}]Tensor[gString,2,{c___,Indice[-i_],d___}]:=Tensor[p,r,{a,c,d,b}];
Tensor/:Tensor[gString,2,{Indice[i_],Indice[-i_]}]:=Dim;
Tensor/:Tensor[gString,2,{Indice[-i_],Indice[i_]}]:=Dim;
]

(*DefTensor*)
DefTensor::RankArg="`1`\:5fc5\:987b\:662f\:4e00\:4e2a\:6b63\:6574\:6570"
DefTensor[p_,rank_]:=Module[{pString},
Which[
!IntegerQ[rank]||rank<=0,Message[DefTensor::RankArg,rank],
True,
pString=ToString[p];
p=Tensor[pString,rank,{}];
(*Protect[p];*)
Print[" ",rank," \:9636\:5f20\:91cf ",pString," \:5df2\:5b9a\:4e49 "];
]
]

(*Initial*)
Initial[g_,dim_,{i__}]:=Module[{},
Print[" \:7ef4\:6570\:4e3a ",dim];
DefMetric[g];
Dim=dim;
DefIndice[i];
]

(*InsertIndice*)
InsertIndice::RankArg="\:5f20\:91cf`1`\:6307\:6807\:6570\:91cf\:5fc5\:987b\:662f`2`\:4e2a"
InsertIndice[Tensor[p_String,rank_,{}],{i__Indice}]:=
Which[Length[{i}]!=rank,Message[InsertIndice::RankArg,p,rank],
True,Tensor[p,rank,{i}]
]
HoldPattern[InsertIndice[c_*p_Tensor,{i__Indice}]]:=c*InsertIndice[p,{i}]
HoldPattern[InsertIndice[x_Plus,{i__Indice}]]:=Module[{Insert},
Insert[xx_]:=InsertIndice[xx,{i}];
Insert/@x
]


End[]


EndPackage[]
