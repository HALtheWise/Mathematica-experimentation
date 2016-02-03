(* ::Package:: *)

BeginPackage[ "StocksAndFlows`"]

PlotAll::usage = 
	"plots all of functions in an output of DSolve or NDSolve"


Begin[ "Private`"]


PlotAll[fList_/;MatchQ[fList,{{_Symbol->_InterpolatingFunction}..}], {ivar_,min_:-\[Infinity],max_:\[Infinity]}]:= 
(
ifRange=(fList/.{{_->func_ ..}..}->func["Domain"])[[1]];
compmin=Max[min, ifRange/.{ifmin_,_}->ifmin];compmax=Min[max, ifRange/.{_,ifmax_}->ifmax];
Print[ifRange," ",compmin," ",compmax,"  ",{ivar,Evaluate@compmin,Evaluate@compmax}];
    Plot[Evaluate[fList/.({_->rhs_}->rhs[ivar])],{ivar,Evaluate@compmin,Evaluate@compmax},PlotRange->All,
AxesLabel-> Automatic(*, PlotLegends->If[Length[fList]>1, Map[ToString,Table[(i/.Rule->List)[[1]][[1]],{i,fList}]],None]*)]
)

PlotAll[fList_/;MatchQ[fList,{{_Symbol->_Function}..}], {ivar_,min_,max_}]:= 
(
    Plot[Evaluate[fList/.({_->_Function[_,rhs_]}->rhs)],{ivar,min,max},PlotRange->All,
AxesLabel-> Automatic, PlotLegends->If[Length[fList]>1, fList/.{name_->Function[_,rhs_]}->name,None]]
)

Options[PlotAll]=Options[Plot];


End[]

EndPackage[]
