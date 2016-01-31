(* ::Package:: *)

BeginPackage[ "StocksAndFlows`"]

FlowGUI::usage = 
	"FlowGUI [stocks] creates a nice-looking GUI for inputting flow functions."

RunODE::usage = 
    "RunODE [stocks] executes NDSolve[] on the given stocks and flows."

t
Flows={{flow1,0,Null,Null},{flow2,0,Null,Null}}

Begin[ "Private`"]

(* The weird assignment syntax here basically caches the calculation to avoid a Mathematica 
bug resulting in excessive CPU usage *)
simpleStocks[stocksIn_]:=simpleStocks[stocksIn]=(stocks=If[!ListQ[stocksIn],{stocksIn},stocksIn];
If[MatchQ[stocks,{_Rule..}], Transpose[stocks/.Rule->List][[1]], stocks]
)

FlowGUI[ stocksIn_] :=(
Panel[Dynamic[
stocks = simpleStocks[stocksIn];
Grid[Join[{{"Flow",SpanFromLeft,"Equation",SpanFromLeft,"Source","Destination"}},Table[With[{i=i},
{TraditionalForm[Flows[[i]][[1]][t]],
"=",
InputField@Dynamic[Flows[[i]][[2]]],
Dynamic[TraditionalForm@Flows[[i]][[2]]],
PopupMenu[Dynamic[Flows[[i]][[3]]],Join[{Null},stocks]],
PopupMenu[Dynamic[Flows[[i]][[4]]],Join[{Null},stocks]]
}],{i,Length[Flows]}]],ItemStyle->{Automatic,{1->"Subsection"}}]]]  
)

RunODE[stocks_] :=
(stocksList=simpleStocks[stocks];
Block[{t,eqns},

eqns=Join[{stocksList[[1]]'[t]==(Flows[[1]][[2]]-Flows[[2]][[2]])},Table[i[0]==(i/.stocks),{i,stocksList}]];
NDSolve[eqns,stocksList,{t,0,30}]]
)

Off[Plot::argr] (* Prevents Mathematica from getting mad about feeding 1 argument to Plot *)
Unprotect[Plot]
Plot[fList_/;MatchQ[fList,{{_Symbol->_InterpolatingFunction}..}]]:= 
    Plot[Evaluate[Table[(i/.Rule->List)[[1]][[1]][t],{i,fList}]/.fList],Evaluate@Join[{t},(fList/.Rule->List)[[1]][[1]][[2]]["Domain"][[1]]],PlotRange->All,
AxesLabel-> Automatic,PlotLegends-> Map[ToString,Table[(i/.Rule->List)[[1]][[1]],{i,fList}]]]
Protect[Plot]


 End[]

EndPackage[]



