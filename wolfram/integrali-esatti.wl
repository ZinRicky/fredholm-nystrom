(* ::Package:: *)

(* ::Title:: *)
(*Equazioni integrali di Fredholm*)


(* ::Subtitle:: *)
(*Valutazioni esatte (quando possibile) degli integrali che compaiono nelle equazioni trattate*)


(* ::Text:: *)
(*In tutti i casi, il termine noto adeguato si trova cos\[IGrave]:*)


y[\[Lambda]_, x_, integrale_]:=\[Lambda] x[t1, t2] - integrale


(* ::Text:: *)
(*Il calcolo di "integrale" presuppone gi\[AGrave] che si sia passata la candidata soluzione x.*)


(* ::Section:: *)
(*Esagono*)


esagono = Polygon[{
	{0,0},
	{1/5,1/10},
	{1/10,2/5},
	{-1/10,1/2},
	{-3/10,3/10},
	{-1/10,1/5}
}];


(* ::Subsection:: *)
(*Nucleo esponenziale*)


integraleEsagonoExp = Integrate[
	Exp[s1 s2 + t1 t2](s1 + s2),
	{s1, s2} \[Element] esagono
]


(* ::Subsection:: *)
(*Nucleo sinusoidale*)


integraleEsagonoSin = Integrate[
	Sin[s1 + s2 + t1 + t2] (s1^2 - s2^2),
	{s1, s2} \[Element] esagono
]


(* ::Section:: *)
(*Caramella*)


caramella[n_] := {
	Polygon[
		Drop[Table[{Cos[t],Sin[t]}/5,{t,Subdivide[0,2\[Pi],2n]}],1]
	],
	Polygon[
		Join[
			{-1,1}+#&/@Table[{Cos[t],Sin[t]},{t,Subdivide[-\[Pi]/2,-\[Pi],n-1]}][[2;;]],
			{-1,-1}+#&/@Table[{Cos[t],Sin[t]},{t,Subdivide[\[Pi],\[Pi]/2,n-1]}]
		]/5
	],
	Polygon[
		Join[
			{1,1}+#&/@Table[{Cos[t],Sin[t]},{t,Subdivide[-\[Pi]/2,0,n-1]}][[2;;]],
			{1,-1}+#&/@Table[{Cos[t],Sin[t]},{t,Subdivide[0,\[Pi]/2,n-1]}]
		]/5
	]
}


(* ::Subsection:: *)
(*Nucleo esponenziale*)


integraleCaramellaExp = ParallelSum[
	Integrate[
		(s1^2 - s2^2) Exp[t1 + t2 + s1 + s2] (s1 + s2)^2,
		{s1, s2} \[Element] X
	],
	{X, caramella[2]}
] // Together


triangolazioneCaramellone = Flatten[
	MeshPrimitives[TriangulateMesh[#], 2]& /@ caramella[10]
];


integraleCaramelloneExp = Exp[t1 + t2] ParallelSum[
	NIntegrate[
		(s1^2 - s2^2) Exp[s1 + s2] (s1 + s2)^2,
		{s1, s2} \[Element] X
	],
	{X, triangolazioneCaramellone}
]


(* ::Subsection:: *)
(*Nucleo esponenziale, soluzione non liscia*)


integraleCaramellaExpAbs = ParallelSum[
	Integrate[
		(s1^2 - s2^2) Exp[t1 + t2 + s1 + s2] Abs[s1 + s2],
		{s1, s2} \[Element] X
	],
	{X, caramella[2]}
] // Together


integraleCaramelloneExpAbs = ParallelSum[
	NIntegrate[
		(s1^2 - s2^2) Exp[s1 + s2] Abs[s1 + s2],
		{s1, s2} \[Element] X
	],
	{X, triangolazioneCaramellone}
]


(* ::Subsection:: *)
(*Nucleo sinusoidale*)


integraleCaramellaSin = ParallelSum[
	Integrate[
		Sin[t1 + t2] s1 s2 (s1 + s2)^2,
		{s1, s2} \[Element] X],
	{X, caramella[2]}
]


integraleCaramelloneSin = ParallelSum[
	Integrate[
		Sin[t1 + t2] s1 s2 (s1 + s2)^2,
		{s1, s2} \[Element] X],
	{X, caramella[10]}
] // Expand // Factor// Together


(* ::Subsection:: *)
(*Nucleo lineare, soluzione non liscia*)


(* ::Text:: *)
(*Si \[EGrave] optato per una soluzione approssimata.*)


campioniIntegraleCaramelloneLin = Flatten[
	ParallelTable[{t1,t2,
		Sum[NIntegrate[(s1+s2+t1+t2)(s1^2+s2^2)^(5/2),{s1,s2}\[Element]X],
		{X, caramella[10]}]},
		{t1, -.4, .4, .05}, {t2, -.2, .2, .05}
	],
1];


integraleCaramelloneLin = (a1 + a2 t1 + a3 t2) /. FindFit[
	campioniIntegraleCaramelloneLin,
	a1 + a2 t1 + a3 t2,
	{a1, a2, a3},
	{t1, t2}
]


(* ::Subsection:: *)
(*Nucleo non liscio*)


integraleCaramellaSqrt = ParallelSum[
	Integrate[
		(Sqrt[Abs[t1 t2]]+Sqrt[Abs[s1 s2]])Abs[s1+s2],
		{s1, s2} \[Element] X
	],
	{X, caramella[2]}
] // Together


(* ::Section:: *)
(*Icosagono con "buchi"*)


cPseudocerchio = Drop[Table[{Cos[t], Sin[t]},
	{t, Subdivide[0, 2\[Pi], 20]}],-1];
cPentagono = Drop[Table[{Cos[t + \[Pi] / 10], Sin[t + \[Pi] / 10]},
	{t, Subdivide[0, 2\[Pi], 5]}],-1];
cTriangolo = {
	{0,-Cos[\[Pi]/5]},
	{Cos[\[Pi]/10]+Cos[5\[Pi]/10],Sin[\[Pi]/10]+Sin[5\[Pi]/10]}/2,
	{-Cos[\[Pi]/10]-Cos[5\[Pi]/10],Sin[\[Pi]/10]+Sin[5\[Pi]/10]}/2
};


pseudocerchio = Polygon[cPseudocerchio];
pentagono = Polygon[cPentagono];
triangolo = Polygon[cTriangolo];


concerchio = Polygon[
	{cPseudocerchio->{cPentagono}, cTriangolo}
];


(* ::Subsection:: *)
(*Nucleo esponenziale*)


(* ::Text:: *)
(*La versione esatta \[EGrave] particolarmente lenta e porta a maggiori errori di arrotondamento sul moltiplicatore di Exp[t1 + t2].*)


iC = ParallelSum[
	Integrate[
		Exp[s1+s2](s1^2-s2^2)(s1+s2)^2,
		{s1,s2}\[Element]X
	],
	{
		X,
		Triangle[Join[{{1,0}},#]]& /@
			Partition[cPseudocerchio[[2;;]],2,1]
	}
];
iP = Integrate[Exp[s1+s2](s1^2-s2^2)(s1+s2)^2,{s1,s2}\[Element]pentagono];
iT = Integrate[Exp[s1+s2](s1^2-s2^2)(s1+s2)^2,{s1,s2}\[Element]triangolo];


integraleConcerchioExp = (iC - iP + iT) Exp[t1 + t2]


(* ::Text:: *)
(*Si preferisca, invece, la seguente soluzione approssimata.*)


integraleConcerchioExp = Factor[Expand[
	NIntegrate[#, {s1,s2} \[Element] pseudocerchio]
	-NIntegrate[#, {s1,s2} \[Element] pentagono]
	+NIntegrate[#, {s1,s2} \[Element] triangolo]
]]&[Exp[s1 + s2](s1^2 - s2^2)(s1 + s2)^2] Exp[t1 + t2]


(* ::Subsection:: *)
(*Nucleo sinusoidale*)


integraleConcerchioSin = Factor[Expand[
	Integrate[#, {s1,s2} \[Element] pseudocerchio]
	-Integrate[#, {s1,s2} \[Element] pentagono]
	+Integrate[#, {s1,s2} \[Element] triangolo]
]]&[Sin[t1 + t2] s1 s2 (s1 + s2)^2]


(* ::Subsection:: *)
(*Nucleo lineare, soluzione non liscia*)


campioniIntegraleConcerchioLin = Flatten[
	ParallelTable[{t1, t2,
		(NIntegrate[#, {s1,s2}\[Element]pseudocerchio]-
		NIntegrate[#, {s1,s2}\[Element]pentagono]+
		NIntegrate[#,{s1,s2}\[Element]triangolo])& @(
		(s1+s2+t1+t2)(s1^2+s2^2)^(5/2))},
	{t1,-1,1,.05},{t2,-1,1,.05}],
1];


integraleConcerchioLin = (a1 + a2 t1 + a3 t2) /. FindFit[
	campioniIntegraleConcerchioLin,
	a1 + a2 t1 + a3 t2,
	{a1, a2, a3},
	{t1, t2}
]
