(* ::Package:: *)

(* ::Title:: *)
(*Norme integrali*)


(* ::Text:: *)
(*Eventuali simboli mancanti possono essere calcolati nel file sugli integrali esatti.*)


(* ::Section:: *)
(*Esagono*)


grigliaEsagonoExp = Flatten[
	ParallelTable[{t1,t2,
		NIntegrate[Exp[s1 s2+t1 t2],{s1,s2}\[Element]esagono]},
		{t1,-.3,.2,.01},{t2,0.,.5,.01}
	],
1];
normaEsagonoExp = Last[Last[SortBy[
	Cases[
		grigliaEsagonoExp,
		{x_,y_,w_}/;RegionMember[esagono,{x,y}]
	],
Last]]]


grigliaEsagonoSin = Flatten[
	ParallelTable[{t1,t2,
		NIntegrate[Abs@Sin[s1+s2+t1+t2],{s1,s2}\[Element]esagono]},
		{t1,-.3,.2,.01},{t2,0.,.5,.01}
	],
1];
normaEsagonoSin = Last[Last[SortBy[
	Cases[
		grigliaEsagonoSin,
		{x_,y_,w_}/;RegionMember[esagono,{x,y}]
	],
Last]]]


(* ::Section:: *)
(*Caramelle*)


(* ::Subsection:: *)
(*Caramella*)


normaCaramellaSin = Max[
	Sum[Integrate[Abs[s1 s2],{s1,s2}\[Element]X],{X,caramella[2]}]
	(NMaxValue[Abs[Sin[t1+t2]],{t1,t2}\[Element]#]& /@ caramella[2])
]


normaCaramellaExp = Max[MaxValue[
	Assuming[
		{t1\[Element]Reals,t2\[Element]Reals},
		Sum[
			Integrate[
				Abs[(s1^2-s2^2) Exp[s1+s2+t1+t2]],
				{s1,s2}\[Element]X
			],
			{X,caramella[2]}
		]
	],
	{t1,t2} \[Element] #]& /@ caramella[2]
]


normaCaramellaSqrt = Max[MaxValue[ParallelSum[
	Integrate[
		Sqrt[Abs[t1 t2]]+Sqrt[Abs[s1 s2]],
		{s1, s2} \[Element] X
	],
	{X, caramella[2]}
] ,{t1, t2} \[Element] #]& /@ caramella[2]]


(* ::Subsection:: *)
(*Caramellone*)


normaCaramelloneSin = Max[ParallelSum[
	NIntegrate[Abs[s1 s2], {s1,s2} \[Element] Y], {Y,caramella[10]}]
	ParallelMap[
		NMaxValue[Abs[Sin[t1+t2]],{t1,t2} \[Element] #]&,
		triangolazioneCaramellone
	]
]


normaCaramelloneExp = Max[ParallelSum[
	NIntegrate[Abs[Exp[s1 + s2] (s1^2 - s2^2)], {s1,s2} \[Element] Y],
	{Y,caramella[10]}]
	ParallelMap[
		NMaxValue[Abs[Exp[t1 + t2]],{t1,t2} \[Element] #]&,
		triangolazioneCaramellone
	]
]


grigliaCaramelloneLin = Flatten[
	Table[{t1, t2, ParallelSum[
		NIntegrate[Abs[s1+s2+t1+t2],{s1,s2} \[Element] X],
		{X, triangolazioneCaramellone}]},
		{t1,-.4,.4,.1}, {t2,-.2,.2,.1}
	],
1];
normaCaramelloneLin = Last[Last[SortBy[
	Parallelize@Cases[
		grigliaCaramelloneLin,
		{x_,y_,w_}/;
		Or @@ (RegionMember[#,{x,y}]& /@ caramella[10])
	],
Last]]]


(* ::Section:: *)
(*Icosagono con "buchi"*)


intConcerchioSin = (NIntegrate[#,{s1,s2}\[Element]N@pseudocerchio]-
	NIntegrate[#,{s1,s2}\[Element]N@pentagono]+
	NIntegrate[#,{s1,s2}\[Element]N@triangolo]
)&[Abs[s1 s2]];
grigliaConcerchioSin = Flatten[
	ParallelTable[
		{t1, t2, Abs[Sin[t1+t2]] intConcerchioSin},
		{t1,-1,1,.01},{t2,-1,1,.01}
	],
1];
normaConcerchioSin = Last[Last[SortBy[
	Parallelize@Cases[
		grigliaConcerchioSin,
		{x_,y_,w_}/;
		RegionMember[concerchio, {x,y}]
	],
Last]]]


intConcerchioExp = (NIntegrate[#,{s1,s2}\[Element]N@pseudocerchio]-
	NIntegrate[#,{s1,s2}\[Element]N@pentagono]+
	NIntegrate[#,{s1,s2}\[Element]N@triangolo]
)&[Abs[(s1^2-s2^2)Exp[s1+s2]]];
grigliaConcerchioExp = Flatten[
	ParallelTable[
		{t1, t2, Abs[Exp[t1+t2]] intConcerchioExp},
		{t1,-1,1,.01},{t2,-1,1,.01}
	],
1];
normaConcerchioExp = Last[Last[SortBy[
	Parallelize@Cases[
		grigliaConcerchioExp,
		{x_,y_,w_}/;
		RegionMember[concerchio, {x,y}]
	],
Last]]]


grigliaConcerchioLin = Flatten[
	ParallelTable[
		{t1, t2,
		NIntegrate[Abs[s1+s2+t1+t2],{s1,s2}\[Element]pseudocerchio]-
		NIntegrate[Abs[s1+s2+t1+t2],{s1,s2}\[Element]pentagono]+
		NIntegrate[Abs[s1+s2+t1+t2],{s1,s2}\[Element]triangolo]},
		{t1,-1,1,.025},{t2,-1,1,.025}
	],
1];
normaConcerchioLin = Last[Last[SortBy[
	Cases[
		grigliaConcerchioLin,
		{x_,y_,w_}/;RegionMember[concerchio,{x,y}]
	],
Last]]]
