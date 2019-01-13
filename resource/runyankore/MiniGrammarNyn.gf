--# -path=.:../abstract
concrete MiniGrammarNyn of MiniGrammar = open MiniResNyn, Prelude in 
{
	lincat

	--Common
	  Utt = {s : Str} ;
      Pol = {s : Str ; isTrue: Bool}; -- TRUE= Positive, FALSE=Negative, s filed is left empty for parsing
      {-
			Temp is a parameter for temporal features such as Simul and Anteriority:
      		TRUE =  Simultainity
      		FALSE = Anteriority
      -}
      Temp = {s : Str ; isPres : Bool} ;
     --cat
      Imp = {s : Bool => Str} ;
      S  = {s : Str} ;
      QS = {s : Str} ;
      {-
	      -- Clause is a combination of a Subject, Verb and Object(s)
	      -- i.e. Subj, verb with polarity and temp features and verb complement
	      -- which is the Objects, NPs PPs APs etc.
	 -}
      Cl, QCl = {   -- word order is fixed in S and QS
      subj : Str ;
      subjAgr : Agreement;
      root : Str;
      morphs : VFormMini => VerbMorphPos =>Str;
      {-
      inf  : Str;
      pres  : Str; 
      past  : Str; 
      presPart  : Str; 
      pastPart  : Str;                              -- subject
      --root : Str ; -- dep. on Pol,Temp, e.g. "does","sleep"
      -}
      compl : Str                              -- after verb: complement, adverbs
      } ;
      
      Imp = {s : Bool => Str} ;
      VP =  VerbPhrase; --{s: VerbFormMini=>Str; comp:Str ; agr : AgrExist}; -- verb phrase e.g. "lives here"
      AP = Adjective ;     -- adjectival phrase         	e.g. "very warm"
      CN = Noun ;     -- common noun (without determiner)   e.g. "red house"
      NP = NounPhrase;     -- noun phrase (subject or object)     e.g. "the red house"
	  Pron = Pronoun;   -- personal pronoun                    e.g. "she"
      Det = Determiner;
      Conj = {s : AgrConj =>Str ;s2 : Str ; n : Number} ; -- conjunction e.g. "and"
      Prep = {s : Str ; other: Str} ;
      V  = Verb;      			-- one-place verb             e.g. "sleep" 
      V2 = Verb2;     			-- two-place verb             e.g. "love"
      A = Adjective;      	-- one-place adjective        e.g. "warm"
      N = Noun;      		-- common noun                e.g. "house"
      S  = {s : Str};
      CN = Noun ;
      PN = ProperNoun;     				-- proper name                e.g. "Paris"
      Adv =AdverbP;   -- adverbial phrase           e.g. "in the house"
      


   -- lin part of the grammar
   lin
	-- Noun
	    DetCN  det cn =  mkDetCN det cn;       -- the man
		UsePN pn = {s = \\ _ =>  pn.s; agr = pn.a}; -- John
		UsePron pron = { s = pron.s; agr = pron.agr};  --: Pron -> NP ;            -- he
		MassNP cn = {s = \\_ =>cn.s ! Complete ! Pl; agr = AgP3 Pl cn.gender};   --: CN -> NP ;              -- milk
	{-
		In the following determiners, I am ignoring the role
		of the initial vowel in conveying supposed meaning
		of the definitie and indefinite vowels

		Note: This is why we choose the complete form of the noun for use
	-}
		a_Det = {s =[] ; ntype = Complete; num = Sg; pos = PreDeterminer};     --: Det ; indefinite singular ---s
		aPl_Det = {s =[]; ntype = Complete; num = Pl; pos = PreDeterminer}; -- : Det ;indefinite plural   ---s
		the_Det = {s =[]; ntype = Complete; num = Sg; pos = PreDeterminer};  --: Det ;                   -- definite singular   ---s
		thePl_Det = {s =[]; ntype = Complete; num = Pl; pos = PreDeterminer}; --: Det ;definite plural     ---s
		UseN noun = noun;     --: N -> CN ;               -- house
		--Noun : Type = {s : NounState => Number => Str ; gender : Gender} ;
		
		AdjCN ap cn = 
			case <ap.isPre, ap.isProper, > of {
					<True, True> => { 
										s = \\ ns, num =>ap.s ++ cn.s ! ns ! num ; 
										gender = cn.gender 
									};
					<False, False> => case ap.isPrep of {
										 False 	=>	{ 
										 				s = \\ ns, num => cn.s ! ns ! num ++ mkAdjPronIVClitic (AgP3 num cn.gender) 
									   						 ++ ap.post ; 
									   					gender = cn.gender	 
													};
										 True  =>  { 
														s = \\ ns, num => (cn.s ! ns ! num) ++ 
																mkGenPrepNoIVClitic (AgP3 num cn.gender) ++ ap.post ; 
														gender = cn.gender 
													}
									};
					<True, False> => { 
										s = \\ ns, num => mkAdjPronIVClitic (AgP3 num cn.gender) 
															 ++ ap.s ++ (cn.s ! ns ! num) ; 
										gender = cn.gender 
									};
					<False, True> => { 
									   s = \\ ns, num => (cn.s ! ns ! num) ++ ap.post ; 
									   gender = cn.gender
									}										

		};        -- big house

	-- Phrase
	UttS      s  = s; --: S  -> Utt ;
	UttQS 	  qs = qs ; --: QS -> Utt ;-- does John walk
	UttNP     np = {s= np.s!Acc}; --: NP -> Utt ;

	UttAdv   adv = {s = adv.s}; --: Adv -> Utt ;        -- in the house

	UttImpSg  pol imp = {s = 
		case pol.isTrue of {
				True  => imp.s!True;
				False => (mkSubjClitic (AgMUBAP2 Sg)) ++ imp.s!False					
			}
		};--: Pol -> Imp -> Utt ; -- (do not) walk ----s

-- Sentence
	{-
		TO DO: Investigate on how to make proper questions for the few cases
		where there is a question word such as noija? Are you coming?
		However, most questions are achieved by change in intonation or pronounciation
	-}
    UseQCl   = UseCl; -- : Temp -> Pol -> Cl   -> S ;  -- John has not walked
    UseCl  temp pol cl = let 
    								subj = cl.subj;
    								clitic = mkSubjClitic cl.subjAgr;
    								simul = cl.morphs ! Pres; --this is not delivering the string
    								ant = cl.morphs ! PastPart; --this is not delivering the string
    								root = cl.root;
    								presRestOfVerb = cl.morphs ! Pres ! RestOfVerb;
    								pastRestOfVerb = cl.morphs ! PastPart ! RestOfVerb;

    								compl = cl.compl
    							  in 
    	case <temp.isPres, pol.isTrue> of {
					<True, True> => {s = subj ++ clitic ++ --Predef.BIND ++ 
									root ++ Predef.BIND ++ presRestOfVerb ++ compl};
					{-Note: when I use pol.s instead of ti, the word alignment instead becomes worse-}
					<True, False> => {s = subj ++ "ti" ++ Predef.BIND ++ clitic ++ --Predef.BIND ++ 
										root ++ presRestOfVerb ++ compl};
					<False, True> => {s = subj ++ clitic ++ --Predef.BIND ++ 
										ant!TAMarker ++ root ++ Predef.BIND ++ pastRestOfVerb ++ compl};				
					<False, False> =>{s = subj ++ "ti" ++ Predef.BIND ++ clitic ++ --Predef.BIND ++ 
											ant!TAMarker ++ root ++ Predef.BIND ++ pastRestOfVerb ++ compl}
    		};  --: Temp -> Pol -> QCl  -> QS ; -- has John walked
    QuestCl cl = cl;  --: Cl -> QCl ; -- does John (not) walk
    PredVP np vp = case vp.isCompApStem of{
						False    => {
								    	subj = np.s ! Nom;   --: NP -> VP -> Cl ;            -- John walks / John does not walk
								    	subjAgr = np.agr;
								    	root = vp.s;
								    	morphs = vp.morphs;
								    	{-
								    	inf  = mkVerbInrf vp.root;
										pres  = mkVerbPres vp.root; 
										past  = mkVerbPast vp.root; 
										presPart  = mkVerbPresPart vp.root; 
										pastPart  = mkVerbPastPart vp.root;                              -- subject
										-}
										--root = vp.root ;
								    	compl = vp.comp
								    	};
						True    =>  {
								    	subj = np.s ! Nom;   --: NP -> VP -> Cl ;            -- John walks / John does not walk
								    	subjAgr = np.agr;
								    	root = vp.s;
								    	morphs = vp.morphs;
								    	{-
								    	inf  = mkVerbInrf vp.root;
										pres  = mkVerbPres vp.root; 
										past  = mkVerbPast vp.root; 
										presPart  = mkVerbPresPart vp.root; 
										pastPart  = mkVerbPastPart vp.root;                              -- subject
										-}
										--root = vp.root ;
								    	compl = mkSubjClitic np.agr ++ Predef.BIND ++ vp.comp --mkSubjClitic np.agr ++ Predef.BIND ++ vp.comp
								    }
    	};--: NP -> VP -> Cl ; -- John walks / John does not walk
   	{-
		Note: It seems mkSubjClitic comes with a Predef.BIND already
		prepared for the next token to bind.
		Reason: When I add a BIND command, I get two bind tokens in the linearizations
   	-}
    ImpVP  vp = {
		    	s =table{
		    		True=> vp.s ++ Predef.BIND ++ vp.morphs!Inf!RestOfVerb ++ vp.comp;
		    		False =>  case vp.isCompApStem of {   -- How do I make the number dynamic use case?
							    	True =>vp.morphs!Pres!SecNegM ++ Predef.BIND ++ vp.s ++ Predef.BIND ++ 
							    				vp.morphs!Inf!RestOfVerb ++ (mkAdjPronNoIVClitic (AgMUBAP2 Sg)) ++ vp.comp;
							    	False  => vp.morphs!Pres!SecNegM ++ Predef.BIND ++ vp.s ++ Predef.BIND ++ 
							    				vp.morphs!Inf!RestOfVerb ++ vp.comp
		    				}
		    	} 
    };  --: VP -> Imp ;                 -- walk / do not walk
    
-- Verb
    UseV    v = {s = v.s ; morphs = v.morphs; comp =[]; isCompApStem = False; agr = AgrNo};  --: V   -> VP; -- sleep --ignoring object agreement
  	{-
		The V2 sometimes uses preopsitions for formation
		of direct object. Unlike in English where the verb 
		and the preposition are disjunctive such as "send to",
		In runyakore and rukiga, the verb and preposition are
		conjunctive such as sindik-ira.

		Because of the fusion, I have deffered including this in 
		the compPrep. Actually, it is going to be empty in the next version
  	-}
    ComplV2  v2 np = { 
    	s =v2.s;
    	morphs = v2.morphs; 
    	comp = v2.comp ++  np.s ! Acc;
    	isCompApStem = False; 
    	agr = AgrYes np.agr
    };
    
    {-
    	TO DO: 
    	How do we solve anaphoric resolution for incomlete Adjectives i.e.
    	Adjectival stems:
    	I am small == Ndi mu-kye
    	Solution: The adjective picks the stem from the subject instead of the
    	noun as what we did in AdjCN
    -}
    
    UseAP  ap =
    	let 
    		root 	= be_GVerb.s ! False;
    		be_morphs 	= be_GVerb.morphs;
    	in
    		case <ap.isPre, ap.isProper> of {
    				 <True, True> => {s = root ; morphs = be_morphs; comp = ap.s; isCompApStem = False; agr = AgrNo };    -- : AP  -> VP ;-- be small ---s
    				 <False, True> => {s= root ; morphs = be_morphs; comp = ap.post; isCompApStem = False; agr = AgrNo };
    				 <True, False> => {s= root ; morphs = be_morphs; comp = ap.s; isCompApStem = True; agr = AgrNo };
    				 <False, False> =>{s= root ; morphs = be_morphs; comp = ap.post; isCompApStem = True; agr = AgrNo }
    			};
    UseNP   np ={s = be_GVerb.s ! False; morphs = be_GVerb.morphs; comp = np.s!Nom; isCompApStem = False; agr = AgrYes np.agr}; --: NP  -> VP ; -- be a man ---s
    UseAdv  adv = {s = be_GVerb.s ! False; morphs = be_GVerb.morphs; comp = adv.s; isCompApStem = False; agr = AgrNo};  --: Adv -> VP ; -- be in the house ---s

    AdvVP  vp adv = {s = vp.s; morphs = vp.morphs; comp = vp.comp ++ adv.s; isCompApStem = False; agr = AgrNo};  --: VP -> Adv -> VP ;       -- sleep here

-- Adjective: Positive, Comparative, Superative
    PositA    adj = adj;  --: A  -> AP ; -- warm i.e positive

-- Adverb
    PrepNP    prep np = {s = prep.s ++ np.s ! Acc; agr = AgrYes np.agr}; --: Prep -> NP -> Adv ;     -- in the house

-- Conjunction
   --CoordS    s1 s2 ={}; --: Conj -> S -> S -> S ;   -- he walks and she runs ---s
   CoordS conj a b = {s = a.s ++ conj.s!(AConj Other) ++ b.s} ;
-- Tense
    PPos = {s = [] ; s2 = [] ; isTrue = True};  --: Pol ; -- nimbyama [positive polarity]
    PNeg = {s = "ti"  ; s2 = "ta" ; isTrue = False}; --: Pol  -- tinkubyama [negative polarity]
	TSim = {s = []    ; isPres = True} ;     --: Temp ;                  -- simultanous: she sleeps ---s
    TAnt = {s = []    ; isPres = False} ;     --: Temp ;                  -- anterior: she has slept ---s
-- Determiners
every_Det = mkDet "buri" Incomplete Sg PreDeterminer ;
{-
	Functions not in abstract syntanx
-}
--few_Det = mkDet "buri" Complete Pl PostDeterminer ;
--many_Det = mkDet "ingi" Complete Pl PostDeterminer ;



  		---Structural
  		{-
  			--there are several and i.e. 
		    -- na (two nouns, 2 Noun Phrases, 2 Pronouns, 2 relative subject clauses, )
		    --kandi (clauses having a commonality of subjects, object or tense)
		    --the best structure is a table 
		    --mkConjunction "na" "kandi" and_Conj ;
		 -}
		 and_Conj  = {
		 	s = table {
		 			AConj Other => "kandi";
		 			_ => "na"
		 		};
		 		
		 	s2 =[];
		 	n  = Pl	
		 	}; 

		 {-

		 	TODO: Look at the grammar books by Mpairwe & Kahangi Pg 155
		 	and investigate or to find out its arguments but for now
		 	I will assume nari works on all types of 
		 	ConjArg (Conjunction Arguments)

		 	nari is the general or
			
			These are candidates for Extra module if they are not specific
			to the type of argument.
		 	nînga for Runynakore and 

		 	nainga for rukiga
		 -}

		 
    	 or_Conj = {
		 	s = \\ _ => "nari";	
		 	s2 =[];
		 	n  = Sg	
		 	};

		 -- several words depending on use omuri??
		 in_Prep        = mkPrep "omu" "omuri";

		 --aha-ri Kamukuzi??? works for places 
    	 on_Prep        = mkPrep "aha" "ahari"; 
    	 
    	 --na --please this string varies with vowels use combine_morphemes or 
    	 --combine_words when using it.
    	 with_Prep      = mkPrep "na" []; 

    	 i_Pron         = mkPron "nyowe" "nyowe" (AgMUBAP1 Sg);
	     youSg_Pron     = mkPron "iwe" "we" (AgMUBAP2 Sg); 
	     he_Pron, she_Pron = mkPron "uwe" "uwe" (AgP3 Sg MU_BA);
	     we_Pron        = mkPron "itwe" "itwe" (AgMUBAP1 Pl);
	     youPl_Pron     = mkPron "imwe" "imwe" (AgMUBAP2 Pl);
	     they_Pron      = mkPron "bo" "bo" (AgP3 Pl MU_BA);

	     have_V2 ={s= "ine"; morphs = mkVerbMorphs; comp = []};  --: V2 ;
	     

}