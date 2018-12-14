concrete MiniGrammarNyn of MiniGrammar = open MiniResNyn, Prelude in 
{
	lincat
	  Utt = {s : Str} ;
      Pol  = {s : Str ; isTrue : Bool} ; -- the s field is empty, but needed for parsing
      Temp = {s : Str ; isPres : Bool} ;
      S  = {s : Str} ;
      Prep = {s : Str ; prepForm: PrepForm} ;
      Pol  = {s : Str ; s2: Str ; isTrue}; -- TRUE= Positive, FALSE=Negative

      -- Temp is a parameter for temporal features such as Simul and Anteriority:
      -- 	TRUE =  Simultainity
      --    FALSE = Anteriority
      Temp = {s : Str : isPres : Bool};
      S  = {s : Str} ;
      QS = {s : Str} ;

      -- Clause is a combination of a Subject, Verb and Object(s)
      -- i.e. Subj, verb with polarity and temp features and verb complement
      -- which is the Objects, NPs PPs APs etc.
      Cl, QCl = {   -- word order is fixed in S and QS
      subj : Str ;                             -- subject
      verb : Bool => Bool => {fin,inf : Str} ; -- dep. on Pol,Temp, e.g. "does","sleep"
      compl : Str                              -- after verb: complement, adverbs
      } ;

      Imp = {s : Bool => Str} ;

      Conj = {s : AgrConj =>Str ;s2 : Str ; n : Number} ;
      


      -- lin part of the grammar
      lin
      	-- Noun

      	{-

			-- Phrase
    UttS      s =s; --: S  -> Utt ;
    UttNP     np = {s=np.s}; --: NP -> Utt ;

-- Sentence
    UsePresCl pol cl = {s= pol.s ++ cl.s!pol.b}; --: Pol -> Cl  -> S ;        -- John does not walk ---s
    PredVP np vp = {s= table{b => np.s ++  vp.verb.s!b !(VPres np.a) ++ vp.comp}}; --: NP -> VP -> Cl ;        -- John walks / John does not walk
-- Verb
    UseV    v = {verb= v ; comp =[]; objA = AgrNo};  --: V   -> VP; -- sleep
    ComplV2  v2 np = {
       verb ={
           s= v2.s; 
           isAux = v2.isAux
           }; 
       comp =v2.c ++ np.s ;
       objA   =  AgrYes np.a --: V2  -> NP -> VP ;       -- love it  ---s
    };
    
    UseAP  ap = {verb = copRiNi; comp = ap.s; objA = AgrNo };    -- : AP  -> VP ;             -- be small ---s

    AdvVP  vp adv = {verb = vp.verb; comp = vp.comp ++ adv.s; objA = AgrNo};  --: VP -> Adv -> VP ;       -- sleep here

-- Noun

    DetCN det cn ={s = 
        case det.isPrefix of{
            True  => det.s!cn.nc ++ cn.s!det.n;
            False => cn.s!det.n ++ det.s!cn.nc
        };
        a = Agr cn.nc det.n Per3
    };
        


    UsePN  propn   = {s = propn.s; a = propn.a}; --: PN -> NP ;              -- John

    UsePron  pron = {s = pron.s; a = pron.a}; --: Pron -> NP ;            -- he

    MassNP   cn   = {s = cn.s!Pl; a = Agr cn.nc Pl Per3};                   --CN -> NP ;    --This is problematic          -- milk

    CoordS conj a b = {s = a.s ++ conj.s ++ b.s} ;
    a_Det     = prefixDeterminer "" Sg ;                   -- indefinite singular ---s
    aPl_Det   = prefixDeterminer "" Pl ;                   -- indefinite plural   ---s
    thePl_Det = prefixDeterminer "" Pl ; --= postfixDeterminer "ngi" Pl ;
    the_Det   = prefixDeterminer "" Sg ;                   -- definite plural     ---s
    UseN n    = n ;  
    AdjCN     ap cn = {s = case ap.isPrefix of {
                                True => table {
                                    Sg => objMarkerInAnim !False! cn.nc!Sg ++ ap.s ++ cn.s!Sg;
                                    Pl => objMarkerInAnim !False! cn.nc!Pl ++ ap.s ++ cn.s!Pl};
                                        
                                False =>table {
                                    Sg => cn.s!Sg ++ objMarkerInAnim !False!cn.nc!Sg ++ ap.s;
                                    Pl => cn.s!Pl ++ objMarkerInAnim !False!cn.nc!Pl ++ ap.s}
                                         --(Agr cn.nc Pl Per3)}
                                    };
                                nc =  cn.nc };    --: AP -> CN  -> CN ;       -- big house


-- Adjective: Positive, Comparative, Superative
    PositA    adj = adj;  --: A  -> AP ;              -- warm

-- Adverb
    PrepNP    prep np = {s = prep.s ++ np.s}; --: Prep -> NP -> Adv ;     -- in the house

-- Conjunction
    --CoordS    s1 s2 ={}; --: Conj -> S -> S -> S ;   -- he walks and she runs ---s
 
-- Tense
    PPos      = {s = [] ; b = True}; --: Pol ;                   -- I sleep  [positive polarity]
    PNeg      = {s = [] ; b = False}; --: Pol                     -- I do not sleep [negative polarity]

    

      	-}
    	
    	-- Determiners
    	every_Det = mkDet "buri" Incomplete Sg PreDeterminer ;
  		few_Det = mkDet "buri" Complete Pl PostDeterminer ;
  		many_Det = mkDet "ingi" Complete Pl PostDeterminer ;



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
			to the type pf argument.
		 	nînga for Runynakore and 

		 	nainga for rukiga
		 -}
    	 or_Conj = {
		 	s = \\ _ => nari	
		 	s2 =[];
		 	n  = Sg	
		 	};

		 -- several words depending on use omuri??
		 in_Prep        = mkPrep "omu" "omuri";

		 --aha-ri Kamukuzi??? works for places 
    	 on_Prep        = mkPrep "aha" "ahari"; 
    	 
    	 --na --please this string varies with vowels use combine_morphemes or 
    	 --combine_words when using it.
    	 with_Prep      = mkPrep "na" ""; 

    	 i_Pron         = mkPron "nyowe" "nyowe" AgMUBAP1 Sg;
	     youSg_Pron     = mkPron "iwe" "we" AgMUBAP2 Sg; 
	     he_Pron, she_Pron = mkPron "uwe" "uwe" AgP3 Sg MU_BA;
	     we_Pron        = mkPron "itwe" "itwe" AgMUBAP1 Pl;
	     youPl_Pron     = mkPron "imwe" "imwe" AgMUBAP2 Pl;
	     they_Pron      = mkPron "bo" "bo" AgP3 Pl MU_BA;

}