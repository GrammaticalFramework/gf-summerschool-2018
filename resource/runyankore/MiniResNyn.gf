resource MiniResNyn = --ParamX ** 
	open Prelude in 
  {

param 
	Number = Sg | Pl;
	Person = Per1 | Per2 | Per3;
  --Use Noun Class (NClass) as suggested in J&M chapter 3 pg 86.
	Gender =  MU_BA | KI_BI | N_N | KU_MA  |  BU_MA | 
					  RU_BU | GU_GA | ZERO_ZERO  |  MU_MI |  RI_MA | 
					  I_MA  | KA_BU | KA_TU | RU_N |  RU_MA |  HA | 
					  MU |  KU  |  ZERO_BU  |  ZERO_BI | ZERO_MA |  
					  ZERO_MI |  ZERO_TU |  ZERO_N  | I_ZERO  |  
					  RI_ZERO |  KU_ZERO | MU_ZERO |  RU_ZERO |  
					  KA_ZERO |ZERO_BAA | N_ZERO;
	Case = Acc | Nom ;

  PersonalPronounType = SubjM | Obj  | RelSubj | RelObj |
                          AdjPron2 | -- aAdjectival Prefixes with initial vowel with the semantics of "the" e.g. -- omuntu o-mu-rungi 
                          AdjPron  | -- without initial vowel i.e. -- omuntu mu-rungi           
                          --GenPron  | -- different types of pronouns
                          GenPrep1 |
                          GenPrep2 |
                          GenAdj   |
                          SStandPron ; --Self-standing pronouns
  {-
    --there are several and i.e. 
    -- na (two nouns, 2 Noun Phrases, 2 Pronouns, 2 relative subject clauses, )
    --kandi (clauses having a commonality of subjects, object or tense)
    --the best structure is a table
  -}
  ConjArg = Nn_Nn | Nps_Nps | Pns_Pns | RelSubjCls | Other;
  AgrConj = AConj ConjArg;
	Agreement =  AgP3 Number Gender | AgMUBAP1 Number |AgMUBAP2 Number ;
  AgrExist = AgrNo | AgrYes Agreement;
  Position = PostDeterminer | PreDeterminer ;
  
	--Functional forms of the regular verb
	Mood = Infinitive | Imperative | Subjunctive | Perfective;
	VerbCat = Simple | Prepositional | Causative;
	Voice = Active | Passive;
  -- all forms of normal Eng verbs, although not yet used in MiniGrammar
  VForm = VF Voice Mood VerbCat ;
  -- may not need it
  NounCat = ComNoun | PropNoun; --prepositions agree with nouns to form adverbial Phrases
  PrepForm = Form1 | Form2; -- omu and omuri, aha, ahari
{-
	Complete = Nouns with IV, 
	Incomplete = Nouns without IV: important for use with pre-determiners
	like buri i.e every

-}
NounState  = Complete | Incomplete ; 

oper
  -- the is for Common Nouns only 
  Noun : Type = {s : NounState => Number => Str ; gender : Gender} ;

  ivs : pattern Str = #("a" | "e" | "o"); --pattern for initial vowels
  
  human_relations: pattern Str = --expand this list
  	#("Taata" | "Maama" | "Shwento" | "Shwenkuru" | "Nyinento" | "Nyinenkuru");

  mkNoun : Str -> Str ->Gender ->Noun = \sg,pl, g -> {
     s = table {
       Complete   => table { Sg => sg ; Pl => pl};
       Incomplete => table { 
       	Sg => case sg of {
         	  	(#ivs + _) => Predef.drop 1 sg;
         	   			_ => sg };
       	Pl => case pl of {
       			(#ivs + _) => Predef.drop 1 pl;
         		 		_  => pl }
   				}
        };
      gender = g
    } ;
  
  {-  Smart paradigm
      This operation needs through testing with all verbs from a file
  -} 
  smartNoun : Str -> Gender -> Noun 
    = \omuntu, g ->
      case <omuntu , g> of {
        -- Handling the Tone System is also another problem.
        
        < "o" + "mu" + stem, MU_BA > => mkNoun omuntu ("aba" + stem) g ;
        --special cases like omwana, omwishiki, omwojo
        
        < "o" + "mw" +  stem, MU_BA > => mkNoun omuntu (combine_morphemes "aba" stem) g ; --same as mu_ba but the "u" + "a" of the stem to form mwa  
        < "o" + "mu" +  stem, MU_MI > => mkNoun omuntu (combine_morphemes "emi" stem) g ;
        < "o" + "ru" +  stem, RU_MA > => mkNoun omuntu (combine_morphemes "ama" stem) g  ;
        < "o" + "ru" +  stem, RU_N >  => mkNoun omuntu (combine_morphemes "en" stem) g  ; --desist from providing a singlar only but give both
        < "o" + "bu" +  stem, BU_MA >  => mkNoun omuntu (combine_morphemes "ama" stem) g ;
        < "o" + "gu" +  stem, GU_GA >  => mkNoun omuntu (combine_morphemes "aga" stem) g  ;
        < "o" + ("ku" | "kw") +  stem, KU_MA >  => mkNoun omuntu (combine_morphemes "ama" stem) g  ;
        < "o" +  "kw" +  stem, KU_ZERO >  => mkNoun omuntu (combine_morphemes "" "") g  ;
        < "o" + "ku" +  stem, KU_ZERO >  => mkNoun omuntu (combine_morphemes "" "") g  ; -- does not exist as plural
        < "o" + "mu" +  stem, MU_ZERO >  => mkNoun omuntu (combine_morphemes "" "") g  ; -- does not exist as plural
        < "o" + "mu" +  stem, MU_ZERO >  => mkNoun omuntu (combine_morphemes "" "") g  ; -- does not exist as plural
        < "o" + "ru" +  stem, RU_BU >  => mkNoun omuntu (combine_morphemes "obu" stem) g  ;
        < "o" + "ru" +  stem, RU_ZERO >  => mkNoun omuntu (combine_morphemes "" "") g  ; -- does not exist as plural

        < "a" + "ha" + stem, HA_ZERO > => mkNoun omuntu (combine_morphemes "" "") g  ; -- does not exist as plural
        < "a" + "ka" + stem, KA_BU > => mkNoun omuntu (combine_morphemes "obu" stem) g  ;
        < "a" + "ka" + stem, KA_ZERO > => mkNoun omuntu (combine_morphemes "" "") g ; -- does not exist as plural

        < "e" + "ki" + stem, KI_BI > => mkNoun omuntu (combine_morphemes "ebi" stem) g  ;
        < "e" + "ki" + stem, KI_ZERO > => mkNoun omuntu (combine_morphemes "" "") g  ; -- does not exist as plural
        < "e" + "i" + stem, I_MA > => mkNoun omuntu (combine_morphemes "ama" "") g  ;
        < "e" + "i" + stem, I_ZERO > => mkNoun omuntu (combine_morphemes "" "") g  ; -- does not exist as plural
        < "e" + "ri" + stem, RI_MA > => mkNoun omuntu (combine_morphemes "ama" stem) g  ;
        < "e" + "ri" + stem, RI_ZERO > => mkNoun omuntu (combine_morphemes "" "") g ; -- does not exist as plural
        < "e" + "ry" + stem, I_MA | RI_MA> => mkNoun omuntu (combine_morphemes "ama" stem) g  ;
        -- --special cases shall be added with due course as errors are identified
        <"e" + "ky" + stem, KI_BI> => mkNoun omuntu (combine_morphemes "ebi" stem) g  ; 
        < _ ,N_N | ZERO_MA | ZERO_ZERO > => mkNoun omuntu  omuntu g  ;
        --< _ ,ZERO_MA > => mkNoun omuntu  ("ama" + stem) g (Predef.drop 1 omuntu);
        --< _ ,> => mkNoun omuntu  omuntu g (Predef.drop 1 omuntu);
        <_ , ZERO_BAA>  => mkNoun omuntu ("baa" + omuntu) g ;
        < _ ,_ > => mkNoun omuntu  omuntu g-- improve as we go on.
    };



{-
      This function tries to handle phonological-conditioning.

      Usage: Use it whenever you are trying to combine morphemes especially in:
      1. Pronouns
      2. Verbs and verb Phrases.
      3. Noun Phrases
      3. Adjectival Phrases e.t.c

      Given two morphemes A and B to combine,
      1. compare the last letter of the first morpheme A with the first letter of the second morpheme B
      2. Use parttern matching to obtain the right letters for the comnined word
      
      Source of rules:
      1. personal experience
      2. Morris and Kirwan Runynakore Grammar 
      3. but we shall add more as we meet them during debugging
  -}
  combine_morphemes : Str -> Str -> Str ;
  combine_morphemes = \ f, s ->
    case <(Predef.dp 1 f), (Predef.take 1 s)> of {
         <"n" , "r"> => f + "d" + (Predef.drop 1 s) ;
         <"u" , "a" | "e" | "o" | "i"> => Predef.tk 1 f + "w" + s ;
         <"i" , "a" | "e" | "o"> => Predef.tk 1 f + "y" + s ;
         <"n" , "b" | "p"> => Predef.tk 1 f + "m" + s ;
         <"n" , "m"> => Predef.tk 1 f + s ; -- However, note that for pronouns, the n changes to m
         <"n" , "h"> => Predef.tk 1 f + "mp" + Predef.drop 1 s ;
         <"i", "i">  => f + Predef.drop 1 s ;
         <_ , _ > => f + s
    } ;
  

    {-
      combine_morphemes = \ f, s ->
      case <(f), (s)> of {
           <a+ "n" , "r"+ g> =>  f ++  "d" ++   g;
           <a+ "nd" , "i"+ g> =>  a ++ Predef.BIND ++  "nz" ++ Predef.BIND ++  s;
           <a+ "u" , ("a" | "e" | "o" | "i") + g> =>  a ++ Predef.BIND ++  "w" ++ Predef.BIND ++ s ;
           <a+ "i" , ("a" | "e" | "o") +g > =>  a ++ Predef.BIND ++  "y" ++ Predef.BIND ++  s ;
           <a+ "n" , ("b" | "p") + g> =>  a ++ Predef.BIND ++  "m" ++ Predef.BIND ++ s ;
           <a+ "n" , "m" + g> =>  a ++ Predef.BIND ++  s ; -- However, note that for pronouns, the n changes to m
           <a+ "n" , "h" +g > =>  a ++ Predef.BIND ++  "mp" ++ Predef.BIND ++  g ;
           <a+ "i", "i" + g>  =>  f ++ Predef.BIND ++  g ;
           <_ , _ > => f ++   s
      } ;
    -}

    --separate
    {-
      dealing with ProperNouns 
      They do not have plurals but when a proper noun
       refers to a place then it is important to keep
       that label because is helps us diambiguate which
       preposition to use for in, on and at i.e LOCATIVES omuri, ahari, etc
    -}
    ProperNoun : Type = {s: Str ; a:Agreement ; isPlace : Bool};
    mkPN : Str -> Agreement -> Bool -> ProperNoun = \ pn, a, b->
    {
      s = pn ;
      a = a;
      isPlace = b;
    } ;

    -- concatenates the string left to right
    mkClitic : Str -> Str = \c -> c ++ Predef.BIND ;

    -- concatenates the string right to left
    mkSuffix : Str -> Str = \c -> Predef.BIND ++ c ;

    -- creating clitics depending on number
    mkClitics : Str -> Str -> Number -> Str = \sg,pl,n ->
      case n of {
          Sg => mkClitic sg ; 
          Pl => mkClitic pl
        } ;

    mkSubjClitic : Agreement -> Str = \a ->
      case a of {
          AgMUBAP1 n => mkClitics "n" "tu" n;
          --AgMUBAP1 Pl => "tu" ;
          AgMUBAP2 n => mkClitics "o" "mu" n;
          --AgMUBAP2 Pl => "mu" ;
          AgP3 n MU_BA  => mkClitics "a" "ba" n;
          --AgP3 Pl MU_BA  => "ba" ;          
          AgP3 Sg KI_BI   => mkClitic "ki" ;
          AgP3 Pl (KI_BI | ZERO_BI)   => mkClitic "bi" ;
          AgP3 Sg (RU_N | RU_MA | RU_ZERO | RU_BU)   => mkClitic "ru" ; 
          AgP3 Pl RU_N => mkClitic "zi"; --| "i"; 
          AgP3 Sg N_N => mkClitic "e";
          AgP3 Pl N_N => mkClitic "zi"; --| "i";
          AgP3 Sg (MU_MI | MU_ZERO)   => mkClitic "gu" ; 
          AgP3 Pl MU_MI   => "e" ;
          AgP3 Sg (RI_MA | RI_ZERO | I_ZERO) =>mkClitic  "ri"; 
          AgP3 Pl (RI_MA | BU_MA | KU_MA | ZERO_MA | I_MA |RU_MA)  => mkClitic "ga" ;
          AgP3 Sg (KA_BU | KA_ZERO | KA_TU)   => mkClitic "ka" ; 
          AgP3 Pl (KA_BU | RU_BU)  => mkClitic "bu" ;
          AgP3 Sg ZERO_BU  => mkClitic "bu" ; 
          AgP3 Pl ZERO_BU  => mkClitic "bu" ;
          AgP3 Sg ZERO_BI  => mkClitic "bi" ; 
          AgP3 Sg ZERO_MA  => mkClitic "ga" ;
          AgP3 Pl RI_ZERO  => mkClitic "ga" ;
          AgP3 Sg KU_ZERO  => mkClitic "ku" ;
          AgP3 Pl KU_ZERO  => mkClitic "ku" ;
          AgP3 Pl MU_ZERO  => mkClitic "gu" ;
          AgP3 Pl RU_ZERO  => mkClitic "ru" ;
          AgP3 Sg ZERO_TU  => mkClitic "tu" ;
          AgP3 Pl ZERO_TU  => mkClitic "tu" ;
          AgP3 Sg (ZERO_MI | ZERO_ZERO)  => mkClitic "" ;
          AgP3 Pl ZERO_MI  => mkClitic "e" ;
          AgP3 Pl KA_ZERO  => mkClitic "" ;
          _        => mkClitic "SubjNotKnown" --for checking if there is some class unaccounted for
      };

       {-Object particle may be used as 
          1. a prefix: e.g mu-kwate = catch him,
          2. an infix: o-mu-kwate   = you catch him

      -}
      mkObjClitic : Agreement -> Str = \a ->case a of {
       
          AgMUBAP1 n => mkClitics "n" "tu" n;
          --AgMUBAP1 Pl => "tu" ;
          AgMUBAP2 n => mkClitics "ku" "ba" n;
          --AgMUBAP2 Pl => "ba" ;
          AgP3 Sg MU_BA => mkClitic "mu" ;
          AgP3 Pl MU_BA =>  mkClitic "ba";
          AgP3 Pl (ZERO_BU | KA_BU | KA_TU | RU_BU) => mkClitic "bu" ;
          AgP3 Sg BU_MA => mkClitic "bu" ;
          AgP3 Sg KI_BI => mkClitic "ki" ; 
          AgP3 Pl (KI_BI | ZERO_BI) => mkClitic "bi";
          AgP3 Pl (ZERO_MA | KU_MA | RI_MA | I_MA | BU_MA) => mkClitic "ga" ;
          AgP3 (Sg | Pl) HA => mkClitic "ha";
          AgP3 Sg (I_ZERO | I_MA | RI_MA) => mkClitic "ri" ;
          AgP3 Sg (KA_ZERO | KA_BU | KA_TU) => mkClitic "ka" ;
          AgP3 Sg (KU_ZERO | KU_MA) => mkClitic "ku" ;
          {- 
             #comment for the following two lines
             the follwing partciles are all used by Noun Classes of Place i.e. HA, KU and MU
             We take the particle to be "ha" for all of them although noun class KU can use
             another particle "gi" -- see Table of Concords in Appendix of Dictionary by Mpairwe and Kahangi

             Note: The particles do not change with respect to gender

             TODO: obtain clear examples of usage
          -}
          AgP3 (Sg | Pl) (HA | MU) => mkClitic "ha" ;
          AgP3 (Sg | Pl) KU => mkClitic "ha" ;  -- gi is also possible -- see comment above

          AgP3 Sg (RU_N | RU_ZERO | RU_BU | RU_MA) => mkClitic "ru" ;
          AgP3 Pl (KA_TU | ZERO_TU) => mkClitic "tu" ;
          
          AgP3 Sg (N_N | ZERO_ZERO) => mkClitic "gi" ; 
         
          AgP3 Sg (MU_MI | MU_ZERO) => mkClitic "gu" ;
          AgP3 Pl  GU_GA => "ga" ; 
          AgP3 Pl (MU_MI | ZERO_MI) => mkClitic "gi" ; 
          {-
              According to Mpaiwe & Kahangi in their table of concords, the particle for the plural
              of noun classes N_N , ZERO_ZERO , ZERO_N & RU_N can be either "i" or "zi" depending
              on object they refer to. 
              
              Problem:
              However, we cannot use the | operator in strings as GF will
              fail to compile to comletion. 
              Implication:
              Some of our output strings will have the wrong object particle attached.
              Even if the operator | worked, we would generate two versions of the linearized 
              string of which one would be right and the other wrong 
              What is the solution to this? 
          -}
          AgP3 Pl (N_N | ZERO_ZERO | ZERO_N | RU_N) => mkClitic "zi" ; --some cases require use of particle "i" 
          
          _ => mkClitic "-" -- Hopefully exhausted all forms 
        };
    AdverbP = {s :Str; agr : AgrExist} ;
    mkAdv: Str -> AgrExist -> AdverbP =\str, agr ->{s=str; agr=agr};
    --dealing with the adjective 
    {-
      The Adjective can be before the noun for TRUE or
      it can be after the noun (FLASE)
      Most Adjectives are stems which are meaningless 
      without adjectival prefixes. These prefixes are concords that agree
      with the noun before the them. 

      However ome adjectives are self-standing.
      

      You can introduce a more meaningful name or using
      Inari's method of avoiding tables
      i.e. Adjective: Type = { pre : Str  ; post : Str; isPre: Bool; isProper : Bool}
            pre -- the adjective or adjective stem that comes before noun
            post --the adjective or adjective stem that comes after noun
            isPre --whether the adjective comes before (TRUE) or after (FALSE) the noun
            isProper -- True for a full adjective anf False for an adjectival stem
            isPrep -- does the adjective need a preposition especially those that come after the noun.
      improve that further by avoiding carrying a table of strings 
      using arne's technique
    -}

    Adjective : Type = {s : Str ; post : Str; isPre : Bool; isProper : Bool; isPrep: Bool} ;
    mkAdjective: Str -> Bool -> Bool -> Bool -> Adjective = \ a , isPre, isProper, isPrep -> case isPre of {
      True => { s = a ; post = [] ; isPre = True; isProper = isProper; isPrep = isPrep} ; 
      --this is supposed to be a concatenation use bind and I will do so later
      False => { s = [] ; post = a; isPre = False; isProper = isProper; isPrep = isPrep} -- requires agreement later  

      };
    {-
        TO DO:
        --Subject prefixes / particles of clitics using bind
    -}
    -- Adjectival Prefixes with initial vowel with the semantics of the 
    mkAdjPronIVClitic : Agreement -> Str = \a -> case a of {
              AgMUBAP1 n => mkClitics "omu" "aba" n;
              --AgMUBAP1 Pl => "aba" ;
              AgMUBAP2 n => mkClitics "omu" "aba" n; --probably an error check your grammar book
              --AgMUBAP2 Pl => "aba" ;
              AgP3 n MU_BA => mkClitics "omu" "aba" n;
              --AgP3 Pl MU_BA => "aba" ;
              AgP3 Pl ZERO_BU => mkClitic "obu" ;
              AgP3 Sg BU_MA => mkClitic "obu" ;
              AgP3 Pl (KA_BU | RU_BU) => mkClitic "obu" ;
              AgP3 Pl (KI_BI | ZERO_BI) => mkClitic "ebi" ;
              AgP3 Pl (ZERO_MA | KU_MA | RI_MA | I_MA | BU_MA) => mkClitic "ama";
              AgP3 (Sg | Pl) (HA | MU) => mkClitic "aha" ; -- of place HA & MU
              AgP3 (Sg | Pl) KU => mkClitic "en" ; -- of place KU
              AgP3 Sg (I_ZERO | I_MA | RI_MA) =>mkClitic "eri" ;
              AgP3 Sg (KA_ZERO | KA_BU) =>mkClitic "aka" ;
              AgP3 Sg KI_BI   => mkClitic "eki" ;
              AgP3 Sg (KU_ZERO | KU_MA) => mkClitic "oku" ;
              AgP3 Sg (MU_MI | MU_ZERO) => mkClitic "omu" ;
              AgP3 Sg (RU_ZERO | RU_BU | RU_MA| RU_N) => mkClitic "oru" ;
              AgP3 Pl (ZERO_TU | KA_TU) =>mkClitic "otu" ;
              AgP3 Sg (ZERO_ZERO | N_N) =>mkClitic "en" ;
              AgP3 Pl ZERO_MI =>mkClitic "en" ;
              AgP3 Pl MU_MI => mkClitic "emi";
              AgP3 Pl (ZERO_ZERO | ZERO_N | N_N | RU_N)  =>mkClitic "en" ;
              AgP3 Sg GU_GA => mkClitic "ogu" ;
              AgP3 Pl GU_GA => mkClitic "aga" ;
              _  => mkClitic "XXX" -- error checking for any case not catered for

    };

    -- Adjectival Prefixes without initial vowel with the semantics for adjectives used in Imperative negative form
    mkAdjPronNoIVClitic : Agreement -> Str = \a -> case a of {
              AgMUBAP1 n => mkClitics "mu" "ba" n;
              --AgMUBAP1 Pl => "aba" ;
              AgMUBAP2 n => mkClitics "mu" "ba" n; --probably an error check your grammar book
              --AgMUBAP2 Pl => "aba" ;
              AgP3 n MU_BA => mkClitics "mu" "ba" n;
              --AgP3 Pl MU_BA => "aba" ;
              AgP3 Pl ZERO_BU => mkClitic "bu" ;
              AgP3 Sg BU_MA => mkClitic "bu" ;
              AgP3 Pl (KA_BU | RU_BU) => mkClitic "bu" ;
              AgP3 Pl (KI_BI | ZERO_BI) => mkClitic "bi" ;
              AgP3 Pl (ZERO_MA | KU_MA | RI_MA | I_MA | BU_MA) => mkClitic "ma";
              AgP3 (Sg | Pl) (HA | MU) => mkClitic "ha" ; -- of place HA & MU
              AgP3 (Sg | Pl) KU => mkClitic "n" ; -- of place KU
              AgP3 Sg (I_ZERO | I_MA | RI_MA) =>mkClitic "ri" ;
              AgP3 Sg (KA_ZERO | KA_BU) =>mkClitic "ka" ;
              AgP3 Sg KI_BI   => mkClitic "ki" ;
              AgP3 Sg (KU_ZERO | KU_MA) => mkClitic "ku" ;
              AgP3 Sg (MU_MI | MU_ZERO) => mkClitic "mu" ;
              AgP3 Sg (RU_ZERO | RU_BU | RU_MA| RU_N) => mkClitic "ru" ;
              AgP3 Pl (ZERO_TU | KA_TU) =>mkClitic "tu" ;
              AgP3 Sg (ZERO_ZERO | N_N) =>mkClitic "n" ;
              AgP3 Pl ZERO_MI =>mkClitic "n" ;
              AgP3 Pl MU_MI => mkClitic "mi";
              AgP3 Pl (ZERO_ZERO | ZERO_N | N_N | RU_N)  =>mkClitic "n" ;
              AgP3 Sg GU_GA => mkClitic "gu" ;
              AgP3 Pl GU_GA => mkClitic "ga" ;
              _  => mkClitic "XX" -- error checking for any case not catered for

    };
    -- Genetive Preposition: simple "of" without Initila vowel
    mkGenPrepNoIVClitic : Agreement -> Str = \a -> case a of {
              AgMUBAP1 n => mkClitics "wa" "ba" n;
              --AgMUBAP1 Pl => "aba" ;
              AgMUBAP2 n => mkClitics "wa" "ba" n; --probably an error check your grammar book
              --AgMUBAP2 Pl => "aba" ;
              AgP3 n MU_BA => mkClitics "wa" "ba" n;
              --AgP3 Pl MU_BA => "aba" ;
              AgP3 Pl ZERO_BU => mkClitic "bwa" ;
              AgP3 Sg BU_MA => mkClitic "bwa" ;
              AgP3 Pl (KA_BU | RU_BU) => mkClitic "bwa" ;
              AgP3 Pl (KI_BI | ZERO_BI) => mkClitic "bya" ;
              AgP3 Pl (ZERO_MA | KU_MA | RI_MA | I_MA | BU_MA) => mkClitic "ga";
              AgP3 (Sg | Pl) HA => mkClitic "ha" ; -- of place HA 
              AgP3 (Sg | Pl) MU => mkClitic "mwa" ; -- of place MU
              AgP3 (Sg | Pl) KU => mkClitic "ya" ; -- of place KU
              AgP3 Sg (I_ZERO | I_MA | RI_MA) =>mkClitic "rya" ;
              AgP3 Sg (KA_ZERO | KA_BU) =>mkClitic "ka" ;
              AgP3 Sg KI_BI   => mkClitic "kya" ;
              AgP3 Sg (KU_ZERO | KU_MA) => mkClitic "kwa" ;
              AgP3 Sg (MU_MI | MU_ZERO) => mkClitic "gwa" ;
              AgP3 Sg (RU_ZERO | RU_BU | RU_MA| RU_N) => mkClitic "rwa" ;
              AgP3 Pl (ZERO_TU | KA_TU) =>mkClitic "twa" ;
              AgP3 Sg (ZERO_ZERO | N_N) =>mkClitic "ya" ;
              AgP3 Pl ZERO_MI =>mkClitic "ya" ;
              AgP3 Pl MU_MI => mkClitic "emi";
              AgP3 Pl (ZERO_ZERO | ZERO_N | N_N | RU_N)  =>mkClitic "za" ;
              AgP3 Sg GU_GA => mkClitic "gwa" ;
              AgP3 Pl GU_GA => mkClitic "ga" ;
              _  => mkClitic "Error mkGenPrepNoIVClitic" -- error checking for any case not catered for

    };

    -- type for Determier necessary for catCgg.gf
    Determiner : Type = {s : Str ; ntype : NounState ; num : Number ; pos : Position };
    mkDet : Str -> NounState -> Number -> Position -> Determiner 
      = \ det, ns, num,pos ->
        {
          s = det;
          ntype = ns;
          num = num;
          pos = pos;
        };

    -- Pronouns must have agreement because they are used 
    -- depending on gender, Number and person
    -- all noun classes have pronouns in the third person
    --This is a big problem, probably we create our own abstract syntax
    {-
        TO-DO: DONE but not tested yet. Here I will simply use one
        class KI_BI
    -}
    Pronoun : Type ={s : Case => Str ; agr : Agreement} ;
    mkPron : Str -> Str -> Agreement->Pronoun =\ nom,acc, a ->
      {
        s = table {Nom => nom; Acc => acc};
        agr = a;
      };

    

    PolTemp = {s : Agreement => Str * Str ; end : Str} ; -- a tupple of two strings
    
    -- Structural
    Preposition = {s,other : Str}; -- prepositions sometimes have two kinds
    mkPrep : Str -> Str -> Preposition = \ first, other -> {
      s = first ;
      other = other ;
    };
    NounPhrase : Type = {s :Case => Str; agr : Agreement};
    {-
      Operation to create Noun Phrases from a Determiner and Nouns.
      In Runyankore and Rukiga, depending on the particular Determiner,
      it can appear before (we call PreDeterminer) or after (PostDeterminer) the noun.
      Examples:
        A. PreDeterminers
            1. Definite aricles: Usually using the initial vowel sufficient
            2. Demonstratives: ogu muntu (This person)
            3. Every: every man = "buri muntu"
        B. PostDeterminers
            1. Definite aricles: Usually using the initial vowel is sufficient
            2. Demonstratives: omuntu ogu (person this)
            3. few: omuntu mu-kye
      Note: Problem stil exists because we cannot know when the  determiner string is empty
            There is a mistake here. If the determiner is empty, we end up with a 
            meaningless subject particle standing alone. we can test if det.s is a 
            string or empty.

  -}
  mkDetCN : Determiner -> Noun -> NounPhrase = \ det, cn ->
    case <det.pos> of { 
         <PostDeterminer> => {s = \\_=>
          let 
            subjClitic = mkSubjClitic (AgP3 det.num cn.gender) 
          in  cn.s!det.ntype!det.num ++ subjClitic ++ det.s; agr = AgP3 det.num cn.gender};
        <PreDeterminer> => { s =\\_ => det.s ++ cn.s ! det.ntype ! det.num; agr = AgP3 det.num cn.gender} --;
        --<PostDeterminer, PFalse> => {s = \\_=> cn.s!det.ntype!det.num; agr = AgP3 det.num cn.gender }    
         };
  

    
    
      --Verb
      param
        VFormMini =  Inf | Pres | Past | PastPart | PresPart ;
      oper
      --Verb : Type = {s : VFormMini => Str};
      Verb : Type = {s : Str; morphs: VFormMini => VerbMorphPos=> Str};
      
      GVerb : Type = {s : Bool =>Str ; morphs: VFormMini => VerbMorphPos =>Str; isAux : Bool};
      {-
        The V2 sometimes uses preopsitions for formation
        of direct object. Unlike in English where the verb 
        and the preposition are disjunctive such as "send to",
        In runyakore and rukiga, the verb and preposition are
        conjunctive such as sindik-ira.

        Because of the fusion, I have deffered including this in 
        the compPrep. Actually, it is going to be empty in the next version
      -}
      Verb2 : Type = Verb ** {comp:Str};
      {-
        Given a root, can you form the different verbforms?
      -}
      param
        VerbMorphPos = PreVerb | PriNegM | --ObjRel | SubjMarker | 
                       SecNegM | TAMarker | PersisiveMarker| --DObjM | IDobjM |
                       RestOfVerb;
      oper
      VMorphs : Type = VFormMini => VerbMorphPos => Str;
      VerbPhrase: Type = {s:Str; morphs: VMorphs ; comp:Str ; isCompApStem : Bool; agr : AgrExist};
      -- in VP formation, all verbs are lifted to GVerb, but morphology doesn't need to know this
     verb2gverb : Verb ->Str -> GVerb = \v, ba -> {
            s = table{
                    True =>  v.s; 
                    False => ba --the special verb to be
                      };
            morphs = v.morphs; 
            isAux = False
          }; 
     {-
        In Runynakore & Rukiga the verb to be in english has two
        Infinitives i.e. 
          a) ri --used when it is the only main and therefore licenses a subject
          b) ba --used usually as commands, such as Imperatives or when another
                  verb is acting as the main verb. It also acts as the infitive form
     -} 
     be_GVerb : GVerb = {
        s= table{True => "ri"; False =>"b" }; 
        morphs = mkVerbMorphs; 
        isAux = True};

      {-
      --copulative conjugations of ni and ri as used for adjectives
      
      copRiNi :Verb ={
        s= table {
        True => table{
                VPres (Agr (NC_mu_ba) Sg Per1)  => "ndi" ;
                VPres (Agr (NC_mu_ba) Pl Per1)  => "turi";
                VPres (Agr (NC_mu_ba) Sg Per2)  => "ori" ;
                VPres (Agr (NC_mu_ba) Pl Per2)  => "muri";
                VPres (Agr (NC_mu_ba) Sg Per3)  => "ari" ;
                VPres (Agr (NC_mu_ba) Pl Per3)  => "bari";
                VPres (Agr (_) _ _)=> ""
      };
      -}
      {-
          This function packages the different morphemes of the each tense of verb
          that are commonly used and have not more than two possibilities i.e.
          1. infintive marker at begining of verb
          2. ni - continuous marker at begining of the present participle / present continuous
          3. ti - Primary Negative marker
          4. ta - Secondary Negative Marker
          5. Tense and aspect markers
          6. Persistive Marker
          7. Rest of Verb. 
        These can be increased further. Note: Only those tenses clossest to the english equivalent 
        have been chosen. The full resource shall require even more.
      -}
      mkVerbMorphs: VMorphs = table{
            Inf => table{ 
                          PreVerb => "ku";
                          PriNegM => []; 
                          --ObjRel => [];
                          --SubjMarker =[]; 
                          SecNegM => "ta"; 
                          TAMarker => []; 
                          PersisiveMarker => [];
                          --DObjM => [];
                          --IDobjM => [];
                          RestOfVerb =>"a"
                        };
            Pres => table{
                          PreVerb => [];
                          PriNegM => "ti"; 
                          --ObjRel => [];
                          --SubjMarker =[]; 
                          SecNegM => "ta"; 
                          TAMarker => []; 
                          PersisiveMarker => [];
                          --DObjM => [];
                          --IDobjM => [];
                          RestOfVerb =>"a"
                        };
            --mkVerbPast:Str -> Str =\root -> Predef.BIND ++"ti" ++ Predef.BIND ++ "PriNegM" ++ Predef.BIND ++ "TM1" ++ Predef.BIND ++ "Empy" ++ Predef.BIND ++ "TM2" ++ Predef.BIND ++ "Empty" ++ Predef.BIND ++ "stem" ++ Predef.BIND ++ root ++ Predef.BIND ++ "ire"; 
            Past => table{
                          PreVerb => [];
                          PriNegM => "ti"; 
                          --ObjRel => [];
                          --SubjMarker =[]; 
                          SecNegM => []; 
                          TAMarker => []; 
                          PersisiveMarker => [];
                          --DObjM => [];
                          --IDobjM => [];
                          RestOfVerb =>"ire"
                        };
            --"ni" ++ Predef.BIND ++ "ContM" ++ Predef.BIND ++ "ti" ++ Predef.BIND ++ "PriNegM" ++ Predef.BIND ++ "TM1" ++ Predef.BIND ++ "Empy" ++ Predef.BIND ++ "TM2" ++ Predef.BIND ++ "Empty" ++ Predef.BIND ++ "stem" ++ Predef.BIND ++ root ++ Predef.BIND ++"a";
            PresPart => table{
                          PreVerb => "ni";
                          PriNegM => "ti"; 
                          --ObjRel => [];
                          --SubjMarker =[]; 
                          SecNegM => []; 
                          TAMarker => []; 
                          PersisiveMarker => [];
                          --DObjM => [];
                          --IDobjM => [];
                          RestOfVerb =>"a"
                        };
            PastPart => table{
                          PreVerb => [];
                          PriNegM => "ti"; 
                          --ObjRel => [];
                          --SubjMarker =[]; 
                          SecNegM => []; 
                          TAMarker => "aa"; 
                          PersisiveMarker => [];
                          --DObjM => [];
                          --IDobjM => [];
                          RestOfVerb =>"a"
                        }

      };

      --oper
        --Concatenates two strings at runtime without spaces
        
        glue: Str -> Str ->Str =\ x, y -> x ++ BIND ++ y;
}
