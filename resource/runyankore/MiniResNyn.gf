resource MiniResNyn = --ParamX ** 
	open Prelude in {

param 
	Number = Sg | Pl;
	Person = Per1 | Per2 | Per3;
	Gender =  MU_BA | KI_BI | N_N | KU_MA  |  BU_MA | 
					  RU_BU | GU_GA | ZERO_ZERO  |  MU_MI |  RI_MA | 
					  I_MA  | KA_BU | KA_TU | RU_N |  RU_MA |  HA | 
					  MU |  KU  |  ZERO_BU  |  ZERO_BI | ZERO_MA |  
					  ZERO_MI |  ZERO_TU |  ZERO_N  | I_ZERO  |  
					  RI_ZERO |  KU_ZERO | MU_ZERO |  RU_ZERO |  
					  KA_ZERO |ZERO_BAA;
	Case = Acc | Nom ;
  {-
    --there are several and i.e. 
    -- na (two nouns, 2 Noun Phrases, 2 Pronouns, 2 relative subject clauses, )
    --kandi (clauses having a commonality of subjects, object or tense)
    --the best structure is a table
  -}
  ConjArg = Nn_Nn | Nps_Nps | Pns_Pns | RelSubjCls | Other
  AgrConj = AConj ConjArg
	Agreement =  AgP3 Number Gender | AgMUBAP1 Number |AgMUBAP2 Number ;

	--Functional forms of the regular verb
	Mood = Infinitive | Imperative | Subjunctive | Perfective;
	VerbCat = Simple | Prepositional | Causative;
	Voice = Active | Passive;
  -- all forms of normal Eng verbs, although not yet used in MiniGrammar
  VForm = VF Voice Mood VerbCat ;
  -- may not need it
  NounCat = ComNoun | PropNoun; --prepositions agree with nouns to form adverbial Phrases
  PrepForm = Normal | Other; -- omu and omuri, aha, ahari
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
        < _ ,_ > => mkNoun omuntu  omuntu g b-- improve as we go on.
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
          AgMUBAP2 n => mkClitics "wa" "a" n;
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


    --dealing with the adjective 
    {-
      The Adjective can be before the noun for TRUE or
      it can be after the noun (FLASE)

      You can introduce a more meaningful name or using
      Inari's method of avoiding tables
      i.e. Adjective: Type = { pre : Str  ; post : Agr => Str}

      improve that further by avoiding carrying a table of strings 
      using arne's technique
    -}

    Adjective : Type = {s : Str ; post : Str} ;
    mkAdjective: Str-> Bool -> Adjective = \ a , b -> case b of {
      True => { s = [] ; post = a} ; 
      --this is supposed to be a concatenation use bind and I will do so later
      False => { s = a ; post = []} -- requires agreement later                              
      };

    --Subject prefixes / particles of clitics using bind

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

    -- type for Determier necessary for catCgg.gf
    Determiner : Type = {s : Str ; ntype : NounType ; num : Number ; pos : Position };
    mkDet : Str -> NounType -> Number -> Position -> Determiner 
      = \ det, nt, num,pos ->
        {
          s = str;
          ntype = nt;
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
    Pronoun : Type ={s : Case => Str ; agr : Agr} ;
    mkPron : Str -> Str -> Agreement->Pronoun =\ nom,acc, a ->
      {
        s = table {Nom => nom; Acc => acc};
        agr = a;
      };

    --verbs
    Verb  = {root : Str } ;

    PolTemp = {s : Agr => Str * Str ; end : Str} ; -- a tupple of two strings
    
    -- Structural
    Preposition = {s,other : Str}
    mkPrep : Str -> Str -> Preposition = \ first, other -> {
      s = first ;
      other = other ;
    }
    {-

    have_V2 : V2 ;
    -}
}
{-
  regNoun : Str -> Noun = \sg -> mkNoun sg (sg + "s") ; -- ignored

  -- smart paradigm
  smartNoun : Str -> Noun = \sg -> case sg of {
    _ + ("ay"|"ey"|"oy"|"uy") => regNoun sg ;
    x + "y"                   => mkNoun sg (x + "ies") ;
    _ + ("ch"|"sh"|"s"|"o")   => mkNoun sg (sg + "es") ;
    _                         => regNoun sg
    } ;

  Adjective : Type = {s : Str} ;

  Verb : Type = {s : VForm => Str} ;

  mkVerb : (inf,pres,past,pastpart,prespart : Str) -> Verb
    = \inf,pres,past,pastpart,prespart -> {
    s = table {
      Inf => inf ;
      PresSg3 => pres ;
      Past => past ;
      PastPart => pastpart ;
      PresPart => prespart
      }
    } ;

  regVerb : (inf : Str) -> Verb = \inf ->
    mkVerb inf (inf + "s") (inf + "ed") (inf + "ed") (inf + "ing") ;

  -- regular verbs with predictable variations
  smartVerb : Str -> Verb = \inf -> case inf of {
     pl  +  ("a"|"e"|"i"|"o"|"u") + "y" => regVerb inf ;
     cr  +  "y" =>  mkVerb inf (cr + "ies") (cr + "ied") (cr + "ied") (inf + "ing") ;
     lov + "e"  => mkVerb inf (inf + "s") (lov + "ed") (lov + "ed") (lov + "ing") ;
     kis + ("s"|"sh"|"x") => mkVerb inf (inf + "es") (inf + "ed") (inf + "ed") (inf + "ing") ;
     _ => regVerb inf
     } ;

  -- normal irregular verbs e.g. drink,drank,drunk
  irregVerb : (inf,past,pastpart : Str) -> Verb =
    \inf,past,pastpart ->
      let verb = smartVerb inf
      in mkVerb inf (verb.s ! PresSg3) past pastpart (verb.s ! PresPart) ;   

  negation : Bool -> Str = \b -> case b of {True => [] ; False => "not"} ; 

  -- two-place verb with "case" as preposition; for transitive verbs, c=[]
  Verb2 : Type = Verb ** {c : Str} ;

  -- generalized verb, here just "be"
 param
   GVForm = VF VForm | PresSg1 | PresPl | PastPl ;

 oper
  GVerb : Type = {
     s : GVForm => Str ;
     isAux : Bool
     } ;

  be_GVerb : GVerb = {
     s = table {
       PresSg1 => "am" ;
       PresPl  => "are" ;
       PastPl  => "were" ;
       VF vf   => (mkVerb "be" "is" "was" "been" "being").s ! vf
       } ;
     isAux = True
     } ;

  -- in VP formation, all verbs are lifted to GVerb, but morphology doesn't need to know this
   verb2gverb : Verb -> GVerb = \v -> {s =
     table {
       PresSg1 => v.s ! Inf ;
       PresPl  => v.s ! Inf ;
       PastPl  => v.s ! Past ;
       VF vf   => v.s ! vf
       } ;
     isAux = False
     } ;

}
}
-}