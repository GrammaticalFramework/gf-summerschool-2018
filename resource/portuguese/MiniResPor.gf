resource MiniResPor = open Prelude in {

  param
    Gender = Masc | Fem ;

    Number = Sg | Pl ;

    Case   = Nom | Acc | Dat ;

    Person = Per1 | Per2 | Per3 ;

    Agreement = Agr Gender Number Person ;

    ClitAgr   = CAgrNo | CAgr Agreement ;

    VForm = VInf
      | VPres Number Person
      | VPast Number Person
      | VImp  ImpNumPer ;

    ImpNumPer  = SgPer2 | PlPer1 | PlPer2 ;

  oper
    genNumStr : Type = Gender => Number => Str ;

     ---
    -- Noun
    NP : Type = {
      s : Case => {clit : Clit ; obj : Str} ;
      a : Agreement
      } ;

    Noun : Type = {s : Number => Str ; g : Gender} ;

    mkNoun : Str -> Str -> Gender -> Noun = \sg,pl,g -> {
      s = table {Sg => sg ; Pl => pl} ;
      g = g
      } ;

    regNoun : Str -> Gender -> Noun ;
    regNoun sg g = mkNoun sg (sg + "s") g;

    -- smart paradigms
    smartGenNoun : Str -> Gender -> Noun ;
    smartGenNoun vinho g = case vinho of {
      -- rapaz/Masc, flor/Fem
      rapa + z@("z"|"r"|"s")           =>
        mkNoun vinho (vinho + "es") g ;
      -- canal/Masc, vogal/Fem
      can  + v@("a"|"e"|"o"|"u") + "l" =>
        mkNoun vinho (can + v + "is") g ;
      -- homem/Masc, nuvem/nuvens
      home  + "m"  => mkNoun vinho (home + "ns") g ;
      -- tórax/Masc, xerox/Fem
      tóra + "x"                       =>
        mkNoun vinho vinho g ;
      -- regular inflection
      _                                =>
        regNoun vinho g
      } ;

    smartNoun : Str -> Noun ;
    smartNoun vinho = case vinho of {
      cas   + "a"  => regNoun vinho Fem ;
      vinh  + "o"  => regNoun vinho Masc ;
      falc  + "ão" =>
        mkNoun vinho (falc + "ões") Masc ; -- other rules depend on
                                           -- stress, can this be
                                           -- built with gf?
      artes + "ã"  => regNoun vinho Fem ;
      líque + "n"  => regNoun vinho Masc ;
      obu  + "s"   => mkNoun vinho (vinho + "es") Masc ;
      can  + "il"  =>
        mkNoun vinho (can + "is") Masc ; -- what about fóssil?
      _           => smartGenNoun vinho Masc
      } ;

    ---
    -- PN
    ProperName : Type = {s : Str ; g : Gender} ;

    ---
    -- Pron
    Pron : Type = {s : Case => Str ; a : Agreement} ;

    mkPron : (_,_,_ :Str) -> Gender -> Number -> Person -> Pron ;
    mkPron eu me mim g n p = {
      s = table {Nom => eu ; Acc => me ; Dat => mim} ;
      a = Agr g n p
      } ;

    iMasc_Pron : Pron = mkPron "eu" "me" "mim" Masc Sg Per1 ;

    iFem_Pron : Pron = femPron iMasc_Pron ;

    youMascSg_Pron : Pron = mkPron "você" "lhe" "lhe" Masc Sg Per2 ;

    youFemSg_Pron : Pron = femPron youMascSg_Pron ;

    weMasc_Pron : Pron = mkPron "nós" "nos" "nós" Masc Pl Per1 ;

    youMascPl_Pron : Pron = mkPron "vocês" "lhes" "lhes" Masc Pl Per2 ;

    femPron : Pron -> Pron ;
    femPron pr = case pr.a of {
      (Agr _ n pe) => pr ** {a = Agr Fem n pe}
      } ;

    ---
    -- NP
    employNP : Case -> NP -> Str = \c,np ->
      let nps = np.s ! c in case nps.clit.hasClit of {
        True => nps.clit.s ;
        _    => nps.obj
      } ;

    ---
    -- Adjective
    Adjective : Type = {s : genNumStr ; isPre : Bool} ;

    mkAdjective : (_,_,_,_ : Str) -> Bool -> Adjective = \bom,boa,bons,boas,p -> {
      s = table {
        Masc => table {Sg => bom ; Pl => bons} ;
        Fem  => table {Sg => boa ; Pl => boas}
        } ;
      isPre = p
      } ;

    regAdjective : Str -> Adjective = \preto -> case preto of {
      pret + "o" =>
        mkAdjective preto (pret + "a") (preto + "s") (pret + "as") False ;
      pret + "e" =>
        mkAdjective preto preto (preto + "s") (preto + "s") False ;
      _          => mkAdjective preto preto preto preto False
      } ;

    preAdjective : Str -> Bool -> Adjective = \preto,b ->
      let pretoA = regAdjective preto in
      case b of {
        True => preA pretoA ;
        _    => pretoA
      } ;

    preA : Adjective -> Adjective
      = \a -> {s = a.s ; isPre = True} ;

    ---
    -- Verb
    Clit : Type = {s : Str ; hasClit : Bool} ;

    emptyClit : Clit = {s = [] ; hasClit = False} ;

    VP = {
      verb : Verb ;
      clit : Clit ;
      clitAgr : ClitAgr ;
      compl : Agreement => Str ;
      } ;

    Verb : Type = {s : VForm => Str } ;

    agrV : Verb -> Agreement -> Bool => Str = \v,a -> case a of {
      Agr _ n p => table {
        True  => v.s ! VPres n p ;
        False => v.s ! VPast n p
        }
      } ;

    neg : Bool -> Str = \b -> case b of {True => [] ; False => "não"} ;

    ser_V = mkVerb "ser" "sou" "é" "somos" "são"
      "fui" "foi" "fomos" "foram" "seja" "sejamos" "sejam";
    estar_V = mkVerb "estar" "estou" "está" "estamos" "estão"
      "estive" "esteve" "estivemos" "estiveram"
      "esteja" "estejamos" "estejam" ;
    ter_V = mkVerb "ter" "tenho" "tem" "temos" "tem"
      "tive" "teve" "tivemos" "tiveram"
      "tenha" "tenhamos" "tenham" ;

    mkVerb : (_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> Verb =
      \amar,amo,ama,amamos,amam,amei,amou,amamos,amaram,ame,amemos,amem -> {
      s = table {
        VInf          => amar ;
        VPres Sg Per1 => amo ;
        VPres Sg _    => ama ;
        VPres Pl Per1 => amamos ;
        VPres Pl _    => amam ;
        VPast Sg Per1 => amei ;
        VPast Sg _    => amou ;
        VPast Pl Per1 => amamos ;
        VPast Pl _    => amaram ;
        VImp SgPer2   => ame ;
        VImp PlPer1   => amemos ;
        VImp PlPer2   => amem
        } ;
      } ;

    smartVerb : Str -> Verb = \inf -> case inf of {
      -- 1st conj
      am + "ar"  => mkVerb inf (am+"o") (am+"a") (am+"amos") (am+"am")
        (am+"ei") (am+"ou") (am+"amos") (am+"aram")
        (am+"e")  (am+"emos") (am+"em") ;
      -- 2nd conj
      com + "er" => mkVerb inf (com+"o") (com+"e") (com+"emos") (com+"em")
        (com+"i") (com+"eu") (com+"emos") (com+"eram")
        (com+"a") (com+"amos") (com+"am") ;
      --3rd conj
      part + "ir" =>
        mkVerb inf (part+"o") (part+"e") (part+"imos") (part+"em")
        (part+"i") (part+"iu") (part+"imos") (part+"iram")
        (part+"a") (part+"amos") (part+"am") ;
      -- only pôr, supor and derivatives
      _ => mkVerb inf inf inf inf inf inf inf inf inf inf inf inf
      } ;

    Verb2 : Type = Verb ** {c : Case ; p : Str} ;


    ---
    -- Det
    adjDet : Adjective -> Number -> {s : Gender => Case => Str ; n : Number} =
      \adj,n -> {
        s = \\g,c => adj.s ! g ! n ;
        n = n
      } ;

    unDet = adjDet (regAdjective []) Sg ;
    um_adjDet = mkAdjective "um" "uma" "uns" "umas" True ;
    o_adjDet = mkAdjective "o" "a" "os" "as" True ;

    ---
    -- Prep
    Preposition : Type = {s : genNumStr } ;

    no_Prep : Preposition = {
      s = table {
        Masc => table {
          Sg => "no" ;
          Pl => "nos"
          } ;
        Fem => table {
          Sg => "na" ;
          Pl => "nas"
          }
        } ;
      } ;

} ;
