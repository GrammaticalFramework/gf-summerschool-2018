resource MiniResPor = open Prelude in {

  param
    Gender = Masc | Fem ;

    Number = Sg | Pl ;

    Case   = Nom | Acc ;

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
    NP = {
      s : Case => {clit,obj : Str ; isClit : Bool} ;
      a : Agreement
      } ;

    Noun : Type = {s : Number => Str ; g : Gender} ;

    mkNoun : Str -> Str -> Gender -> Noun = \sg,pl,g -> {
      s = table {Sg => sg ; Pl => pl} ;
      g = g
      } ;

    regNoun : Str -> Gender -> Noun = \sg,g -> mkNoun sg (sg + "s") g;

    -- smart paradigms
    smartGenNoun : Str -> Gender -> Noun = \vinho,g -> case vinho of {
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

    smartNoun : Str -> Noun = \vinho -> case vinho of {
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

    mkN = overload {
      mkN : Str -> Noun                     = smartNoun ;
      mkN : Str -> Gender -> Noun           = smartGenNoun ;
      mkN : Str -> Str    -> Gender -> Noun = mkNoun ;
      } ;

    ---
    -- PN
    ProperName : Type = {s : Str ; g : Gender} ;

    mkPN : Str -> Gender -> ProperName = \s,g -> {s = s ; g = g} ;

    ---
    -- Pron
    Pron : Type = {s : Case => Str ; a : Agreement} ;

    iMasc_Pron : Pron = {
      s = table {Nom => "eu" ; Acc => "me"} ;
      a = Agr Masc Sg Per1
      } ;

    youMascSg_Pron : Pron = {
      s = table {Nom => "você" ; Acc => "lhe"} ;
      a = Agr Masc Sg Per2
      } ;

    weMasc_Pron : Pron = {
      s = table {Nom => "nós" ; Acc => "nos"} ;
      a = Agr Masc Pl Per1
      };

    youMascPl_Pron : Pron = {
      s = table {Nom => "vocês" ; Acc => "lhes"} ;
      a = Agr Masc Pl Per2
      } ;

    genderPron : Gender -> Pron -> Pron ;
    genderPron g pr = case pr.a of {
      (Agr _ n pe) => {s = pr.s ; a = Agr g n pe}
      } ;

    employNP : Case -> NP -> Str = \c,np ->
      let nps = np.s ! c in case nps.isClit of {
        True => nps.clit ;
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

    mkA = overload {
      mkA : Str             -> Adjective         = regAdjective ;
      mkA : Str             -> Bool -> Adjective = preAdjective ;
      mkA : (_,_,_,_ : Str) -> Bool -> Adjective = mkAdjective ;
      } ;

    preA : Adjective -> Adjective
      = \a -> {s = a.s ; isPre = True} ;

    ---
    -- Verb
    VP = {
      verb : Verb ;
      clit : Str ;
      clitAgr : ClitAgr ;
      compl : Agreement => Str ;
      } ;

    Verb : Type = {s : VForm => Str } ;

    agrV : Verb -> Agreement -> Str = \v,a -> case a of {
      Agr _ n p => v.s ! VPres n p
      } ;

    neg : Bool -> Str = \b -> case b of {True => [] ; False => "não"} ;

    ser_V = mkV "ser" "sou" "é" "somos" "são"
      "fui" "foi" "fomos" "foram" "seja" "sejamos" "sejam";
    estar_V = mkV "estar" "estou" "está" "estamos" "estão"
      "estive" "esteve" "estivemos" "estiveram"
      "esteja" "estejamos" "estejam" ;

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

    mkV = overload {
      mkV : Str -> Verb = smartVerb ;
      mkV : (_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> Verb = mkVerb ;
      } ;

    Verb2 : Type = Verb ** {c : Case ; p : Str} ;

    mkV2 = overload {
      mkV2 : Str -> Verb2 =
        \s   -> mkV s ** {c = Nom ; p = []} ;
      mkV2 : Str -> Case -> Verb2 =
        \s,c -> mkV s ** {c = c ; p = []} ;
      mkV2 : Str -> Str -> Verb2 =
        \s,p -> mkV s ** {c = Nom ; p = p} ;
      mkV2 : Str  -> Case -> Str -> Verb2 =
        \s,c,p -> mkV s ** {c = c ; p = p} ;
      mkV2 : Verb -> Verb2 =
        \v -> v ** {c = Nom ; p = []} ;
      mkV2 : Verb -> Case -> Verb2 =
        \v,c -> v ** {c = c ; p = []} ;
      mkV2 : Verb -> Str -> Verb2 =
        \v,p -> v ** {c = Nom ; p = p} ;
      mkV2 : Verb -> Case -> Str -> Verb2 =
        \v,c,p -> v ** {c = c ; p = p} ;
      } ;

    ---
    -- Adverb
    Adverb : Type = {s : Str} ;

    mkAdv : Str -> Adverb = \s -> {s = s} ;

    ---
    -- Det
    -- [ ] is this ok por port?
    adjDet : Adjective -> Number -> {s : Gender => Case => Str ; n : Number} =
      \adj,n -> {
        s = \\g,c => adj.s ! g ! n ;
        n = n
      } ;

    ---
    -- Prep
    Prep : Type = {s : genNumStr } ;
    no_Prep : Prep = { s = table {
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
