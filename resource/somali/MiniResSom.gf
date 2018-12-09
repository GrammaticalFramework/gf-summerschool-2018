resource MiniResSom = open Prelude, Predef in {
--------------------------------------------------------------------------------
-- Phonological definitions
  oper

    v : pattern Str = #("a" | "e" | "i" | "o" | "u") ;
    vv : pattern Str = #("aa" | "ee" | "ii" | "oo" | "uu") ;
    vstar : pattern Str = #("a" | "e" | "i" | "o" | "u" | "y" | "w") ; -- semivowels included
    c : pattern Str = #("m" | "n" | "p" | "b" | "t" | "d" | "k" | "g" | "f" | "v" | "s" | "h" | "l" | "j" | "r" | "z" | "c" | "q" | "y" | "w") ;
    lmnr : pattern Str = #("l" | "m" | "n" | "r") ;
    kpt : pattern Str = #("k" | "p" | "t") ;
    gbd : pattern Str = #("g" | "b" | "d") ;

--------------------------------------------------------------------------------
-- Nominal morphology

param
  Number = Sg | Pl ;
  Case = Nom | Abs ;
  Gender = Masc | Fem ;
  Person = Per1 | Per2 | Per3 ;
  Vowel = vA | vE | vI | vO | vU ; -- For vowel assimilation

  Inclusion = Excl | Incl ;
  Agreement = Sg1 | Sg2 | Sg3 Gender | Pl1 Inclusion | Pl2 | Pl3 | Impers ;

  NForm = Indef Number
        | Def Number Vowel -- Stems for definite and determinative suffixes
        | Numerative       -- When modified by a number (only distinct for some feminine nouns)
        | IndefNom ;       -- Special form, only fem. nouns ending in consonant

oper
  getAgr : NForm -> Gender -> Agreement = \n,g ->
    case n of { Indef Pl|Def Pl _ => Pl3 ;
                _                 => Sg3 g } ;
  getNum : Agreement -> Number = \a ->
    case a of { Sg1|Sg2|Sg3 _ => Sg ; _ => Pl } ;
  p2agr : Number -> Agreement = \n ->
    case n of { Sg => Sg2 ; Pl => Pl2 } ;

  Noun : Type = {s : NForm => Str ; g : Gender} ;

  CNoun : Type = Noun ** { mod : Number => Case => Str ; hasMod : Bool } ;

  mkNoun : (x1,_,_,x4 : Str) -> Gender -> Noun = \wiil,wiilka,wiilal,wiilasha,gender ->
    let -- Feminine nouns that end in consonant have a special nominative
        bisadi = case gender of
                   { Fem  => case wiil of { _ + #c => wiil+"i" ; _ => wiil} ;
                     Masc => wiil } ;

        -- Feminine nouns that end in -o in plural have a special numerative
        bisadood =  case gender of
                       { Fem  => case wiilal of { _ + "o" => wiilal+"od" ; _ => wiil} ;
                         Masc => wiil } ;

        defStems : Str -> Vowel => Str = \s -> case s of {
          ilk + "aha" => -- Vowel assimilates to the following suffix after h
               table { vA => ilk+"ah" ;
                       vE => ilk+"eh" ;
                       vI => ilk+"ih" ;
                       vO => ilk+"oh" ;
                       vU => ilk+"uh"
                       } ;
          _ => table { _ => init s } -- No assimilations
          } ;

    in { s = table {
           Indef Sg => wiil ;
           Indef Pl => wiilal ;
           IndefNom => bisadi ;
           Numerative => bisadood ;
           Def Sg vow => defStems wiilka ! vow ;
           Def Pl vow => defStems wiilasha ! vow } ;
         g = gender } ;

-------------------------
-- Regular noun paradigms
  nHooyo, nAabbe, nMas, nUl, nGuri, nXayawaan : Str -> Noun ;

  --1) Feminine nouns that end in -o
  nHooyo hooyo =
    mkNoun hooyo (init hooyo + "ada") (hooyo + "oyin") (hooyo + "oyinka") Fem ;

  --2) Masculine nouns that end in -e
  nAabbe aabbe = let aabb = init aabbe in
    mkNoun aabbe (aabb + "aha") (aabb + "ayaal") (aabb + "ayaasha") Masc ;

  -- 3) Masculine, plural with duplication
  nMas mas = let s = last mas ;
                 ka = allomorph mKa mas ;
                 ta = allomorph mTa mas ;
                 sha = case ta of {"sha" => ta ; _ => s + ta } in
    mkNoun mas (mas + ka) (mas + "a" + s) (mas + "a" + sha) Masc ;

  -- 4a) Feminine, plural with o
  nUl ul = let o  = case last ul of { "i" => "yo" ; _ => "o" } ;
               u  = case last ul of { "l" => init ul ; _ => ul } ;
               sha = allomorph mTa ul in
    mkNoun ul (u + sha) (ul + o) (ul + "aha") Fem ;

  -- 4b) Masculine, plural with ó, 2 syllables
  nGuri guri = let o = allomorph mO guri ;
                   ga = allomorph mKa guri ;
                   gury = case last guri of { -- TODO does this generalise? Or just exception?
                                 "i" => init guri + "y" ;
                                 _   => guri } in
    mkNoun guri (guri + ga) (gury + o) (gury + "aha") Masc ;

  -- 4c) Masculine, plural with -ó, 3 syllables or longer
  nXayawaan x = let ka = allomorph mKa x ;
                    o = allomorph mO x ;
                    xo = x + o in
    mkNoun x (x + ka) xo (init xo + "ada") Masc ;

  nMaalin : (_,_ : Str) -> Gender -> Noun = \maalin,maalmo,g ->
   let ta = case g of { Masc => allomorph mKa maalin ;
                        Fem  => allomorph mTa maalin } ;
       aha = case g of { Masc|Fem  => "aha" } ; ---- ?
   in mkNoun maalin (maalin + ta) maalmo (init maalmo + aha) g ;

  allomorph : Morpheme -> Str -> Str = \x,stem ->
    case x of {
      mO => case last stem of {
                  d@("b"|"d"|"r"|"l"|"m"|"n") => d + "o" ;
                  "c"|"g"|"i"|"j"|"x"|"s"     => "yo" ;
                  _                           => "o" } ;

      -- Based on the table on page 21 in http://morgannilsson.se/Somalisk%20minigrammatik.pdf
      mTa => case stem of {
                   _ + ("dh")  => "a" ;
                   _ + ("d"|"c"|"h"|"x"|"q"|"'"|"i"|"y"|"w") => "da" ;
                   _ + "l" => "sha" ;
                   _       => "ta" } ;

      mKa => case stem of {
                   _ + ("g"|"aa"|"i"|"y"|"w") => "ga" ;
                   _ + ("c"|"h"|"x"|"q"|"'")  => "a" ;
                   _ + ("e"|"o")              => "ha" ;
                   _                          => "ka" }
    } ;

  voiced : Str -> Str = \s -> case s of {
    "k" => "g" ;
    "t" => "d" ;
    "p" => "b" ;
     _  => s } ;

param
  Morpheme = mO | mKa | mTa ;


---------------------------------------------
-- NP
oper

  BaseNP : Type = {
    a : Agreement ;
    isPron : Bool ;
    sp : Str } ;

  NP : Type = BaseNP ** { s : Case => Str } ;

  useN : Noun -> CNoun ** BaseNP = \n -> n **
    { mod = \\_,_ => [] ; hasMod = False ;
      a = Sg3 n.g ; isPron = False ; sp = [] } ;

--------------------------------------------------------------------------------
-- Determiners

  Det : Type = {
    s : Case => Str ;
   sp : Gender => Str ;
    d : NForm
    } ;

  mkDet : (x1,_,x3 : Str) -> NForm -> Det = mkDetBind False ;

  -- TODO: generalise better for all dets
  mkDetBind : (bind : Bool) -> (x1,_,x3 : Str) -> NForm -> Det = \b,an,kani,tani,nf ->
    let ani : Str = case an of { _ + #c => an+"i" ;
                                 _      => case nf of { Def _ _ => "u" ;
                                                        _       => [] }
                               } ;
        bind : Str -> Str = \x -> case b of {False => x ; True => BIND ++ x} ;
    in { s = table { Nom => bind ani ; Abs => bind an } ;
        sp = table { Fem => tani ; Masc => kani } ;
         d = nf
       } ;



--------------------------------------------------------------------------------
-- Adjectives

param
  AForm = AF Number Case ;

oper
  Adjective : Type = { s : AForm => Str } ;

  duplA : Str -> Adjective = \yar ->
    let yaryar = duplicate yar
    in mkAdj yar yaryar ;

  mkAdj : (str,pl : Str) -> Adjective = \sg,pl -> {
    s = table {
          AF Sg Abs => sg ;
          AF Pl Abs => pl ;
          AF Sg Nom => sg + "i" ;
          AF Pl Nom => pl + "i" }
    } ;

  duplicate : Str -> Str = \sg -> case sg of {
    -- some hard-coded cases; in general, better to
    -- use 2-paradigm mkAdj for irregular adjectives.
    "dheer" => "dhaadheer" ;
    "weyn"  => "waaweyn" ;

    -- general patterns
    a@#v + d@#c + ? + ?  -- 4 letters of form CVXX
        => a + d + sg ; -- ad+adag
    g@#c + aa@#vv + _
        => g + aa + sg ; -- gaa+gaaban
    y@#c + a@#v + r@#c + _
        => y + a + r + sg ; -- yar+yar ; fud+fudud
    d@#c + h@#c + uu@#vv + _
        => d + h + uu + sg ; -- dhuu+dhuuban
    q@#c + a@#v + y@#vstar + b@#c + _
        => q + a + y + b + sg ; --qayb+qaybsan, fiic+fiican
    _   => sg + sg } ;

--------------------------------------------------------------------------------
-- Verb

param
   VForm =
     VInf
   | VPres Agreement Bool
   | VNegPast
   | VPast Agreement
   | VImp Number Bool ; -- Not all forms, just those needed for the Doctor app

oper

  Verb : Type = {s : VForm => Str} ;

  Verb2 : Type = Verb ** {c2 : Preposition} ;

  mkVerb : (imperative,sg1,pl2 : Str) -> Verb = \qaado,qaat,ark ->
    let stems : {p1 : Str ; p2 : Str} = case ark of {
          a + r@#c + k@#c -- two consonants need a vowel in between
            => <ark + "i", a + r + a + voiced k> ;
          _ + #c -- if the pl2 root ends in consonant, infinitive needs a vowel
            => <ark + "i", ark> ;
          yar + "ee"  -- double e turns into ey
            => <ark + "n", yar + "ey"> ;
          _ => <ark + "n", ark> -- no changes, just add n for infinitive
        } ;
        arki = stems.p1 ;
        arag = stems.p2 ;
        arkin = case last arki of { -- The negative past tense ends in n:
                  "n" => arki ;         -- if infinitive ends in n, no change;
                   _  => arki + "n" } ; -- otherwise add n.

        -- Some predictable sound changes
        t : Str = case arag of {
               _ + ("i"|"y") => "s" ; -- t changes into s in front of i/y:
               _ + ("x"|"q"|"c") => "d" ;
               _             => "t" } ; -- kari+seen, (sug|joogsa|qaada)+teen
        ay : Str = case ark of {
               _ + ("i"|"e") => "ey" ;
               _             => "ay" } ;
        n : Str = case arag of {
               _ + #v => "nn" ; -- n duplicates after vowel
               _      => "n" } ;
        an : Str = case qaado of {
               _ + "o" => "an" ; -- Allomorph for imperatives
               _       => "in" } ;
   in { s = table {
          VPres (Sg1|Sg3 Masc|Impers) pol
                        => qaat + if_then_Str pol "aa" "o" ;
          VPres (Sg2|Sg3 Fem) pol
                        => arag + t + if_then_Str pol "aa" "o" ;
          VPres (Pl1 _) pol
                        => arag + n + if_then_Str pol "aa" "o"  ;
          VPres Pl2 pol => arag + t + "aan" ;
          VPres Pl3 pol => qaat + "aan" ;

          VPast (Sg1|Sg3 Masc|Impers)
                        => qaat + ay ;
          VPast (Sg2|Sg3 Fem)
                        => arag + t + ay ; -- t, d or s
          VPast (Pl1 _) => arag + n + ay ;
          VPast Pl2     => arag + t + "een" ; -- t, d or s
          VPast Pl3     => qaat + "een" ;

          VImp Sg True     => qaado ;
          VImp Pl True     => qaat + "a" ;
          VImp Sg False    => arag + an ;
          VImp Pl False    => qaat + "ina" ;

          VInf             => arki ;
          VNegPast         => arkin }
      } ;

-------------------------
-- Regular verb paradigms

  cSug, cKari, cYaree, cJoogso, cQaado : Str -> Verb ;

  cSug sug =
    let cabb : Str = case sug of {
          _ + "b" => sug + "b" ; -- TODO: more duplication patterns
          _       => sug }
     in mkVerb sug cabb sug ;

  cKari, cYaree = \kari -> mkVerb kari (kari+"y") kari ;

  cJoogso joogso =
    let joogsa = init joogso + "a" ;
     in mkVerb joogso (joogsa + "d") joogsa ;

  cQaado qaado =
    let qaa = drop 2 qaado
     in mkVerb qaado  -- Imperative sg, with the vowel
              (qaa + "t")  -- Per1 Sg, Per3 Pl and Per3 Sg Masc
              (qaa + "da") ;  -- Per2 Pl and others

------------------
-- Irregular verbs

  copula : Verb = {
    s = table {
          VPres Sg1 pol    => if_then_Str pol "ahay" "ihi" ;
          VPres Sg2 pol    => if_then_Str pol "tahay" "ihid" ;
          VPres (Sg3 Masc|Impers) pol => if_then_Str pol "yahay" "aha" ;
          VPres (Sg3 Fem)  pol => if_then_Str pol "tahay" "aha" ;
          VPres (Pl1 _) pol => if_then_Str pol "nahay" "ihin" ;
          VPres Pl2 pol     => if_then_Str pol "tihiin" "ihidin" ;
          VPres Pl3 pol     => if_then_Str pol "yihiin" "aha" ;

          VPast (Sg1|Sg3 Masc|Impers)
                          => "ahaa" ;
          VPast (Sg2|Sg3 Fem)
                          => "ahayd" ;
          VPast (Pl1 _)   => "ahayn" ;
          VPast Pl2       => "ahaydeen" ;
          VPast Pl3       => "ahaayeen" ;
          VNegPast        => "ahi" ;
          VImp Sg pol     => if_then_Str pol "ahaw" "ahaanin" ;
          VImp Pl pol     => if_then_Str pol "ahaada" "ahaanina" ;
          VInf            => "ahaan" }
     } ;

  have_V : Verb =
   let hold_V = mkVerb "hayso" "haysat" "haysa" in {
    s = table {
          VPres Sg1        True => "leeyahay" ;
          VPres Sg2        True => "leedahay" ;
          VPres (Sg3 Fem)  True => "leedahay" ;
          VPres (Sg3 Masc|Impers) True
                                => "leeyahay" ;
          VPres (Pl1 _)    True => "leenahay" ;
          VPres Pl2        True => "leedihiin" ;
          VPres Pl3        True => "leeyihiin" ;
          VPast x               => "l" + copula.s ! VPast x ;
          x                     => hold_V.s ! x }
    } ;

  -- Constructs verbs like u baahan+ahay
  prefixV : Str -> Verb -> Verb = \s,v -> {
    s = \\vf => s + v.s ! vf
  } ;

------------------
-- VP
  Adv : Type = {s,s2 : Str} ; -- Adverb placement is complex

  VP : Type = Verb ** {
    compl : Agreement => {p1,p2 : Str} ;
    isCop : Bool ;
    adv : Adv ;
    c2, c3 : Preposition -- Prepositions contract, need a parameter instead of Str
    } ;

  useV : Verb -> VP = \v -> v ** {
    compl = \\_ => <[],[]> ;
    isCop = False ;
    adv = {s,s2 = []} ;
    c2,c3 = noPrep ;
    } ;

  compl : NP -> VP -> Str = \np,vp ->
    prepCombTable ! np.a ! combine vp.c2 vp.c3 ;

  complV2 : NP -> Verb2 -> Str = \np,vp ->
    prepCombTable ! np.a ! combine vp.c2 noPrep ;

------------------
-- Sentence type markers

  stmarker : Agreement => Bool => Str = \\a,b =>
    let stm = if_then_Str b "waa" "ma"
     in stm ++ subjpron ! a ;

  -- A version that contracts
  -- stmarkerContr : Agreement => Bool => Str = \\a,b =>
  --   let stm = if_then_Str b "w" "m"
  --    in stm + subjpron ! a ;

  subjpron : Agreement => Str = table {
    Sg1|Pl1 _ => "aan" ;
    Sg2|Pl2   => "aad" ;
    Sg3 Masc  => "uu" ;
    _         => "ay" } ;

--------------------------------------------------------------------------------
-- Clause

param
  Tense = Simultanous | Anterior ;
  QForm = Question | Statement ;

oper
  -- choose the right form of the verb
  inflVerb : Tense -> Bool -> Agreement -> VP -> Str = \t,b,a,vp ->
    vp.s ! case <t,b> of {
             <Simultaneous,_> => VPres a b ;
             <Anterior,True>  => VPast a ;
             <Anterior,False> => VNegPast } ;

--------------------------------------------------------------------------------
-- Prepositions and their contractions

  Prep : Type = { s : Agreement => Str } ;

  mkPrep : (x1,_,_,_,_,x6 : Str) -> Prep = \ku,ii,kuu,noo,idiin,loo -> {
    s = table {
          Sg1      => ii ;
          Sg2      => kuu ;
          Pl2      => idiin ;
          Pl1 Excl => noo ;
          Pl1 Incl => "i" + noo ;
          Impers   => loo ;
          _        => ku
        }
    } ;


param
  -- These are exported via MiniParadigmsSom
  Preposition = u | ku | ka | la | noPrep ;

  -- These are completely internal, and user shouldn't need to write these ever
  PrepCombination = ugu | uga | ula | kaga | kula | kala
                  | Single Preposition ;

oper

  combine : Preposition -> Preposition -> PrepCombination = \p1,p2 ->
    let oneWay : Preposition => Preposition => PrepCombination =
          \\x,y => case <x,y> of {
                      <u,u|ku> => ugu ;
                      <u,ka>   => uga ;
                      <u,la>   => ula ;
                      <ku|ka,
                        ku|ka> => kaga ;
                      <ku,la>  => kula ;
                      <ka,la>  => kala ;
                      <noPrep,p> => Single p ;
                      <p,noPrep> => Single p ;
                      <p,_> => Single p } -- for trying both ways
    in case oneWay ! p2 ! p1 of {
              Single x => oneWay ! p1 ! p2 ;
              x        => x } ;

  prepTable : Preposition => Prep = table {
    ku => mkPrep "ku" "igu" "kugu" "nagu" "idinku" "lagu" ; -- Object pronouns fused with ku
    ka => mkPrep "ka" "iga" "kaa" "naga" "idinka" "laga" ;  -- Object pronouns fused with ka
    la => mkPrep "la" "ila" "kula" "nala" "idinla" "lala" ; -- Object pronouns fused with la
    u  => mkPrep "u" "ii" "kuu" "noo" "idiin" "loo" ;       -- Object pronouns fused with u
    noPrep => mkPrep [] "i" "ku" "na" "idin" "la" -- Just object pronouns
  } ;

   showPrep : Preposition -> Str = \p ->
     case p of {
       noPrep => "noPrep" ;
       _      => (prepTable ! p).s ! Pl3
     } ;

  prepCombTable : Agreement => PrepCombination => Str = table {
    Sg1 => table { ugu => "iigu" ; uga => "iiga" ;
                   ula => "iila" ; kaga => "igaga" ;
                   kula => "igula" ; kala => "igala" ;
                   Single x => (prepTable ! x).s ! Sg1 } ;
    Sg2 => table { ugu => "kuugu" ; uga => "kaaga" ;
                   ula => "kuula" ; kaga => "kaaga" ;
                   kula => "kugula" ; kala => "kaala" ;
                   Single x => (prepTable ! x).s ! Sg2 } ;
    Pl1 Excl =>
           table { ugu => "noogu" ; uga => "nooga" ;
                   ula => "noola" ; kaga => "nagaga" ;
                   kula => "nagula" ; kala => "nagala" ;
                   Single x => (prepTable ! x).s ! Pl1 Excl } ;
    Pl1 Incl =>
           table { ugu => "inoogu" ; uga => "inooga" ;
                   ula => "inoola" ; kaga => "inagaga" ;
                   kula => "inagula" ; kala => "inagala" ;
                   Single x => (prepTable ! x).s ! Pl1 Incl } ;

    Pl2 => table { ugu => "idiinku" ; uga => "idiinka" ;
                   ula => "idiinla" ; kaga => "idinkaga" ;
                   kula => "idinkula" ; kala => "idinkala" ;
                   Single x => (prepTable ! x).s ! Pl2 } ;
    Impers =>
           table { ugu => "loogu" ; uga => "looga" ;
                   ula => "loola" ; kaga => "lagaga" ;
                   kula => "lagula" ; kala => "lagala" ;
                   Single x => (prepTable ! x).s ! Impers } ;

    y   => table { ugu => "ugu" ; uga => "uga" ;
                   ula => "ula" ; kaga => "kaga" ;
                   kula => "kula" ; kala => "kala" ;
                   Single x => (prepTable ! x).s ! y }
  } ;
}
