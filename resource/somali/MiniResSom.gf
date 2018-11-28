resource MiniResSom = open Prelude, Predef in {

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
-- Nominal morphology

param
  Number = Sg | Pl ;
  Case = Nom | Abs ;
  Gender = Masc | Fem ;
  Person = Per1 | Per2 | Per3 ;
  Vowel = A | E | I | O | U ; -- For vowel assimilation

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
               table { A => ilk+"ah" ;
                       E => ilk+"eh" ;
                       I => ilk+"ih" ;
                       O => ilk+"oh" ;
                       U => ilk+"uh"
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

      -- Based on the table on page 21--TODO find generalisations in patterns
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

oper

   --TODO: make patterns actually adjusted to Somali, these are just copied from elsewhere
  v : pattern Str = #("a" | "e" | "i" | "o" | "u") ;
  vv : pattern Str = #("aa" | "ee" | "ii" | "oo" | "uu") ;
  c : pattern Str = #("m" | "n" | "p" | "b" | "t" | "d" | "k" | "g" | "f" | "v" | "s" | "h" | "l" | "j" | "r" | "z" | "c" | "q" | "y" | "w") ;
  lmnr : pattern Str = #("l" | "m" | "n" | "r") ;
  kpt : pattern Str = #("k" | "p" | "t") ;
  gbd : pattern Str = #("g" | "b" | "d") ;


  mkN = overload {
    mkN : Str -> Noun = mkN1 ;
    mkN : Str -> Gender -> Noun = mkNg ;
    mkN : (_,_ : Str) -> Gender -> Noun = nMaalin ;
    mkN : Noun -> Gender -> Noun = \n,g ->
      n ** { g = g } ;
  } ;

  mkN1 : Str -> Noun = \n -> case n of {
      _ + ("ad"|"adh") => nUl n ;
      _ + "o"          => nHooyo n ;
      _ + "e"          => nAabbe n ;
      _ + "ri"         => nGuri n ;
      (#c + #v + #v + #c) -- One syllable words
       | (#v + #v + #c)
       | (#c + #v + #c)
       | (#v + #c)     => nMas n ;
      _                => nXayawaan n } ;

  mkNg : Str -> Gender -> Noun = \n,g -> case n of {
      _ + ("r"|"n"|"l"|"g")
          => case g of {
                  Fem  => nUl n ;
                  Masc => mkN1 n } ;
      _   => mkN1 n
   } ; -- TODO: add more exceptional cases

  ---------------------------------------------
  -- NP

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

  mkDet : (x1,_,x3 : Str) -> NForm -> Det = mkDetBind True ;

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
  AForm = AF Number Case ; ---- TODO: past tense

oper

-- Sequences of adjectives follow the rules for restrictive relatives clauses, i.e. are linked by oo 'and' on an indefinite head NP and by ee 'and' on a definite NP (8.1).

 -- Komparativ
 -- För att uttrycka motsvarigheten till svenskans komparativ placerar man på somaliska helt enkelt prepositionen ká 'från, av, än' framför adjektivet i fråga. Adjektivet får ingen ändelse.
 -- Shan waa ay ká yar tahay siddéed. Fem är mindre än åtta.
 -- Superlativ
 -- Motsvarigheten till svenskans superlativ bildas med prepositionsklustret ugú som till sin betydelse närmast motsvarar svenskans allra, t.ex.
 -- ugu horrayntii (det att komma) allra först

  Adjective : Type = { s : AForm => Str } ;

  mkA : Str -> Adjective = \yar ->
    let yaryar = duplicate yar
    in { s = table {
           AF Sg Abs => yar ;
           AF Pl Abs => yaryar ;
           AF Sg Nom => yar + "i" ;
           AF Pl Nom => yaryar + "i" }
       } ;

  duplicate : Str -> Str = \yar -> case yar of {
    "dheer" => "dhaadheer" ;
    "weyn"  => "waaweyn" ; -- TODO eventually handle irregular adjectives elsewhere
    y@#c + a@#v + r@#c + _ => y + a + r + yar ;
    g@#c + aa@#vv      + _ => g + aa + yar ; --TODO: proper patterns
    _                      => yar + ":plural" } ;

--------------------------------------------------------------------------------
-- Verb

param
   VForm =
     VInf
   | VPres Agreement Bool
   | VNegPast
   | VPast Agreement
   | VImp Number Bool ;

-- TODO:
-- tre aspekter (enkel, progressiv, habituell),
-- fem modus (indikativ, imperativ, konjunktiv, kontiditonalis, optativ)

oper


  Verb : Type = { s : VForm => Str } ;

  Verb2 : Type = Verb ** { c2 : Preposition } ;

  mkVerb : (imperative,p2pl,p1sg : Str) -> Verb = \qaado,ark,qaat ->
    let stems : {p1 : Str ; p2 : Str} = case ark of {
          a + r@#c + k@#c -- two consonants need a vowel in between
            => <ark + "i", a + r + a + voiced k> ;
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
                        => arag + t + ay ;
          VPast (Pl1 _) => arag + n + ay ;
          VPast Pl2     => arag + t + "een" ;
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

  cSug sug = mkVerb sug sug sug ; -- TODO: stem/dictionary form of verbs with consonant clusters?

  cKari, cYaree = \kari -> mkVerb kari kari (kari+"y") ;

  cJoogso joogso =
    let joogsa = init joogso + "a" ;
     in mkVerb joogso joogsa (joogsa + "d") ;

  cQaado qaado =
    let qaa = drop 2 qaado
     in mkVerb qaado  -- Imperative sg, with the vowel
              (qaa + "da")  -- Per2 Pl and others
              (qaa + "t") ; -- Per1 Sg, Per3 Pl and Per3 Sg Masc

  -- Smart paradigms
  mkV : Str -> Verb = \s -> case s of {
    _ + #c + #c + "o" => cJoogso s ;
    _           + "o" => cQaado s ; ----
    _           + "i" => cKari s ;
    _          + "ee" => cYaree s ;
    _                 => cSug s
    } ;


  mkV2 = overload {
    mkV2 : Str -> Verb2 = \s -> mkV s ** {c2 = noPrep} ;
    mkV2 : Verb -> Verb2 = \v -> v ** {c2 = noPrep} ;
    mkV2 : Str -> Preposition -> Verb2 = \s,p -> mkV s ** {c2 = p}
    } ;
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
   let hold_V = mkVerb "hayso" "haysa" "haysat" in {
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
  Preposition = u | ku | ka | la | noPrep ;
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
    ku => mkPrep "ku" "igu" "kugu" "nagu" "idinku" "lagu" ;
    ka => mkPrep "ka" "iga" "kaa" "naga" "idinka" "laga" ;
    la => mkPrep "la" "ila" "kula" "nala" "idinla" "lala" ;
    u  => mkPrep "u" "ii" "kuu" "noo" "idiin" "loo" ;
    noPrep => mkPrep [] "i" "ku" "na" "idin" "la"
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
