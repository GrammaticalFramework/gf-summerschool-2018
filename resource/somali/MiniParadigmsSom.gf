resource MiniParadigmsSom = open

  MiniGrammarSom,
  MiniResSom

in {

oper
-- These are just declarations, definitions start after --.

-- Parameters exported from MiniResSom
  Gender : Type ;
  masc, fem : Gender ;

  -- Nouns
  mkN : overload {
    mkN : Str -> N ;                     -- Predictable nouns
    mkN : Str -> Gender -> N ;           -- Predictable inflection, but unexpected gender
    mkN : (sg,pl : Str) -> Gender -> N ; -- Irregular, need singular and plural
  } ;

  -- Adjectives

  mkA : overload {
    mkA : (yar : Str) -> A ;
    mkA : (sg,pl : Str) -> A
  } ;

  -- Verbs
  mkV : overload {
    mkV : (imp : Str) -> V ;    -- Predictable verbs, imperative form
    mkV : (imp,sg1,pl2 : Str) -> V ; -- Less predictable verbs: imperative, 1st person singular and 2nd person plural
    mkV : Str -> V -> V  -- Add a prefix to an existing verb, e.g. u baahan+ahay
  } ;

  copula : V ; -- The copula verb 'ahay'

-- Transitive verb (V2) needs an optional preposition
-- Due to preposition contraction (with head noun and with each other),
-- we need to keep them as parameters instead of strings;
-- once we know all the prepositions and their heads, can we choose a string.
  Preposition : Type ;
  u, ku, ka, la : Preposition ;

  mkV2 : overload {
    mkV2 : Str -> V2 ;  -- Predictable V2, no preposition
    mkV2 : V -> V2 ;    -- Make a V2 out of a V, no preposition
    mkV2 : Str -> Preposition -> V2 ; -- Predictable V2, specify preposition
    mkV2 : V -> Preposition -> V2 ;  -- Make a V2 out of a V and a preposition
    } ;

-- Adverbs
  mkAdv : Str -> Adv ;

--------------------------------------------------------------------------------
--.
-- Definitions of the API

  Gender = MiniResSom.Gender ;
  masc = MiniResSom.Masc ;
  fem  = MiniResSom.Fem ;

  -- Top-level, overloaded constructor with different concrete implementations
  mkN = overload {
    mkN : Str -> Noun =
      \s -> lin N (mkN1 s) ; -- Predictable nouns
    mkN : Str -> Gender -> Noun =
      \s,g -> lin N (mkNg s g) ; -- Unexpected gender
    mkN : (_,_ : Str) -> Gender -> Noun =
      \sg,pl,g -> lin N (nMaalin sg pl g) ; -- Consonant cluster + epenthetic vowel
  } ;

  -- Noun constructor with 1 string argument, pattern matches the string
  -- and chooses the appropriate low-level constructor from MiniResSom
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

  -- Like mkN1, but takes gender as an argument, and checks for tricky cases
  mkNg : Str -> Gender -> Noun = \n,g -> case n of {
    _ + "ad" -- Most nouns that end in "ad" are feminine
          => case g of {
                  Fem  => nUl n ;
                  Masc => nUl n ** {g = Masc} -- Force masc. gender
             } ;
    _ + ("r"|"n"|"l"|"g")
          => case g of {
                  Fem  => nUl n ;
                  Masc => mkN1 n
             } ;
      _   => mkN1 n
   } ; -- TODO: add more patterns

-- A
  mkA = overload {
    mkA : (yar : Str)   -> A = \s -> lin A (duplA s) ;
    mkA : (sg,pl : Str) -> A = \s,p -> lin A (mkAdj s p)
    } ;

-- V

-- Top-level, overloaded constructor (so far only one option)
  mkV = overload {
    mkV : (imp : Str) -> V = \v -> lin V (mk1V v) ;
    mkV : (imp,pl2,sg1 : Str) -> V = \i,p,s -> lin V (mkVerb i p s) ;
    mkV : Str -> V -> V = \s,v -> lin V (prefixV s v)
  } ;

  copula = MiniResSom.copula ;
  -- Verb constructor with 1 string argument, pattern matches the string
  -- and chooses the appropriate low-level constructor from MiniResSom
  mk1V : Str -> Verb = \s -> case s of {
    _ + #c + #c + "o" => cJoogso s ;
    _           + "o" => cQaado s ;
    _           + "i" => cKari s ;
    _          + "ee" => cYaree s ;
    _                 => cSug s
    } ;

-- V2
  Preposition = MiniResSom.Preposition ;
  u  = MiniResSom.u ;
  ku = MiniResSom.ku ;
  ka = MiniResSom.ka ;
  la = MiniResSom.la ;

  mkV2 = overload {
    mkV2 : Str -> V2 = \s -> lin V2 (mk1V s ** {c2 = noPrep}) ;
    mkV2 : V -> V2 = \v -> lin V2 (v ** {c2 = noPrep}) ;
    mkV2 : Str -> Preposition -> V2 = \s,p -> lin V2 (mk1V s ** {c2 = p}) ;
    mkV2 : V -> Preposition -> V2 = \v,p -> lin V2 (v ** {c2 = p})
  } ;

-- Adv

  mkAdv : Str -> Adv = \s -> lin Adv {s = [] ; s2 = s} ; -- The first slot is for preposition
}
