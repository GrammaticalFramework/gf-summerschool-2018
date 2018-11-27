resource MiniParadigmsEng = open

  MiniGrammarEng,
  MiniResEng
  
in {

oper
  mkN = overload {
    mkN : Str -> Noun
      = \n -> lin N (smartNoun n) ;
    mkN : Str -> Str -> Noun
      = \sg,pl -> lin N (mkNoun sg pl) ;
    } ;

  mkPN : Str -> PN = \s -> lin PN {s = s} ;

  mkA : Str -> A
    = \s -> lin A {s = s} ;

  mkV = overload {
    mkV : (inf : Str) -> V
      = \s -> lin V (smartVerb s) ;
    mkV : (inf,pres,part : Str) -> V
      = \inf,pres,part -> lin V (irregVerb inf pres part) ;
    } ;

  mkV2 = overload {
    mkV2 : Str         -> V2
      = \s   -> lin V2 (smartVerb s ** {c = []}) ;
    mkV2 : Str  -> Str -> V2
      = \s,p -> lin V2 (smartVerb s ** {c = p}) ;
    mkV2 : V -> V2
      = \v   -> lin V2 (v ** {c = []}) ;
    mkV2 : V -> Str -> V2
      = \v,p -> lin V2 (v ** {c = p}) ;
    } ;

  mkAdv : Str -> Adv = \s -> lin Adv {s = s} ;
  
  mkPrep : Str -> Prep = \s -> lin Prep {s = s} ;


}