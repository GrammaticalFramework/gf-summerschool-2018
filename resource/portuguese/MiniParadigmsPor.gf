resource MiniParadigmsPor = open

  MiniGrammarPor,
  MiniResPor,
  Prelude

  in {

  oper
    mkN = overload {
      mkN : Str -> N                     = \s -> lin N (smartNoun s) ;
      mkN : Str -> Gender -> N           = \s,g -> lin N (smartGenNoun s g) ;
      mkN : Str -> Str    -> Gender -> N = \sg,pl,g -> lin N (mkNoun sg pl g) ;
      } ;

    mkPN : Str -> Gender -> PN ;
    mkPN s g = lin PN {s = s ; g = g} ;

    mkA = overload {
      mkA : Str             -> A         = \s -> lin A (regAdjective s) ;
      mkA : Str             -> Bool -> A = \s,isPre -> lin A (preAdjective s isPre) ;
      mkA : (_,_,_,_ : Str) -> A =
        \sgm,sgf,plm,plf -> lin A (mkAdjective sgm sgf plm plf False) ;
      mkA : (_,_,_,_ : Str) -> Bool -> A =
        \sgm,sgf,plm,plf,isPre -> lin A (mkAdjective sgm sgf plm plf isPre) ;
      mkA : A -> Bool -> A = \a,b -> a ** {isPre = b} ;
      } ;

    mkV = overload {
      mkV : Str -> V = \s -> lin V (smartVerb s) ;

      mkV : (_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> V =
        \amar,amo,ama,amamos,amam,amei,amou,amamos,amaram,ame,amemos,amem ->
             lin V (mkVerb amar amo
                      ama amamos amam amei amou amamos amaram ame amemos amem) ;
      } ;

    mkV2 = overload {
      mkV2 : Str -> V2 =
        \s   -> lin V2 (smartVerb s ** {c = Acc ; p = []}) ;
      mkV2 : Str -> Case -> V2 =
        \s,c -> lin V2 (smartVerb s ** {c = c ; p = []}) ;
      mkV2 : Str -> Str -> V2 =
        \s,p -> lin V2 (smartVerb s ** {c = Acc ; p = p}) ;
      mkV2 : Str  -> Case -> Str -> V2 =
        \s,c,p -> lin V2 (smartVerb s ** {c = c ; p = p}) ;
      mkV2 : V -> V2 =
        \v -> lin V2 (v ** {c = Acc ; p = []}) ;
      mkV2 : V -> Case -> V2 =
        \v,c -> lin V2 (v ** {c = c ; p = []}) ;
      mkV2 : V -> Str -> V2 =
        \v,p -> lin V2 (v ** {c = Acc ; p = p}) ;
      mkV2 : V -> Case -> Str -> V2 =
        \v,c,p -> lin V2 (v ** {c = c ; p = p}) ;
      } ;

    mkAdv : Str -> Adv ;
    mkAdv s = lin Adv {s = s} ;

    mkPrep = overload {
      mkPrep : Str -> Prep = \s -> lin Prep {s = (mkA s s s s).s} ;
      mkPrep : (_,_,_,_ : Str) -> Prep =
        \sgm,sgf,plm,plf -> lin Prep {s = (mkA sgm sgf plm plf).s} ;
      } ;

} ;
