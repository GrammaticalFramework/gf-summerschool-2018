resource MiniSyntaxEng =
  MiniGrammarEng [
    -- Structural words
    Conj, and_Conj, or_Conj,
    Det, every_Det, a_Det, aPl_Det, the_Det, thePl_Det,
    Prep, in_Prep, on_Prep, with_Prep,
    Pron, i_Pron, youSg_Pron, he_Pron, she_Pron,
          we_Pron, youPl_Pron, they_Pron
  ]
  ** open MiniGrammarEng in {

oper

  mkUtt = overload {
    mkUtt : S -> Utt
      = UttS ;
    mkUtt : QS -> Utt
      = UttQS ;
    mkUtt : NP -> Utt
      = UttNP ;
  } ;

  mkS = overload {
    mkS : Temp -> Pol -> Cl -> S
      = UseCl ;
    mkS : Pol -> Cl -> S
      = UseCl TSim ;
    mkS : Temp -> Cl -> S
      = \t -> UseCl t PPos ;
    mkS : Cl -> S
      = UseCl TSim PPos ;
  } ;
  
  mkQS = overload {
    mkQS : Temp -> Pol -> QCl -> QS
      = UseQCl ;
    mkQS : Pol -> QCl -> QS
      = UseQCl TSim ;
    mkQS : Temp -> QCl -> QS
      = \t -> UseQCl t PPos ;
    mkQS : QCl -> QS
      = UseQCl TSim PPos ;
  } ;

  mkCl = overload {
    mkCl : NP -> VP -> Cl
      = PredVP ;
    mkCl : NP -> V -> Cl
      = \np,v -> PredVP np (UseV v) ;
    mkCl : NP -> V2 -> NP -> Cl
      = \np,v,obj -> PredVP np (ComplV2 v obj) ;
    mkCl : NP -> AP -> Cl
      = \np,ap -> PredVP np (UseAP ap) ;
    mkCl : NP -> A -> Cl
      = \np,a -> PredVP np (UseAP (PositA a)) ;
  } ;

  mkQCl = overload {
    mkQCl : Cl -> QCl
      = QuestCl
  } ;

  mkVP = overload {
    mkVP : V -> VP
      = \v -> UseV v ;
    mkVP : V2 -> NP -> VP
      = \v,obj -> ComplV2 v obj ;
    mkVP : AP -> VP
      = \ap -> UseAP ap ;
    mkVP : A -> VP
      = \a -> UseAP (PositA a) ;
    mkVP : VP -> Adv -> VP
      = AdvVP ;

  } ;

  mkNP = overload {
    mkNP : Det -> CN -> NP
      = DetCN ;
    mkNP : Det -> N -> NP
      = \det,n -> DetCN det (UseN n) ;
    mkNP : Pron -> NP
      = UsePron ;
    mkNP : PN -> NP
      = UsePN ;
    mkNP : CN -> NP
      = MassNP ;
    mkNP : N -> NP
      = \n -> MassNP (UseN n) ;
  } ;

{-
  mkUtt = overload {


  } ;

  mkUtt = overload {


  } ;

  mkUtt = overload {


  } ;

  mkUtt = overload {


  } ;

  mkUtt = overload {


  } ;
-}


}