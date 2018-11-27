incomplete resource MiniSyntax =
  open MiniGrammar

in {

oper

  mkUtt = overload {
    mkUtt : S -> Utt
      = UttS ;
    mkUtt : QS -> Utt
      = UttQS ;
    mkUtt : NP -> Utt
      = UttNP ;
    mkUtt : Adv -> Utt
      = UttAdv ;
    mkUtt : Pol -> Imp -> Utt
      = UttImpSg ;
    mkUtt : Imp -> Utt
      = UttImpSg PPos
  } ;

  mkImp = overload {
    mkImp : VP -> Imp
      = ImpVP ;
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
    mkS : Conj -> S -> S -> S
      = CoordS ;
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

  positivePol : Pol
    = PPos ;
  negativePol : Pol
    = PNeg ;

  simultaneousAnt : Temp
    = TSim ;
  anteriorAnt : Temp
    = TAnt ;

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
      = UseV ;
    mkVP : V2 -> NP -> VP
      = ComplV2 ;
    mkVP : AP -> VP
      = UseAP ;
    mkVP : A -> VP
      = \a -> UseAP (PositA a) ;
    mkVP : NP -> VP
      = UseNP ;
    mkVP : Adv -> VP
      = UseAdv ;
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

    i_NP : NP
      = UsePron i_Pron ;
    you_NP : NP
      = UsePron youSg_Pron ;
    he_NP : NP
      = UsePron he_Pron ;
    she_NP : NP
      = UsePron she_Pron ;

  mkCN = overload {
    mkCN : N -> CN
      = UseN ;
    mkCN : AP -> CN -> CN
      = AdjCN ;
    mkCN : A -> N -> CN
      = \a,n -> AdjCN (PositA a) (UseN n) ;
    mkCN : A -> CN -> CN
      = \a,cn -> AdjCN (PositA a) cn ;
  } ;

  mkAP = overload {
    mkAP : A -> AP
      = PositA ;
  } ;
  
  mkAdv = overload {
    mkAdv : Prep -> NP -> Adv
      = PrepNP ;
  } ;

}