concrete MinimalFin of Minimal =
  open
    SyntaxFin,
    (S=SyntaxFin),
    ParadigmsFin,
    ExtraFin
  in
{

lincat
  S  = S.S ;
  NP = S.NP ;
  N = S.N ;
  V = S.V2 ;
  Temp = S.Tense ;
  Rel = Adv ;

lin
  Pred1 temp rel verb subj =
    relS rel (mkS temp (mkCl subj <verb : V>)) ;
    
  Pred2 temp rel verb subj obj =
    relS rel (mkS temp (mkCl subj verb obj)) ;
  
  UseN n = mkNP n ;

--  sleep_V = mkV2 "nukkua" ;
  read_V = mkV2 "lukea" ;

  i_NP  = mkNP (ProDrop i_Pron) ;
  it_NP = mkNP (it_Pron) ;
  
  mom_N = mkN "äiti" ;
  dad_N = mkN "isä" ;

  Pres = presentTense ;
  Past = pastTense ;

  NoRel = ParadigmsFin.mkAdv "" ;
  When = ParadigmsFin.mkAdv "kun" ;

oper
  relS : Adv -> S -> S = \adv,s -> lin S {
    s = adv.s ++ s.s
  } ;

}