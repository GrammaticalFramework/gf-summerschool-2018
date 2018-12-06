concrete MinimalEng of Minimal =
  open
    SyntaxEng,
    (S=SyntaxEng),
    ParadigmsEng,
    IrregEng
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
  read_V = mkV2 IrregEng.read_V ;

  i_NP  = mkNP (i_Pron) ;
  it_NP = mkNP (it_Pron) ;
  
  mom_N = mkN "mom" ;
  dad_N = mkN "dad" ;

  Pres = presentTense ;
  Past = pastTense ;

  NoRel = ParadigmsEng.mkAdv "" ;
  When = ParadigmsEng.mkAdv "when" ;

oper
  relS : Adv -> S -> S = \adv,s -> lin S {
    s = adv.s ++ s.s
  } ;

}