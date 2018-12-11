concrete MinimalFin of Minimal =
  open
    SyntaxFin,
    (S=SyntaxFin),
    ParadigmsFin,
    ExtraFin,
    (L=LexiconFin)
  in
{

lincat
  S  = S.S ;
  NP = S.NP ;
  N = S.N ;
  V = S.V2 ;

  PolTemp = {p : S.Pol ; t : S.Tense} ;
  Rel = Adv ;

lin
  Pred1 poltemp rel verb subj =
    relS rel (mkS poltemp.t poltemp.p (mkCl subj <verb : V>)) ;
    
  Pred2 poltemp rel verb subj obj =
    relS rel (mkS poltemp.t poltemp.p (mkCl subj verb obj)) ;
  
  SingularN n = mkNP n ;
  PluralN n = mkNP aPl_Det n ;

  read_V = mkV2 "lukea" ;
  fall_V = mkV2 "kaatua" ;

  i_NP  = mkNP (ProDrop i_Pron) ;
  youSg_NP  = mkNP (ProDrop youSg_Pron) ;
  it_NP = mkNP it_Pron ;
  we_NP  = mkNP (ProDrop we_Pron) ;
  youPl_NP  = mkNP (ProDrop youPl_Pron) ;
  they_NP = mkNP they_Pron ;
  
  man_N = L.man_N ;
  woman_N = mkN "nainen" ;
  tree_N = mkN "puu" ;
  eye_N = mkN "silmä" ;
  book_N = mkN "kirja" ;
  lamp_N = mkN "lamppu" ;
  freedom_N = mkN "vapaus" ;
  reading_N = mkN "lukeminen" ;

  PosPres = {p = positivePol ; t = presentTense} ;
  NegPres = {p = negativePol ; t = presentTense} ;
  
  PosPast = {p = positivePol ; t = pastTense} ;
  NegPast = {p = negativePol ; t = pastTense} ;

  NoRel = ParadigmsFin.mkAdv "" ;
  When = ParadigmsFin.mkAdv "kun" ;
  Which = ParadigmsFin.mkAdv "jota" ;

oper
  relS : Adv -> S -> S = \adv,s -> lin S {
    s = adv.s ++ s.s
  } ;

}