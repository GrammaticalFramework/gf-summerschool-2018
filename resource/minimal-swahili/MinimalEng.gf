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
  
  PolTemp = {p : S.Pol ; t : S.Tense} ;
  Rel = Adv ;

lin
  Pred1 poltemp rel verb subj =
    relS rel (mkS poltemp.t poltemp.p (mkCl subj <verb : V>)) ;
    
  Pred2 poltemp rel verb subj obj =
    relS rel (mkS poltemp.t poltemp.p (mkCl subj verb obj)) ;
  
  SingularN n = mkNP n ;
  PluralN n = mkNP aPl_Det n ;

  read_V = mkV2 IrregEng.read_V ;
  fall_V = mkV2 IrregEng.fall_V ;

  i_NP  = mkNP i_Pron ;
  youSg_NP  = mkNP youSg_Pron ;
  it_NP = mkNP it_Pron ;
  we_NP  = mkNP we_Pron ;
  youPl_NP  = mkNP youPl_Pron ;
  they_NP = mkNP they_Pron ;
  
  man_N = mkN "man" "men" ;
  woman_N = mkN "woman" "women" ;
  tree_N = mkN "tree" ;
  eye_N = mkN "eye" ;
  book_N = mkN "book" ;
  lamp_N = mkN "lamp" ;
  freedom_N = mkN "freedom" ;
  reading_N = mkN "reading" ;

  PosPres = {p = positivePol ; t = presentTense} ;
  NegPres = {p = negativePol ; t = presentTense} ;
  
  PosPast = {p = positivePol ; t = pastTense} ;
  NegPast = {p = negativePol ; t = pastTense} ;

  NoRel = ParadigmsEng.mkAdv "" ;
  When = ParadigmsEng.mkAdv "when" ;
  Which = ParadigmsEng.mkAdv "which" ;

oper
  relS : Adv -> S -> S = \adv,s -> lin S {
    s = adv.s ++ s.s
  } ;

}