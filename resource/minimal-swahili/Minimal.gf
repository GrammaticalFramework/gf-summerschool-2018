abstract Minimal = {

cat
  S ; NP ; N ; V ; PolTemp ; Rel ;

fun
  Pred1 : PolTemp -> Rel -> V -> NP -> S ;
  Pred2 : PolTemp -> Rel -> V -> NP -> NP -> S ;

  SingularN : N -> NP ;
  PluralN : N -> NP ;

  read_V : V ; -- soma
  fall_V : V ; -- anguka 
  see_V : V ; -- ona 

  i_NP     : NP ;
  youSg_NP : NP ;
  it_NP    : NP ;
  we_NP    : NP ;
  youPl_NP : NP ;
  they_NP  : NP ;

  man_N : N ;  -- mtu,watu 0102
  woman_N : N ; -- mwamanke,wanawake 0102
  tree_N : N ; -- mti,miti 0304
  eye_N : N ;  -- jicho,macho 0506
  book_N : N ; -- kitabu,vitabu 0708
  lamp_N : N ; -- taa,taa 0910
  freedom_N : N ; -- uhuru 11
  reading_N : N ; -- kusoma 15 

  PosPres, NegPres : PolTemp ;

  PosPast, NegPast : PolTemp ;

  NoRel, When, Which : Rel ;

}