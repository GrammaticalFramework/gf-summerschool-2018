abstract Minimal = {

cat
  S ; NP ; N ; V ; Temp ; Rel ;

fun
  Pred1 : Temp -> Rel -> V -> NP -> S ;
  Pred2 : Temp -> Rel -> V -> NP -> NP -> S ;

  UseN : N -> NP ;

--  sleep_V  : V ;
  read_V : V ;

  i_NP : NP ;
  it_NP : NP ;
  
  mom_N : N ;
  dad_N : N ;

  Pres,Past : Temp ;

  NoRel, When : Rel ;



}