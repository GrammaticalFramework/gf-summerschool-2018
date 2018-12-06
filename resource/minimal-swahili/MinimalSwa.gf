concrete MinimalSwa of Minimal = {

param
  Number = Sg | Pl ;
  Person = Per1 | Per2 | Per3 ;
  Class = C01 | C09 ;
  Agr =
     APron Number Person
   | AClass Class
   ;

lincat
  S  = {s : Str} ;
  NP,N = {s : Str ; a : Agr} ;
  V  = {root : Str } ;
  Temp = {s : Str} ;
  Rel = {s : Str} ;

lin
  Pred1 temp rel verb subj = {
    s = subj.s ++
        subjClitic subj.a ++
        temp.s ++
	rel.s ++
	verb.root
    } ;
    
  Pred2 temp rel verb subj obj = {
    s = subj.s ++
        subjClitic subj.a ++
        temp.s ++
	rel.s ++
	objClitic obj.a ++
	verb.root ++
	obj.s
    } ;
  
  UseN n = n ;

--  sleep_V = {root = "lala"} ;
  read_V = {root = "soma"} ;

  i_NP  = {s = [] ; a = APron Sg Per1} ;
  it_NP = {s = [] ; a = APron Sg Per3} ;
  
  mom_N = {s = "mama" ; a = AClass C09} ;
  dad_N = {s = "baba" ; a = AClass C09} ;

  Pres = {s = mkClitic "na"} ;
  Past = {s = mkClitic "li"} ;

  NoRel = {s = []} ;
  When = {s = mkClitic "po"} ;

oper
  subjClitic : Agr -> Str = \a -> case a of {
    APron Sg Per1 => mkClitic "ni" ;
    _ => mkClitic "a"
    } ;
    
  objClitic : Agr -> Str = \a -> case a of {
    APron Sg _ => mkClitic "ki" ;
    _ => mkClitic "vi"
    } ;

  mkClitic : Str -> Str = \c -> c ++ Predef.BIND ;

}