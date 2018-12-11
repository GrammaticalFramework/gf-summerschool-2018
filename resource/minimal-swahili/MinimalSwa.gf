concrete MinimalSwa of Minimal = {

param
  Number = Sg | Pl ;
  
  Person = Per1 | Per2 | Per3 ;
  
  Class  =
     C0102
   | C0304
   | C0506
   | C0708
   | C0910
   | C11
   | C15
   | C16
   | C17
   | C18
   ;

  Agr =
     APron Number Person
   | AClass Class Number
   ;

lincat
  S  = {s : Str} ;
  NP = {s : Str ; a : Agr} ;
  N  = {sg,pl : Str ; c : Class} ;
  V  = {root : Str } ;
  
  PolTemp = {s : Agr => Str * Str ; end : Str} ;
  Rel = {s : Str} ;

lin
  Pred1 poltemp rel verb subj = {
    s = subj.s ++
        (poltemp.s ! subj.a).p1 ++
        subjClitic subj.a ++
        (poltemp.s ! subj.a).p2 ++
	rel.s ++
	verb.root ++
	poltemp.end
    } ;
    
  Pred2 poltemp rel verb subj obj = {
    s = subj.s ++
        (poltemp.s ! subj.a).p1 ++
        subjClitic subj.a ++
        (poltemp.s ! subj.a).p2 ++    
	rel.s ++
	objClitic obj.a ++
	verb.root ++
	poltemp.end ++
	obj.s
    } ;
  
  SingularN n = {s = n.sg ; a = AClass n.c Sg} ;
  PluralN n   = {s = n.pl ; a = AClass n.c Pl} ;

  read_V = {root = "som"} ;
  fall_V = {root = "anguk"} ;

  i_NP     = {s = [] ; a = APron Sg Per1} ;
  youSg_NP = {s = [] ; a = APron Sg Per2} ;
  it_NP    = {s = [] ; a = APron Sg Per3} ;
  we_NP    = {s = [] ; a = APron Pl Per1} ;
  youPl_NP = {s = [] ; a = APron Pl Per2} ;
  they_NP  = {s = [] ; a = APron Pl Per3} ;
  
  man_N = {sg = "mtu" ; pl = "watu" ; c = C0102} ;
  woman_N = {sg = "mwanamke" ; pl = "wanawake" ; c = C0102} ;
  tree_N = {sg = "mti" ; pl = "miti" ; c = C0304} ;
  eye_N = {sg = "jicho" ; pl = "macho" ; c = C0506} ;
  book_N = {sg = "kitabu" ; pl = "vitabu" ; c = C0708} ;
  lamp_N = {sg,pl = "taa" ; c = C0910} ;
  freedom_N = {sg,pl = "uhuru" ; c = C11} ;
  reading_N = {sg,pl = "kusoma" ; c = C15} ;

  PosPres = {
    s = \\a => <[], mkClitic "na"> ;
    end = mkSuffix "a"
    } ;
    
  NegPres = {
    s = \\a => <negClitic a,[]> ;
    end = mkSuffix "i"
    } ;
    
  PosPast = {
    s = \\a => <[], mkClitic "li"> ;
    end = mkSuffix "a"
    } ;

  NegPast = {
    s = \\a => case a of {
          AClass C15 _ => <negClitic a,""> ;
          _ =>  <negClitic a, mkClitic "ku">
	  } ;
    end = mkSuffix "a"
    } ;

  NoRel = {s = []} ;
  When  = {s = mkClitic "po"} ;
  Which = {s = mkClitic "ye"} ;

oper
  subjClitic : Agr -> Str = \a -> case a of {
    APron Sg Per1 => mkClitic "ni" ;
    APron Sg Per2 => mkClitic "u" ;
    APron Sg Per3 => mkClitic "a" ;
    APron Pl Per1 => mkClitic "tu" ;
    APron Pl Per2 => mkClitic "m" ;
    APron Pl Per3 => mkClitic "wa" ;
    AClass C0102 n => mkClitics "a" "wa" n ;
    AClass C0304 n => mkClitics "u" "i" n ;
    AClass C0506 n => mkClitics "li" "ya" n ;
    AClass C0708 n => mkClitics "ki" "vi" n ;
    AClass C0910 n => mkClitics "i" "zi" n ;
    AClass C11 _ => mkClitic "u" ;
    AClass C15 _ => mkClitic "ku" ;
    AClass C16 _ => mkClitic "pa" ;
    AClass C17 _ => mkClitic "ku" ;
    AClass C18 _ => mkClitic "m"
    } ;
    
  objClitic : Agr -> Str = \a -> case a of {
    APron Sg Per1 => mkClitic "ni" ;
    APron Sg Per2 => mkClitic "ku" ;
    APron Sg Per3 => mkClitic "m" ;
    APron Pl Per1 => mkClitic "tu" ;
    APron Pl Per2 => mkClitic "wa" ;
    APron Pl Per3 => mkClitic "wa" ;
    AClass C0102 n => mkClitics "m" "wa" n ;
    AClass C0304 n => mkClitics "u" "ya" n ;
    AClass C0506 n => mkClitics "li" "ya" n ;
    AClass C0708 n => mkClitics "ki" "vi" n ;
    AClass C0910 n => mkClitics "i" "zi" n ;
    AClass C11 _   => mkClitic "u" ;
    AClass C15 _   => mkClitic "ku" ;
    AClass C16 _   => mkClitic "pa" ;
    AClass C17 _   => mkClitic "ku" ;
    AClass C18 _   => mkClitic "m"
    } ;

  negClitic : Agr -> Str = \a -> case a of {
    APron Sg Per1 => mkClitic "si" ;
    APron Sg Per2 => mkClitic "hu" ;
    APron Sg Per3 => mkClitic "h" ; --- h + a = ha
    APron Pl Per1 => mkClitic "ha" ;
    APron Pl Per2 => mkClitic "ha" ;
    APron Pl Per3 => mkClitic "ha" ;
    AClass C0102 Sg => mkClitic "h" ; --- h + a = ha
    AClass _ _ => mkClitic "ha"
    } ;

  mkClitic : Str -> Str = \c -> c ++ Predef.BIND ;
  mkSuffix : Str -> Str = \c -> Predef.BIND ++ c ;

  mkClitics : Str -> Str -> Number -> Str = \sg,pl,n ->
      case n of {Sg => mkClitic sg ; Pl => mkClitic pl} ;


}