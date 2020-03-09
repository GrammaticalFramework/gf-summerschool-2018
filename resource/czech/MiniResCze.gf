resource MiniResCze = open Prelude in {

param
  Number = Sg | Pl ;

  Animacy = Anim | Inanim ;
  Gender = Masc Animacy | Fem | Neutr ;

  Case = Nom | Gen | Dat | Acc | Voc | Loc | Ins ; -- traditional order

oper
  hardConsonant : pattern Str = #("d"|"t"|"g"|"h"|"k"|"n"|"r") ;
  softConsonant : pattern Str = #("ť"|"ď"|"j"|"ň"|"ř"|"š"|"c"|"č"|"ž") ;

  dropFleetingE : Str -> Str = \s -> case s of {
    x + "e" + c@("k"|"c") => x + c ;
    x + "e" + "ň" => x + "n" ;
    _ => s
    } ;

  shortenVowel : Str -> Str = \s -> case s of {
    x + "á" + y => x + "a" + y ;
    x + "é" + y => x + "e" + y ;
    x + "í" + y => x + "i" + y ;
    x + "ý" + y => x + "y" + y ;
    x + "ó" + y => x + "o" + y ;
    x + "ú" + y => x + "u" + y ;
    x + "ů" + y => x + "o" + y ;
    _ => s
    } ;

  addI : Str -> Str = \s -> case s of {
    klu + "k"  => klu + "ci" ;
    vra + "h"  => vra + "zi" ;
    ce  + "ch" => ce  + "ši" ;
    dokto + "r" => dokto + "ři" ;
    pan => pan + "i"
    } ;

  -- 3.4.10, in particular when also final 'a' is dropped
  addE : Str -> Str = \s -> case s of {
    re + "k"   => re + "ce" ;
    pra + ("g"|"h") => pra + "ze" ;
    stre + "ch" => stre  + "še" ;
    sest + "r" => sest + "ře" ;
    pan => pan + "ě"
    } ;

  addEch : Str -> Str = \s -> case s of {
    klu + "k" => klu + "cich" ;
    vra + ("h"|"g") => vra + "zich" ;
    ce  + "ch" => ce + "šich" ;
    pan => pan + "ech"
    } ;

  shortFemPlGen : Str -> Str = \s -> case s of {
    ul  + "ice" => ul + "ic" ;
    koleg + "yně" => koleg + "yň" ;
    ruz + "e" => ruz + "i" ;
    _ => Predef.error ("shortFemPlGen does not apply to" ++ s)
    } ; 
		    
---------------

  Noun : Type = {s : Number => Case => Str ; g : Gender} ;

  mkNoun :
    {snom,sgen,sdat,sacc,svoc,sloc,sins, pnom,pgen,pdat,pacc,ploc,pins : Str} ->
      Gender -> Noun
    = \forms, g -> {
      s = table {
        Sg => table {
	  Nom => forms.snom ;
	  Gen => forms.sgen ;
	  Dat => forms.sdat ;
	  Acc => forms.sacc ;
	  Voc => forms.svoc ;
	  Loc => forms.sloc ;
	  Ins => forms.sins
	  } ;
        Pl => table {
	  Nom | Voc => forms.pnom ;
	  Gen => forms.pgen ;
	  Dat => forms.pdat ;
	  Acc => forms.pacc ;
	  Loc => forms.ploc ;
	  Ins => forms.pins
	  }
	} ;
      g = g
      } ;


  DeclensionType : Type = Str -> Noun ;
  
  declensionType : (nom,gen : Str) -> Gender -> DeclensionType
    = \nom,gen,g -> case <g, nom, gen> of {
      <Masc Anim,   _ + #hardConsonant, _ + "a"> => declPAN ;
      <Masc Anim,   _ + "a"           , _ + "a"> => declPREDSEDA ;
      <Masc Inanim, _ + #hardConsonant, _ + "u"> => declHRAD ;
      <Fem,         _ + "a"           , _ + "y"> => declZENA ;
      <Neutr,       _ + "o"           , _ + "a"> => declMESTO ;
      <Masc Anim,   _ + #softConsonant, _ + "e"> => declMUZ ;
      <Masc Anim,   _ + "tel"         , _ + "e"> => declMUZ ;
      <Masc Anim,   _ + "ce"          , _ + "e"> => declSOUDCE ;
      <Masc Inanim, _ + #softConsonant, _ + "e"> => declSTROJ ;
      <Fem,         _ + ("e"|"ě")     , _ + ("e"|"ě")> => declRUZE ;
      <Fem,         _ + #softConsonant, _ + "e"> => declPISEN ;
      <Fem,         _ + "ost"         , _ + "i"> => declKOST ;  --- also many other "st" 3.6.3
      <Neutr,       _ + "e"           , _+"ete"> => declKURE ;
      <Neutr,       _ + "e"           , _ + "e"> => declMORE ;
      <Neutr,       _ + "í"           , _ + "í"> => declSTAVENI ;
      _ => Predef.error ("cannot infer declension type for" ++ nom ++ gen)
      } ;

-- source: https://en.wikipedia.org/wiki/Czech_declension

  declPAN : DeclensionType = \pan ->  --- plural nom ové|i|é should be given as extra arg 3.5.1 
    mkNoun {
      snom      = pan ;
      sgen,sacc = pan + "a" ;
      sdat,sloc = pan + "ovi" ; --- pánu
      svoc      = shortenVowel pan + "e" ; --- "irregular shortening" 3.5.1
      sins      = pan + "em" ;

      pnom      = addI pan ;       -- pani, kluk-kluci --- panové, host-hosté
      pgen      = pan + "ů" ;
      pdat      = pan + "ům" ;
      pacc,pins = pan + "y" ;
      ploc      = addEch pan 
      }
      (Masc Anim)
      ;
      
  declPREDSEDA : DeclensionType = \predseda -> --- 3.5.4: sgen y/i
    let predsed = init predseda
    in
    mkNoun {
      snom      = predseda ;
      sgen      = predsed + "y" ; -- pacc,pins --- i
      sdat,sloc = predsed + "ovi" ;
      sacc      = predsed + "u" ;
      svoc      = predsed + "o" ;
      sins      = predsed + "ou" ;

      pnom      = case predseda of {
        tur + "ista" => tur + "isté" ;
        _ => predsed + "ové"
	} ;
      pgen      = predsed + "ů" ;
      pdat      = predsed + "ům" ;
      pacc,pins = predsed + "y" ;
      ploc      = addEch predsed
      }
      (Masc Anim)
      ;

  declHRAD : DeclensionType = \hrad -> --- 3.5.2: sloc u/ě/e  extra arg, sport-u, hrad-ě ; sgen u/a
    let hrd = dropFleetingE hrad
    in
    mkNoun {
      snom,sacc = hrad ;
      sgen,sdat = hrd + "u" ; --- Berlín-a
      sloc      = hrd + "u" ; --- addE hrad ;  -- stůl-stole
      svoc      = hrd + "e" ;
      sins      = hrd + "em" ;

      pnom,pacc,pins = hrd + "y" ;
      pgen           = hrd + "ů" ;
      pdat           = hrd + "ům" ;
      ploc           = addEch hrd
      }
      (Masc Inanim)
      ;

  declZENA : DeclensionType = \zena -> --- 3.6.1 sge y/i ; pgen sometimes shortening
    let zen = init zena
    in
    mkNoun {
      snom      = zena ;
      sgen      = zen + "y" ;  --- i after soft cons sometimes
      sdat,sloc = zen + "ě" ;  --- i after soft cons sometimes
      sacc      = zen + "u" ;
      svoc      = shortenVowel zen + "o" ; ---- shorten ?
      sins      = zen + "ou" ;

      pnom,pacc = zen + "y" ;  --- also sgen
      pgen      = zen ; --- sometimes with vowel shortening
      pdat      = zen + "ám" ;
      ploc      = zen + "ách" ;
      pins      = zen + "ami"
      }
      Fem
      ;

  declMESTO : DeclensionType = \mesto -> --- 3.7.1 sloc u/e ; pgen vowel shortening sometimes ; ploc variations
    let mest = init mesto
    in
    mkNoun {
      snom,sacc,svoc = mesto ;
      sgen      = mest + "a" ;
      sdat      = mest + "u" ;
      sloc      = mest + "u" ; --- "ě"
      sins      = mest + "em" ;

      pnom,pacc = mest + "a" ;
      pgen      = mest ;  --- léta - let
      pdat      = mest + "ům" ;
      ploc      = mest + "ech" ; --- with variations
      pins      = mest + "y"
      }
      Neutr
      ;

  declMUZ : DeclensionType = \muz_ -> --- 3.5.3 : sdat,sloc ; pnom
    let muz = dropFleetingE muz_
    in
    mkNoun {
      snom      = muz_ ;
      sgen,sacc = muz + "e" ;   --- pacc
      sdat,sloc = muz + "i" ;   --- muzovi
      svoc      = case muz_ of {
        chlap + "ec" => chlap + "če" ;
        _ => muz + "i"
	} ;
      sins      = muz + "em" ;

      pnom = case muz_ of {
        uci + "tel" => uci + "tele" ;
        _ => muz + "i"  --- muzové
	} ;
      pgen = muz + "ů" ;
      pacc = muz + "e" ;
      pdat = muz + "ům" ;
      ploc = muz + "ích" ;
      pins = muz + "i"
      }
      (Masc Anim)
      ;

  declSOUDCE : DeclensionType = \soudce ->   --- 3.5.3: sdat/sloc i,ovi ; pnom i/ové
    let soudc = init soudce
    in
    mkNoun {
      snom,sgen,sacc,svoc = soudce ;      ---- pacc
      sdat,sloc           = soudc + "i" ; --- soudcovi
      sins                = soudc + "em" ;

      pnom                = soudc + "i" ; --- soudcové
      pgen                = soudc + "ů" ;
      pdat                = soudc + "ům" ;
      pacc                = soudce ;
      ploc                = soudc + "ích" ;
      pins                = soudc + "i"
      }
      (Masc Anim)
      ;

  declSTROJ : DeclensionType = \stroj ->
    mkNoun {
      snom,sacc      = stroj ;
      sgen           = stroj + "e" ; --- pnom,pacc
      sdat,svoc,sloc = stroj + "i" ; --- pins ---- svoc shorten?
      sins           = stroj + "em" ;

      pnom,pacc      = stroj + "e" ;
      pgen           = stroj + "ů" ;
      pdat           = stroj + "ům" ;
      ploc           = stroj + "ích" ;
      pins           = stroj + "i"
      }
      (Masc Inanim)
      ;

  declRUZE : DeclensionType = \ruze -> --- 3.6.2: pgen ulice-ulic, chvile-cvil
    let ruz = init ruze
    in
    mkNoun {
      snom,sgen,svoc      = ruze ; --- pnom,pacc
      sdat,sacc,sloc,sins = ruz + "i" ; 

      pnom,pacc = ruze ;
      pgen      = shortFemPlGen ruze ;
      pdat      = ruz + "ím" ;
      ploc      = ruz + "ích" ;
      pins      = ruz + "emi"
      }
      Fem
      ;

  declPISEN : DeclensionType = \pisen ->
    let pisn = dropFleetingE pisen 
    in
    mkNoun {
      snom,sacc      = pisen ;
      sgen           = pisn + "ě" ;
      sdat,svoc,sloc = pisn + "i" ; -- not shortened
      sins           = pisn + "í" ;

      pnom,pacc      = pisn + "ě" ;
      pgen           = pisn + "í" ;
      pdat           = pisn + "ím" ;
      ploc           = pisn + "ích" ;
      pins           = pisn + "ěmi"
      }
      Fem
      ;

  declKOST : DeclensionType = \kost ->
    mkNoun {
      snom,sacc           = kost ;
      sgen,sdat,svoc,sloc = kost + "i" ; --- pnom,pacc
      sins                = kost + "í" ; --- pgen

      pnom,pacc      = kost + "i" ;
      pgen           = kost + "í" ;
      pdat           = kost + "em" ;
      ploc           = kost + "ech" ;
      pins           = kost + "mi"
      }
      Fem
      ;

  declKURE : DeclensionType = \kure ->
    let kur = init kure
    in
    mkNoun {
      snom,sacc,svoc = kure ;
      sgen           = kur  + "ete" ;
      sdat,sloc      = kur  + "eti" ;
      sins           = kur  + "etem" ;

      pnom,pacc = kur + "ata" ;
      pgen      = kur + "at" ;
      pdat      = kur + "atům" ;
      ploc      = kur + "atech" ;
      pins      = kur + "aty"
      }
      Neutr
      ;
      
  declMORE : DeclensionType = \more -> --- 3.7.2 pgen zero sometimes
    let mor = init more
    in
    mkNoun {
      snom,sgen,sacc,svoc = more ;      --- pnom
      sdat,sloc           = mor + "i" ; --- pins
      sins                = mor + "em" ;

      pnom,pacc = more ;
      pgen      = mor + "í" ;  --- 
      pdat      = mor + "ím" ;
      ploc      = mor + "ích" ; 
      pins      = mor + "i"
      }
      Neutr
      ;
      
  declSTAVENI : DeclensionType = \staveni ->
    mkNoun {
      snom,sgen,sdat,sacc,svoc,sloc = staveni ;
      sins                          = staveni + "m" ;

      pnom,pgen,pacc = staveni ;
      pdat           = staveni + "m" ;
      ploc           = staveni + "ch" ;
      pins           = staveni + "mi"
      }
      Neutr
      ;

}