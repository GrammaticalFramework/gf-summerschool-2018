resource MiniResCze = open Prelude in {

param
  Number = Sg | Pl ;
  Gender = Masc | Fem | Neutr ;
  Animacy = Anim | Inanim ;
  Case = Nom | Gen | Dat | Acc | Voc | Loc | Ins ; -- traditional order

oper
  hardConsonant : pattern Str = #("d"|"t"|"g"|"h"|"k"|"n"|"r") ;
  softConsonant : pattern Str = #("ť"|"ď"|"j"|"ň"|"ř"|"š"|"c"|"č"|"ž") ;

  dropFleetingE : Str -> Str = \s -> case s of {
    x + "e" + c@("k"|"c"|"ň") => x + c ;
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

  Noun : Type = {s : Number => Case => Str ; g : Gender ; a : Animacy} ;

  mkNoun :
    (snom,sgen,sdat,sacc,svoc,sloc,sins, pnom,pgen,pdat,pacc,ploc,pins : Str) ->
      Gender -> Animacy -> Noun
    = \snom,sgen,sdat,sacc,svoc,sloc,sins, pnom,pgen,pdat,pacc,ploc,pins, g,a -> {
      s = table {
        Sg => table {
	  Nom => snom ;
	  Gen => sgen ;
	  Dat => sdat ;
	  Acc => sacc ;
	  Voc => svoc ;
	  Loc => sloc ;
	  Ins => sins
	  } ;
        Pl => table {
	  Nom | Voc => pnom ;
	  Gen => pgen ;
	  Dat => pdat ;
	  Acc => pacc ;
	  Loc => ploc ;
	  Ins => pins
	  }
	} ;
      g = g ;
      a = a
      } ;
      
  mkrNoun :
    {snom,sgen,sdat,sacc,svoc,sloc,sins, pnom,pgen,pdat,pacc,ploc,pins : Str} ->
      Gender -> Animacy -> Noun
    = \forms, g,a -> {
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
      g = g ;
      a = a
      } ;


  DeclensionType : Type = Str -> Noun ;
  
  declensionType : (nom,gen : Str) -> Gender -> Animacy -> DeclensionType
    = \nom,gen,g,a -> case <g,a,nom, gen> of {
      <Masc, Anim,   _ + #hardConsonant, _ + "a"> => declPAN ;
      <Masc, Anim,   _ + "a"           , _ + "a"> => declPAN ;
      <Masc, Inanim, _ + #hardConsonant, _ + "u"> => declHRAD ;
      <Fem,  _,      _ + "a"           , _ + "y"> => declZENA ;
      <Neutr,_,      _ + "o"           , _ + "a"> => declMESTO ;
      <Masc, Anim,   _ + #softConsonant, _ + "e"> => declMUZ ;
      <Masc, Anim,   _ + "tel"         , _ + "e"> => declMUZ ;
      <Masc, Anim,   _ + "ce"          , _ + "e"> => declSOUDCE ;
      <Masc, Inanim, _ + #softConsonant, _ + "e"> => declSTROJ ;
      <Fem,  _,      _ + "e"           , _ + "e"> => declRUZE ;
      <Fem,  _,      _ + #softConsonant, _ + "e"> => declPISEN ;
      <Fem,  _,      _ + "ost"         , _ + "i"> => declKOST ;
      <Neutr,_,      _ + "e"           , _ + "e"> => declMESTO ;
      --- but also KURE for "young animals, devce (girl)"
      <Neutr,_,      _ + "í"           , _ + "í"> => declSTAVENI ;
      _ => Predef.error ("cannot infer declension type for" ++ nom ++ gen)
      } ;

  declPAN : DeclensionType = \pan ->
    mkrNoun {
      snom      = pan ;
      sgen,sacc = pan + "a" ;
      sdat,sloc = pan + "ovi" ; --- pánu
      svoc      = shortenVowel pan + "e" ;
      sins      = pan + "em" ;

      pnom      = pan + "ové" ; --- páni
      pgen      = pan + "ů" ;
      pdat      = pan + "ům" ;
      pacc,pins = pan + "y" ;
      ploc      = pan + "ech"
      }
      Masc
      Anim
      ;

  declHRAD : DeclensionType = \hrad ->
    mkrNoun {
      snom,sacc = hrad ;
      sgen,sdat = hrad + "u" ;
      sloc      = hrad + "u" ; --- hradě
      svoc      = shortenVowel hrad + "e" ; ---- shorten?
      sins      = hrad + "em" ;

      pnom,pacc,pins = hrad + "y" ;
      pgen           = hrad + "ů" ;
      pdat           = hrad + "ům" ;
      ploc           = hrad + "ech" 
      }
      Masc
      Inanim
      ;

  declZENA : DeclensionType = \zena ->
    let zen = init zena
    in
    mkrNoun {
      snom      = zena ;
      sgen      = zen + "y" ;
      sdat,sloc = zen + "ě" ;
      sacc      = zen + "u" ;
      svoc      = shortenVowel zen + "o" ; ---- shorten ?
      sins      = zen + "ou" ;

      pnom,pacc = zen + "y" ;  --- also sgen
      pgen      = zen ;
      pdat      = zen + "ám" ;
      ploc      = zen + "ách" ;
      pins      = zen + "ami"
      }
      Fem
      Anim ----
      ;

  declMESTO : DeclensionType = \mesto ->
    let mest = init mesto
    in
    mkrNoun {
      snom,sacc = mesto ;                   ---- svoc?
      sgen      = mest + "a" ;
      sdat      = mest + "u" ;
      svoc      = shortenVowel mest + "o" ; ----
      sloc      = mest + "ě" ; --- mestu
      sins      = mest + "em" ;

      pnom,pacc = mest + "a" ;
      pgen      = mest ;
      pdat      = mest + "ům" ;
      ploc      = mest + "ech" ;
      pins      = mest + "y"
      }
      Neutr
      Inanim ----
      ;

  declMUZ : DeclensionType = \muz ->
    mkrNoun {
      snom      = muz ;
      sgen,sacc = muz + "e" ;   --- pacc
      sdat      = muz + "ovi" ; --- muzi
      svoc      = shortenVowel muz + "i" ; ----
      sloc      = muz + "ovi" ; --- muzi
      sins      = muz + "em" ;

      pnom = muz + "ové" ; --- muzi
      pgen = muz + "ů" ;
      pacc = muz + "e" ;
      pdat = muz + "ům" ;
      ploc = muz + "ích" ;
      pins = muz + "i"
      }
      Masc
      Anim
      ;

  declSOUDCE : DeclensionType = \soudce ->
    let soudc = init soudce
    in
    mkNoun
      soudce
      (soudce)
      (soudc + "ovi") --- soudci
      (soudce)
      (shortenVowel soudce) ----
      (soudc + "ovi") --- soudci
      (soudc + "em")

      (soudc + "ové") --- soudci
      (soudc + "ů")
      (soudc + "ům")
      (soudc + "e")
      (soudc + "ích")
      (soudc + "i")
      Masc
      Anim ----
      ;

  declSTROJ : DeclensionType = \stroj ->
    mkNoun
      stroj
      (stroj + "e")
      (stroj + "i")
      (stroj)
      (shortenVowel stroj + "i") ----
      (stroj + "i")
      (stroj + "em")

      (stroj + "e")
      (stroj + "ů")
      (stroj + "ům")
      (stroj + "e")
      (stroj + "ích")
      (stroj + "i")
      Masc
      Inanim
      ;

  declRUZE : DeclensionType = \ruze ->
    let ruz = init ruze
    in
    mkNoun
      ruze
      (ruze)
      (ruz + "i")
      (ruz + "i")
      (ruz + "e") -- no shortening
      (ruz + "i")
      (ruz + "i")

      (ruze)
      (ruz + "í")
      (ruz + "ím")
      (ruz + "e")
      (ruz + "ích")
      (ruz + "emi")
      Fem
      Inanim ----
      ;

  declPISEN : DeclensionType = \pan ->
    mkNoun
      pan
      (pan + "a")
      (pan + "ovi") --- pánu
      (pan + "a")
      (shortenVowel pan + "e")
      (pan + "ovi") --- pánu
      (pan + "em")

      (pan + "ové") --- páni
      (pan + "ů")
      (pan + "ům")
      (pan + "y")
      (pan + "ech")
      (pan + "y")
      Masc
      Anim
      ;

  declKOST : DeclensionType = \pan ->
    mkNoun
      pan
      (pan + "a")
      (pan + "ovi") --- pánu
      (pan + "a")
      (shortenVowel pan + "e")
      (pan + "ovi") --- pánu
      (pan + "em")

      (pan + "ové") --- páni
      (pan + "ů")
      (pan + "ům")
      (pan + "y")
      (pan + "ech")
      (pan + "y")
      Masc
      Anim
      ;

  declSTAVENI : DeclensionType = \pan ->
    mkNoun
      pan
      (pan + "a")
      (pan + "ovi") --- pánu
      (pan + "a")
      (shortenVowel pan + "e")
      (pan + "ovi") --- pánu
      (pan + "em")

      (pan + "ové") --- páni
      (pan + "ů")
      (pan + "ům")
      (pan + "y")
      (pan + "ech")
      (pan + "y")
      Masc
      Anim
      ;

}