resource MiniResourceCze = open Prelude in {

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

  DeclensionType : Type = Str ; ----
  
  declensionType : (nom,gen : Str) -> Gender -> Animacy -> DeclensionType
    = \nom,gen,g,a -> case <g,a,nom, gen> of {
      <Masc, Anim,   _ + #hardConsonant, _ + "a"> => "declPAN" ;
      <Masc, Anim,   _ + "a"           , _ + "a"> => "declPAN" ;
      <Masc, Inanim, _ + #hardConsonant, _ + "u"> => "declHRAD" ;
      <Fem,  _,      _ + "a"           , _ + "y"> => "declZENA" ;
      <Neutr,_,      _ + "o"           , _ + "a"> => "declMESTO" ;
      <Masc, Anim,   _ + #softConsonant, _ + "e"> => "declMUZ" ;
      <Masc, Anim,   _ + "tel"         , _ + "e"> => "declMUZ" ;
      <Masc, Anim,   _ + "ce"          , _ + "e"> => "declSOUDCE" ;
      <Masc, Inanim, _ + #softConsonant, _ + "e"> => "declSTROJ" ;
      <Fem,  _,      _ + "e"           , _ + "e"> => "declRUZE" ;
      <Fem,  _,      _ + #softConsonant, _ + "e"> => "declPISEN" ;
      <Fem,  _,      _ + "ost"         , _ + "i"> => "declKOST" ;
      <Neutr,_,      _ + "e"           , _ + "e"> => "declMESTO" ;
      --- but also KURE for "young animals, devce (girl)"
      <Neutr,_,      _ + "í"           , _ + "í"> => "declSTAVENI" ;
      _ => Predef.error ("cannot infer declension type for" ++ nom ++ gen)
      } ;

}