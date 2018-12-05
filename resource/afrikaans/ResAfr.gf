resource ResAfr = open Prelude, Predef in {

  param
    Number = Sg | Pl ;
    Case   = Nom | Acc ;
    Agr    = Ag Number Person Gender ;
    TTense = TPres | TPerf | TPast | TFut ;
    TPol = TPos | TNeg ;
    Person = Per1 | Per2 | Per3 ;
    Gender = Masc | Fem | Neuter ;

    VType = VReg | VAux | VBe ;

    AForm = APredic | AAttrib | AGen ;

    VForm =         --!
       VInfa     -- wees / ophou / loop
     | VInfb     -- is / ophou / loop
     | VPres     -- is / hou op / loop
     | VPast     -- was / hou op / geloop
     | VPerf     -- gewees / opgehou / geloop
     ;

    Order = SVO | SOV | VSO ;

    oper

    -- worst case opers
      mkNoun : Str -> Str -> {s : Number => Str} =
      \ev,mv -> {s = table {Sg => ev ; Pl => mv} } ;

      mkAdj : Str -> Str -> Str -> {s : AForm => Str} =
      \ap,aa,ag -> {s = table {APredic => ap ; AAttrib => aa ; AGen => ag} } ;

      mkVerb : Str -> Str -> Str -> Str -> Str -> Verb =
      \vinfa,vinfb,vpres,vpast,vperf ->
      {
          s = table {
            VInfa => vinfa ;
            VInfb => vinfb ;
            VPres => vpres ;
            VPast => vpast ;
            VPerf => vperf
          } ;
          p = [] ;
          hasPart = False ;
          vtype = VReg
      } ;

      mkAux : Str -> Str -> Verb =
      \vpres,vpast ->
      {
          s = table {
            VInfa => vpres ;
            VInfb => vpast ;
            VPres => vpres ;
            VPast => vpast ;
            VPerf => vpast
          } ;
          p = [] ;
          hasPart = False ;
          vtype = VAux
      } ;

      mkVerbPart : Str -> Str -> Str -> Str -> Str -> Str -> Verb =
      \vinfa,vinfb,vpres,vpast,vperf,part ->
      {
          s = table {
            VInfa => vinfa ;
            VInfb => vinfb ;
            VPres => vpres ;
            VPast => vpast ;
            VPerf => vperf
          } ;
          p = part ;
          hasPart = True ;
          vtype = VReg
      } ;

      regVerb : Str -> Verb = \hou ->
      {
         s = table {
           VInfa => hou ;
           VInfb => hou ;
           VPres => hou ;
           VPast => case hou of {
             "e"+_ => "geë"+(drop 1 hou) ;
             "i"+_ => "geï"+(drop 1 hou) ;
             _ => "ge"+hou
           } ;
           VPerf => case hou of {
             "e"+_ => "geë"+(drop 1 hou) ;
             "i"+_ => "geï"+(drop 1 hou) ;
             _ => "ge"+hou
           }
         } ;
         p = [] ;
         hasPart = False ;
         vtype = VReg
       } ;

       regVerbPart : Str -> Str -> Verb = \hou,op ->
       {
          s = table {
            VInfa => op+hou ;
            VInfb => op+hou ;
            VPres => hou ;
            VPast => case hou of {
              "e"+_ => op+"geë"+(drop 1 hou) ;
              "i"+_ => op+"geï"+(drop 1 hou) ;
              _ => op+"ge"+hou
            } ;
            VPerf => case hou of {
              "e"+_ => op+"geë"+(drop 1 hou) ;
              "i"+_ => op+"geï"+(drop 1 hou) ;
              _ => op+"ge"+hou
            }
          } ;
          p = op ;
          hasPart = True ;
          vtype = VReg
        } ;

        be_V : Verb = {
          s = table {
            VInfa => "wees" ;
            VInfb => "wees" ;
            VPres => "is" ;
            VPast => "was" ;
            VPerf => "was"
          } ;
          p = [] ;
          hasPart = False ;
          vtype = VBe
        } ;

      -- mkVerb2 : Verb -> Str -> Verb = \v,c ->
      -- {
      --   s = v.s ;
      --   p = v.p ;
      --   hasPart = v.hasPart ;
      --   c = c
      -- } ;

    -- regular case opers
      regNoun : Str -> {s : Number => Str} = \s -> case s of {
        _ + #cons + ("i" | "o" | "u" ) => mkNoun s (s + "'s") ; --ski, ski's --R13.7
        #cons* + ("ie" | "oe") =>mkNoun s (s + "ë") ; --knie, knieë --R13.10
        #cons* + ("ee") =>mkNoun s (init s + "ë") ; --fee, feë --R13.10
        #cons* + "a" => mkNoun s (s + "'s") ; --ma, ma's R13.7
        _ + ("a" | "e" | "ie" | "ee" | "é" | "ê" | "ô") => mkNoun s (s + "s") ; --gogga, goggas --R13.5

        b + v@("oo") + "g" => mkNoun s (b + init v + "ë") ; --boog, boë --R13.11
        b + v@("e"|"ie"|"o"|"oe") + "g" => mkNoun s (b + v + "ë") ; --kroeg, kroeë --R13.11
        b + v@("aa") + "g" => mkNoun s (b + init v + "e") ; --kraag, krae --R13.11
        b + v@("a") + "g" => mkNoun s (b + v + "e") ; --dag, dae --R13.11
        b + v@("ei"|"eu"|"oe"|"ou"|"ie"|"y"|"ui") + "g" => mkNoun s (b + v + "e") ; --tuig, tuie --R13.1

        _ + ("oir" | "ion" | "je") => mkNoun s (s + "s") ; --uit Nederlandse reël

        _ + ("rm" | "lm") => mkNoun s (s + "s") ; --R13.3

        ? + ? + ? + _ +
          ("ël" |"el" | "em" | "um" | "ing" | "or" | "ior" | "er" | "êr" | "erd" | "aar" | "aard" | "ier") => -- unstressed
                                              mkNoun s (s + "s") ; --R13.3

        ? + ? + _ + (#cons + "en") => mkNoun s (s + "s") ; --R13.3


        _ +                     ("i"|"u")  => mkNoun s (s + "e") ; --R13.4
        b + v@("aa"|"ee"|"oo"|"uu") + c@?  => mkNoun s (b + shortVoc v c + "e") ; --brood, brode --R13.1
        b + ("ei"|"eu"|"oe"|"ou"|"ie"|"y"|"ui") + ? => mkNoun s (endCons s + "e") ; --geluid, geluide --R13.1
        b + v@("a"|"e"|"i"|"o"|"u" ) + "f" => mkNoun s (b + v + "ww" + "e") ; --stof, stowwe --R13.1
        b + v@("a"|"e"|"i"|"o"|"u" ) + c@? => mkNoun s (b + v + c + c + "e") ; --dak, dakke --R13.1
        _ => mkNoun s (endCons s + "e") --R13.1
        } ;

      shortVoc : Str -> Str -> Str = \v,s -> init v + endCons s ;

      endCons : Str -> Str = \s -> case s of {
          _ + ("ts" |"rs" | "ls" | "ds" | "ns" | "ms") => s ;
          b + "f" => b + "w" ;
          _ => s
      } ;

      vowel : pattern Str = #("a"|"e"|"i"|"o"|"u") ;
      cons : pattern Str = #("b"|"c"|"d"|"f"|"g"|"h"|"j"|"k"|"l"|"m"|"n"|"p"|"q"|"r"|"s"|"t"|"v"|"w"|"x"|"z") ;
      dupCons : pattern Str = #("b"|"d"|"f"|"g"|"k"|"l"|"m"|"n"|"p"|"r"|"s"|"t") ;

      regAdj : Str -> {s : AForm => Str} = \s ->
        let
          se : Str = case s of {
            b + v@("aal"|"baar"|"eel"|"loos") => b + init (init v) + last v + "e" ; --p288
            _ + ("agtig"|"ant"|"ent"|"êr"|"ies"|"ig"|"lik"|"matig"|"s") => s + "e" ; --p288
            b + "ief" => b + "iewe" ; --p288

            --b + ("ei"|"eu"|"oe"|"ou"|"ie"|"y"|"ui") + ?  => endCons s + "e" ;
            b + v@("ou"|"y") + "d"  => b + v + "e" ; --koud, koue / wyd, wye

            --b + v@("oo"|"ee") + "d" => b + init v + "ë" ; --leeg, leë
            b + v@("oo"|"ee") + ("g"|"d") => b + init v + "ë" ; --leeg, leë
            b + v@("e"|"ie"|"o"|"oe") + "g" => b + v + "ë" ; --moeg, moeë
            b + v@("aa") + "g" => b + init v + "e" ; --vaag, vae
            b + v@("a") + "g" => b + v + "e" ; --kan nog nie aan 'n voorbeeld dink nie

            b + v@("aa"|"ee"|"oo"|"uu") + "r" => s ; --duur, duur
            b + v@("oo") + "t" => s ; --groot, groot
            b + v@("aa"|"ee"|"oo"|"uu") + c@#cons => b + shortVoc v c + "e" ; --gaaf, gawe
            b + v@("a"|"e"|"i"|"o"|"u" ) + "f" => b + v + "ww" + "e" ; --grof, growwe
            --b + v@("a"|"e"|"i"|"o"|"u" ) + c@? => b + v + c + c + "e" ; --stom, growwe
            _ + "d" => s + "e" ; --p286
            _ => s
            } ;
        in
        mkAdj s se (s+"s") ;

      regAdv : Str -> TPol -> { s : Str ; p : TPol } = \adv,pol ->
      {
        s = adv ;
        p = pol
      } ;

      Verb : Type = {
          s : VForm => Str ;
          p : Str ; -- "op" van "ophou"
          hasPart : Bool ; -- is 'n deeltjiewerkwoord
          vtype : VType
      } ;

    -- in building a clause, the final nie is always inserted for negative polarity, the initial nie depends on other factors
      VP : Type = {
          v  : Verb ;
          n2a, n2b, subCl : Str ; -- two slots for object noun phrase
          --n2b : Str ;
          --n3  : Str ;
          --hasPrep : Bool ; -- TMP: VP already contains a prep NP
          --nPerson : Bool ; -- true: V2 direct obj is person, V3 indir object is person
          adv : Str ; -- vinnig
          --adV : Str ; -- altyd/nooit
          filled : Bool ; -- insert first "nie" if sentence polarity is negative, because certain slots are filled (hy loop nie/hy loop *nie* goed nie)
          nword : Bool ; -- the vp contains the equivalent of an nword, which forces the second "nie"
          finNie : Bool ; -- final "nie" present irrespective of clause polarity; overrides any need for a final "nie"
          --double2 : Bool ;  -- always insert second "nie", because of n-word (niemand *nie*), unless a final "nie" is already present...
          --objNeg : Bool ;   -- a final "nie" is present in an object NP
          --inf : Str * Bool ;
          --ppart : Str * Bool ;
          --ext : Str ;
          --subcl : Str * Bool * Bool -- <str, isPresent, finNie>
          compV : VForm => Str
      } ;

      addCompV : Verb -> (VForm => Str) -> (VForm => Str) = \v,compv ->
        \\vf => v.s!vf ++ compv!vf ;

      pronNP : Str -> Str -> Number -> Person -> Gender -> TPol ->
               {s : Case => Str ; a : Agr ; isPron : Bool ; p : TPol } =
      \ek,my,num,per,g,p ->
      {
        s = table { Nom => ek ; Acc => my } ;
        a = Ag num per g ;
        isPron = True ;
        p = p
      } ;

      fillNeg1 : TTense -> Bool -> Bool = \t,b ->
      case <t,b> of {
        <TPres,True> => True ;
        <TPres,False> => False ;
        <_,_> => True
      } ;

      -- receives np.p of the subject and vp.nword
      fillNeg2Pos : TPol -> Bool -> Bool -> Bool = \p,nword,finNie ->
        case finNie of {
          True => False ;
          False => case p of {
                    TPos => nword ;
                    TNeg => True
                  }
      } ;

      finNiePos : TPol -> Bool -> Bool -> Bool = \p,nword,finNie ->
        case finNie of {
          True => True ;
          False => case p of {
                    TPos => nword ;
                    TNeg => True
                  }
      } ;

      fillNeg2Neg : Bool -> Bool = \finNie ->
        case finNie of {
          True => False ;
          False => True
      } ;

      putNie : Bool -> Str = \b ->
        case b of {
          True => "nie" ;
          False => []
        } ;
}
