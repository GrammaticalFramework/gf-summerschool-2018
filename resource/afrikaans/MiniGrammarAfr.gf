concrete MiniGrammarAfr of MiniGrammar = open Prelude,ResAfr in {

-- collected from GF/lib/src/abstract/*.gf
-- the functions marked ---s are shortcuts
-- the leading comments, e.g. "-- Common", indicate the standard RGL module

  lincat

-- Common
    Utt = { s : Str } ;
    Pol = { s : Str ; p : TPol} ;
    Temp = { s : Str ; t : TTense} ;

-- Cat
    Imp = { s : TPol => Str } ;
    S  = { s : Order => Str ; finNie : Bool } ;
    QS = { s : Order => Str ; finNie : Bool } ;
    Cl = { s : TTense => TPol => Order => Str ; finNie : Bool } ;
    QCl = { s : TTense => TPol => Order => Str ; finNie : Bool } ;
    VP = ResAfr.VP ;
    AP = { s : AForm => Str } ;
    CN = { s : Number => Str ; g : Gender } ;
    NP = { s : Case => Str ; a : Agr ; isPron : Bool ; p : TPol } ;
    Pron = { s : Case => Str ; a : Agr ; isPron : Bool ; p : TPol } ;
    Det = {s : Str ; n : Number ; p : TPol } ;
    Prep = { s : Str } ;
    Conj = { s : Str } ;
    V = Verb ;
    V2 = { v : Verb ; c : Str ; hasC : Bool } ;
    N = { s : Number => Str ; g : Gender } ;
    A = { s : AForm => Str } ;
    PN = { s : Str ; a : Agr } ;
    Adv = { s : Str ; p : TPol } ;

  lin
-- Phrase
    UttS s = { s = s.s ! SVO } ;
    UttQS q = { s = q.s ! VSO ++ "?" } ;

    UttNP np = { s = np.s!Nom } ;
    UttAdv adv = { s = adv.s } ;        -- in the house
    UttImpSg pol imp = { s = imp.s!pol.p } ; -- (do not) walk ----s

-- Sentence
    UseCl t p cl = {
      s = \\o => t.s ++ p.s ++ cl.s!t.t!p.p!o ;
      finNie = cl.finNie --! p.p
    } ;

    UseQCl t p cl = {
      s = \\o => t.s ++ p.s ++ cl.s!t.t!p.p!o ;
      finNie = cl.finNie --! p.p
    } ;

    QuestCl cl = cl ; -- guessing this will work...

    PredVP np vp = {
      s = \\t,p,f =>
        let
          subj = np.s ! Nom ;
          verba = case <(vp.v).vtype,t> of {
            <VAux,TPres> => (vp.v).s!VPres ; -- kan
            <VAux,TPast> => (vp.v).s!VPast ; -- kon
            <VAux,TPerf> => (vp.v).s!VPast ; -- kon
            <_,TPres> => (vp.v).s!VPres ; -- sien
            <VBe,TPast> => (vp.v).s!VPast ;
            <VBe,TPerf> => (vp.v).s!VPres ;
            <VReg,TPast> => [] ;
            <VReg,TPerf> => [] ;
            <_,TFut> => "sal"
          } ;
          verbhet = case <(vp.v).vtype,t> of {
            <VReg,TPast> => "het" ;
            <VReg,TPerf> => "het" ;
            <_,_> => []
          } ;
          verbb = case <(vp.v).vtype,t> of {
            <VAux,TPres> => vp.compV!VInfa ; -- (kan) sien
            <VAux,TPast> => vp.compV!VInfa ; -- (kon) sien
            <VAux,TPerf> => vp.compV!VInfb ; -- (kon) gesien
            <VAux,TFut> => (vp.v).s!VInfa ; -- (sal) kan

            <VReg,TPres> => (vp.v).p ; -- (kyk) op
            <VReg,TPast> => (vp.v).s!VPast ; -- (het) gesien
            <VReg,TPerf> => (vp.v).s!VPerf ; -- (het) gesien
            <VReg,TFut> => (vp.v).s!VInfa ;-- (sal) sien

            <_,_> => []
          } ;
          verbc = case <(vp.v).vtype,t> of {
            <VBe,TPast> => "gewees" ; -- (was) gewees
            <VAux,TPast> => "het" ; -- (kon gesien) het
            <VAux,TFut> => vp.compV!VInfa ; -- (sal kan) sien
            <_,_> => []
          } ;
          obja = case vp.v.vtype of {
            VBe => [] ;
            _ => vp.n2a
          } ;
          objb = case vp.v.vtype of {
            VBe => vp.n2a ;
            _ => vp.n2b
          } ;
          subcl = vp.subCl ;
          adv = vp.adv ;
          neg1 : TPol => Str = table { TPos => [] ; TNeg => putNie (fillNeg1 t vp.filled)} ;--table { Pos => [] ; Neg => case vp.double1 of {True => "nie" ; False => []}} ;
          neg2 : TPol => Str = table { TPos => putNie (fillNeg2Pos np.p vp.nword vp.finNie) ;
                                       TNeg => putNie (fillNeg2Neg vp.finNie) } ;
        in case f of {
          SVO => subj ++ verba ++ verbhet ++ obja ++ neg1!p ++ adv ++ objb ++ verbb ++ verbc ++ subcl ++ neg2!p ;
          SOV => subj ++ obja ++ neg1!p ++ adv ++ objb ++ verba ++ verbb ++ verbhet ++ verbc ++ subcl ++ neg2!p ;
          VSO => verba ++ verbhet ++ subj ++ obja ++ neg1!p ++ adv ++ objb ++ subcl ++ verbb ++ verbc ++ neg2!p
        } ;
      -- finNie = table {
      --   TPos => finNiePos np.p vp.nword vp.finNie ;
      --   TNeg => True
      --   }
      finNie = finNiePos np.p vp.nword vp.finNie
    } ;

    ImpVP vp = {
      s =
        let
          verbapos = (vp.v).s!VPres ; -- kan
          verbaneg = (vp.v).s!VInfa ;
          verbbpos = case (vp.v).vtype of {
            VAux => vp.compV!VInfa ; -- (kan) sien
            _ => (vp.v).p  -- (kyk) op
          } ;
          verbbneg = vp.compV!VInfa ;
          obja = vp.n2a ;
          objb = vp.n2b ;
          subcl = vp.subCl ;
          adv = vp.adv ;
          neg1 : TPol => Str = table { TPos => [] ; TNeg => putNie (fillNeg1 TPres vp.filled)} ;--table { Pos => [] ; Neg => case vp.double1 of {True => "nie" ; False => []}} ;
          neg2 : TPol => Str = table { TPos => putNie (fillNeg2Pos TPos vp.nword vp.finNie) ;
                                       TNeg => putNie (fillNeg2Neg vp.finNie) } ;
        in
          table {
            TPos => verbapos ++ obja ++ neg1!TPos ++ adv ++ objb ++ verbbpos ++ subcl ++ neg2!TPos ;
            TNeg => "moenie" ++ obja ++ adv ++ objb ++ verbbneg ++ verbaneg ++ subcl ++ neg2!TNeg
          }
    } ;

-- Verb
    UseV v = {
      v = v ;
      n2a = [] ;
      n2b = [] ;
      subCl = [] ;
      adv = [] ;
      filled = v.hasPart ; -- True => "hy hou [nie] *op* nie" ; False => "hy loop [] nie"
      nword = False ;
      finNie = False ;
      compV = \\_ => []
    } ;

    ComplV2 v2 np = {
      v = v2.v ;
      inf = <[],[]> ;
      n2a = case <np.isPron,v2.hasC> of {
        <True,False> => v2.c ++ np.s ! Acc ; -- hy sien [my] (nie) nie
        <_,_> => []
      } ;
      n2b = case <np.isPron,v2.hasC> of {
        <True,False> => [] ;
        <_,_> => v2.c ++ np.s ! Acc -- alle ander gevalle: hy kyk nie altyd [na my] nie; hy sien nie [die vrou] nie
      } ;
      subCl = [] ;
      adv = [] ;
      -- filled = case <np.isPron,v2.hasC> of {
      --   <True,False> => False ;
      --   <_,_> => True
      -- } ; ***2018-10-03
      filled = case <np.isPron,v2.hasC> of {
        <True,True> => True ;
        <True,False> => False ;
        <False,_> => True
      } ;
      nword = case np.p of {
        TPos => False ;
        TNeg => True
      } ;
      finNie = False ;
      compV = \\_ => []
    } ;

    UseAP ap = {
      v = be_V ;
      n2a = ap.s!APredic ;
      n2b = [] ;
      subCl = [] ;
      adv = [] ;
      filled = True ; -- True => "hy hou [nie] *op* nie" ; False => "hy loop [] nie"
      nword = False ;
      finNie = False ;
      compV = \\_ => []
    } ;

    UseNP np = {
      v = be_V ;
      n2a = np.s!Nom ;
      n2b = [] ;
      subCl = [] ;
      adv = [] ;
      filled = case np.isPron of {
        True => False ;
        False => True
      } ;
      nword = False ;
      finNie = False ;
      compV = \\_ => []
    } ;

    UseAdv adv = {
      v = be_V ;
      n2a = adv.s ;
      n2b = [] ;
      subCl = [] ;
      adv = [] ;
      filled = False ; -- True => "hy hou [nie] *op* nie" ; False => "hy loop [] nie"
      nword = case adv.p of {
        TPos => False ;
        TNeg => True
      } ;
      finNie = False ;
      compV = \\_ => []
    } ;

    AdvVP vp adv = {
      v = vp.v ;
      inf = vp.inf ;
      n2a = vp.n2a ;
      n2b = vp.n2b ;
      subCl = vp.subCl ;
      adv = adv.s ;
      filled = True ;
      nword = case adv.p of {
        TPos => vp.nword ;
        TNeg => True
      } ;
      finNie = vp.finNie ;
      compV = vp.compV
    } ;

-- Noun
    DetCN det cn = {
     s = \\_ => det.s ++ cn.s ! det.n ;
     a = Ag det.n Per3 cn.g ;
     isPron = False ;
     p = det.p
    } ;

    UsePN pn = {
      s = \\_ => pn.s ;
      a = pn.a ;
      isPron = False ;
      p = TPos
    } ;

    UsePron pron = pron ;
    MassNP cn = {
      s = \\_ => cn.s!Sg ;
      a = Ag Sg Per3 cn.g ;
      isPron = False ;
      p = TPos
    } ;

    a_Det = { s = "'n" ; n = Sg ; p = TPos } ;
    aPl_Det = { s = [] ; n = Pl ; p = TPos } ;
    the_Det = { s = "die" ; n = Sg ; p = TPos } ;
    thePl_Det = { s = "die" ; n = Pl ; p = TPos } ;

    UseN n = n ;
    AdjCN ap cn = {
     s = \\n => ap.s ! AAttrib ++ cn.s ! n ;
     g = cn.g
    } ;

-- Adjective
    PositA adj = adj ;

-- Adverb
    PrepNP prep np = {
      s = prep.s ++ np.s!Acc ;
      p = np.p
    } ;

-- Conjunction
    CoordS conj s1 s2 = {
      s = \\o => s1.s!o ++ conj.s ++ s2.s!o ;
      finNie = s2.finNie
    } ;

-- Tense
    PPos  = {s = [] ; p = TPos} ;
    PNeg  = {s = [] ; p = TNeg} ;
    TSim = {s = [] ; t = TPres} ;
    TAnt = {s = [] ; t = TPast} ;

-- Structural
    and_Conj = { s = "en" } ;
    or_Conj = { s = "of" } ;

    every_Det = { s = "elke" ; n = Sg ; p = TPos } ;

    in_Prep = {s = "in"}  ;
    on_Prep = {s = "op"}  ;
    with_Prep = {s = "met"}  ;

    i_Pron = pronNP "ek" "my" Sg Per1 Neuter TPos ;
    youSg_Pron = pronNP "jy" "jou" Sg Per2 Neuter TPos ;
    he_Pron = pronNP "hy" "hom" Sg Per3 Masc TPos ;
    she_Pron = pronNP "sy" "haar" Sg Per3 Fem TPos ;
    we_Pron = pronNP "ons" "ons" Sg Per1 Neuter TPos ;
    youPl_Pron = pronNP "julle" "julle" Sg Per2 Neuter TPos ;
    they_Pron = pronNP "hulle" "hulle" Sg Per3 Neuter TPos ;

    have_V2 = { v = mkVerb "hê" "hê" "het" "gehad" "gehad" ; c = [] ; hasC = False }  ;
}
