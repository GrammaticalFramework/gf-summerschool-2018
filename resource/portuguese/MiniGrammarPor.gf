--# -path=.:../abstract
concrete MiniGrammarPor of MiniGrammar = open MiniResPor, Prelude in {

  lincat
    Utt  = {s : Str} ;
    Pol  = {s : Str ; isPos : Bool} ;
    Temp = {s : Str ; isPres : Bool} ;

    Imp = {s : Bool => Str} ;

    S  = {s : Str} ;
    QS = {s : Str} ;

    Cl, QCl  = {s : Bool => Bool => Str} ;

    VP = MiniResPor.VP ;

    AP = Adjective ;

    CN = Noun ;
    NP = MiniResPor.NP ;

    Pron = MiniResPor.Pron ;

    Det = {s : Gender => Case => Str ; n : Number} ;
    Conj = {s : Str} ;
    Prep = MiniResPor.Prep ;

    V = Verb ;
    V2 = Verb2 ;

    A = Adjective ;

    N = Noun ;
    PN = ProperName ;

    Adv = Adverb ;

  lin
    -- Phrase
    UttS  s  = s ;
    UttQS qs = qs ;

    UttNP np = ss (employNP Nom np) ;

    UttAdv adv = adv ;

    UttImpSg pol imp = {s = pol.s ++ imp.s ! pol.isPos} ;

    -- Sentence
    UseCl temp pol cl = {
      s = temp.s ++ pol.s ++ cl.s ! pol.isPos ! temp.isPres
      } ;

    UseQCl temp pol qcl = {
      s = temp.s ++ pol.s ++ qcl.s ! pol.isPos ! temp.isPres
      } ;

    QuestCl cl = cl ; -- WIP

    PredVP np vp = let subj = (np.s ! Nom).obj ;
                       obj = vp.compl ! np.a ;
                       clit = vp.clit ;
                       verb = agrV vp.verb np.a
      in {
        s = \\isPos,isPres => subj ++ clit ++ neg isPos ++ verb ! isPres ++ obj
      } ;

    ImpVP vp = {
      -- WIP what about other agreements?
      s = table {
        True  => vp.verb.s ! VImp SgPer2 ++ vp.clit ++ vp.compl ! (Agr Masc Sg Per3) ;
        False => neg False ++ vp.clit ++
          vp.verb.s ! VImp SgPer2 ++ vp.compl ! (Agr Masc Sg Per3)
        }
      } ;

    -- Verb
    UseV v = {
      verb = v ;
      clit = [] ;
      clitAgr = CAgrNo ;
      compl = \\_ => []
      } ;

    ComplV2 v2 np = let nps = np.s ! v2.c in {
      verb = {s = v2.s} ;
      clit = nps.clit ;
      clitAgr = case <nps.isClit,v2.c> of {
        <True,Acc> => CAgr np.a ;
        _          => CAgrNo
        } ;
      compl = \\_ => v2.p ++ nps.obj
      } ;

    UseNP np = { -- WIP
      verb = ser_V ;
      clit = [] ;
      clitAgr = CAgrNo ;
      compl = \\_ => (np.s ! Nom).obj
      } ;

    UseAdv adv = {
      verb = estar_V ;
      clit = [] ;
      clitAgr = CAgrNo ;
      compl = \\_ => adv.s
      } ;

    UseAP ap = {
      verb = ser_V | estar_V ; -- [ ] remove variants
      clit = [] ;
      clitAgr = CAgrNo ;
      compl = \\agr => case agr of {
        Agr g n _ => ap.s ! g ! n
        }
      } ;

    AdvVP vp adv = vp ** {compl = \\agr => vp.compl ! agr ++ adv.s } ;

    -- Noun
    DetCN det cn = {
      s = \\c => {clit = [] ;
                  obj = det.s ! cn.g ! c ++ cn.s ! det.n ;
                  isClit = False
        } ;
      a = Agr cn.g det.n Per3 ;
      } ;

    UsePN pn = {
      s = \\_ => {clit = [] ; obj = pn.s ; isClit = False} ;
      a = Agr pn.g Sg Per3
      } ;

    UsePron p = {
      s = table {
        Nom => {clit = [] ;
                obj = p.s ! Nom ;
                isClit = False} ;
        Acc => {clit = p.s ! Acc ;
                obj = [] ;
                isClit = True}
        } ;
      a = p.a
      } ;

    MassNP cn = {
      s = \\_ => {clit = [] ; obj = cn.s ! Sg ; isClit = False} ;
      a = Agr cn.g Sg Per3
      } ;

    a_Det   = adjDet um_adjDet Sg ;
    aPl_Det = adjDet um_adjDet Pl ;

    the_Det   = adjDet um_adjDet Sg ;
    thePl_Det = adjDet um_adjDet Pl ;

    UseN n = n ;

    AdjCN ap cn = case ap.isPre of {
        True => cn ** {s = table {n => ap.s ! cn.g ! n ++ cn.s ! n}} ;
        False => cn ** {s = table {n => cn.s ! n ++ ap.s ! cn.g ! n}}
      } ;

    -- Adjective
    PositA a = a ;

    -- Adverb
    PrepNP prep np = case np.a of {
      Agr g n _ => {s = prep.s ! g ! n ++ employNP Nom np}
      } ;

    -- Conjunction
    CoordS conj a b = {s = a.s ++ conj.s ++ b.s} ;

    -- Tense
    PPos  = {s = [] ; isPos = True} ;
    PNeg  = {s = [] ; isPos = False} ;

    TSim  = {s = [] ; isPres = True} ;
    TAnt  = {s = [] ; isPres = False} ;

    -- Structural
    and_Conj = {s = "e"} ;
    or_Conj = {s = "ou"} ;

    every_Det = adjDet (mkAdjective "todo" "toda" [] [] True) Sg ;

    in_Prep = no_Prep ;
    on_Prep = no_Prep ;
    with_Prep = {s = \\_ => \\_ => "com"} ;

    i_Pron = iMasc_Pron | genderPron Fem iMasc_Pron ;
    youSg_Pron = youMascSg_Pron | genderPron Fem youMascSg_Pron ;
    he_Pron = {
      s = table {Nom => "ele" ; Acc => "o"} ;
      a = Agr Masc Sg Per3
      } ;
    she_Pron = {
      s = table {Nom => "ela" ; Acc => "a"} ;
      a = Agr Fem Sg Per3
      } ;
    we_Pron    = weMasc_Pron | genderPron Fem weMasc_Pron ;
    youPl_Pron = youMascPl_Pron | genderPron Fem youMascPl_Pron ;
    they_Pron = {
      s = table {Nom => "eles" ; Acc => "os"} ;
      a = Agr Masc Pl Per2
      }
      | {
        s = table {Nom => "elas" ; Acc => "as"} ;
        a = Agr Fem Pl Per2
          } ;

    have_V2 = mkV2 ter_V ;


} ;
