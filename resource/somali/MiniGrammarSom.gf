concrete MiniGrammarSom of MiniGrammar = open
  (M=MiniResSom), -- qualified import: can write M.someOper instead of full name, MiniResSom.someOper
  MiniResSom, -- import also without qualification, so we can write just someOper
  Prelude in {





  lincat
-- Common
    Utt = SS ;
    Pol = {s : Str ; p : Bool} ;
    Temp = {s : Str ; t : Tense} ;

-- Cat
    Imp = {s : Number => Bool => Str} ;
    S,
    QS = SS ;
    Cl  = {s : QForm => Tense => Bool => Str} ;
    QCl = {s : Tense => Bool => Str} ;
    VP = M.VP ;
    AP = Adjective ;
    CN = CNoun ;
    NP,
    Pron = M.NP ; -- M.NP because the name NP would be otherwise ambiguous
    Det = M.Det ; -- M.Det because the name Det would be otherwise ambiguous
    Conj = {s : Str} ;
    Prep = M.Prep ;
    V = Verb ;
    V2 = Verb2 ;
    A = Adjective ;
    N = Noun ;
    PN = M.NP ;
    Adv = M.Adv ;


  lin
-- Phrase
    UttS s = s ;
    UttQS qs = qs ;
    UttNP np = { s = np.s ! Abs } ;
    UttAdv adv = {s = adv.s ++ adv.s2} ;
    UttImpSg pol imp = {s = imp.s ! Sg ! pol.p} ;

-- Sentence
    UseCl temp pol cl = {
      s = temp.s ++ pol.s ++ cl.s ! Statement ! temp.t ! pol.p
      } ;
    UseQCl temp pol qcl = {
        s = temp.s ++ pol.s ++ qcl.s ! temp.t ! pol.p
        } ;
    QuestCl cl = {s = cl.s ! Question} ;
    PredVP np vp = let compl = vp.compl ! np.a in {
      s = \\q,t,b =>
           if_then_Str np.isPron [] (np.s ! Nom) -- Subject, if it's not pronoun
        ++ compl.p1  -- object, if it's a noun
        ++ case <q,b,vp.isCop,np.a> of { --sentence type marker + subj. pronoun
              <Statement,True,True,Sg3 _> => "waa" ;
              <Question,_,_,_> => "ma" ; -- question looks like negation,
                                         -- but subject pronoun is not included.
              _ => case <np.isPron,b> of {
                     <True,True>  => "waa" ++ np.s ! Nom ; -- subj is pronoun
                     <True,False> => "ma"  ++ np.s ! Nom ; -- subj is pronoun
                     <False,_>    => stmarker ! np.a ! b -- subj is not pronoun
           }}
        ++ vp.adv.s
        ++ compl.p2  -- object, if it's a pronoun
	      ++ M.inflVerb t b np.a vp -- the verb form picked: we need tense, polarity and subject
        ++ vp.adv.s2
      } ;

    ImpVP vp =  {
      s = \\n,b =>
        let compl = vp.compl ! p2agr n -- Imperative is for 2nd person, we just don't know yet if it's Sg or Pl.
         in vp.s ! VImp n b ++ compl.p1 ++ vp.adv.s ++ compl.p2 ++ vp.adv.s2 } ;

-- Verb
    UseV = useV ;

    ComplV2 v2 np = useV v2 ** {
      compl = \\a =>
        case np.isPron of {
          True  => {p1 = [] ; -- object is a pronoun => nothing is placed before the verb
                    p2 = complV2 np v2} ; -- object combines with the preposition of the verb
          False => {p1 = np.s ! Abs ; -- object is a noun => it will come before verb in the sentence
                    p2 = complV2 np v2}  -- object combines with the preposition of the verb
        } ;
      isCop = False ; -- There is an actual verb, not copula ("X is Y")
      c2 = v2.c2
      } ;

    UseAP ap = useV copula ** {
      compl = \\a => <[], ap.s ! AF (getNum a) Abs> ;
      isCop = True -- the verb is copula, e.g. "the child is small"
      } ;

    UseNP np = useV copula ** {
      compl = \\a => <[], np.s ! Abs> ;
      isCop = True -- the verb is copula, e.g. "the child is a boy"
    } ;

    UseAdv adv = useV copula ** {
      compl = \\a => <[], adv.s ++ adv.s2> ;
      isCop = True -- the verb is copula, e.g. "the child is in the house"
    } ;

    AdvVP vp adv = vp ** { adv = adv } ;

    DetCN det cn = useN cn ** {
      s = \\c =>
           let nfc : {nf:NForm ; c:Case} =
             case <c,cn.hasMod,det.d> of {
                <Nom,True, Indef Sg>  => {nf=Indef Sg  ; c=Abs} ;
                <Nom,False,Indef Sg>  => {nf=IndefNom  ; c=Nom} ; -- special form for fem. nouns
                <Nom,True,Def x vA>  => {nf=Def x vA ; c=Abs} ;
                <Nom,False,Def x vA> => {nf=Def x vU ; c=Nom} ;
                _                     => {nf=det.d ; c=c}
             } ;
            in cn.s ! nfc.nf
            ++ det.s ! nfc.c
            ++ cn.mod ! getNum (getAgr det.d Masc) ! c ;
      a = getAgr det.d cn.g ;
      stm = stmarker ! getAgr det.d cn.g
      } ;
    UsePN,
    UsePron = \p -> p ;

    MassNP cn = useN cn ** {
      s = table { Nom => cn.s ! IndefNom ++ cn.mod ! Sg ! Nom ;
                  Abs => cn.s ! Indef Sg ++ cn.mod ! Sg ! Abs }
      } ;

    UseN = useN ;

    a_Det = mkDet [] "uu" [] (Indef Sg) ;
    aPl_Det = mkDet [] "ay" [] (Indef Pl) ;
    the_Det = mkDetBind True "a" "kani" "tani" (Def Sg vA) ;
    thePl_Det = mkDetBind True "a" "kuwan" "kuwan" (Def Pl vA) ;

    AdjCN ap cn = cn ** {
      s = table { IndefNom => cn.s ! Indef Sg ; -- When an adjective is added, noun loses case marker.
                  x        => cn.s ! x } ;
      mod = \\n,c => cn.mod ! n ! Abs -- If there was something before, it loses its case marker
                  ++ ap.s ! AF n c ;
      hasMod = True
      } ;

    PositA a = a ;

    PrepNP prep np = {
      s = prep.s ! np.a ;
      s2 = case np.isPron of {
            True  => [] ; -- pronoun is included in the preposition's "agreement" already, don't add it
            False => np.s ! Abs } -- if the NP is a noun, add it
      } ;

    CoordS conj a b = {s = a.s ++ conj.s ++ b.s} ;

    PPos  = {s = [] ; p = True} ;
    PNeg  = {s = [] ; p = False} ;

    TSim  = {s = [] ; t = Simultanous} ;
    TAnt  = {s = [] ; t = Anterior} ;

-- Structural
    and_Conj = {s = "oo"} ; -- iyo for nouns

    or_Conj = {s = "ama"} ; -- mise with interrogatives

    every_Det = mkDet "kasta" "kasta" "kasta" (Indef Sg) ;

    -- This is complicated, because prepositions combine with each other.
    in_Prep,
    on_Prep = prepTable ! ku ;
    with_Prep = prepTable ! la ;

    i_Pron = {
      s = table {Nom => "aan" ; Abs => "i"} ;
      a = Sg1 ; isPron = True ; sp = "aniga"
      } ;
    youSg_Pron = {
      s = table {Nom => "aad" ; Abs => "ku"} ;
      a = Sg2 ; isPron = True ; sp = "adiga"
      } ;
    he_Pron = {
      s = table {Nom => "uu" ; Abs => []} ;
      a = Sg3 Masc ; isPron = True ; sp = "isaga"
      } ;
    she_Pron = {
      s = table {Nom => "ay" ; Abs => []} ;
      a = Sg3 Fem ; isPron = True ; sp = "iyada"
      } ;
    we_Pron = {
      s = table {Nom => "aan" ; Abs => "na"} ;
      a = Pl1 Incl ; isPron = True ; sp = "innaga"
      } ;
    youPl_Pron = {
      s = table {Nom => "aad" ; Abs => "idin"} ;
      a =  Pl2 ; isPron = True ; sp = "idinka"
      } ;
    they_Pron = {
      s = table {Nom => "ay" ; Abs => []} ;
      a = Pl3 ; isPron = True ; sp = "iyaga"
      } ;

    have_V2 = M.have_V ** {c2 = noPrep} ;
}
