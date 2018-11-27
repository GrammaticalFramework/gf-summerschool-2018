concrete MiniGrammarEng of MiniGrammar = open MiniResEng, Prelude in {


  lincat
    Utt = {s : Str} ;
    Pol  = {s : Str ; isTrue : Bool} ;
    Temp = {s : Str ; isPres : Bool} ;
    
    S  = {s : Str} ;
    QS = {s : Str} ;
    Cl  = {subj : Str ; verb : Bool => Bool => {fin,inf : Str} ; compl : Str} ;
    QCl = {subj : Str ; verb : Bool => Bool => {fin,inf : Str} ; compl : Str} ;
    VP = {verb : GVerb ; compl : Str} ;
    AP = Adjective ;
    CN = Noun ;
    NP = {s : Case => Str ; a : Agreement} ;
    Pron = {s : Case => Str ; a : Agreement} ;
    Det = {s : Str ; n : Number} ;
    Conj = {s : Str} ;
    Prep = {s : Str} ;
    V = Verb ;
    V2 = Verb2 ;
    A = Adjective ;
    N = Noun ;
    PN = {s : Str} ;
    Adv = {s : Str} ;

  lin
    UttS s = s ;
    UttQS s = s ;
    UttNP np = {s = np.s ! Acc} ;

    UseCl temp pol cl =
      let clt = cl.verb ! pol.isTrue ! temp.isPres
      in {
        s = cl.subj ++ clt.fin ++ pol.s ++ temp.s ++ clt.inf ++ cl.compl
      } ;
      
    UseQCl temp pol cl =
      let clt = cl.verb ! False ! temp.isPres
      in {
        s = clt.fin ++ cl.subj ++ pol.s ++ temp.s ++ clt.inf ++ cl.compl
      } ;

    QuestCl cl = cl ;

    PredVP np vp = {
      subj = np.s ! Nom ;
      compl = vp.compl ;
      verb = \\plain,isPres => case <vp.verb.isAux, plain, isPres, np.a> of {

        -- non-auxiliary verbs, negative/question present
        <False,False,True,Agr Sg Per3> => {fin = "does" ; inf = vp.verb.s ! VF Inf} ;
        <False,False,True,_          > => {fin = "do"   ; inf = vp.verb.s ! VF Inf} ;
	
        -- non-auxiliary, plain present ; auxiliary, all present
        <_,    _,   True, Agr Sg Per1> => {fin = vp.verb.s ! PresSg1    ; inf = []} ;
        <_,    _,   True, Agr Sg Per3> => {fin = vp.verb.s ! VF PresSg3 ; inf = []} ;
        <_,    _,   True, _>           => {fin = vp.verb.s ! PresPl     ; inf = []} ;

        -- all verbs, past
        <_,   _,    False,Agr Sg Per3> => {fin = "has"  ; inf = vp.verb.s ! VF PastPart} ;
        <_,   _,    False,_          > => {fin = "have" ; inf = vp.verb.s ! VF PastPart} 

      }
    } ;
      
    UseV v = {
      verb = verb2gverb v ;
      compl = []
      } ;
    ComplV2 v2 np = {
      verb = verb2gverb v2 ;
      compl = v2.c ++ np.s ! Acc
      } ;
    UseAP ap = {
      verb = be_GVerb ;
      compl = ap.s
      } ;
    AdvVP vp adv =
      vp ** {compl = vp.compl ++ adv.s} ;
      
    DetCN det cn = {
      s = table {c => det.s ++ cn.s ! det.n} ;
      a = Agr det.n Per3
      } ;
    UsePN pn = {
      s = \\_ => pn.s ;
      a = Agr Sg Per3
      } ;
    UsePron p =
      p ;
    MassNP cn = {
      s = \\_ => cn.s ! Sg ;
      a = Agr Sg Per3
      } ;
    a_Det = {s = "a" ; n = Sg} ;
    aPl_Det = {s = "" ; n = Pl} ;
    the_Det = {s = "the" ; n = Sg} ;
    thePl_Det = {s = "the" ; n = Pl} ;
    UseN n =
      n ;
    AdjCN ap cn = {
      s = table {n => ap.s ++ cn.s ! n}
      } ;

    PositA a = a ;

    PrepNP prep np = {s = prep.s ++ np.s ! Acc} ;

    CoordS conj a b = {s = a.s ++ conj.s ++ b.s} ;
    
    PPos  = {s = []    ; isTrue = True} ;
    PNeg  = {s = "not" ; isTrue = False} ;

    TSim  = {s = []    ; isPres = True} ;
    TAnt  = {s = []    ; isPres = False} ;

    and_Conj = {s = "and"} ;
    or_Conj = {s = "or"} ;

    every_Det = {s = "every" ; n = Sg} ;

    in_Prep = {s = "in"} ;
    on_Prep = {s = "on"} ;
    with_Prep = {s = "with"} ;

    i_Pron = {
      s = table {Nom => "I" ; Acc => "me"} ;
      a = Agr Sg Per1
      } ;
    youSg_Pron = {
      s = \\_ => "you" ;
      a = Agr Sg Per2
      } ;
    he_Pron = {
      s = table {Nom => "he" ; Acc => "him"} ;
      a = Agr Sg Per3
      } ;
    she_Pron = {
      s = table {Nom => "she" ; Acc => "her"} ;
      a = Agr Sg Per3
      } ;
    we_Pron = {
      s = table {Nom => "we" ; Acc => "us"} ;
      a = Agr Pl Per1
      } ;
    youPl_Pron = {
      s = \\_ => "you" ;
      a = Agr Pl Per2
      } ;
    they_Pron = {
      s = table {Nom => "they" ; Acc => "them"} ;
      a = Agr Pl Per2
      } ;

}
