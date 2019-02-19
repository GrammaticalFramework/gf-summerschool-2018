incomplete concrete DoctorFunctor of Doctor =
  open
    Syntax,
    Lexicon,
    Prelude
  in {
lincat
  Phrase = Phr ; -- Arabic Utt has gender parameter
  Fact = Cl ;
  Action = VP ;
  Property = VP ;
  Profession = CN ;
  Person = NP ;
  Place = {at,to : Adv} ;
  Substance = NP ;
  Illness = NP ;

lin
  presPosPhrase fact = mkPhr (mkUtt (mkS fact)) ;
  presNegPhrase fact = mkPhr (mkUtt (mkS negativePol fact)) ;
  pastPosPhrase fact = mkPhr (mkUtt (mkS anteriorAnt fact)) ;
  pastNegPhrase fact = mkPhr (mkUtt (mkS anteriorAnt negativePol fact)) ;
  presQuestionPhrase fact = let p : Phr = mkPhr (mkUtt (mkQS (mkQCl fact))) in p ** {s = invQMark ++ SOFT_BIND ++ p.s ++ SOFT_BIND ++ qMark} ;
  pastQuestionPhrase fact = let p : Phr = mkPhr (mkUtt (mkQS anteriorAnt (mkQCl fact))) in p ** {s = invQMark ++ SOFT_BIND ++ p.s ++ SOFT_BIND ++ qMark} ;

  impPosPhrase action = mkPhr (mkUtt (mkImp action)) ;
  impNegPhrase action = mkPhr (mkUtt negativePol (mkImp action)) ;

  actionFact person action = mkCl person action ;
  propertyFact person property = mkCl person property ;

  isProfessionProperty profession = mkVP (mkNP a_Det profession) ;
  needProfessionProperty profession = mkVP need_V2 (mkNP a_Det profession) ;
  isAtPlaceProperty place = mkVP place.at ;
  haveIllnessProperty illness = mkVP have_V2 illness ;

  theProfessionPerson profession = mkNP the_Det profession ;

  iMascPerson = i_NP ;
  iFemPerson = i_NP ;
  youMascPerson = you_NP ;
  youFemPerson = you_NP ;
  hePerson = he_NP ;
  shePerson = she_NP ;

  goToAction place = mkVP (mkVP Lexicon.go_V) place.to ;
  stayAtAction place = mkVP (mkVP stay_V) place.at ;
  vaccinateAction person = mkVP vaccinate_V2 person ;
  examineAction person = mkVP examine_V2 person ;
  takeSubstanceAction substance = mkVP take_V2 substance ;

  sleepAction = mkVP Lexicon.sleep_V ;
  breatheAction = mkVP Lexicon.breathe_V ;
  eatAction = mkVP <Lexicon.eat_V2 : V> ;
  drinkAction = mkVP <Lexicon.drink_V2 : V> ;

  doctorProfession = mkCN Lexicon.doctor_N ;
  haveChildrenProperty = mkVP have_V2 (mkNP aPl_Det Lexicon.child_N) ;

}
