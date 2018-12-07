-- model implementation using standard RGL

concrete DoctorGer of Doctor =
  open
    SyntaxGer,
    ParadigmsGer,
    IrregGer
  in {

-------------------
-- the first part could be a functor
-- exception in Ger: isProfessionProperty, no article: "I am a doctor - jag är läkare"

lincat
  Phrase = Utt ;
  Fact = Cl ;
  Action = VP ;
  Property = VP ;
  Profession = CN ;
  Person = NP ;
  Place = {at,to : Adv} ;
  Substance = NP ;
  Illness = NP ;

lin
  presPosPhrase fact = mkUtt (mkS fact) ;
  presNegPhrase fact = mkUtt (mkS negativePol fact) ;
  pastPosPhrase fact = mkUtt (mkS anteriorAnt fact) ;
  pastNegPhrase fact = mkUtt (mkS anteriorAnt negativePol fact) ;
  presQuestionPhrase fact = mkUtt (mkQS (mkQCl fact)) ;
  pastQuestionPhrase fact = mkUtt (mkQS anteriorAnt (mkQCl fact)) ;

  impPosPhrase action = mkUtt (mkImp action) ;
  impNegPhrase action = mkUtt negativePol (mkImp action) ;

  actionFact person action = mkCl person action ;
  propertyFact person property = mkCl person property ;
  
  isProfessionProperty profession = mkVP (mkNP profession) ; -- the only difference from Eng
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

  goToAction place = mkVP (mkVP go_V) place.to ;
  stayAtAction place = mkVP (mkVP stay_V) place.at ;
  vaccinateAction person = mkVP vaccinate_V2 person ;
  examineAction person = mkVP examine_V2 person ;
  takeSubstanceAction substance = mkVP take_V2 substance ;

-- end of functor

  coughAction = mkVP (mkV "husten") ;
  breatheAction = mkVP (mkV "atmen") ;
  vomitAction = mkVP (fixprefixV "er" brechen_V) ;
  sleepAction = mkVP schlafen_V ;
  undressAction = mkVP (reflV (fixprefixV "ent" (mkV "kleiden")) accusative) ;
  dressAction = mkVP (reflV (mkV "an" ziehen_V) accusative) ;
  eatAction = mkVP essen_V ;
  drinkAction = mkVP trinken_V ;
  smokeAction = mkVP (mkV "rauchen") ;
  measureTemperatureAction = mkVP (mkV2 messen_V) (mkNP the_Det (mkN "Fieber" neuter)) ;
  measureBloodPressureAction = mkVP (mkV2 messen_V) (mkNP the_Det (mkN "Blutdruck")) ;

  hospitalPlace = {at = pAdv "im Krankenhaus" ; to = pAdv "in das Krankenhaus"} ;
  homePlace = {at = pAdv "zu Hause" ; to = pAdv "nach Hause"} ;
  schoolPlace = {at = pAdv "in der Schule" ; to = pAdv "in die Schule"} ;
  workPlace = {at = pAdv "auf Arbeit" ; to = pAdv "zur Arbeit"} ;

  doctorProfession = mkCN (mkN "Arzt") ;
  nurseProfession = mkCN ((mkN "Krankenpfleger") | (mkN "Pfleger") | (mkN "Krankenschwester") ) ;
  interpreterProfession = mkCN (mkN "Übersetzer") ;

  bePregnantProperty = mkVP (mkA "schwanger") ;
  beIllProperty = mkVP (mkA "krank") ;
  beWellProperty = mkVP (mkA "gesund") ;
  beDeadProperty = mkVP (mkA "tot") ;
  haveAllergiesProperty = mkVP have_V2 (mkNP aPl_Det (mkN "Allergie")) ;
  havePainsProperty = mkVP have_V2 (mkNP aPl_Det (mkN "Schmerz" "Schmerzen" masculine)) ;
  haveChildrenProperty = mkVP have_V2 (mkNP aPl_Det (mkN "Kind" "Kinder" neuter)) ;
  
  feverIllness = mkNP (mkN "Fieber") ;
  fluIllness = mkNP (the_Det|a_Det) (mkN "Grippe") ;
  headacheIllness = mkNP (mkN "Kopfschmerzen") ;
  diarrheaIllness = mkNP (mkN "Durchfall") ;
  heartDiseaseIllness = mkNP aPl_Det (mkN "Herzerkrankung") ;
  lungDiseaseIllness = mkNP aPl_Det (mkN "Lungenerkrankung") ;
  hypertensionIllness = mkNP (mkN "Bluthochdruck") ;

  alcoholSubstance = mkNP (mkN "Alkohol") ;
  medicineSubstance = mkNP aPl_Det (mkN "Medikament") ;
  drugsSubstance = mkNP aPl_Det (mkN "Droge") ;

oper
  pAdv : Str -> Adv = ParadigmsGer.mkAdv ;

  go_V = seinV gehen_V ;
  stay_V = bleiben_V ;
  need_V2 = mkV2 (mkV "brauchen") ;
  take_V2 = mkV2 nehmen_V ;
  put_V2 = mkV2 (mkV "setzen") ;
  vaccinate_V2 = mkV2 (mkV "impfen") ;
  examine_V2 = mkV2 (fixprefixV "unter" (mkV "suchen")) ;

}
