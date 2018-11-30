--# -path=.:../resource/abstract:../resource/portuguese:../resource/api

-- model implementation using Mini RGL

concrete DoctorPor of Doctor =
  open
  MiniSyntaxPor,
  MiniParadigmsPor,
  (R = MiniResPor),
  MiniLexiconPor
  in {

-------------------
-- the first part could be a functor

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
  
  isProfessionProperty profession = mkVP (mkNP a_Det profession) ;
  needProfessionProperty profession = mkVP need_V2 (mkNP a_Det profession) ;
  isAtPlaceProperty place = mkVP place.at ;
  haveIllnessProperty illness = mkVP have_V2 illness ;

  theProfessionPerson profession = mkNP the_Det profession ;

  iMascPerson = i_NP ;
  iFemPerson = mkNP (lin Pron (R.genderPron R.Fem i_Pron)) ;
  youMascPerson = you_NP ;
  youFemPerson = you_NP ;
  hePerson = he_NP ;
  shePerson = she_NP ;

  goToAction place = mkVP (mkVP go_V) place.to ;
  stayAtAction place = mkVP (mkVP stay_V) place.at ;
  vaccinateAction person = mkVP vaccinate_V2 person ;
  examineAction person = mkVP examine_V2 person ;
  takeSubstancAction substance = mkVP take_V2 substance ;

-- end of what could be a functor
--------------------------------

  coughAction = mkVP (mkV "tossir") ;
  breatheAction = mkVP (mkV "respirar") ;
  vomitAction = mkVP (mkV "vomitar") ;
  sleepAction = mkVP (mkV "dormir") ;
  undressAction = mkVP take_V2 (mkNP thePl_Det (mkN "roupa")) ;
  dressAction = mkVP put_V2 (mkNP thePl_Det (mkN "roupa")) ;
  eatAction = mkVP (mkV "comer") ;
  drinkAction = mkVP (mkV "beber") ;
  smokeAction = mkVP (mkV "fumar") ;
  measureTemperatureAction = mkVP (mkV2 (mkV "medir"))
    (mkNP the_Det (mkN "temperatura corporal" R.Fem)) ;
  measureBloodPressureAction = mkVP (mkV2 (mkV "medir"))
    (mkNP the_Det (mkN "pressão sanguínea")) ;
  
  hospitalPlace = {at = pAdv "no hospital" ; to = pAdv "para o hospital"} ;
  homePlace = {at = pAdv "em casa" ; to = pAdv "casa"} ;
  schoolPlace = {at = pAdv "na escola" ; to = pAdv "para a escola"} ;
  workPlace = {at = pAdv "no trabalho" ; to = pAdv "para o trabalho"} ;

  doctorProfession = mkCN (mkN "médico") ;
  nurseProfession = mkCN (mkN "enfermeira") ;
  interpreterProfession = mkCN (mkN "intérprete") ;

  bePregnantProperty = mkVP (mkA "grávida") ;
  beIllProperty = mkVP (mkA "doente") ;
  beWellProperty = mkVP (mkA "bem") ;
  beDeadProperty = mkVP (mkA "morto") ;
  haveAllergiesProperty = mkVP have_V2 (mkNP aPl_Det (mkN "alergia")) ;
  havePainsProperty = mkVP have_V2 (mkNP aPl_Det (mkN "dor")) ;
  haveChildrenProperty = mkVP have_V2 (mkNP aPl_Det (mkN "filho")) ;
  
  feverIllness = mkNP a_Det (mkN "febre") ;
  fluIllness = mkNP a_Det (mkN "gripe") ;
  headacheIllness = mkNP a_Det (mkN "dor-de-cabeça") ;
  diarrheaIllness = mkNP a_Det (mkN "diarreia") ;
  heartDiseaseIllness = mkNP a_Det (mkN "doença cardíaca") ;
  lungDiseaseIllness = mkNP a_Det (mkN "doença pulmonar") ;
  hypertensionIllness = mkNP (mkN "hipertensão") ;

  alcoholSubstance = mkNP (mkN "álcool") ;
  medicineSubstance = mkNP a_Det (mkN "remédio") ;
  drugsSubstance = mkNP aPl_Det (mkN "droga") ;

oper
  pAdv : Str -> Adv = MiniParadigmsPor.mkAdv ;

  stay_V = mkV "ficar" ;
  need_V2 = mkV2 (mkV "precisar") ;
  take_V2 = mkV2 (mkV "tomar") ;
  put_V2 = mkV2 (mkV "pôr") ;
  vaccinate_V2 = mkV2 (mkV "vacinar") ;
  examine_V2 = mkV2 (mkV "examinar") ;

} ;
