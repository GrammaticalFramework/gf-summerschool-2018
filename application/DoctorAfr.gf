--# -path=.:../resource/abstract:../resource/afrikaans:../resource/api

-- model implementation using Mini RGL

concrete DoctorAfr of Doctor =
  open
    MiniSyntaxAfr,
    ParadigmsAfr
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
  iFemPerson = i_NP ;
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

  coughAction = mkVP (mkV "hoes") ;
  breatheAction = mkVP (mkV "haal" "asem") ;
  vomitAction = mkVP (mkV "gooi" "op") ;
  sleepAction = mkVP (mkV "slaap") ;
  undressAction = mkVP (mkV2 (mkV "trek" "uit")) (mkNP thePl_Det (mkN "kleed" "klere")) ;
  dressAction = mkVP (mkV2 (mkV "trek" "aan")) (mkNP thePl_Det (mkN "kleed" "klere")) ;
  eatAction = mkVP (mkV "eet") ;
  drinkAction = mkVP (mkV "drink") ;
  smokeAction = mkVP (mkV "rook") ;
  measureTemperatureAction = mkVP (mkV2 (mkV "meet")) (mkNP the_Det (mkN "liggaamstemperatuur")) ;
  measureBloodPressureAction = mkVP (mkV2 (mkV "meet")) (mkNP the_Det (mkN "bloeddruk")) ;

  hospitalPlace = {at = pAdv ("by"++"die"++"hospitaal") ; to = pAdv ("na"++"die"++"hospitaal")} ;
  homePlace = {at = pAdv ("by"++"die"++"huis") ; to = pAdv ("huis"++"toe")} ;
  schoolPlace = {at = pAdv ("by"++"die"++"skool") ; to = pAdv ("skool"++"toe")} ;
  workPlace = {at = pAdv ("by"++"die"++"skool") ; to = pAdv ("werk"++"toe")} ;

  doctorProfession = mkCN (mkN "dokter") ;
  nurseProfession = mkCN (mkN "verpleegster") ;
  interpreterProfession = mkCN (mkN "interpreteerder") ;

  bePregnantProperty = mkVP (mkA "swanger") ;
  beIllProperty = mkVP (mkA "siek") ;
  beWellProperty = mkVP (mkA "gesond") ;
  beDeadProperty = mkVP (mkA "dood") ;
  haveAllergiesProperty = mkVP have_V2 (mkNP aPl_Det (mkN "allergie" "allergieë")) ;
  havePainsProperty = mkVP have_V2 (mkNP a_Det (mkN "pyn")) ;
  haveChildrenProperty = mkVP have_V2 (mkNP aPl_Det (mkN "kind" "kinders")) ;

  feverIllness = MassNP (mkN "koors") ;
  fluIllness = MassNP (mkN "griep") ;
  headacheIllness = mkNP a_Det (mkN "headache") ;
  diarrheaIllness = mkNP a_Det (mkN "diaree") ;
  heartDiseaseIllness = mkNP a_Det (mkN "hartsiekte") ;
  lungDiseaseIllness = mkNP a_Det (mkN "longsiekte") ;
  hypertensionIllness = mkNP (mkN "hipertensie") ;

  alcoholSubstance = MassNP (mkN "alkohol") ;
  medicineSubstance = MassNP (mkN "medisyne") ;
  drugsSubstance = mkNP aPl_Det (mkN "dwelm") ;

oper
  pAdv : Str -> Adv = ParadigmsAfr.mkAdv ;

  go_V = mkV "gaan" ;
  stay_V = mkV "bly" ;
  need_V2 = mkV2 (mkV "benodig") ;
  take_V2 = mkV2 (mkV "neem") ;
  put_V2 = mkV2 (mkV "plaas") ;
  vaccinate_V2 = mkV2 (mkV "ent" "in") ;
  examine_V2 = mkV2 (mkV "ondersoek" "ondersoek" "ondersoek" "ondersoek" "ondersoek") ;

}
