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
  vomitAction = mkVP (mkV "erbrechen") ;
  sleepAction = mkVP (mkV "schlafen") ;
  undressAction = mkVP (reflV (mkV "entkleiden") accusative) ;
  dressAction = mkVP (reflV (mkV "anziehen") accusative) ;
  eatAction = mkVP (mkV "essen") ;
  drinkAction = mkVP (mkV "trinken") ;
  smokeAction = mkVP (mkV "rauchen") ;
  measureTemperatureAction = mkVP take_V2 (mkNP the_Det (mkN "temp")) ;
  measureBloodPressureAction = mkVP (mkV2 (mkV "mäter")) (mkNP the_Det (mkN "blotryck" neuter)) ;

  hospitalPlace = {at = pAdv "på sjukhuset" ; to = pAdv "till sjukhuset"} ;
  homePlace = {at = pAdv "hemma" ; to = pAdv "hem"} ;
  schoolPlace = {at = pAdv "i skolan" ; to = pAdv "till skolan"} ;
  workPlace = {at = pAdv "på jobbet" ; to = pAdv "till jobbet"} ;

  doctorProfession = mkCN (mkN "läkare" "läkare" masculine) ;
  nurseProfession = mkCN (mkN "sjuksköterska") ;
  interpreterProfession = mkCN (mkN "tolk") ;

  bePregnantProperty = mkVP (mkA "gravid") ;
  beIllProperty = mkVP (mkA "sjuk") ;
  beWellProperty = mkVP (mkA "frisk") ;
  beDeadProperty = mkVP (mkA "död") ;
  haveAllergiesProperty = mkVP have_V2 (mkNP aPl_Det (mkN "allergi")) ;
  havePainsProperty = mkVP have_V2 (mkNP aPl_Det (mkN "smärta")) ;
  haveChildrenProperty = mkVP have_V2 (mkNP aPl_Det (mkN "Kind" neuter)) ;
  
  feverIllness = mkNP (mkN "feber") ;
  fluIllness = mkNP a_Det (mkN "förkylning") ;
  headacheIllness = mkNP (mkN "Kopfschmerzen") ;
  diarrheaIllness = mkNP (mkN "Durchfall") ;
  heartDiseaseIllness = mkNP aPl_Det (mkN "hjärtsjukdom") ;
  lungDiseaseIllness = mkNP aPl_Det (mkN "lungsjukdom") ;
  hypertensionIllness = mkNP (mkN "hypertoni") ;

  alcoholSubstance = mkNP (mkN "alkohol") ;
  medicineSubstance = mkNP aPl_Det (mkN "läkemedel") ;
  drugsSubstance = mkNP aPl_Det (mkN "drog") ;

oper
  pAdv : Str -> Adv = ParadigmsGer.mkAdv ;

  go_V = gehen_V ;
  stay_V = bleiben_V ;
  need_V2 = mkV2 (mkV "brauchen") ;
  take_V2 = mkV2 nehmen_V ;
  put_V2 = mkV2 (mkV "setzen") ;
  vaccinate_V2 = mkV2 (mkV "impfen") ;
  examine_V2 = mkV2 (mkV "untersuchen") ;

}
