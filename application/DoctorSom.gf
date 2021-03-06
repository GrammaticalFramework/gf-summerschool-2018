--# -path=.:../resource/abstract:../resource/somali:../resource/api

concrete DoctorSom of Doctor =
  open
    MiniSyntaxSom,
    MiniParadigmsSom,
    MiniLexiconSom,
    Prelude
  in {

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
  presQuestionPhrase fact = let p : Utt = mkUtt (mkQS (mkQCl fact)) in p ** {s = p.s ++ SOFT_BIND ++ "?"} ;
  pastQuestionPhrase fact = let p : Utt = mkUtt (mkQS anteriorAnt (mkQCl fact)) in p ** {s = p.s ++ SOFT_BIND ++ "?"} ;


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
  takeSubstanceAction substance = mkVP take_V2 substance ;

-- end of what could be a functor
--------------------------------

-- All of these words come from Google Translate /IL

  coughAction = mkVP (mkV "qufac") ;
  breatheAction = mkVP (mkV "neef") ;
  vomitAction = mkVP (mkV "matag") ;
  sleepAction = mkVP (mkV "seex") ;
  -- undressAction = mkVP (mkVP take_V2 (mkNP thePl_Det (mkN "clothe"))) (pAdv "off") ;
  -- dressAction = mkVP (mkVP put_V2 (mkNP thePl_Det (mkN "clothe"))) (pAdv "on") ;
  eatAction = mkVP (mkV "cun") ;
  drinkAction = mkVP (mkV "cab") ;
  smokeAction = mkVP (mkV2 "cab") (mkNP (mkN "sigaar")) ; ---- ?
  -- measureTemperatureAction = mkVP (mkV2 (mkV "measure")) (mkNP the_Det (mkN "body temperature")) ;
  -- measureBloodPressureAction = mkVP (mkV2 (mkV "measure")) (mkNP the_Det (mkN "blood pressure")) ;
  --
  -- hospitalPlace = {at = pAdv "at the hospital" ; to = pAdv "to the hospital"} ;
  -- homePlace = {at = pAdv "at home" ; to = pAdv "home"} ;
  -- schoolPlace = {at = pAdv "at school" ; to = pAdv "to school"} ;
  -- workPlace = {at = pAdv "at work" ; to = pAdv "to work"} ;
  --
  doctorProfession = mkCN (mkN "dhakhtar") ;
  nurseProfession = mkCN (mkN "kalkaalisada") ;
  interpreterProfession = mkCN (mkN "turjumaan") ;

  bePregnantProperty = mkVP (mkA "uur") ;
  beIllProperty = mkVP (mkA "xanuunsan") ;
  beWellProperty = mkVP (mkA "fiic") ;
--  beDeadProperty = mkVP (mkA "dead") ;
  haveAllergiesProperty = mkVP have_V2 (mkNP aPl_Det (mkN "xasaasiyad")) ;
  havePainsProperty = mkVP have_V2 (mkNP aPl_Det (mkN "canuun")) ;
  haveChildrenProperty = mkVP have_V2 (mkNP aPl_Det child_N) ;

  feverIllness = mkNP a_Det (mkN "qandho") ;
  fluIllness = mkNP a_Det (mkN "hargab") ;
  headacheIllness = mkNP a_Det (mkN "madax xanuun") ;
  diarrheaIllness = mkNP a_Det (mkN "shuban") ;
  -- heartDiseaseIllness = mkNP a_Det (mkN "heart disease") ;
  -- lungDiseaseIllness = mkNP a_Det (mkN "lung disease") ;
  -- hypertensionIllness = mkNP (mkN "hypertension") ;

  alcoholSubstance = mkNP (mkN "aalkolo") ;
  medicineSubstance = mkNP a_Det (mkN "daroogada") ;
  drugsSubstance = mkNP aPl_Det (mkN "daawo") ;

oper

  go_V = mkV "tag" ;
  stay_V = mkV "joog" ;
  need_V2 = mkV2 (mkV "u baahan" copula) ;
  take_V2 = mkV2 (mkV "qaado") ;
  put_V2 = mkV2 (mkV "dhig") ;
  vaccinate_V2 = mkV2 (mkV "tallaali") ;
  examine_V2 = mkV2 (mkV "baaray") ;

}
