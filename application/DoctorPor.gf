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
  Profession = {s : R.Gender => CN} ;
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

  isProfessionProperty profession = lin VP {
    verb = R.ser_V ;
    clit = R.emptyClit ;
    clitAgr = R.CAgrNo ;
    compl = \\agr => case agr of {
      R.Agr g n _ => (profession.s ! g).s ! n
      }
    } ;

  needProfessionProperty profession = mkVP need_V2 (mkNP a_Det (profession.s ! R.Masc)) ;
  isAtPlaceProperty place = mkVP place.at ;
  haveIllnessProperty illness = mkVP have_V2 illness ;

  theProfessionPerson profession = mkNP the_Det (profession.s ! R.Masc) ;

  iMascPerson = i_NP ;
  iFemPerson = mkNP (lin Pron R.iFem_Pron) ;
  youMascPerson = you_NP ;
  youFemPerson = mkNP (lin Pron R.youFemSg_Pron) ;
  hePerson = he_NP ;
  shePerson = she_NP ;

  lin
  goToAction place = mkVP (mkVP go_V) place.to ;
  stayAtAction place = mkVP (mkVP stay_V) place.at ;
  vaccinateAction person = mkVP vaccinate_V2 person ;
  examineAction person = mkVP examine_V2 person ;
  takeSubstanceAction substance = mkVP tomar_V2 substance ;

-- end of what could be a functor
--------------------------------

  coughAction = mkVP cough_V ;
  breatheAction = mkVP (mkV "respirar") ;
  vomitAction = mkVP (mkV "vomitar") ;
  sleepAction = mkVP sleep_V ;
  undressAction = mkVP tirar_V2 (mkNP thePl_Det roupa_N) ;
  dressAction = mkVP put_V2 (mkNP thePl_Det roupa_N) ;
  eatAction = mkVP (mkV "comer") ;
  drinkAction = mkVP (mkV "beber") ;
  smokeAction = mkVP (mkV "fumar") ;
  measureTemperatureAction = mkVP (mkV2 measure_V)
    (mkNP the_Det (mkN "temperatura corporal" R.Fem)) ;
  measureBloodPressureAction = mkVP (mkV2 measure_V)
    (mkNP the_Det (mkN "pressão sanguínea")) ;

  hospitalPlace = {at = pAdv "no hospital" ; to = pAdv "para o hospital"} ;
  homePlace = {at = pAdv "em casa" ; to = pAdv "para casa"} ;
  schoolPlace = {at = pAdv "na escola" ; to = pAdv "para a escola"} ;
  workPlace = {at = pAdv "no trabalho" ; to = pAdv "para o trabalho"} ;

  doctorProfession      = mkProfession "médico" ;
  nurseProfession       = mkProfession "enfermeiro" ;
  interpreterProfession = mkProfession "intérprete" ;

  bePregnantProperty = estarUseAP (mkA "grávido") ;
  beIllProperty = estarUseAP (mkA "doente") ;
  beWellProperty = estarUseAP (mkA "bem") ;
  beDeadProperty = estarUseAP (mkA "morto") ;
  haveAllergiesProperty = mkVP have_V2 (mkNP aPl_Det (mkN "alergia")) ;
  havePainsProperty = mkVP have_V2 (mkNP aPl_Det (mkN "dor")) ;
  haveChildrenProperty = mkVP have_V2 (mkNP aPl_Det (mkN "filho")) ;

  oper
    estarUseAP : A -> VP ;
    estarUseAP a = UseAP a ** {verb = estar_V} ;

    mkProfession : Str -> Profession ;
    mkProfession p = let p : R.Adjective = mkA p
      in lin Profession {s = \\g => lin CN {s = p.s ! g ; g = g} } ;

  lin
  feverIllness        = mkNP a_Det (mkN "febre" R.Fem) ;
  fluIllness          = mkNP a_Det (mkN "gripe" R.Fem) ;
  headacheIllness     = mkNP a_Det (mkN "dor-de-cabeça") ;
  diarrheaIllness     = mkNP a_Det (mkN "diarreia") ;
  heartDiseaseIllness = mkNP a_Det (mkN "doença cardíaca") ;
  lungDiseaseIllness  = mkNP a_Det (mkN "doença pulmonar") ;
  hypertensionIllness = mkNP (mkN "hipertensão" R.Fem) ;

  alcoholSubstance = mkNP (mkN "álcool") ;
  medicineSubstance = mkNP a_Det (mkN "remédio") ;
  drugsSubstance = mkNP aPl_Det (mkN "droga") ;

oper
  pAdv : Str -> Adv = MiniParadigmsPor.mkAdv ;

  cough_V = mkV "tossir" "tusso" "tosse" "tossimos" "tossem"
    "tossi" "tossiu" "tossimos" "tossiram" "tussa" "tussamos" "tussam" ;
  examine_V2 = mkV2 (mkV "examinar") ;
  estar_V = lin V R.estar_V ;
  measure_V = mkV "medir" "meço" "mede" "medimos" "medem"
    "medi" "mediu" "medimos" "mediram" "meça" "meçamos" "meçam" ;
  need_V2 = mkV2 (mkV "precisar") "de" ;
  put_V2 = mkV2 (mkV "pôr" "ponho" "põe" "pomos" "põem"
    "pus" "pôs" "pusemos" "puseram" "ponha" "ponhamos" "ponham") ;
  roupa_N = mkN "roupa" ;
  stay_V = mkV "ficar" "fico" "fica" "ficamos" "ficam"
    "fiquei" "ficou" "ficamos" "ficaram" "fique" "fiquemos" "fiquem" ;
  tomar_V2 = mkV2 (mkV "tomar") ;
  tirar_V2 = mkV2 (mkV "tirar") ;
  vaccinate_V2 = mkV2 (mkV "vacinar") ;


} ;
