--# -path=.:alltenses:prelude

concrete DoctorSpa of Doctor = DoctorFunctor - [
  Profession,needProfessionProperty,theProfessionPerson,isProfessionProperty,iFemPerson,youFemPerson]
  with (Syntax=SyntaxSpa) **
  open
  ParadigmsSpa,
  LexiconSpa,
  ExtendSpa,
  IrregSpa

  in {

-- Functor overrides

lincat
  Profession = AP ; -- for gender
lin
  needProfessionProperty profession = mkVP need_V2 (mkNP a_Det (AdjAsCN profession)) ;
  theProfessionPerson profession = mkNP the_Det (AdjAsCN profession) ;
  isProfessionProperty profession = mkVP profession ;

  iFemPerson = mkNP iFem_Pron ;
  youFemPerson = mkNP ExtendSpa.youFem_Pron ;

-- Lexicon

  coughAction = mkVP cough_V ;
  breatheAction = mkVP (mkV "respirar") ;
  vomitAction = mkVP (mkV "vomitar") ;
  sleepAction = mkVP sleep_V ;
  undressAction = mkVP quitarse_V2 (mkNP the_Det ropa_N) ;
  dressAction = mkVP ponerse_V2 (mkNP the_Det ropa_N) ;
  eatAction = mkVP (mkV "comer") ;
  drinkAction = mkVP (mkV "beber") ;
  smokeAction = mkVP (mkV "fumar") ;
  measureTemperatureAction = mkVP (mkV2 measure_V)
    (mkNP the_Det (mkCN (mkA "corporal") (mkN "temperatura"))) ;
  measureBloodPressureAction = mkVP (mkV2 measure_V)
    (mkNP the_Det (mkCN (mkA "sanguínea") (mkN "presión"))) ;

  hospitalPlace = {at = pAdv "en el hospital" ; to = pAdv "al hospital"} ;
  homePlace = {at = pAdv "en casa" ; to = pAdv "a casa"} ;
  schoolPlace = {at = pAdv "en la escuela" ; to = pAdv "a la escuela"} ;
  workPlace = {at = pAdv "en el trabajo" ; to = pAdv "al trabajo"} ;

  doctorProfession      = mkProfession "médico";
  nurseProfession       = mkProfession "enfermero";
  interpreterProfession = mkProfession "intérprete";

  bePregnantProperty = estarVP "embarazado" ;
  beIllProperty = estarVP "enfermo" ;
  beWellProperty = estarVP "bien" ;
  beDeadProperty = estarVP "muerte" ;
  haveAllergiesProperty = mkVP have_V2 (mkNP aPl_Det (mkN "alergia")) ;
  havePainsProperty = mkVP have_V2 (mkNP aPl_Det (mkN "dolor")) ;
  haveChildrenProperty = mkVP have_V2 (mkNP aPl_Det (mkN "hijo")) ;

  oper
    estarVP : Str -> VP ;
    estarVP a = UseComp_estar (mkComp (mkAP (mkA a))) ;

    mkProfession : Str -> AP = \a -> mkAP (mkA a) ;

  lin
  feverIllness        = mkNP a_Det (mkN "fiebre" feminine) ;
  fluIllness          = mkNP a_Det (mkN "gripe" feminine) ;
  headacheIllness     = mkNP a_Det (mkN (mkN "dolor") "de cabeza") ;
  diarrheaIllness     = mkNP a_Det (mkN "diarrea") ;
  heartDiseaseIllness = mkNP a_Det (mkCN (mkA "cardíaca") enfermedad_N) ;
  lungDiseaseIllness  = mkNP a_Det (mkCN (mkA "pulmonar") enfermedad_N) ;
  hypertensionIllness = mkNP (mkN "hipertensión") ;

  alcoholSubstance = mkNP (mkN "alcohol") ;
  medicineSubstance = mkNP a_Det (mkN "medicina") ;
  drugsSubstance = mkNP aPl_Det (mkN "droga") ;

oper
  pAdv : Str -> Adv = ParadigmsSpa.mkAdv ;
  take_V2 = mkV2 (mkV "tomar") ;
  cough_V = mkV "toser" ;
  examine_V2 = mkV2 (mkV "examinar") ;
  measure_V = medir_V ;
  need_V2 = mkV2 (mkV "necesitar") ;
  ropa_N = mkN "ropa" ;
  stay_V = mkV "quedar" ;
  quitarse_V2 = mkV2 (reflV (mkV "quitar")) ;
  ponerse_V2 = mkV2 (reflV poner_V) ;
  vaccinate_V2 = mkV2 (mkV "vacunar") ;
  enfermedad_N = mkN "enfermedad" feminine ;


} ;
