--# -path=.:alltenses

concrete DoctorFPor of Doctor = DoctorFunctor - [
  Profession -- gender
    ,needProfessionProperty,theProfessionPerson
    ,isProfessionProperty,doctorProfession,
  iFemPerson,youFemPerson ]
  with (Syntax=SyntaxPor),(Lexicon=LexiconPor) **
  open
  ParadigmsPor,
  LexiconPor,
  ExtendPor,
  IrregPor

  in {

-- Functor overrides
lincat
  Profession = AP ; -- for gender
lin
  needProfessionProperty profession = mkVP need_V2 (mkNP a_Det (AdjAsCN profession)) ;
  theProfessionPerson profession = mkNP the_Det (AdjAsCN profession) ;
  isProfessionProperty profession = mkVP profession ;

  iFemPerson = mkNP iFem_Pron ;
  youFemPerson = mkNP youFem_Pron ;

-- Lexicon

  coughAction = mkVP cough_V ;
  vomitAction = mkVP (mkV "vomitar") ;
  undressAction = mkVP tirar_V2 (mkNP thePl_Det roupa_N) ;
  dressAction = mkVP put_V2 (mkNP thePl_Det roupa_N) ;
  smokeAction = mkVP (mkV "fumar") ;
  measureTemperatureAction = mkVP (mkV2 measure_V)
    (mkNP the_Det (mkCN (mkA "corporal") (mkN "temperatura"))) ;
  measureBloodPressureAction = mkVP (mkV2 measure_V)
    (mkNP the_Det (mkCN (mkA "sanguíneo") (mkN "pressão" feminine))) ;

  hospitalPlace = {at = pAdv "no hospital" ; to = pAdv "para o hospital"} ;
  homePlace = {at = pAdv "em casa" ; to = pAdv "para casa"} ;
  schoolPlace = {at = pAdv "na escola" ; to = pAdv "para a escola"} ;
  workPlace = {at = pAdv "no trabalho" ; to = pAdv "para o trabalho"} ;

  doctorProfession      = mkProfession "médico";
  nurseProfession       = mkProfession "enfermeiro";
  interpreterProfession = mkProfession "intérprete";

  bePregnantProperty = estarVP "grávido" ;
  beIllProperty = estarVP "doente" ;
  beDeadProperty = estarVP "morto" ;
  beWellProperty = UseComp_estar (mkComp (pAdv "bem")) ;
  haveAllergiesProperty = mkVP have_V2 (mkNP aPl_Det (mkN "alergia")) ;
  havePainsProperty = mkVP have_V2 (mkNP aPl_Det (mkN "dor")) ;
  haveChildrenProperty = mkVP have_V2 (mkNP aPl_Det (mkN "filho")) ;

  oper
    estarVP : Str -> VP ;
    estarVP a = UseComp_estar (mkComp (mkAP (mkA a))) ;

    mkProfession : Str -> AP = \a -> mkAP (mkA a) ;

  lin
  feverIllness        = mkNP (mkN "febre" feminine) ;
  fluIllness          = mkNP a_Det (mkN "gripe" feminine) ;
  headacheIllness     = mkNP a_Det (mkN (mkN "dor" feminine) "de cabeça") ;
  diarrheaIllness     = mkNP (mkN "diarreia") ;
  heartDiseaseIllness = mkNP a_Det (mkCN (mkA "cardíaco") doença_N) ;
  lungDiseaseIllness  = mkNP a_Det (mkCN (mkA "pulmonar" "pulmonar") doença_N) ;
  hypertensionIllness = mkNP (mkN "hipertensão") ;

  alcoholSubstance = mkNP (mkN "álcool") ;
  medicineSubstance = mkNP a_Det (mkN "remédio") ;
  drugsSubstance = mkNP aPl_Det (mkN "droga") ;

oper
  pAdv : Str -> Adv = ParadigmsPor.mkAdv ;
  measure_V = medir_V ;
  need_V2 = mkV2 (mkV "precisar") genitive ;
  doença_N = mkN "doença" feminine ;
  qMark : Str = "?" ;
  invQMark = [] ;
  examine_V2 = mkV2 "examinar" ;
  roupa_N = mkN "roupa" ;
  cough_V = mkV "tossir" ;
  tirar_V2 = mkV2 tirar_V ;
  take_V2 = mkV2 "tomar" ;
  stay_V = ficar_V ;
  vaccinate_V2 = mkV2 "vacinar" ;

} ;
