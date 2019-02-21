--# -path=.:alltenses

concrete DoctorPes of Doctor = DoctorFunctor with
  (Syntax=SyntaxPes),
  (Lexicon=LexiconPes)
  ** open ParadigmsPes,(L=LexiconPes) in {

lin
  coughAction = mkVP cough_V ;
  vomitAction = mkVP vomit_V ;
  undressAction = mkVP remove_V2 (mkNP the_Det clothes_N) ;
  dressAction = mkVP put_on_V2 (mkNP the_Det clothes_N) ;
  smokeAction = mkVP (compoundV "سیگار" pullVerb) ;
  measureTemperatureAction = mkVP (mkV2 measure_V)
    (mkNP the_Det (mkN "درجه حرارت")) ;
  measureBloodPressureAction = mkVP (mkV2 measure_V)
    (mkNP the_Det (mkN "فشارخون" )) ;

  hospitalPlace = {at = pAdv "در بیمارستان" ; to = pAdv "به بیمارستان"} ;
  homePlace = {at = pAdv "در خانه" ; to = pAdv "خانه"} ;
  schoolPlace = {at = pAdv "در مدرسه" ; to = pAdv "به مدرسه"} ;
  workPlace = {at = pAdv "در شغل" ; to = pAdv "به شغل"} ;

  nurseProfession       = mkCN (mkN "پرستار") ;
  interpreterProfession = mkCN (mkN "مترجم") ;

  bePregnantProperty = mkVP (mkA "حامله") ;
  beIllProperty = mkVP (mkA "بیمار") ;
  beWellProperty = mkVP (mkAdv good_A) ; ----
  beDeadProperty = mkVP (mkA "مرده") ;
  haveAllergiesProperty = mkVP have_V2 (mkNP aPl_Det (mkN "حساسیت")) ;
  havePainsProperty = mkVP have_V2 (mkNP aPl_Det pain_N) ;
  haveChildrenProperty = mkVP have_V2 (mkNP aPl_Det L.child_N) ;

  lin
  feverIllness        = mkNP (mkN "تب") ;
  fluIllness          = mkNP (mkN "آنفولانزا") ;
  headacheIllness     = mkNP (mkCN pain_N (mkNP L.head_N)) ; ---- ?
  diarrheaIllness     = mkNP (mkN "اسهال" "اسهال" inanimate) ;
  heartDiseaseIllness = mkNP (mkCN cardiac_A disease_N) ;
  lungDiseaseIllness  = mkNP (mkCN pulmonary_A disease_N) ;
  hypertensionIllness = mkNP (mkN "فشارخون بالا") ;

  alcoholSubstance = mkNP (mkN "مشروبات الکلی") ;
  medicineSubstance = mkNP a_Det medicine_N ;
  drugsSubstance = mkNP aPl_Det (mkCN narcotic_A substance_N) ;

oper
  pAdv : Str -> Adv = ParadigmsPes.mkAdv ;
  take_V2 = mkV2 takeVerb ;
  cough_V = compoundV "سرفه" doVerb ;
  vomit_V = compoundV "استفراغ" doVerb ;
  examine_V2 = mkV2 (compoundV "معاینه" doVerb) ;
  measure_V = compoundV "اندازه" takeVerb ;
  need_V2 = mkV2 (compoundV "احتیاج" haveVerb) ;
  clothes_N = mkN "لباس" ;
  stay_V = mkV "ماندن" ;
  remove_V2 = mkV2 (mkV "درآوردن") ;
  put_on_V2 = mkV2 (mkV "پوشیدن") ;
  vaccinate_V2 = mkV2 (compoundV "واکسیناسیون" doVerb) ;
  disease_N = mkN "بیماری" ;
  pulmonary_A = mkA "ریوی" ;
  cardiac_A = mkA "قلبی" ;
  narcotic_A = mkA "مخدر" ;
  substance_N = mkN "مادّه" "مواد" inanimate ;
  medicine_N = mkN "دارو" ;
  pain_N = mkN "درد" ;

  doVerb = mkV "کردن" "کن" ;
  pullVerb = mkV  "کشیدن" ;
  takeVerb = mkV "گرفتن" "گیر" ;

  qMark : Str = "؟" ;
  invQMark : Str = "" ;

} ;
