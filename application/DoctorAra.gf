--# -path=.:alltenses

concrete DoctorAra of Doctor = DoctorFunctor -
  [ drinkAction,sleepAction
  , iFemPerson, youFemPerson ]
  with
   (Syntax=SyntaxAra),
   (Lexicon=LexiconAra) **
  open
    ParadigmsAra,
    ExtendAra,
    (L=LexiconAra),
    (R=ResAra)
  in {

lin
  iFemPerson = mkNP iFem_Pron ;
  youFemPerson = mkNP youFem_Pron ;

  coughAction = mkVP (mkV "سعل" va vu  "سُعَالاَ") ;
  vomitAction = mkVP (mkV "قيء" FormV) ;
  sleepAction = mkVP ( mkV "نوم" va va "نَوْم") ;
  undressAction = mkVP take_off_V2 (mkNP thePl_Det clothes_N) ;
  dressAction = mkVP put_on_V2 (mkNP thePl_Det clothes_N) ;
  drinkAction = mkVP (mkV "شرب" vi va "شُرْب") ; -- TODO fix masdars in RG
  smokeAction = mkVP (mkV "دخن" FormII) ;
  measureTemperatureAction = mkVP measure_V2 (mkNP the_Det temperature_N) ;
  measureBloodPressureAction = mkVP measure_V2 (mkNP the_Det blood_pressure_N) ;

  hospitalPlace = {at = sAdv in_Prep hospital_NP ; to = sAdv liPrep hospital_NP} ;
  homePlace = {at = pAdv "فِي المَنْزِل" ; to = pAdv "لِلمَنْزِل"} ;
  schoolPlace = {at = sAdv in_Prep school_NP ; to = sAdv liPrep school_NP} ;
  workPlace = {at = sAdv in_Prep work_NP ; to = sAdv liPrep work_NP} ;

  nurseProfession = mkCN (mkN "مُمَرِّضَة") ;
  interpreterProfession = mkCN (mkN "مُتَرْجِم") ;

  bePregnantProperty = mkVP pregnant_A ;
  beIllProperty = mkVP (mkA "مرض" facIl "فَعْلَى") ;
  beWellProperty = mkVP healthy_A ;
  beDeadProperty = mkVP (mkA "ميت" "فَعّل") ;
  haveAllergiesProperty = mkVP have_V2 (mkNP aPl_Det (mkN "حَسَاسِيَّة")) ;
  havePainsProperty = mkVP have_V2 (mkNP aPl_Det (mkN "أَلَم" "آلَام" masc nohum)) ;

  feverIllness = mkNP a_Det (mkN "حَرَارة") ;
  fluIllness = mkNP a_Det (mkN "إِنْفْلُوِنْزَا") ; ---- Wiktionary
  headacheIllness = mkNP a_Det (mkN (mkN "صُدَاع") head_N) ;
  diarrheaIllness = mkNP a_Det (mkN "إِسْهَال") ;
  heartDiseaseIllness = mkNP a_Det heart_disease_N ;
  lungDiseaseIllness = mkNP a_Det (mkCN respiratory_A disease_N) ;
  hypertensionIllness = mkNP (mkN (mkN "إرْتِفَاع") blood_pressure_N) ;

  alcoholSubstance = mkNP (mkN "كُحُول" "كُحُوْلِيَّات" masc nohum) ;
  medicineSubstance = mkNP a_Det (mkN "دَوَاء" "أَدْوِيَة" masc nohum) ;
  drugsSubstance = mkNP aPl_Det (mkN "مُخَدِّر" "مُخَدِّرات" masc nohum) ;

oper
  sAdv : Prep -> NP -> Adv = SyntaxAra.mkAdv ;
  pAdv : Str -> Adv = ParadigmsAra.mkAdv ;

  stay_V = mkV "بقي" vi va  "بَقَاء" ;
  need_V2 = mkV2 (mkV "حوج" FormVIII) ;
  take_V2 = mkV2 (mkV "ءخذ" va vu "أَخْذ") ;
  take_off_V2 = mkV2 (mkV "خلع" va va) ; ----
  put_on_V2 = mkV2 (mkV "لبس" va va) ; ----
  vaccinate_V2 = mkV2 (mkV "لقح" FormII) ;
  examine_V2 = mkV2 (mkV "فحص" va va  "فَحْص") ;
  measure_V2 = mkV2 (mkV "قيس" va vi "قِياس") ;
  temperature_N = mkN "حَرَارَة" ;
  clothes_N = mkN "مَلْبَس" "مَلَابِس" fem nohum ;
  blood_pressure_N = compN pressure_N blood_N ;
  pressure_N = mkN "ضَغْط" "ضُغُوط" masc nohum ;
  blood_N = mkN "دَم" "دِمَاء" masc nohum ;
  hospital_N = sdfN "شفي" "مُسْتَفْعَل" masc nohum ;
  hospital_NP = mkNP the_Det hospital_N ;
  school_N = mkN "مَدْرَسَة" ;
  school_NP = mkNP the_Det school_N ;
  work_N = mkN "عَمَل" "أَعْمَال" masc nohum ;
  work_NP = mkNP the_Det work_N ;
  pregnant_A = invarGenderA (mkA "حمل" fAcil "فَوَاعِل") ;
  healthy_A = fuskA "بِصِحَّة جَيّدَة" ; -- adverb "in good health"
  disease_N = mkN "مَرَض" "أَمْرَاض" masc nohum ;
  heart_disease_N = mkN (mkN disease_N (nisbaA "قَلْب")) (nisbaA "وِعَائ") ;
  respiratory_A = nisbaA "تَنَفُّس" ;
  head_N = mkN "رَأْس" "رُؤُوس" masc nohum ;

  qMark : Str = "؟" ;
  invQMark : Str = "" ;
-- Compound noun where the head loses its place as head,
-- and possessive suffix attaches to the attribute word
  compN : N -> N -> N = \n1,n2 -> n1 ** {
    s = \\n,s,c => n1.s ! n ! R.Const ! c
      ++ n2.s ! n ! s ! c  -- or Const instead of s? TODO check
      ++ n2.s2 ! n ! s ! c} ;
  fuskA : Str -> A = \s -> degrA s s s ;
  fAcil : Str = "فَاعِل" ;
  facIl : Str = "فَعِيل" ;
}
