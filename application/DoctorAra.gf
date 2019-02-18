--# -path=.:alltenses

concrete DoctorAra of Doctor =
  open
    SyntaxAra,
    ParadigmsAra,
    (L=LexiconAra),
    (R=ResAra)
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

  goToAction place = mkVP (mkVP L.go_V) place.to ;
  stayAtAction place = mkVP (mkVP stay_V) place.at ;
  vaccinateAction person = mkVP vaccinate_V2 person ;
  examineAction person = mkVP examine_V2 person ;
  takeSubstanceAction substance = mkVP take_V2 substance ;

-- end of what could be a functor
--------------------------------

  coughAction = mkVP (mkV "سعل" va vu  "سُعَالاَ") ;
  breatheAction = mkVP (mkV "نفس" FormV) ;
  vomitAction = mkVP (mkV "قيء" FormV) ;
  sleepAction = mkVP ( mkV "نوم" va va "نَوْم") ;
  undressAction = mkVP take_off_V2 (mkNP thePl_Det clothes_N) ;
  dressAction = mkVP put_on_V2 (mkNP thePl_Det clothes_N) ;
  eatAction = mkVP <L.eat_V2 : V> ;
  drinkAction = mkVP (mkV "شرب" vi va "شُرْب") ;
  smokeAction = mkVP (mkV "دخن" FormII) ;
  measureTemperatureAction = mkVP measure_V2 (mkNP the_Det temperature_N) ;
  measureBloodPressureAction = mkVP measure_V2 (mkNP the_Det blood_pressure_N) ;

  hospitalPlace = {at = sAdv in_Prep hospital_NP ; to = sAdv liPrep hospital_NP} ;
  homePlace = {at = pAdv "فِي المَنْزِل" ; to = pAdv "لِلمَنْزِل"} ;
  schoolPlace = {at = sAdv in_Prep school_NP ; to = sAdv liPrep school_NP} ;
  workPlace = {at = sAdv in_Prep work_NP ; to = sAdv liPrep work_NP} ;

  doctorProfession = mkCN L.doctor_N ;
  nurseProfession = mkCN (mkN "مُمَرِّضَة") ;
  interpreterProfession = mkCN (mkN "مُتَرْجِم") ;

  bePregnantProperty = mkVP pregnant_A ;
  beIllProperty = mkVP (mkA "مرض" facIl "فَعْلَى") ;
  beWellProperty = mkVP healthy_A ;
  beDeadProperty = mkVP (mkA "ميت" "فَعّل") ;
  haveAllergiesProperty = mkVP have_V2 (mkNP aPl_Det (mkN "حَسَاسِيَّة")) ;
  havePainsProperty = mkVP have_V2 (mkNP aPl_Det (mkN "أَلَم" "آلَام" masc nohum)) ;
  haveChildrenProperty = mkVP have_V2 (mkNP aPl_Det L.child_N) ;

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
