--# -path=.:../prelude:../abstract:../common

concrete MiniLexiconNyn of MiniLexicon = MiniGrammarNyn **
  open
    MiniParadigmsNyn, MiniResNyn, Prelude
  in {

lin
  --NOTE: Those commented out are not in the abstract
  --burn_V  = mkV "sya" ;
  --die_V   = mkV "fa" ;
  --fly_V   = mkV "guruka" ;
  
  bird_N   = mkN "ekinyonyi" KI_BI ;
  boat_N   = mkN "eryato" RI_MA ;
  book_N   = mkN "ekitabo" KI_BI ;
  boy_N    = mkN "omwojo" "abojo" MU_BA ;
  bread_N  = mkN "omugati" MU_MI;
  car_N    = mkN "emootoka" N_N ;
  cat_N    = mkN "enjangu" N_N ;
  --chair_N  = mkN "entebbe" N_N ;
  child_N  = mkN "omwana" MU_BA ;
  city_N   = mkN "ekibúga" KI_BI; --orurêmbo pl endêmbo
  cloud_N  = mkN "ekikyu" KI_BI ;
  computer_N = mkN "kanyabwêngye" ZERO_ZERO ; --kanyabwêngye, embiikabwengye, kompyuta
  cow_N   = mkN "ente" N_N ;
  dog_N   = mkN "embwa" N_N ;
  --person_N = mkN "omuntu" "abantu" MU_BA ;
  fire_N = mkN "omuriro" MU_MI ;
  fish_N = mkN "eky'ényanja" KI_BI ;
  flower_N = mkN "ekimuri" KI_BI ;
  friend_N = mkN "omunywâni" MU_MI ; -- omunywâni, omunyamukago, omugyenzi
  girl_N   = mkN "omwishiki" MU_BA ;
  --shoe_N   = mkN "ekaito" N_N ;
  --table_N  = mkN "emeza" N_N ;
  --airplane_N = mkN "enyonyi" N_N ; -- mkN "endégye" N_N;
  animal_N = mkN "enyamaishwa" N_N ;
  apple_N = mkN "apple"  ZERO_ZERO ;
  baby_N = mkN "omwana" MU_BA ;
  beer_N = mkN "amarwa" ZERO_MA ;
  bike_N = mkN "egaari"  N_N ;
  bird_N = mkN "ekinyonyi" KI_BI  ;
  blood_N = mkN "eshágama"  N_ZERO ;
  grammar_N = mkN "enyómbeka y'órurími" "enyómbeka z'endími" ZERO_ZERO ; -- two words representing one word
  horse_N = mkN "embaráàsi" N_N ;
  house_N = mkN "enju" N_N ;
  language_N = mkN "orurími" "endími" RU_N ;
  man_N = mkN "omushaija" MU_BA ;
  milk_N = mkN "amate" ZERO_MA ;
  music_N = mkN "music" ZERO_ZERO ; -- I have not found the translation
  river_N = mkN "omugyera" MU_MI ; --omurîndi,
  sea_N = mkN "enyanja" N_N ;
  ship_N = mkN "ekyombo" KI_BI ; -- eméèri [NC_n_n] 
  star_N = mkN "enyonyóòzi" N_N  ;
  train_N = mkN "egaari y'omwika" N_N ; -- plural would be egáàri z'omwika
  tree_N = mkN "omuti" MU_MI ;
  water_N = mkN "amáìzi" ZERO_MA ;
  wine_N = mkN "víìnyo" ZERO_ZERO ;
  woman_N = mkN "omwishiki" MU_BA ;
  
  --Proper Nouns
  john_PN = mkPN "Yohana" (AgP3 Sg MU_BA) False;
  paris_PN = mkPN "Paris" (AgP3 Sg N_N) True; --Noun class for places???
 
  --Adjectives
  bad_A    = mkAdjective "bi" False False False; --False means the adjective is a stem and comes after the complete noun
  --beautiful_A = mkAdjective "rungi" False;
  big_A = mkAdjective "hango" False False False;
  black_A = mkAdjective "kwirangura" False False False;
  blue_A = mkAdjective "buuru" False True True ;
  clean_A = mkAdjective "yonjo" False False False; --: A ;
  cold_A = mkAdjective "rikufuka" False False False; --: A ;
  good_A =mkAdjective "rungi" False False False; --: A ;
  heavy_A = mkAdjective "rikuremeera" False False False; --: A ; --notice ri as a verb is
  hot_A = mkAdjective "rikwotsya" False False False; -- rikutagata -- problematic words like hot we need a new set of clitics
  new_A = mkAdjective "sya" False False False; --: A ;
  old_A = mkAdjective "kúru" False False False; --: A ;
  ready_A = mkAdjective "eteekateekire" False False False; --: A ;
  red_A = mkAdjective "ríkutukura" False False False; --: A ;
  small_A = mkAdjective "kye" False False False;
  warm_A = mkAdjective "rikutagata" False False False;--: A ;
  white_A = mkAdjective "rikwera" False False False;--: A ;
  yellow_A = mkAdjective "kinekye" False True True;--: A ; or yero, or kyenju
  young_A = mkAdjective "to" False False False;--: A ;
  green_A =mkAdjective "kijubwe" False False True;

  --ditransitive verbs
  --bite_V2 = mkV2 "rum";
  break_V2 = mkV2 "hend"; --: V2 ;
  buy_V2   = mkV2 "gur" ;  --: V2 ;
  --close_V2 = mkV2 "king";
  --count_V2 = mkV2 "bar";
  --cut_V2 = mkV2 "shar";
  --do_V2 = mkV2 "kor";
  drink_V2 = mkV2 "nyw";
  eat_V2 = mkV2 "ry";
  --fear_V2 = mkV2 "tiin";
  find_V2 = mkV2 "bon" ; --: V2 ; -- many words; kureeba, kubóna,kushanga, kumamya,kujumbura
  kill_V2 = mkV2 "it"; --: V2 ;
  love_V2 = mkV2 "kûnd"; --: V2 ;
  read_V2 = mkV2 "shoma";--: V2 ;
  see_V2 = mkV2 "reeb"; --: V2 ;
  teach_V2 = mkV2 "shomes" ; --: V2 ; or kwegyesa
  understand_V2 = mkV2 "étegyerez"; --: V2 ;
  wait_V2 = mkV2 "tegyerez"; --: V2 ;

  

-- Intransitive verbs
  come_V = mkV "ij";
  go_V = mkV "gyend"; --: V ; -- Many words: kuza, kuraba,kutoora, kugyenda=go away, kushuma=go down
  jump_V = mkV "guruk" ;
  play_V = mkV "zaan"; --: V ;
  live_V = mkV "tuur" ; --manyF: kutuura i.e. live somewhere, stay = kuráàra
  run_V = mkV "íruk"; -- : V ;
  sleep_V = mkV "nyama" ; --: V ;--Kugwejegyera, kubyama
  swim_V = mkV "og"; --: V ;
  travel_V = mkV "gyend";--: V ;
  walk_V = mkV "tabur"; --: V ; or kuribata

  --Adverbs
  now_Adv = mkAdv "hati" AgrNo;
  --far_Adv = mkAdv "hare";
  
  --today_Adv = mkAdv "erizooba" AgrNo;
{-
      TO DO: Find the right translations for the
             following words commented out.
             They are part of the abstract syntax
  -}
  --already_Adv ; --has aspectual realization
  --dirty_A
  --clever_A : mkAdjective ;
  --green_A =mkAdjective : --A ; 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

}


