--# -path=.:../prelude:../abstract:../common

concrete LexiconCgg of Lexicon = CatCgg ** 
  open ParadigmsCgg, ResCgg, Prelude in {

lin
  
  burn_V  = mkV "sya" ;
  die_V   = mkV "fa" ;
  fly_V   = mkV "guruka" ;
  run_V   = mkV "iruka" ;
  sleep_V = mkV "byama" ;
  walk_V  = mkV "tabula" ;
  
  bird_N   = mkN "ekinyonyi" KI_BI ;
  boat_N   = mkN "eryato" RI_MA ;
  book_N   = mkN "ekitabo" KI_BI ;
  boy_N    = mkN "omwojo" "abojo" MU_BA ;
  car_N    = mkN "emootoka" N_N ;
  cat_N    = mkN "enjangu" N_N ;
  chair_N  = mkN "entebbe" N_N ;
  child_N  = mkN "omwana" MU_BA ;
  city_N   = mkN "ekibúga" KI_BI; --orurêmbo pl endêmbo
  cloud_N  = mkN "ekikyu" KI_BI ;
  computer_N = mkN "kanyabwêngye" ZERO_ZERO ; --kanyabwêngye, embiikabwengye, kompyuta
  cow_N   = mkN "ente" N_N ;
  dog_N   = mkN "embwa" N_N ;
  person_N = mkN "omuntu" "abantu" MU_BA ;
  fire_N = mkN "omuriro" MU_MI ;
  fish_N = mkN "eky'ényanja" KI_BI ;
  flower_N = mkN "ekimuri" KI_BI ;
  friend_N = mkN "omunywâni" MU_MI ; -- omunywâni, omunyamukago, omugyenzi
  girl_N   = mkN "omwishiki" MU_BA ;
  shoe_N   = mkN "ekaito" N_N ;
  table_N  = mkN "emeza" N_N ;
  bad_A    = mkAdjective "bi" False ; --False means the adjective is a stem and comes after the complet noun
  beautiful_A = mkAdjective "rungi" False; 
  far_Adv = mkAdv "hare";
  now_Adv = mkAdv "hati";
  today_Adv = mkAdv "erizooba";
  bite_V2 = mkV2 "ruma";
  break_V2 = mkV2 "henda";
  buy_V2 = mkV2 "gura";
  close_V2 = mkV2 "kinga";
  count_V2 = mkV2 "bara";
  cut_V2 = mkV2 "shara";
  do_V2 = mkV2 "kora";
  drink_V2 = mkV2 "nywa";
  eat_V2 = mkV2 "rya";
  fear_V2 = mkV2 "tiina";
  airplane_N = mkN "enyonyi" N_N ;
  animal_N = mkN "enyamaishwa" N_N ;
  apple_N = mkN "apple"  ZERO_ZERO ;
  baby_N = mkN "omwana" MU_BA ;
  beer_N = mkN "amarwa" ZERO_MA ;
  bike_N = mkN "egaari"  N_N ;
  bird_N = mkN "ekinyonyi" KI_BI  ;
  blood_N = mkN "eshágama"  N_ZERO ;

  grammar_N = mkN "enyómbeka y'órurími" "enyómbeka z'endími" ZERO_ZERO ; -- two words representing one word
  horse_N = mkN "embaráàsi" N_N ;
  --house_N = mkN "enju" N_N ;
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

lin

  john_PN = mkPN "Yohana" MU_BA Sg;
  paris_PN = mkPN "Paris" KI_BI Sg; --Noun class for places???
 
  --Adjectives

  bad_A = mkAdjective "bi" False;
  
  big_A = mkAdjective "hango" False ;
  
  black_A = mkAdjective "kwirangura" False;
  
  blue_A = mkAdjective "buuru" False;

  
  --already_Adv ; --has aspectual realization
  
  
  
  
  break_V2 = mkV2 "kuhenda" False; --: V2 ;
  buy_V2   = mkV2 "kugura" False;  --: V2 ;
  
  clean_A = mkAdjective "yonjo" False; --: A ;
  --clever_A : mkAdjective ;
  
  cold_A = mkAdjective "rikufuka" False; --: A ;
  come_V = mkV "kwija" False;
 
 
  drink_V2 = mkV2 "kunywa" False; --: V2 ;
  eat_V2 = mkV2 "kurya" False; --: V2 ;
  find_V2 = mkV2 "kubona" False; --: V2 ; -- many words; kureeba, kubóna,kushanga, kumamya,kujumbura
  
 
  good_A =mkAdjective "rungi" False; --: A ;
  go_V = mkV "kuza" False; --: V ; -- Many words: kuza, kuraba,kutoora, kugyenda=go away, kushuma=go down
  
  --green_A =mkAdjective : A ; No word for green
  heavy_A = mkAdjective "rikuremeera" False; --: A ; --notice ri as a verb is

  hot_A = mkAdjective "rikwotsya" False; -- rikutagata
  
  

  jump_V = mkV "kuguruka" False;
  kill_V2 = mkV2 "kwita" False; --: V2 ;

  live_V = mkV "Kúbaho" False; --many: kutuura i.e. live somewhere, stay = kuráàra
  love_V2 = mkV2 "kukûnda" False; --: V2 ;

  new_A = mkAdjective "sya" False; --: A ;
  now_Adv =mkAdv "hati"; --: Adv ;
  old_A = mkAdjective "kúru" False; --: A ;
  
  play_V = mkV "Kuzaana" False; --: V ;
  read_V2 = mkV2 "Kushoma" False;--: V2 ;
  ready_A = mkAdjective "eteekateekire" False; --: A ;
  red_A = mkAdjective "ríkutukura" False; --: A ;
  run_V = mkV "Kwíruka" False; -- : V ;
  see_V2 = mkV2 "kureeba" False; --: V2 ;
  sleep_V = mkV "kunyama" False; --: V ;--Kugwejegyera, kubyama
  small_A = mkAdjective "kye" False;
  swim_V = mkV "kwoga" False; --: V ;
  teach_V2 = mkV2 "kushomesa" False; --: V2 ; or kwegyesa
  travel_V = mkV "kugyenda" False;--: V ;
  understand_V2 = mkV2 "kwétegyereza" False; --: V2 ;
  wait_V2 = mkV2 "kutegyereza" False; --: V2 ;
  walk_V = mkV "kugyenda" False; --: V ; or kuribata
  warm_A = mkAdjective "rikutagata" False;--: A ;
  white_A = mkAdjective "rikwera" False;--: A ;
  yellow_A = mkAdjective "kinekye" False;--: A ; or yero, or kyenju
  young_A = mkAdjective "to" False;--: A ;
  --add_V3 : V3 ;
{-
abstract Lexicon = Cat ** {
fun
  add_V3 : V3 ;
  alas_Interj : Interj ;
  already_Adv : Adv ;
  
  answer_V2S : V2S ;
  apartment_N : N ;
  art_N : N ;
  ashes_N : N ;
  ask_V2Q : V2Q ;
  back_N : N ;
  bank_N : N ;
  bark_N : N ;
  beautiful_A : A ;
  become_VA : VA ;
  beer_N : N ;
  beg_V2V : V2V ;
  belly_N : N ;
  big_A : A ;
  bike_N : N ;
  bird_N : N ;
  bite_V2 : V2 ;
  black_A : A ;
  blood_N : N ;
  blow_V : V ;
  blue_A : A ;
  boat_N : N ;
  bone_N : N ;
  book_N : N ;
  boot_N : N ;
  boss_N : N ;
  boy_N : N ;
  bread_N : N ;
  break_V2 : V2 ;
  breast_N : N ;
  breathe_V : V ;
  broad_A : A ;
  brother_N2 : N2 ;
  brown_A : A ;
  burn_V : V ;
  butter_N : N ;
  buy_V2 : V2 ;
  camera_N : N ;
  cap_N : N ;
  car_N : N ;
  carpet_N : N ;
  cat_N : N ;
  ceiling_N : N ;
  chair_N : N ;
  cheese_N : N ;
  child_N : N ;
  church_N : N ;
  city_N : N ;
  clean_A : A ;
  clever_A : A ;
  close_V2 : V2 ;
  cloud_N : N ;
  coat_N : N ;
  cold_A : A ;
  come_V : V ;
  computer_N : N ;
  correct_A : A ;
  country_N : N ;
  count_V2 : V2 ;
  cousin_N : N ;
  cow_N : N ;
  cut_V2 : V2 ;
  day_N : N ;
  die_V : V ;
  dig_V : V ;
  dirty_A : A ;
  distance_N3 : N3 ;
  doctor_N : N ;
  dog_N : N ;
  door_N : N ;
  do_V2 : V2 ;
  drink_V2 : V2 ;
  dry_A : A ;
  dull_A : A ;
  dust_N : N ;
  ear_N : N ;
  earth_N : N ;
  easy_A2V : A2 ;
  eat_V2 : V2 ;
  egg_N : N ;
  empty_A : A ;
  enemy_N : N ;
  eye_N : N ;
  factory_N : N ;
  fall_V : V ;
  far_Adv : Adv ;
  father_N2 : N2 ;
  fat_N : N ;
  fear_VS : VS ;
  fear_V2 : V2 ;
  feather_N : N ;
  fight_V2 : V2 ;
  find_V2 : V2 ;
  fingernail_N : N ;
  fire_N : N ;
  fish_N : N ;
  float_V : V ;
  floor_N : N ;
  flower_N : N ;
  flow_V : V ;
  fly_V : V ;
  fog_N : N ;
  foot_N : N ;
  forest_N : N ;
  forget_V2 : V2 ;
  freeze_V : V ;
  fridge_N : N ;
  friend_N : N ;
  fruit_N : N ;
  full_A : A ;
  fun_AV : A ;
  garden_N : N ;
  girl_N : N ;
  give_V3 : V3 ;
  glove_N : N ;
  gold_N : N ;
  good_A : A ;
  go_V : V ;
  grammar_N : N ;
  grass_N : N ;
  green_A : A ;
  guts_N : N ;
  hair_N : N ;
  hand_N : N ;
  harbour_N : N ;
  hate_V2 : V2 ;
  hat_N : N ;
  head_N : N ;
  heart_N : N ;
  hear_V2 : V2 ;
  heavy_A : A ;
  hill_N : N ;
  hit_V2 : V2 ;
  hold_V2 : V2 ;
  hope_VS : VS ;
  horn_N : N ;
  horse_N : N ;
  hot_A : A ;
  house_N : N ;
  hunt_V2 : V2 ;
  husband_N : N ;
  ice_N : N ;
  important_A : A ;
  industry_N : N ;
  iron_N : N ;
  john_PN : PN ;
  jump_V : V ;
  kill_V2 : V2 ;
  king_N : N ;
  knee_N : N ;
  know_V2 : V2 ;
  know_VQ : VQ ;
  know_VS : VS ;
  lake_N : N ;
  lamp_N : N ;
  language_N : N ;
  laugh_V : V ;
  leaf_N : N ;
  learn_V2 : V2 ;
  leather_N : N ;
  leave_V2 : V2 ;
  left_Ord : Ord ;
  leg_N : N ;
  lie_V : V ;
  like_V2 : V2 ;
  listen_V2 : V2 ;
  liver_N : N ;
  live_V : V ;
  long_A : A ;
  lose_V2 : V2 ;
  louse_N : N ;
  love_N : N ;
  love_V2 : V2 ;
  man_N : N ;
  married_A2 : A2 ;
  meat_N : N ;
  milk_N : N ;
  moon_N : N ;
  mother_N2 : N2 ;
  mountain_N : N ;
  mouth_N : N ;
  music_N : N ;
  name_N : N ;
  narrow_A : A ;
  near_A : A ;
  neck_N : N ;
  new_A : A ;
  newspaper_N : N ;
  night_N : N ;
  nose_N : N ;
  now_Adv : Adv ;
  number_N : N ;
  oil_N : N ;
  old_A : A ;
  open_V2 : V2 ;
  paint_V2A : V2A ;
  paper_N : N ;
  paris_PN : PN ;
  peace_N : N ;
  pen_N : N ;
  person_N : N ;
  planet_N : N ;
  plastic_N : N ;
  play_V2 : V2 ;
  play_V : V ;
  policeman_N : N ;
  priest_N : N ;
  probable_AS : A ;
  pull_V2 : V2 ;
  push_V2 : V2 ;
  put_V2 : V2 ;
  queen_N : N ;
  question_N : N ;
  radio_N : N ;
  rain_N : N ;
  rain_V0 : V ;
  read_V2 : V2 ;
  ready_A : A ;
  reason_N : N ;
  red_A : A ;
  religion_N : N ;
  restaurant_N : N ;
  right_Ord : Ord ;
  river_N : N ;
  road_N : N ;
  rock_N : N ;
  roof_N : N ;
  root_N : N ;
  rope_N : N ;
  rotten_A : A ;
  round_A : A ;
  rubber_N : N ;
  rub_V2 : V2 ;
  rule_N : N ;
  run_V : V ;
  salt_N : N ;
  sand_N : N ;
  say_VS : VS ;
  school_N : N ;
  science_N : N ;
  scratch_V2 : V2 ;
  sea_N : N ;
  seed_N : N ;
  seek_V2 : V2 ;
  see_V2 : V2 ;
  sell_V3 : V3 ;
  send_V3 : V3 ;
  sew_V : V ;
  sharp_A : A ;
  sheep_N : N ;
  ship_N : N ;
  shirt_N : N ;
  shoe_N : N ;
  shop_N : N ;
  short_A : A ;
  silver_N : N ;
  sing_V : V ;
  sister_N : N ;
  sit_V : V ;
  skin_N : N ;
  sky_N : N ;
  sleep_V : V ;
  small_A : A ;
  smell_V : V ;
  smoke_N : N ;
  smooth_A : A ;
  snake_N : N ;
  snow_N : N ;
  sock_N : N ;
  song_N : N ;
  speak_V2 : V2 ;
  spit_V : V ;
  split_V2 : V2 ;
  squeeze_V2 : V2 ;
  stab_V2 : V2 ;
  stand_V : V ;
  star_N : N ;
  steel_N : N ;
  stick_N : N ;
  stone_N : N ;
  stop_V : V ;
  stove_N : N ;
  straight_A : A ;
  student_N : N ;
  stupid_A : A ;
  suck_V2 : V2 ;
  sun_N : N ;
  swell_V : V ;
  swim_V : V ;
  switch8off_V2 : V2 ;
  switch8on_V2 : V2 ;
  table_N : N ;
  tail_N : N ;
  talk_V3 : V3 ;
  teacher_N : N ;
  teach_V2 : V2 ;
  television_N : N ;
  thick_A : A ;
  thin_A : A ;
  think_V : V ;
  throw_V2 : V2 ;
  tie_V2 : V2 ;
  today_Adv : Adv ;
  tongue_N : N ;
  tooth_N : N ;
  train_N : N ;
  travel_V : V ;
  tree_N : N ;
  turn_V : V ;
  ugly_A : A ;
  uncertain_A : A ;
  understand_V2 : V2 ;
  university_N : N ;
  village_N : N ;
  vomit_V : V ;
  wait_V2 : V2 ;
  walk_V : V ;
  warm_A : A ;
  war_N : N ;
  wash_V2 : V2 ;
  watch_V2 : V2 ;
  water_N : N ;
  wet_A : A ;
  white_A : A ;
  wide_A : A ;
  wife_N : N ;
  wind_N : N ;
  window_N : N ;
  wine_N : N ;
  wing_N : N ;
  win_V2 : V2 ;
  wipe_V2 : V2 ;
  woman_N : N ;
  wonder_VQ : VQ ;
  wood_N : N ;
  worm_N : N ;
  write_V2 : V2 ;
  year_N : N ;
  yellow_A : A ;
  young_A : A ;

-}

