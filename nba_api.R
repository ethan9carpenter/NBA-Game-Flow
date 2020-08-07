library(jsonlite)

get_pbp_data <- function(game_id, date){
  date <- get_nba_date_id(date)
  data <- fromJSON(paste0('http://data.nba.net/json/cms/noseason/game/', 
                          date, '/', game_id, '/pbp_all.json'))
  data <- data$sports_content$game$play
  return (data.frame(data))
}

get_games <- function(date, statuses=c(2, 3)){
  date <- get_nba_date_id(date)
  data <- fromJSON(paste0('http://data.nba.net/10s/prod/v1/', date, '/scoreboard.json'))$games %>%
    filter(statusNum %in% statuses) %>% #game is in progress or finished
    select(gameId, vTeam, hTeam) %>%
    mutate(hTeam=hTeam$triCode,
           vTeam=vTeam$triCode)
  return (data)
}

get_nba_date_id <- function(date){
  format(as.Date(date), format='%Y%m%d')
}

get_player_headshot <- function(player_id){
  if (file.exists(paste0('player_headshots/', player_id, '.png')))
    file <- paste0('player_headshots/', player_id, '.png')
  else {
    url <- paste0('https://ak-static.cms.nba.com/wp-content/uploads/headshots/nba/latest/260x190/',
                  player_id, '.png')
    file <- RCurl::getURLContent(url)
  }
  img <- png::readPNG(file)
  
  if (!file.exists(paste0('player_headshots/', player_id, '.png')))
    writePNG(img, paste0('player_headshots/', player_id, '.png'))
  
  return (img)
}

get_player_action_shot <- function(player_id){
  if (file.exists(paste0('player_action_shots/', player_id, '.png')))
    file <- paste0('player_action_shots/', player_id, '.png')
  else {
    url <- paste0('https://ak-static.cms.nba.com/wp-content/uploads/silos/nba/latest/440x700/',
                  player_id, '.png')
    file <- RCurl::getURLContent(url)
  }
  img <- png::readPNG(file)
  if (!file.exists(paste0('player_action_shots/', player_id, '.png')))
    writePNG(img, paste0('player_action_shots/', player_id, '.png'))
  
  return (img)
}

get_continous_lineups <- function(game_id, game_date){
  fourth_pbp_players <- get_pbp_data(game_id, game_date) %>%
    filter(period==4, player_code!='') %>%
    select(player_code) %>%
    distinct() %>%
    mutate(player_code=map_chr(str_split(player_code, '_'), 2),
           player_code=tools::toTitleCase(player_code))
  first_pbp_players <- get_pbp_data(game_id, game_date) %>%
    filter(period==1, player_code!='') %>%
    select(player_code) %>%
    distinct() %>%
    mutate(player_code=map_chr(str_split(player_code, '_'), 2),
           player_code=tools::toTitleCase(player_code))
  third_pbp_players <- get_pbp_data(game_id, game_date) %>%
    filter(period==3, player_code!='') %>%
    select(player_code) %>%
    distinct() %>%
    mutate(player_code=map_chr(str_split(player_code, '_'), 2),
           player_code=tools::toTitleCase(player_code))
  second_pbp_players <- get_pbp_data(game_id, game_date) %>%
    filter(period==2, player_code!='') %>%
    select(player_code) %>%
    distinct() %>%
    mutate(player_code=map_chr(str_split(player_code, '_'), 2),
           player_code=tools::toTitleCase(player_code))
    
  pbp_dt <- get_pbp_data(game_id, game_date) %>%
    select(player_code, person_id, description, period, clock, team_abr) %>%
    filter(stringr::str_detect(description, 'Substitution')) %>%
    mutate(players=stringr::str_split(description, ' Substitution replaced by '),
           EnterPlayer=purrr::map_chr(players, 2),
           ExitPlayer=purrr::map_chr(players, 1),
           ExitPlayer=purrr::map_chr(stringr::str_split(ExitPlayer, '] '), 2),
           clock=sapply(clock, str_time_to_secs),
           period=as.integer(period),
           secElapsed=pmin(period, 4)*60*12 + pmax(period-4, 0)*60*5 - clock) %>%
    select(-period, -clock, -description, -players)
  all_minutes <- data.frame(secElapsed=0:2880)
  
  players <- distinct(pbp_dt, EnterPlayer) %>%
    append(distinct(pbp_dt, ExitPlayer)) %>%
    unlist() %>%
    unique()
  
  for (player in players){
    minutes <- nba_api_get_player_continuous_minutes(player, pbp_dt, fourth_pbp_players, 
                                                     first_pbp_players, third_pbp_players,
                                                     second_pbp_players) %>%
      mutate(!!player := is_in_game) %>%
      select(-is_in_game)
    all_minutes <- all_minutes %>%
      full_join(minutes, by=c('secElapsed'='secElapsed'))
  }
  all_minutes[is.na(all_minutes)] <- 0
  all_minutes %>%
    rmarkdown::paged_table()
}

#################
# nba_api_get_player_continuous_minutes
#################

.fourth_quarter_checker <- function(dt, fourth_pbp_players, player){
  #if last sub before fourth and last action ==1
  #   if description exists in fourth quarter
  #       then add sub at start of fourth
  last_row <- dt[nrow(dt), ] %>%
    unlist()
  
  if (last_row['secElapsed'] < 60 * 12 * 3 & last_row['action'] == -1 & player %in% fourth_pbp_players$player_code){
      dt <- dt %>%
        rbind(data.frame(secElapsed=12*60*3,
                         action=1))
  }
  dt
}

.handle_start <- function(dt){
  if (dt[1, 'action'] == -1){
    dt <- list(secElapsed=floor(min(dt$secElapsed-1)/12/60)*12*60,
               action=1) %>%
      rbind(dt)
  }
  dt
}

.third_quarter_checker <- function(dt, third_pbp_players, player){
  if (length(dt[dt$secElapsed==1440, 'action']) > 0){
    if(dt[dt$secElapsed==1440, 'action'] == -1 & 
       length(dt[dt$secElapsed==2160, 'action'])==0 & 
       player %in% third_pbp_players$player_code){
      dt <- dt %>%
        filter(secElapsed!=1440) %>%
        rbind(data.frame(secElapsed=2160,
                         action=-1)) %>%
        arrange(secElapsed)
    }
  }
  dt
}

.second_quarter_checker <- function(dt, second_pbp_players, player){
    if(dt[dt$secElapsed==720, 'action'] == -1 & 
       length(dt[dt$secElapsed==1440, 'action'])==0 & 
       player %in% third_pbp_players$player_code){
      dt <- dt %>%
        filter(secElapsed!=720) %>%
        rbind(data.frame(secElapsed=1440,
                         action=-1)) %>%
        arrange(secElapsed)
    }
  dt
}

.handle_first_sub_at_quarter <- function(dt, first_pbp_players, player){
  if (dt[1, 'secElapsed'] > 12*60 & player %in% first_pbp_players$player_code){
    dt <- data.frame(secElapsed=floor(dt[1, 'secElapsed']/12/60)*12*60,
                     action=-dt[1, 'action']) %>%
      rbind(dt)
  }
  dt
}

nba_api_get_player_continuous_minutes <- function(player, dt, fourth_pbp_players, first_pbp_players,
                                                  third_pbp_players, second_pbp_players){
  dt <- dt %>%
    filter((EnterPlayer==player) | (ExitPlayer==player)) %>%
    mutate(action=(EnterPlayer==player)-(ExitPlayer==player)) %>%
    select(secElapsed, action) %>%
    arrange(secElapsed)

  #dt <- .handle_first_sub_at_quarter(dt, first_pbp_players, player)
  dt <- .handle_start(dt)
  

  quarter_break_sub <- dt %>%
    mutate(prev_action=lag(action),
           prev_sec=lag(secElapsed)) %>%
    filter(prev_action==action) %>%
    mutate(quarter_end=floor((secElapsed+prev_sec)/2/12/60),
           action=-action) %>%
    select(quarter_end, action) %>%
    filter(action==1)
  
  if (!(player %in% third_pbp_players$player_code)){
    quarter_break_sub <- quarter_break_sub %>%
      mutate(quarter_end=ifelse(quarter_end==2, 3, quarter_end))
  }
  if (!(player %in% second_pbp_players$player_code)){
    quarter_break_sub <- quarter_break_sub %>%
      mutate(quarter_end=ifelse(quarter_end==1, 2, quarter_end))
  }
  if (!(player %in% first_pbp_players$player_code)){
    quarter_break_sub <- quarter_break_sub %>%
      mutate(quarter_end=ifelse(quarter_end==1, 2, quarter_end))
  }
  dt <- quarter_break_sub %>%
    mutate(quarter_end=720*quarter_end) %>%
    rename(secElapsed=quarter_end) %>%
    rbind(dt) %>%
    arrange(secElapsed)
  
  quarter_break_sub <- dt %>%
    mutate(prev_action=lag(action),
           prev_sec=lag(secElapsed)) %>%
    filter(prev_action==action) %>%
    mutate(quarter_end=floor((secElapsed+prev_sec)/2/12/60),
           action=-action) %>%
    select(quarter_end, action) %>%
    filter(action==-1)
  
  if (!(player %in% third_pbp_players$player_code)){
    quarter_break_sub <- quarter_break_sub %>%
      mutate(quarter_end=ifelse(quarter_end==3, 4, quarter_end))
  }
  if (!(player %in% second_pbp_players$player_code)){
    quarter_break_sub <- quarter_break_sub %>%
      mutate(quarter_end=ifelse(quarter_end==2, 3, quarter_end))
  }
  if (!(player %in% first_pbp_players$player_code)){
    quarter_break_sub <- quarter_break_sub %>%
      mutate(quarter_end=ifelse(quarter_end==1, 2, quarter_end))
  }
  

  dt <- quarter_break_sub %>%
    mutate(quarter_end=720*quarter_end) %>%
    rename(secElapsed=quarter_end) %>%
    rbind(dt) %>%
    arrange(secElapsed)
  #dt <- .second_quarter_checker(dt, second_pbp_players, player)
  #dt <- .third_quarter_checker(dt, third_pbp_players, player)
  #dt <- .fourth_quarter_checker(dt, fourth_pbp_players, player)
  
    
  dt <- dt %>%
    full_join(data.frame(secElapsed=1:2880), by=c('secElapsed')) %>%
    mutate(is_in_game=0) %>%
    arrange(secElapsed)
  
  first_in_game <- dt %>%
    drop_na() %>%
    head(1) %>%
    select(secElapsed) %>%
    unlist()
  dt <- zoo::na.locf(dt, fromLast = FALSE) %>%
    mutate(is_in_game=(action==1)) %>%
    select(secElapsed, is_in_game) %>%
    rbind(data.frame(secElapsed=0:first_in_game,
                     is_in_game=0)) %>%
    arrange(secElapsed) %>%
    distinct() %>%
    group_by(secElapsed) %>%
    summarize(is_in_game=sum(is_in_game)) %>%
    ungroup()
  
  return (dt)
}