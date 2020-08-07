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
    minutes <- nba_api_get_player_continuous_minutes(player, pbp_dt) %>%
      mutate(!!player := in_game) %>%
      select(-in_game)
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

.handle_start <- function(dt){
  if (dt[1, 'action'] == -1){
    dt <- list(secElapsed=floor(min(dt$secElapsed)/12/60)*12*60,
               action=1) %>%
      rbind(dt)
  }
  dt
}

.handle_first_sub_at_quarter <- function(dt){
  if (dt[1, 'action']==1 & dt[1, 'secElapsed'] > 12*60){
    dt <- list(secElapsed=floor(dt[1, 'secElapsed']/12/60)*12*60,
               action=-1) %>%
      rbind(dt)
  }
  dt
}

nba_api_get_player_continuous_minutes <- function(player, dt){
  dt <- dt %>%
    filter((EnterPlayer==player) | (ExitPlayer==player)) %>%
    mutate(action=(EnterPlayer==player)-(ExitPlayer==player)) %>%
    select(secElapsed, action) %>%
    arrange(secElapsed)
  
  dt <- .handle_first_sub_at_quarter(dt)
  dt <- .handle_start(dt)
  
  game_dt <- dt %>%
    rbind(list(secElapsed=c(60*12, 60*12*2, 60*12*3),
               action=rep(-1, 3))) %>%
    #filter to only add quarters that are within player bounds
    arrange(secElapsed)
  original <- game_dt
  
  #something is wrong here
  game_dt <- game_dt %>%
    arrange(secElapsed) %>%
    mutate(prev_action=lag(action, 1),
           prev_time=lag(secElapsed, 1)) %>%
    filter(action==prev_action) %>%
    mutate(quarter_sec=(secElapsed + prev_time)/2/12/60,
           quarter_sec=round(quarter_sec)*12*60,
           action=-1 * action) %>%
    select(secElapsed, action)  %>%
    rbind(original) %>%
    group_by(secElapsed) %>%
    summarize(action=sum(action)) %>%
    ungroup() %>%
    filter(action != 0) %>%
    full_join(data.frame(secElapsed=1:2880), by=c('secElapsed')) %>%
    mutate(is_in_game=0) %>%
    arrange(secElapsed)
  
  game_dt <- zoo::na.locf(game_dt, fromLast = FALSE) %>% 
    mutate(is_in_game=(action==1)) %>%
    select(secElapsed, is_in_game)
  
  plot_dt <- game_dt %>%
    distinct() %>%
    group_by(secElapsed) %>%
    summarize(in_game=sum(is_in_game))
  return (plot_dt)
}