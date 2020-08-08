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

get_continous_lineups <- function(game_id, game_date, team){
  master_pbp_dt <- get_pbp_data(game_id, game_date) %>%
    filter(team_abr==team) %>%
    mutate_if(is.character, tolower)
    
  pbp_dt <- master_pbp_dt %>%
    select(player_code, person_id, description, period, clock, team_abr) %>%
    filter(stringr::str_detect(description, 'substitution')) %>%
    mutate(players=stringr::str_split(description, ' substitution replaced by '),
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
    minutes <- nba_api_get_player_continuous_minutes(player, pbp_dt, master_pbp_dt) %>%
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

.handle_quarter_sits <- function(sub_dt, master_pbp_dt, player){
  pbp_dt <- master_pbp_dt %>%
    select(period, player_code) %>%
    distinct() %>%
    filter(player_code != '') %>%
    mutate(player_code=map_chr(str_split(player_code, '_'), 2),
           player_code=player_code,
           period=as.integer(period))
  
  sub_dt <- sub_dt %>%
    mutate(is_quarter=  secElapsed %% 720 == 0,
           is_next_quarter= lead(secElapsed) %% 720 == 0,
           is_prev_quarter= lag(secElapsed) %% 720 == 0)
  sub_dt[is.na(sub_dt)] <- FALSE
  
  sub_dt$is_in_next_quarter <- sapply(sub_dt$secElapsed, function(x){
    quarter <- ceiling(x/720) + 1
    data <- pbp_dt %>%
      filter(period==quarter,
             player_code==player)
    return (nrow(data) == 1)
  })

  sub_dt %>%
    filter(! (is_quarter & is_next_quarter & is_prev_quarter & !is_in_next_quarter)) %>%
    select(secElapsed, action)
}

nba_api_get_player_continuous_minutes <- function(player, pbp_dt, master_pbp_dt){
  dt <- pbp_dt %>%
    filter((EnterPlayer==player) | (ExitPlayer==player)) %>%
    mutate(action=(EnterPlayer==player)-(ExitPlayer==player)) %>%
    select(secElapsed, action) %>%
    arrange(secElapsed) %>%
    rbind(data.frame(secElapsed=c(0, 720, 720, 1440, 1440, 2160, 2160, 2880),
                     action=c(1, 1, -1, 1, -1, 1, -1, -1))) %>%
    arrange(secElapsed, action)
  
  temp <- dt %>%
    mutate(next_action=lead(action),
           next_sec=lead(secElapsed))
  
  to_rem <- data.frame(secElapsed=numeric(), action=numeric())
  
  for (i in 1:(nrow(temp)-1)){
    if (temp[i, 'action'] == temp[i, 'next_action']){
      if(temp[i, 'secElapsed'] %% 720 == 0){
        to_rem <- to_rem %>%
          rbind(data.frame(secElapsed=temp[i, 'secElapsed'],
                           action=temp[i, 'action']))
      } else {
        to_rem <- to_rem %>%
          rbind(data.frame(secElapsed=temp[i, 'next_sec'],
                           action=temp[i, 'next_action']))
      }
    }
  }
  to_rem <- to_rem %>%
    distinct()
  
  if (nrow(to_rem) > 0)
    for (i in 1:nrow(to_rem)){
      dt <- dt %>%
        filter(secElapsed != to_rem[i, 'secElapsed'] |
               action != to_rem[i, 'action'])
    }
  
  # deal with players sitting entire quarters here
  dt <- .handle_quarter_sits(dt, master_pbp_dt, player)

  temp <- master_pbp_dt %>%
    select(period, player_code) %>%
    distinct() %>%
    filter(player_code != '') %>%
    mutate(player_code=map_chr(str_split(player_code, '_'), 2),
           player_code=player_code,
           period=as.integer(period)) 
  
  # dt <- dt %>%
  #   mutate(avg_with_next_q=secElapsed + zoo::na.fill(lead(secElapsed), 2880),
  #          avg_with_next_q=ceiling(avg_with_next_q/720/2)) %>%
  #   filter(secElapsed %% 720 ==0 & 
  #            lead(secElapsed) %% 720 == 0 & 
  #            action == -1 &
  #            avg_with_next_q %in% ())
  #experimental
  # dt <- dt %>%
  #   filter(!(secElapsed %% 720 == 0 & lag(secElapsed) %% 720 == 0  & action == -1)) %>%
  #   filter(!(secElapsed %% 720 == 0 & lead(secElapsed) %% 720 == 0  & action == 1))

    
  dt <- dt %>%
    full_join(data.frame(secElapsed=0:2880), by=c('secElapsed')) %>%
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
  
  # Sat first quarter
  for (i in 0:3){
    temp <- master_pbp_dt %>%
      select(period, player_code) %>%
      distinct() %>%
      filter(player_code != '') %>%
      mutate(player_code=map_chr(str_split(player_code, '_'), 2),
             player_code=player_code,
             period=as.integer(period)) %>%
      filter(period==i+1,
             player_code==player)
    if (dt[1+720*i, 'is_in_game'] == 1 & nrow(temp) == 0){
      dt[(1+720*i):(720*(i+1)), 'is_in_game'] <- 0
    }
  }
  
  return (dt)
}