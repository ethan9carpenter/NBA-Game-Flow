library(jsonlite)

get_pbp_data <- function(game_id, date, lowercase=TRUE){
  date <- get_nba_date_id(date)
  data <- fromJSON(paste0('http://data.nba.net/json/cms/noseason/game/', 
                          date, '/', game_id, '/pbp_all.json'))
  data <- data$sports_content$game$play
  
  if (lowercase)
    data <- data %>%
      mutate_if(is.character, tolower)
  
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

get_team_logo <- function(team_abr){
  .check_api_then_save_local(
    paste0('input_img/team_logos/', tolower(team_abr), '.png'),
    paste0('https://a.espncdn.com/i/teamlogos/nba/500/',
           tolower(team_abr), '.png'))
}

get_player_headshot <- function(player_id){
  .check_api_then_save_local(
    paste0('input_img/player_headshots/', player_id, '.png'),
    paste0('https://ak-static.cms.nba.com/wp-content/uploads/headshots/nba/latest/260x190/',
                                    player_id, '.png'))
}

get_player_action_shot <- function(player_id){
  .check_api_then_save_local(
    paste0('input_img/player_action_shots/', player_id, '.png'),
    paste0('https://ak-static.cms.nba.com/wp-content/uploads/silos/nba/latest/440x700/',
           player_id, '.png'))
}

.check_api_then_save_local <- function(fp, url){
  if (!file.exists(fp)){
    img <- png::readPNG(RCurl::getURLContent(url))
    writePNG(img, fp)
  } else
    img <- png::readPNG(fp)
  
  return (img)
}