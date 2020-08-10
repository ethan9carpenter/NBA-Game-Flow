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