.handle_quarter_sits <- function(sub_dt, master_pbp_dt, player){
  pbp_dt <- master_pbp_dt %>%
    select(period, player_code) %>%
    distinct() %>%
    filter(player_code != '') %>%
    mutate(player_code=map_chr(str_split(player_code, '_'), 2),
           player_code=player_code,
           period=as.integer(period))
  
  sub_dt[is.na(sub_dt)] <- FALSE
  
  sub_dt$is_in_next_quarter <- sapply(sub_dt$secElapsed, function(x){
    quarter <- ceiling(x/720) + 1
    data <- pbp_dt %>%
      filter(period==quarter,
             player_code==player)
    return (nrow(data) == 1)
  })
  
  sub_dt %>%
    filter(! (secElapsed %% 720 == 0 & 
                lead(secElapsed) %% 720 == 0 & 
                lag(secElapsed) %% 720 == 0 & 
                !is_in_next_quarter)) %>%
    select(secElapsed, action)
}

nba_api_get_player_continuous_minutes <- function(player, pbp_dt, master_pbp_dt){
  dt <- pbp_dt %>%
    filter((EnterPlayer==player) | (ExitPlayer==player)) %>%
    mutate(action=(EnterPlayer==player)-(ExitPlayer==player)) %>%
    select(secElapsed, action) %>%
    rbind(data.frame(secElapsed=c(0, 720, 720, 1440, 1440, 2160, 2160, 2880),
                     action=c(1, 1, -1, 1, -1, 1, -1, -1))) %>%
    arrange(secElapsed, action)
  
  temp <- dt %>%
    mutate(next_action=lead(action),
           next_sec=lead(secElapsed))
  
  to_rem <- data.frame(secElapsed=numeric(), action=numeric())
  
  for (i in 1:(nrow(temp)-1))
    if (temp[i, 'action'] == temp[i, 'next_action'])
      if(temp[i, 'secElapsed'] %% 720 == 0){
        val <- as.integer(temp[i, 'secElapsed'] %% 720 == 0)
        to_rem <- to_rem %>%
          rbind(data.frame(secElapsed=temp[i, 'secElapsed'],
                           action=temp[i, 'action']))
      } else {
        to_rem <- to_rem %>%
          rbind(data.frame(secElapsed=temp[i, 'next_sec'],
                           action=temp[i, 'next_action']))
      }
  dt <- dt %>%
    anti_join(to_rem, by = c("secElapsed", "action")) %>%
    full_join(data.frame(secElapsed=0:2880), by=c('secElapsed')) %>%
    arrange(secElapsed)
  
  first_in_game <- dt %>% 
    drop_na() %>% 
    select(secElapsed) %>% 
    min()
  
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
  
  for (i in 0:3){
    did_player_in_q <- master_pbp_dt %>%
      select(period, player_code) %>%
      filter(player_code != '') %>%
      mutate(player_code=map_chr(str_split(player_code, '_'), 2),
             period=as.integer(period)) %>%
      filter(period==i+1,
             player_code==player) %>%
      nrow() > 0
    if (dt[1+720*i, 'is_in_game'] == 1 & !did_player_in_q)
      dt[(1+720*i):(720*(i+1)), 'is_in_game'] <- 0
  }
  
  return (dt)
}

get_continous_lineups <- function(master_pbp_dt){
  return_data <- list()
  
  for (team in unique(master_pbp_dt$team_abr)){
    pbp_dt <- master_pbp_dt %>%
      select(player_code, person_id, description, period, clock, team_abr) %>%
      filter(stringr::str_detect(description, 'substitution'), team_abr==team) %>%
      mutate(players=stringr::str_split(description, ' substitution replaced by '),
             EnterPlayer=purrr::map_chr(players, 2),
             ExitPlayer=purrr::map_chr(players, 1),
             ExitPlayer=purrr::map_chr(stringr::str_split(ExitPlayer, '] '), 2),
             clock=sapply(clock, str_time_to_secs),
             period=as.integer(period),
             secElapsed=pmin(period, 4)*60*12 + pmax(period-4, 0)*60*5 - clock) %>%
      select(player_code, person_id, team_abr, EnterPlayer, ExitPlayer, secElapsed)
    all_minutes <- data.frame(secElapsed=0:2880)
    
    players <- select(pbp_dt, EnterPlayer) %>%
      append(select(pbp_dt, ExitPlayer)) %>%
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
    return_data[[team]] <- all_minutes
  }
  
  return (return_data)
}

get_lineup_player_codes <- function(pbp_dt){
   pbp_dt %>%
    select(person_id, player_code, team_abr) %>%
    distinct() %>%
    filter(str_detect(player_code, '_')) %>%
    handle_names() %>%
    mutate(player_code=map_chr(str_split(player_code, '_'), 2))
}

get_substitution_plots <- function(pbp_dt, font_family, colors_master, show=TRUE, save=TRUE){
  master_dt <- get_continous_lineups(pbp_dt)
  master_player_codes <- get_lineup_player_codes(pbp_dt)
  
  for (team in names(master_dt)){
    dt <- master_dt[[team]]
    color <- colors_master %>%
      filter(Abbreviation==toupper(team)) %>%
      select(primary_color, secondary_color)
    font_color <- color$secondary_color
    
    player_codes <- master_player_codes %>%
      filter(team_abr==team)
    colnames(dt) <- map_chr(str_split(colnames(dt), ' '), 1)
    dt <- dt[, colnames(dt) %in% player_codes$player_code]
    
    
    new_dt <- data.frame(player=character(),
                         in_game=numeric(),
                         n=numeric(),
                         person_id=character(),
                         team_abr=character(),
                         seconds=numeric())
    dt <- dt[sort(colnames(dt), TRUE)]
    
    for (i in 1:length(colnames(dt))){
      col <- colnames(dt)[[i]]
      
      player_info <- player_codes %>%
        filter(player_code==col) %>%
        select(person_id) %>%
        distinct() %>%
        head(1)
      
      if (nrow(player_info) > 0)
        new_dt <- new_dt %>%
          rbind(data.frame(player=col,
                           in_game=dt[[col]],
                           n=i,
                           person_id=player_info,
                           team_abr=team,
                           seconds=0:(nrow(dt)-1)))
    }
    
    plot_dt <- new_dt %>%
      drop_na() %>%
      filter(in_game==1)
    labels <- plot_dt %>% 
      arrange(n) %>%
      select(player) %>% 
      distinct() %>%
      unlist() %>%
      as.vector()
    
    p <- ggplot(plot_dt) +
      geom_rect(aes(xmin=n-1,
                    xmax=n,
                    ymin=seconds,
                    ymax=seconds+1),
                fill=color$primary_color) +
      geom_hline(yintercept=seq(1, 4)*60*12, color=color$secondary_color) +
      theme(legend.position = 'none',
            panel.grid.major.y = element_blank(),
            panel.grid.major.x = element_line(color=color$secondary_color),
            plot.background = element_rect(fill=color$primary_color, 
                                           color=color$primary_color),
            text = element_text(color=font_color,
                                face='bold',
                                family=font_family),
            axis.text = element_text(color=font_color,
                                     face='bold',
                                     family=font_family),
            panel.border = element_rect(color=color$secondary_color, fill='transparent', size=1)) +
      ggtitle(paste(toupper(team), 'Substitutions')) +
      scale_y_continuous(breaks=1:4 * 720, 
                         labels=c('End Q1', 'End Q2', 'End Q3', 'End Q4')) +
      scale_x_continuous(labels=tools::toTitleCase(labels),
                         breaks=1:length(labels) - 0.5,
                         limits=c(0, max(plot_dt$n))) +
      annotation_raster(png::readPNG(paste0('team_logos/', toupper(team), '.png')),
                        ymin=2880-470,
                        ymax=2880+100,
                        xmin=length(labels)-1.75,
                        xmax=length(labels)+.5) +
      coord_flip()
    
    p <- .add_headshots(p, plot_dt)

    if (show)
      p %>% show()
    if (save)
      ggsave(paste0('substitution_plots/', team, game_id, 'sub_plot.png'))
  }
}

.add_headshots <- function(p, plot_dt){
  for (i in 1:max(plot_dt$n)){
    player_code <- plot_dt %>%
      filter(n==i) %>%
      select(person_id) %>%
      distinct() %>%
      unlist()
    
    if (length(player_code) > 0)
      p <- p +
        annotation_raster(get_player_headshot(player_code),
                          i-1, i,
                          0, 300
        )
  }
  return (p)
}