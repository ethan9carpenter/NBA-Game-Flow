game_flow_plot <- function(game, font_family, font_color, colors_master, win_size_sec, span,
                       show=TRUE, save=TRUE){
  pbp_dt <- get_pbp_data(game$gameId, date) %>% 
    prep_game_flow_dt()
  
  final_score <- list(home=pbp_dt[nrow(pbp_dt), 'home_score'],
                      away=pbp_dt[nrow(pbp_dt), 'visitor_score'])
  colors <- list(home=colors_master[colors_master$Abbreviation==game$hTeam, 'primary_color'],
                 away=colors_master[colors_master$Abbreviation==game$vTeam, 'primary_color'])
  
  if (colors$home==colors$away)
    colors$away <- colors_master[colors_master$Abbreviation==game$vTeam, 'secondary_color']
  
  if (max(pbp_dt$secElapsed) > win_size_sec){
    pbp_dt <- pbp_dt %>% 
      arrange(secElapsed) %>%
      mutate(home_per_48=48*60/win_size_sec * sum_run(home_pts, k=win_size_sec, idx=secElapsed),
             away_per_48=48*60/win_size_sec * sum_run(away_pts, k=win_size_sec, idx=secElapsed),
             diff_per_48=home_per_48-away_per_48) %>%
      filter(secElapsed >= win_size_sec/2)
    
    top_background_color <- ifelse(pbp_dt[nrow(pbp_dt), 'home_per_48'] > pbp_dt[nrow(pbp_dt), 'away_per_48'],
                                   colors$home,
                                   colors$away)
    bottom_background_color <- ifelse(top_background_color == colors$home, colors$away, colors$home)
    dt <- pbp_dt %>%
      select(secElapsed, home_per_48, away_per_48) %>%
      mutate(seconds=secElapsed-lag(secElapsed),
             seconds=ifelse(is.na(seconds), secElapsed, seconds),
             home_lead=home_per_48 > away_per_48,
             away_lead=home_per_48 < away_per_48) %>%
      group_by(home_lead, away_lead) %>%
      summarize(leading=sum(seconds)) %>%
      ungroup() %>%
      mutate(leading=leading/sum(leading)) %>%
      filter(home_lead | away_lead) %>%
      mutate(abbr=ifelse(home_lead & !away_lead, game$hTeam, game$vTeam)) %>%
      select(abbr, leading) %>%
      arrange(-leading)
    
    percent_playing_better_text <- paste(dt[1, 'abbr'], scales::percent(unlist(dt[1, 'leading'])), '-',
                                  dt[2, 'abbr'], scales::percent((unlist(dt[2, 'leading']))))
    
    p <- ggplot(pbp_dt) +
      geom_smooth(aes(x=secElapsed, y=home_per_48, color='Home'), 
                  se=FALSE, span=span, method='loess', formula='y~x', color=colors$home) +
      geom_smooth(aes(x=secElapsed, y=away_per_48, color='Away'), 
                  se=FALSE, span=span, method='loess', formula='y~x', color=colors$away) +
      scale_x_continuous(labels=c('Q1', 'Q2', 'Q3', 'Q4', 'OT'), 
                         breaks=c(seq(0.5, 3.5) * 60 * 12, 12*60*4 + seq(0.5, 0.5) * 5 * 60),
                         minor_breaks = c()) +
      theme(plot.background = element_gradient(fill1 = top_background_color, 
                                               fill2 = bottom_background_color,
                                               color = 'transparent',
                                               direction = 'vertical'),
            text = element_text(color=font_color,
                                face='bold',
                                family=font_family),
            axis.text = element_text(color=font_color,
                                     face='bold',
                                     family=font_family),
            legend.position = 'none',
            axis.ticks = element_line(color='transparent'),
            plot.caption = element_text(size=8),
            plot.tag.position = c(.08, .01),
            plot.tag = element_text(size=8)) +
      labs(x = "", 
           y = "Points per 48 Minutes",
           title = paste("Game Flow:", game$hTeam, final_score$home, '-', game$vTeam, final_score$away),
           subtitle = paste(percent_playing_better_text, '\t', format(as.Date(date), '%b. %e, %Y')),
           caption = "Source: NBA.com",
           tag = '@ethan9carpenter')
  
    p <- neat_y_axis(p)
    
    p <- add_rects(p)
    
    p <- add_team_logos(p, game$hTeam, game$vTeam)
    
    if (show)    
      p %>% show()
    if (save)
      ggsave(paste0('game_plots/', date, '-', game$hTeam, '-', game$vTeam, '-off_plot.png'), dpi=300)
  }
  return (list(plot=p,
               dt=pbp_dt))
}

neat_y_axis <- function(p){
  break_size <- 20
  build <- ggplot_build(p)
  y_min <- build$layout$panel_scales_y[[1]]$range$range[1]
  y_max <- build$layout$panel_scales_y[[1]]$range$range[2]
  
  p + 
    expand_limits(y=round(y_min/break_size) * break_size) +
    scale_y_continuous(breaks=seq(round(y_min/break_size) * break_size,
                                  round(y_max/break_size) * break_size,
                                  break_size),
                       minor_breaks=c())
}

add_rects <- function(p) {
  plot_xmax <- get_plot_bounds(p)$xmax
  rects <- data.frame(xstart = c(-Inf, seq(0, 4) * 12 * 60, Inf), 
                      xend= c(-Inf, seq(1, 4) * 12 * 60, 3180, Inf), 
                      col = as.character(seq(1, 7) %% 2)) %>% 
    filter(xend <= plot_xmax + 120)
  
  p <- p + geom_rect(data = rects, 
                     aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), 
                     alpha = 0.4) +
    scale_fill_manual(values=rep(c('#b3b3b3', '#ffffff'), 4))
}

prep_game_flow_dt <- function(pbp_dt){
  pbp_dt <- get_pbp_data(game$gameId, date) %>% 
    select(home_score, visitor_score, event, clock, period) %>%
    mutate(clock=sapply(clock, str_time_to_secs)) %>% 
    mutate_if(is.character, as.integer) %>%
    mutate(secElapsed=pmin(period, 4)*60*12 + pmax(period-4, 0)*60*5 - clock,
           home_pts=home_score-lag(home_score, 1),
           away_pts=visitor_score-lag(visitor_score, 1)) %>%
    filter(away_pts + home_pts > 0)
  
  return (pbp_dt)
}