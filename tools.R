str_time_to_secs <- function(x){
  x <- ifelse(x=='', '12:00', x)
  x <- strsplit(x, ':') %>%
    lapply(as.integer) %>%
    unlist()
  x <- x[[1]] * 60 + x[[2]]
  return (x)
}

get_colors_master <- function(){
  dbGetQuery(dbConnect(drv=RSQLite::SQLite(), dbname="~/data/NBA_data.sqlite3"), 
             "SELECT Mascot, City, Abbreviation FROM teams") %>% 
    mutate(full_name=paste(City, Mascot),
           primary_color=sapply(full_name, function(x)team_pal(x, 1)),
           secondary_color=sapply(full_name, function(x)team_pal(x, 2)))
}

get_colors <- function(game, colors_master, allow_dupe=FALSE){
  colors <- list(home=colors_master[colors_master$Abbreviation==game$hTeam, 'primary_color'],
                 away=colors_master[colors_master$Abbreviation==game$vTeam, 'primary_color'])

  #modify so if colors are too similar using euclidian distance between rgb values
  diff <- abs(as.numeric(sub('#', '0x', colors$away)) - 
    as.numeric(sub('#', '0x', colors$home)))
  
  if (diff < 10e2 && !allow_dupe)
    colors$away <- colors_master[colors_master$Abbreviation==game$vTeam, 'secondary_color']
  
  return (colors)
}

get_plot_bounds <- function(plot){
  layout <- ggplot_build(plot)$layout
  
  bounds <- list(xmin=layout$panel_scales_x[[1]]$range$range[1],
                 xmax=layout$panel_scales_x[[1]]$range$range[2],
                 ymin=layout$panel_scales_y[[1]]$range$range[1],
                 ymax=layout$panel_scales_y[[1]]$range$range[2])
  
  return (bounds)
}