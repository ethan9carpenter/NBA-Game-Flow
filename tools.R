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
    rbind(list(Abbreviation='BKN',
               City='Brooklyn',
               Mascot='Nets'),
          list(Abbreviation='PHX',
               City='Phoenix',
               Mascot='Suns')) %>%
    mutate(full_name=paste(City, Mascot),
           primary_color=sapply(full_name, function(x)team_pal(x, 1)),
           secondary_color=sapply(full_name, function(x)team_pal(x, 2)))
}

add_image_annotation <- function(plot, img_path, y_loc, build){
  bounds <- get_plot_bounds(plot)
  
  img <- readPNG(img_path)
  
  width <- ncol(img)
  height <- nrow(img)

  x_scale <- (bounds$xmax-bounds$xmin) / (bounds$ymax-bounds$ymin) * height / width * .625
  
  x_width <- 400 / 2 * (bounds$xmax-bounds$xmin) / 2880
  x_shift <- -60 * (bounds$xmax-bounds$xmin) / 2880
  y_height <- x_width / x_scale
  
  plot <- plot +
    annotation_raster(img,
                      ymin=y_loc - y_height,
                      ymax=y_loc +  y_height,
                      xmin=bounds$xmax - x_width + x_shift,
                      xmax=bounds$xmax + x_width + x_shift)
  return (plot)
}

get_colors <- function(game, colors_master, allow_dupe=FALSE){
  colors <- list(home=colors_master[colors_master$Abbreviation==game$hTeam, 'primary_color'],
                 away=colors_master[colors_master$Abbreviation==game$vTeam, 'primary_color'])
  
  #modify so if colors are too similar using euclidian distance between rgb values
  if (colors$home==colors$away && !allow_dupe)
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