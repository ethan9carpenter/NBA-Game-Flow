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
    mutate(full_name=paste(City, Mascot)) %>%
    mutate(primary_color=sapply(full_name, function(x)team_pal(x, 1)),
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

add_team_logos <- function(plot, h_team, v_team){
  build <- ggplot_build(plot)
  
  home_data <- build$data[[1]]$y
  plot <- add_image_annotation(plot, paste0("team_logos/", h_team, ".png"), 
                               home_data[length(home_data)], build)
  
  away_data <- build$data[[2]]$y
  plot <- add_image_annotation(plot, paste0("team_logos/", v_team, ".png"), 
                               away_data[length(away_data)], build)
  
  return (plot)
}

get_plot_bounds <- function(plot){
  layout <- ggplot_build(plot)$layout
  
  bounds <- list(xmin=layout$panel_scales_x[[1]]$range$range[1],
                 xmax=layout$panel_scales_x[[1]]$range$range[2],
                 ymin=layout$panel_scales_y[[1]]$range$range[1],
                 ymax=layout$panel_scales_y[[1]]$range$range[2])
  
  return (bounds)
}