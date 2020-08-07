#https://stackoverflow.com/questions/55355261/how-to-make-a-gradient-colour-in-strip-background-in-ggplot

element_gradient <- function(fill1 = NULL, fill2 = NULL, direction = NULL,
                             colour = NULL, size = NULL,
                             linetype = NULL, color = NULL, inherit.blank = FALSE) {
  if (!is.null(color))  colour <- color
  structure(
    list(fill1 = fill1, fill2 = fill2, direction = direction,
         colour = colour, size = size, linetype = linetype,
         inherit.blank = inherit.blank),
    class = c("element_gradient", "element")
  )
}

element_grob.element_gradient <- function(
  element, 
  fill1 = "white", fill2 = "red", direction = "horizontal", # default: white-red gradient
  x = 0.5, y = 0.5, width = 1, height = 1, colour = NULL, 
  size = NULL, linetype = NULL, ...) {
  
  # define gradient colours & direction
  if(!is.null(element$fill1)) 
    fill1 <- element$fill1
  if(!is.null(element$fill2)) 
    fill2 <- element$fill2
  if(!is.null(element$direction)) 
    direction <- element$direction  
  
  image <- colorRampPalette(c(fill1, fill2), bias=0.1)(1000)  
  if(direction == "horizontal") {
    image <- matrix(image, nrow = 1)
  } else {
    image <- matrix(image, ncol = 1)
  }
  
  gp <- grid::gpar(lwd = ggplot2:::len0_null(size * .pt), col = colour, lty = linetype)
  element_gp <- grid::gpar(lwd = ggplot2:::len0_null(element$size * .pt), col = element$colour,
                           lty = element$linetype, fill = NA)  
  grid::grobTree(
    grid::rasterGrob(image, x, y, width, height, ...),
    grid::rectGrob(x, y, width, height, gp = utils::modifyList(element_gp, gp), ...))
}


begin_modifying <- function(){
  ggplot_global.new <- ggplot2:::ggplot_global
  ggplot_global.new$element_tree$gradient <- ggplot2:::el_def("element_gradient")
  ggplot_global.new$element_tree$plot.background <- ggplot2:::el_def("element_gradient", "gradient")
  ggplot_global.new$element_tree$plot.background.x <- ggplot2:::el_def("element_gradient", "plot.background")
  ggplot_global.new$element_tree$plot.background.y <- ggplot2:::el_def("element_gradient", "plot.background")
}
# Run trace(ggplot2:::merge_element.element, edit = TRUE) and replace
# if (!inherits(new, class(old)[1])) {
#   stop("Only elements of the same class can be merged", call. = FALSE)
# }
# 
# with
# 
# if (!inherits(new, class(old)[1]) & class(new)[1] != "element_gradient") {
#   stop("Only elements of the same class can be merged", call. = FALSE)
# }
#
# Run trace(ggplot2:::validate_element, edit = TRUE) and replace
# else if (!inherits(el, eldef$class) && 
#          !inherits(el, "element_blank")) {
#   stop("Element ", elname, " must be a ", eldef$class, " object.")
# }
# with
# 
# else if (!inherits(el, eldef$class) &&
#          !inherits(el, "element_blank") && eldef$class != "element_gradient") {
#   stop("Element ", elname, " must be a ", eldef$class, " object.")

end_modifying <- function(){
  ggplot_global.new$element_tree$plot.background <- ggplot2:::el_def("element_rect", "rect")
  ggplot_global.new$element_tree$plot.background.x <- ggplot2:::el_def("element_rect", "plot.background")
  ggplot_global.new$element_tree$plot.background.y <- ggplot2:::el_def("element_rect", "plot.background")
  
  untrace(ggplot2:::merge_element.element)
  untrace(ggplot2:::validate_element)
}

custom_merge_element <- function (new, old) 
{
  if (!inherits(new, class(old)[1]) & class(new)[1] != "element_gradient") {
    print(class(new)[1])
    stop("Only elements of the same class can be merged", 
         call. = FALSE)
  }
  idx <- vapply(new, is.null, logical(1))
  idx <- names(idx[idx])
  new[idx] <- old[idx]
  new
}

custom_validate_element <- function (el, elname) 
{
  eldef <- ggplot2:::ggplot_global$element_tree[[elname]]
  if (is.null(eldef)) {
    stop("\"", elname, "\" is not a valid theme element name.")
  }
  if (is.null(el)) 
    return()
  if (eldef$class == "character") {
    if (!is.character(el) && !is.numeric(el)) 
      stop("Element ", elname, " must be a string or numeric vector.")
  }
  else if (eldef$class == "margin") {
    if (!is.unit(el) && length(el) == 4) 
      stop("Element ", elname, " must be a unit vector of length 4.")
  }
  else if (!inherits(el, eldef$class) && 
           !inherits(el, "element_blank") && 
           eldef$class != "element_gradient") {
    stop("Element ", elname, " must be a ", eldef$class, 
         " object.")
  }
  invisible()
}


