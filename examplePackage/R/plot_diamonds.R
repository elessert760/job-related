
#' plot_diamonds
#'
#' @param rows and integer for sampling the diamonds data
#'
#' @return
#' @export
#' @keywords plot
#'
#' @examples
#' plot_diamonds(999)
#' plot_diamonds(10001)
plot_diamonds <- function(rows = 1000){
  
  if(rows<1000){
    warning("rows arg should be > 1000 for a good plot")
  }
  
  diamonds <- diamonds
  keep = sample(1:nrow(diamonds), rows, replace = F)
  
 gg <- ggplot(diamonds[keep,], aes(x=1, y=price, fill = color)) +
   geom_boxplot( alpha = 0.6) + 
   theme_light() +
   theme(legend.position = "bottom", axis.text.y = element_blank()) +
   labs(x = "") +
   coord_flip()
  gg
  
}

plot_diamonds()
