#' explore_diamonds
#'this function prints a nice markdowm summary of the diamonds data set
#' @return
#' @export
#' @keywords explore
#' @examples
#' explore_diamonds()
explore_diamonds <- function(){
  library(tidyverse)
  library(pander)
  diamonds %>% summary %>% pander()
  
  
}
