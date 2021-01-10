#' @include tab3_function.r
#' extract_num
extract_num <- function(vectorInp){
  as.numeric(str_extract(vectorInp, "[[:digit:]]"))
}
