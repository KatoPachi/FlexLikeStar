

#' @title Make table of regression statistics
#'
#' @name reginfotable
#' @param reginfo list of regression statistics extracted by .reginfo function
#'
#' @return data.frame of regression statistics
#' @export

reginfotable <- function(reginfo) {

  stats.list <- data.frame(
    key = c("rsq", "adj.rsq", "f", "n"),
    name = c("R2", "Adjusted R2", "F-stat", "N"),
    stringsAsFactors =  FALSE
  )

  keep <- reginfo %>%
    list.map(as.matrix(.[["list"]])) %>% list.cases()
  keep.list <- stats.list[stats.list$key %in% keep,]

  res <- reginfo %>%
    list.map(~apply(keep.list, MARGIN = 1, function(x) .$value[.$value$key == x[1], "stat"])) %>%
    list.cbind()

  tab <- cbind(keep.list$name, res)
  rownames(tab) <- NULL

  return(tab)

}
