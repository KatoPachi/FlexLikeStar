

#' @title Make table of regression statistics
#'
#' @name reginfotable
#' @param reginfo list of regression statistics extracted by .reginfo function
#'
#' @return data.frame of regression statistics
#'

reginfotable <- function(reginfo) {

  stats.list <- data.frame(
    key = c("n", "rsq", "adj.rsq", "f"),
    name = c("N", "R2", "Adjusted R2", "F-stat"),
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
