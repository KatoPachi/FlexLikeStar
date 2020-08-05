
utils::globalVariables(c("key", "stat"))

#' @title Get Statistics of Linear Regression
#'
#' @name lm.reginfo
#' @param reg regression with class "lm"
#' @param keep.stat statistics you want to obtain
#' @param df logical value whether to show degree-of-freedom
#' @param digits maximum digits
#'
#' @return list: dataframe that only contains statistics of regressions, and names of extracting statistics
#'
#' @export
#'

lm.reginfo <- function(reg, keep.stat = NULL, df = NULL, digits = NULL) {

  full.stat <- data.frame(
    rsq = round(unlist(summary(reg)["r.squared"]), digits),
    adj.rsq = round(unlist(summary(reg)["adj.r.squared"]), digits),
    f = round(unlist(summary(reg)["fstatistic"])[1], digits),
    n = reg$rank + reg$df.residual
  )

  numdf <- unlist(summary(reg)["fstatistic"])[2]
  dendf <- unlist(summary(reg)["fstatistic"])[3]
  dftext <- paste("(df=", numdf, ";", dendf, ")", sep = "")

  if (df) full.stat[,"f"] <- paste(full.stat$f, dftext)

  show.stat <- data.frame(
    key = keep.stat,
    stringsAsFactors = FALSE
  )

  show.stat$stat <- apply(show.stat, MARGIN = 1, function(x) as.character(full.stat[,x[1]]))

  return(list(list = data.frame(keep.stat), value = show.stat))

}
