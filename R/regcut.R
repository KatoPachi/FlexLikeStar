

#' @title Get Results of Regression
#'
#' @name .regcut
#' @param reg regression with specific class
#' @param keep keep variable name
#' @param omit omit variable name
#' @param digits maximum digits
#' @param intercept.include logical value including intercept
#' @param star vector of symbols representing statistically significance at 10\%, 5\%, and 1\% level
#'
#' @return generate column of coefficient of regression
#'

# lm
lm.regcut <- function(
  reg, keep = NULL, omit = NULL, digits = NULL, intercept.include = NULL, star = NULL) {

  sig <- 1:3 %>% list.map(paste("%1.", digits, "f", star[.], sep = ""))
  insig <- paste("%1.", digits, "f", sep = "")
  se <- paste("(%1.", digits, "f)", sep = "")

  covariate.label <- rownames(summary(reg)$coefficient)[-1]

  if (!is.null(omit)) {
    omit.var <- paste(omit, collapse = "|")
    bool.omit <- !str_detect(covariate.label, omit.var)
  } else {
    bool.omit <- rep(TRUE, length(covariate.label))
  }

  if (!is.null(keep)) {
    keep.var <- paste(keep, collapse = "|")
    bool.keep <- str_detect(covariate.label, keep.var)
  } else {
    bool.keep <- rep(TRUE, length(covariate.label))
  }

  bool <- bool.keep + bool.omit == 2

  if (intercept.include) {
    covariate.extract <- c("(Intercept)", covariate.label[bool])
  } else {
    covariate.extract <- covariate.label[bool]
  }

  cut <- summary(reg)$coefficient[covariate.extract, c("Estimate", "Std. Error", "Pr(>|t|)")]
  cut <- matrix(cut, ncol = 3)
  colnames(cut) <- c("coef", "s.e.", "pval")
  rownames(cut) <- covariate.extract
  cut <- data.frame(cut)

  cut$coef <- ifelse(cut$pval > .1, sprintf(insig, cut$coef),
                     ifelse(cut$pval > .05, sprintf(sig[[1]], cut$coef),
                            ifelse(cut$pval > .01, sprintf(sig[[2]], cut$coef), sprintf(sig[[3]], cut$coef))))
  cut$s.e. <- sprintf(se, cut$s.e.)

  cut <- cut[,c("coef", "s.e.")]

  trim.cut <- data.frame(
    variables = rep(covariate.extract, each = 2),
    stat = rep(c("coef", "s.e."), length(covariate.extract)),
    stringsAsFactors = FALSE
  )
  trim.cut$value <- apply(trim.cut, MARGIN = 1, function(x) cut[x[1],x[2]])

  return(trim.cut)

}


#'
#' @name .regcut
#' @param reg regression with specific class
#' @param keep keep variable name
#' @param omit omit variable name
#' @param digits maximum digits
#' @param intercept.include logical value including intercept
#' @param star vector of symbols representing statistically significance at 10\%, 5\%, and 1\% level
#'


# coeftest
coeftest.regcut <- function(
  reg, keep = NULL, omit = NULL, digits = NULL, intercept.include = NULL, star = c("*", "**", "***")) {

  sig <- 1:3 %>% list.map(paste("%1.", digits, "f", star[.], sep = ""))
  insig <- paste("%1.", digits, "f", sep = "")
  se <- paste("(%1.", digits, "f)", sep = "")

  covariate.label <- rownames(reg)[-1]

  if (!is.null(omit)) {
    omit.var <- paste(omit, collapse = "|")
    bool.omit <- !str_detect(covariate.label, omit.var)
  } else {
    bool.omit <- rep(TRUE, length(covariate.label))
  }

  if (!is.null(keep)) {
    keep.var <- paste(keep, collapse = "|")
    bool.keep <- str_detect(covariate.label, keep.var)
  } else {
    bool.keep <- rep(TRUE, length(covariate.label))
  }

  bool <- bool.keep + bool.omit == 2

  if (intercept.include) {
    covariate.extract <- c("(Intercept)", covariate.label[bool])
  } else {
    covariate.extract <- covariate.label[bool]
  }

  cut <- reg[covariate.extract, c("Estimate", "Std. Error", "Pr(>|t|)")]
  colnames(cut) <- c("coef", "s.e.", "pval")
  cut <- data.frame(cut)

  cut$coef <- ifelse(cut$pval > .1, sprintf(insig, cut$coef),
                     ifelse(cut$pval > .05, sprintf(sig[[1]], cut$coef),
                            ifelse(cut$pval > .01, sprintf(sig[[2]], cut$coef), sprintf(sig[[3]], cut$coef))))
  cut$s.e. <- sprintf(se, cut$s.e.)

  cut <- cut[,c("coef", "s.e.")]

  trim.cut <- data.frame(
    variables = rep(covariate.extract, each = 2),
    stat = rep(c("coef", "s.e."), length(covariate.extract)),
    stringsAsFactors = FALSE
  )
  trim.cut$value <- apply(trim.cut, MARGIN = 1, function(x) cut[x[1],x[2]])

  return(trim.cut)

}
