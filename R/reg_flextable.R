
bottom_border <- function(x) {
  a <- hline_bottom(x, part = "header", border = fp_border()) %>%
    hline_bottom(part = "body", border = fp_border())
  return(a)
}

#' @title Generate Regression Table
#'
#' @name reg.flextable
#'
#' @param ... list of regressions which extract regression stats
#' @param robust list of regressions which extract regression coefficients
#' @param title character of table title
#' @param label character of table label which returns (#tab:"label"). When you use the cross-reference, you should use this option
#' @param dep.var.caption a character vector of captions for dependent variables in regression tables.
#' @param dep.var.caption.separate a numeric vector that specifies how dep.var.caption should be laid out across regression table columns.
#' @param column.labels a character vector of labels for columns in regression tables. Their layout, in terms of the number of columns associated with each label, is given by the argument column.separate.
#' @param column.separate a numeric vector that specifies how column.labels should be laid out across regression table columns. A value of c(2, 1, 3), for instance, will apply the first label to the two first columns, the second label to the third column, and the third label will apply to the following three columns (i.e., columns number four, five and six).
#' @param keep a vector of regular expressions that specifies which of the explanatory variables should be kept in the table.
#' @param omit a vector of regular expressions that specifies which of the explanatory variables should be omitted in the table.
#' @param intercept.include logical value including intercept
#' @param order a vector of numerical indexes that indicates the order in which variables will appear in the output.
#' @param covariate.labels a character vector of labels for covariates in regression tables. A value of NA for any element means that stargazer will print the corresponding variable name.
#' @param add.lines a list containing vectors that shows additional lines.
#' @param df a logical value that indicates whether the degrees of freedom of model statistics should be reported.
#' @param digits an integer that indicates how many decimal places should be used.
#' @param notes a character vector containing notes to be included below the table.
#' @param font.size an integer that specifies the font size used in the table.
#' @param autofit a logical value that indicates whether autofit function in flextable works (Strongly recommend TRUE)
#' @param covariate.width a numeric value of width of covariate column (inches)
#' @param reg.width a numeric value of width of regression column (inches)
#' @param minor.change a function that change specific style of table
#' @param star a character vector symbols representing statistically significance at 10\%, 5\%, and 1\% level
#' @param keep.stat a character vector that specifies which model statistics should be kept in the regression table output. For instance keep.stat = c("n","adj.rsq") will produce a table that only includes statistics for the number of observations and Adjusted R-squared.
#'
#' @return object "flextable" with regression table
#'
#' @export

reg.flextable <- function(
  ..., robust = NULL,
  title = NULL, label = NULL,
  dep.var.caption = NULL, dep.var.caption.separate = NULL,
  column.labels = NULL, column.separate = NULL,
  keep = NULL, omit = NULL,
  intercept.include = TRUE,
  order = NULL, covariate.labels = NULL,
  add.lines = NULL,
  df = TRUE, digits = 2,
  notes = NULL, font.size = 12, autofit = TRUE,
  covariate.width = 1.5, reg.width = 1,
  minor.change,
  star = c("*", "**", "***"),
  keep.stat = c("n")
) {

  if (is.null(robust)) {
    regn <- length(...)
  } else {
    if (length(...) != length(robust)) {
      stop("Not same number of baseline and robust regressions")
    } else {
      regn <- length(...)
    }
  }

  if (is.null(robust)) {

    cut <- ... %>%
      list.map(
        if (class(.) == "lm") {
          lm.regcut(., keep = keep, omit = omit, intercept.include = intercept.include ,digits = digits, star = star)
        } else if (class(.) == "coeftest") {
          coeftest.regcut(., keep = keep, omit = omit , intercept.include = intercept.include, digits = digits, star = star)
        } else {
          stop("Unsupported class.")
        }
      )

  } else {

    cut <- robust %>%
      list.map(
        if (class(.) == "lm") {
          lm.regcut(., keep = keep, omit = omit, intercept.include = intercept.include, digits = digits, star = star)
        } else if (class(.) == "coeftest") {
          coeftest.regcut(., keep = keep, omit = omit, intercept.include = intercept.include, digits = digits, star = star)
        } else {
          stop("Unsupported class.")
        }
      )

  }

  cut.tab <- regcuttable(cut, covariate.labels, order)

  info <- ... %>%
    list.map(
      if (class(.) == "lm") {
        lm.reginfo(., keep.stat = keep.stat, df = df, digits = digits)
      } else {
        stop("Unsupported class included. update now...")
      }
    )

  info.tab <- reginfotable(info)

  if(is.null(add.lines)) {

    show.tab <- data.frame(rbind(cut.tab, info.tab))

  } else {
    TF <- add.lines %>% list.map(~ifelse(length(.) == ncol(cut.tab), 0, 1)) %>% unlist() %>% sum()

    if (TF == 0) {
      add.tab <- add.lines %>% list.rbind()
    } else {
      stop("Error: there are vectors with invalid length. You should align with #regressions + 1.")
    }

    show.tab <- data.frame(rbind(cut.tab, add.tab, info.tab))
  }

  reg_name <- 1:regn %>%
    list.map(paste("(", ., ")", sep="")) %>%
    unlist()
  colnames(show.tab) <- c("Variables", reg_name)

  shape <- flextable(show.tab) %>%
    set_header_labels("Variables" = "") %>%
    add_footer_row(
      values = paste(star[3], " Significance at 1% level", sep = ""),
      colwidths = regn + 1) %>%
    add_footer_row(
      values = paste(star[2], " Significance at 5% level", sep = ""),
      colwidths = regn + 1, top = FALSE) %>%
    add_footer_row(
      values = paste(star[1], " Significance at 10% level", sep = ""),
      colwidths = regn + 1, top = FALSE)

  if (!is.null(dep.var.caption)) {

    if (!is.null(dep.var.caption.separate)) {
      sep <- dep.var.caption.separate

      labs <- 1:length(sep) %>%
        list.map(c(rep(dep.var.caption[.], sep[.]))) %>%
        unlist()
      labs <- c("", labs)
    } else {
      labs <- c("", dep.var.caption)
    }

    shape <- shape %>%
      add_header_row(values = labs, top = TRUE) %>%
      merge_h(part = "header")
  }

  if (!is.null(column.labels)) {

    if(!is.null(column.separate)) {
      sep <- column.separate

      labs <- 1:length(sep) %>%
        list.map(c(rep(column.labels[.], sep[.]))) %>%
        unlist()
      labs <- c("", labs)
    } else {
      labs <- c("", column.labels)
    }

    shape <- shape %>%
      add_header_row(values = labs, top = FALSE) %>%
      merge_h(part = "header")
  }

  if (!is.null(notes)) {
    shape <- shape %>%
      add_footer_row(
        values = paste("Note: ", notes, sep = ""),
        colwidths = regn + 1, top = FALSE)
  }

  if (autofit) {shape <- shape %>% autofit()}

  shape <- shape %>% width(j = 1, covariate.width)

  if (is.null(dep.var.caption)) {
    shape <- shape %>% width(j = 2:(regn+1), reg.width)
  } else {
    if (is.null(dep.var.caption.separate)) {
      count <- unique(dep.var.caption) %>%
        list.map(sum(dep.var.caption == .)) %>%
        unlist()

      colnum.from <- NULL
      for (i in 1:length(count)) {
        colnum.from[i] <- 2 + sum(count[1:i]) - count[i]
      }
      colnum.end <- colnum.from + count - 1

      for (i in 1:length(count)) {
        shape <- shape %>%
          width(j = colnum.from[i]:colnum.end[i], width = count[i]*reg.width)
      }
    } else {
      count <- dep.var.caption.separate

      colnum.from <- NULL
      for (i in 1:length(count)) {
        colnum.from[i] <- 2 + sum(count[1:i]) - count[i]
      }
      colnum.end <- colnum.from + count - 1

      for (i in 1:length(count)) {
        shape <- shape %>%
          width(j = colnum.from[i]:colnum.end[i], width = count[i]*reg.width)
      }
    }
  }

  shape <- shape %>%
    align(j = 1, align = "left", part = "all") %>%
    align(j = seq(2, regn + 1), align = "center", part = "all") %>%
    fontsize(size = font.size, part = "all")

  shape <- shape %>%
    border_remove() %>%
    hline_top(part = "header", border = fp_border()) %>%
    hline_bottom(part = "head", border = fp_border()) %>%
    hline_bottom(part = "body", border = fp_border())

  if (!is.null(label)) {
    caption.label <- paste("(\\#tab:", label, ")", sep = "")
    caption <- paste(caption.label, title, sep = " ")
  } else {
    caption <- title
  }

  if (!is.null(caption)) shape <- shape %>% set_caption(caption)


  if(!is.null(dep.var.caption)) {
      shape <- shape %>%
        hline(i = 1, j = 2:(regn+1), part = "header", border = fp_border())
  }

  if (!missing(minor.change)) {shape <- shape %>% minor.change}

  return(shape)

}
