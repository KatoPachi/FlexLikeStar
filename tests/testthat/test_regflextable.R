context("regression table used flextable")

dt <- data.frame(Titanic)
dt <- dt[rep(seq_len(nrow(dt)), dt$Freq), c("Class", "Sex", "Age", "Survived")]
dt$Survived <- ifelse(dt$Survived == "Yes", 1, 0)

reg1 <- lm(Survived ~ Sex, data = dt)
reg2 <- lm(Survived ~ Sex * Class, data = dt)
reg3 <- lm(Survived ~ Sex * Class + Sex * Age, data = dt)

list.reg1 <- list(reg1, reg2, reg3)

test_that("reg.flextable", {

  cut <- list.reg1 %>% list.map(~lm.regcut(., keep = c("Sex"), digits = 3, intercept.include = FALSE, star = c("*", "**", "***")))
  cut.tab <- regcuttable(
    cut,
    covariate.labels = c("female", "female X 2nd Class", "female X 3rd Class", "female X Crew", "female X Adult"),
    order = c(1, 3, 4, 5, 2)
  )

  info <- list.reg1 %>% list.map(~lm.reginfo(., keep.stat = c("n", "adj.rsq"), df = TRUE, digits = 3))
  info.tab <- reginfotable(info)

  show.tab <- data.frame(rbind(cut.tab, info.tab))

  reg_name <- 1:3 %>% list.map(paste("(", ., ")", sep="")) %>% unlist()
  colnames(show.tab) <- c("Variables", reg_name)

  want <- flextable(show.tab) %>%
    set_header_labels("Variables" = "") %>%
    add_footer_row(
      values = paste("***", " Significance at 1% level", sep = ""),
      colwidths = 4) %>%
    add_footer_row(
      values = paste("**", " Significance at 5% level", sep = ""),
      colwidths = 4, top = FALSE) %>%
    add_footer_row(
      values = paste("*", " Significance at 10% level", sep = ""),
      colwidths = 4, top = FALSE) %>%
    add_header_row(values = c("", rep("Survaival", 3)), top = TRUE) %>%
    merge_h(part = "header") %>%
    autofit() %>%
    width(j = 1, 1) %>%
    width(j = 2:4, width = 1.5) %>%
    align(j = 1, align = "left", part = "all") %>%
    align(j = 2:4, align = "center", part = "all") %>%
    fontsize(size = 12, part = "all") %>%
    border_remove() %>%
    hline_top(part = "header", border = fp_border()) %>%
    hline_bottom(part = "head", border = fp_border()) %>%
    hline_bottom(part = "body", border = fp_border()) %>%
    set_caption("(\\#tab:taitanic) Survaival Probability of Taitanic") %>%
    hline(i = 1, j = 2:4, part = "header", border = fp_border())

  test <- reg.flextable(
    list.reg1,
    title = "Survaival Probability of Taitanic",
    label = "taitanic",
    dep.var.caption = c("Survaival"), dep.var.caption.separate = c(3),
    keep = c("Sex"),
    intercept.include = FALSE,
    covariate.labels = c("female", "female X 2nd Class", "female X 3rd Class", "female X Crew", "female X Adult"),
    order = c(1, 3, 4, 5, 2),
    digits = 3, covariate.width = 1, reg.width = .5,
    keep.stat = c("n", "adj.rsq")
  )

  expect_equal(test, want)

})


