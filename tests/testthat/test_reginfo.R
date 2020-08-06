context("regression information")

dt <- data.frame(Titanic)
dt <- dt[rep(seq_len(nrow(dt)), dt$Freq), c("Class", "Sex", "Age", "Survived")]
dt$Survived <- ifelse(dt$Survived == "Yes", 1, 0)

reg1 <- lm(Survived ~ Sex, data = dt)
reg2 <- lm(Survived ~ Sex * Class, data = dt)
reg3 <- lm(Survived ~ Sex * Class + Sex * Age, data = dt)

test_that("lm.reginfo", {

  adj.rsq <- round(unlist(summary(reg2)["adj.r.squared"]), 3)
  n <- reg2$rank + reg2$df.residual
  f <- round(unlist(summary(reg2)["fstatistic"])[1], 3)

  numdf <- unlist(summary(reg2)["fstatistic"])[2]
  dendf <- unlist(summary(reg2)["fstatistic"])[3]
  dftext <- paste("(df=", numdf, ";", dendf, ")", sep = "")

  f <- paste(f, dftext)

  wantdf1 <- data.frame(
    key = c("n", "adj.rsq", "f"),
    stat = c(as.character(n), as.character(adj.rsq), f),
    stringsAsFactors = FALSE
  )

  wantdf2 <- data.frame(keep.stat = c("n", "adj.rsq", "f"))

  testdf <- lm.reginfo(
    reg2,
    keep.stat = c("n", "adj.rsq", "f"), df = TRUE,
    digits = 3
  )

  expect_equal(testdf$value, wantdf1)
  expect_equal(testdf$list, wantdf2)

})

test_that("reginfotable", {

  list.info <- list(reg1, reg2, reg3) %>%
    list.map(~lm.reginfo(., c("n", "adj.rsq"), df = FALSE, digits = 3))

  res <- list.info %>% list.map(~.$value[,"stat"]) %>% list.cbind()
  want.tab <- cbind(c("N", "Adjusted R2"), res)
  rownames(want.tab) <- NULL

  test.tab <- reginfotable(list.info)

  expect_that(test.tab, is_equivalent_to(want.tab))

})
