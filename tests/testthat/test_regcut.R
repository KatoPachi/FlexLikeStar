context("regression results")

library(lmtest)

dt <- data.frame(Titanic)
dt <- dt[rep(seq_len(nrow(dt)), dt$Freq), c("Class", "Sex", "Age", "Survived")]
dt$Survived <- ifelse(dt$Survived == "Yes", 1, 0)

reg1 <- lm(Survived ~ Sex, data = dt)
reg2 <- lm(Survived ~ Sex * Class, data = dt)
reg3 <- lm(Survived ~ Sex * Class + Sex * Age, data = dt)

test1 <- coeftest(reg1, type = "HC1")
test2 <- coeftest(reg2, type = "HC1")
test3 <- coeftest(reg3, type = "HC1")

test_that("lm.regcut", {

  variables <- rownames(summary(reg2)$coefficients)[c(1, 2, 6, 7, 8)]
  variables <- rep(variables, each = 2)
  stat <- rep(c("coef", "s.e."), length(variables)/2)
  wantdf <- data.frame(variables, stat, stringsAsFactors = FALSE)

  value <- summary(reg2)$coefficients[c(1, 2, 6, 7, 8), c("Estimate", "Std. Error", "Pr(>|t|)")]
  colnames(value) <- c("coef", "s.e.", "pval")
  value <- data.frame(value)
  value$s.e. <- sprintf("(%1.2f)", value$s.e.)
  value$coef <- ifelse(value$pval < .01, sprintf("%1.2f***", value$coef),
                       ifelse(value$pval < .05, sprintf("%1.2f**", value$coef),
                              ifelse(value$pval < .1, sprintf("%1.2f*", value$coef), sprintf("%1.2f", value$coef))))

  wantdf$value <- apply(wantdf, MARGIN = 1, function(x) value[x[1], x[2]])

  testdf <- lm.regcut(
    reg2, keep = c("Sex"), omit = NULL,
    digits = 2, intercept.include = TRUE,
    star = c("*", "**", "***")
  )

  expect_equal(testdf, wantdf)

})

test_that("coeftest.regcut", {

  variables <- rownames(test2)[c(1, 2, 6, 7, 8)]
  variables <- rep(variables, each = 2)
  stat <- rep(c("coef", "s.e."), length(variables)/2)
  wantdf <- data.frame(variables, stat, stringsAsFactors = FALSE)

  value <- test2[c(1, 2, 6, 7, 8), c("Estimate", "Std. Error", "Pr(>|t|)")]
  colnames(value) <- c("coef", "s.e.", "pval")
  value <- data.frame(value)
  value$s.e. <- sprintf("(%1.2f)", value$s.e.)
  value$coef <- ifelse(value$pval < .01, sprintf("%1.2f***", value$coef),
                       ifelse(value$pval < .05, sprintf("%1.2f**", value$coef),
                              ifelse(value$pval < .1, sprintf("%1.2f*", value$coef), sprintf("%1.2f", value$coef))))

  wantdf$value <- apply(wantdf, MARGIN = 1, function(x) value[x[1], x[2]])

  testdf <- coeftest.regcut(
    test2,
    keep = c("Sex"),
    digits = 2, intercept.include = TRUE,
    star = c("*", "**", "***")
  )

  expect_equal(testdf, wantdf)
})

test_that("regcuttable", {

  list.reg1 <- list(reg1, reg2, reg3) %>%
    list.map(~lm.regcut(.,keep = "Sex", digits = 3, intercept.include = FALSE, star = c("*", "**", "***")))

  var_name <- list.reg1 %>%
    list.map(~c(.[,"variables"])) %>%
    list.cases()

  var_name <- var_name[c(1, 3, 4, 5, 2)]

  var_merge <- data.frame(
    variables = rep(var_name, each = 2),
    stat = rep(c("coef", "s.e."), length(var_name)),
    stringsAsFactors = FALSE
  )

  coeftab <- list.reg1 %>%
    list.map(~apply(var_merge, MARGIN = 1, function(x) .[.$variables == x[1] & .$stat == x[2], "value"])) %>%
    list.cbind()

  name <- c(c("Female", "Female X 2nd Class"), var_name[-(1:2)])
  name <- name %>% list.map(c(., "")) %>% unlist()

  want.tab <- cbind(name, coeftab)
  rownames(want.tab) <- NULL

  test.tab <- regcuttable(
    regcut = list.reg1,
    covariate.labels = c("Female", "Female X 2nd Class"), order = c(1, 3, 4, 5, 2)
  )

  expect_equal(test.tab, want.tab)
})
