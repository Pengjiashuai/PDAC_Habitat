if (!require("logistf")) install.packages("logistf")
if (!require("caret")) install.packages("caret")
if (!require("dplyr")) install.packages("dplyr")
if (!require("car")) install.packages("car")
if (!require("vcd")) install.packages("vcd")

library(logistf)
library(caret)
library(dplyr)
library(car)
library(vcd)

data <- read.csv("your_data.csv")

outcome_col <- "outcome"
predictors <- c("var1", "var2", "var3")

for (var in predictors) {
  if (is.character(data[[var]])) {
    data[[var]] <- as.factor(data[[var]])
  }
}

formula <- as.formula(paste(outcome_col, "~", paste(predictors, collapse = " + ")))

glm_model <- try(glm(formula, data = data, family = binomial), silent = TRUE)

if (!inherits(glm_model, "try-error")) {
  vif_values <- vif(glm_model)
  print(vif_values)
} else {
  warning("普通 logistic 回归未能收敛")
}

cramer_v <- assocstats(table(data$var1, data$var2))$cramer
print(cramer_v)

firth_model <- logistf(formula, data = data, firth = TRUE)

summary(firth_model)

result <- data.frame(
  variable = names(coef(firth_model))[-1],
  coefficient = coef(firth_model)[-1],
  odds_ratio = exp(coef(firth_model))[-1],
  ci_lower = exp(confint(firth_model))[-1, 1],
  ci_upper = exp(confint(firth_model))[-1, 2],
  p_value = firth_model$prob[-1]
)

print(result)

write.csv(result, "firth_regression_results.csv", row.names = FALSE)