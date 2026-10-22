# Run once if needed:
install.packages(c("haven", "knitr", "marginaleffects"))

library(haven)
library(knitr)
library(marginaleffects)

# a) Read and merge the dataset

aux <- as.data.frame(read_xpt("~/Desktop/Data506 HW3/Problem 1/AUX_I.XPT"))
demo <- as.data.frame(read_xpt("~/Desktop/Data506 HW3/Problem 1/DEMO_I.XPT"))

# Match participants using SEQN; retain only matched records.
nhanes <- merge(aux, demo, by = "SEQN", all = FALSE)

# Print dimensions: rows, then columns.
dim(nhanes)

print(kable(
  data.frame(
    Dataset = "Merged AUX_I and DEMO_I",
    Rows = nrow(nhanes),
    Columns = ncol(nhanes) 
  ),
  caption = "Dimensions of the merged dataset"
))

# b) Clean the variables

dat <- data.frame(
  SEQN = nhanes$SEQN,

  # Values outside the listed levels become NA
  gender = factor(
    nhanes$RIAGENDR,
    levels = c(1, 2), #Set 1 as male and 2 as female
    labels = c("Male", "Female")
  ),

  citizenship = factor(
    nhanes$DMDCITZN,
    levels = c(1, 2), #Set 1 as citizen and 2 as noncitizen
    labels = c("Citizen", "Noncitizen")
  ),

  children = as.numeric(nhanes$DMDHHSZA),

  width_right = as.numeric(nhanes$AUXTWIDR),
  width_left = as.numeric(nhanes$AUXTWIDL)
)

# Children: 3 means 3 or more
# Keep numeric as requested, acknowledging this top-coding
dat$children[!dat$children %in% 0:3] <- NA


# Income ordering issue:
# 12 = $20,000 or more; 13 = under $20,000.
# These broad categories overlap the detailed income brackets.
# They cannot be placed uniquely in the detailed ordering.
#
# Solution: exclude these broad responses from the derived score,
# retain detailed categories, and assign increasing scores 1–12.
# Codes 77 and 99 is unknown also become NA.
#
# This score represents income brackets, NOT dollars, and assumes
# an equal log-mean change per bracket despite unequal widths

income_codes <- c(1:10, 14, 15)

income_labels <- c(
  "$0–4,999",
  "$5,000–9,999",
  "$10,000–14,999",
  "$15,000–19,999",
  "$20,000–24,999",
  "$25,000–34,999",
  "$35,000–44,999",
  "$45,000–54,999",
  "$55,000–64,999",
  "$65,000–74,999",
  "$75,000–99,999",
  "$100,000+"
)

dat$income_category <- factor(
  nhanes$INDHHIN2,
  levels = income_codes,
  labels = income_labels,
  ordered = TRUE
)

# Numeric version for the regression models.
dat$income_score <- as.numeric(dat$income_category)


# Clean special codes in the outcomes.
special_codes <- c(555, 777, 888)

dat$width_right[dat$width_right %in% special_codes] <- NA
dat$width_left[dat$width_left %in% special_codes] <- NA


# Check the cleaned variables.
summary(dat)

print(kable(
  data.frame(
    Variable = names(dat),
    Missing = colSums(is.na(dat)),
    row.names = NULL
  ),
  caption = "Missing values after cleaning"
))


# c) Fit four poisson regression models and repsent the result

m1R <- glm(
  width_right ~ gender,
  family = poisson(link = "log"),
  data = dat,
  na.action = na.omit
)

m2R <- glm(
  width_right ~ gender + citizenship + children + income_score,
  family = poisson(link = "log"),
  data = dat,
  na.action = na.omit
)

m1L <- glm(
  width_left ~ gender,
  family = poisson(link = "log"),
  data = dat,
  na.action = na.omit
)

m2L <- glm(
  width_left ~ gender + citizenship + children + income_score,
  family = poisson(link = "log"),
  data = dat,
  na.action = na.omit
)

models <- list(
  "1R" = m1R,
  "2R" = m2R,
  "1L" = m1L,
  "2L" = m2L
)


# Coefficient table: IRRs and 95% Wald CIs

# IRR = exp(beta).
# Here it represents a ratio of expected widths.
# Omit the intercept because exp(intercept) is a baseline mean.

term_labels <- c(
  genderFemale = "Female vs Male",
  citizenshipNoncitizen = "Noncitizen vs Citizen",
  children = "Children: per 1-unit increase",
  income_score = "Income: per bracket increase"
)

irr_table <- data.frame(
  Predictor = unname(term_labels)
)

for (model_name in names(models)) {

  model <- models[[model_name]]

  beta <- coef(model)
  se <- sqrt(diag(vcov(model)))

  irr <- exp(beta)
  lower <- exp(beta - qnorm(0.975) * se)
  upper <- exp(beta + qnorm(0.975) * se)

  cells <- rep("—", length(term_labels))

  positions <- match(names(term_labels), names(beta))
  present <- !is.na(positions)
  j <- positions[present]

  cells[present] <- sprintf(
    "%.3f (%.3f, %.3f)",
    irr[j], lower[j], upper[j]
  )

  irr_table[[model_name]] <- cells
}

print(kable(
  irr_table,
  caption = "Poisson regression: IRRs with 95% Wald confidence intervals"
))


# Model statistics: N, McFadden pseudo R squared, AIC

# McFadden pseudo squared = 1 - logLik(model) / logLik(null).
# Fit the null model using exactly the same observations.

model_statistics <- function(model) {

  outcome <- model.response(model.frame(model))

  null_model <- glm(
    outcome ~ 1,
    family = poisson(link = "log")
  )

  data.frame(
    N = nobs(model),

    McFadden_R2 = 1 -
      as.numeric(logLik(model)) /
      as.numeric(logLik(null_model)),

    AIC = AIC(model)
  )
}

stats_table <- do.call(
  rbind,
  lapply(models, model_statistics)
)

stats_table <- data.frame(
  Model = rownames(stats_table),
  stats_table,
  row.names = NULL
)

print(kable(
  stats_table,
  digits = c(0, 0, 3, 1),
  col.names = c("Model", "N", "McFadden pseudo-R²", "AIC"),
  caption = "Poisson regression model statistics"
))

# AIC comparisons require the same outcome and observations.
# McFadden pseudo R squared is not the OLS proportion of variance explained

# d) Model 2L gender tests and interpretation

#Test 1: Does the female-to-male IRR differ from 1?

# H0: exp(beta_genderFemale) = 1
# Equivalently: H0: beta_genderFemale = 0.

gender_result <- summary(m2L)$coefficients["genderFemale", ]

b <- unname(gender_result["Estimate"])
se <- unname(gender_result["Std. Error"])

gender_irr <- exp(b)
gender_lower <- exp(b - qnorm(0.975) * se)
gender_upper <- exp(b + qnorm(0.975) * se)

gender_z <- b / se
gender_p <- 2 * pnorm(-abs(gender_z))

gender_test <- data.frame(
  Comparison = "Female vs Male",
  IRR = gender_irr,
  Lower_95 = gender_lower,
  Upper_95 = gender_upper,
  z = gender_z,
  p_value = format.pval(gender_p, digits = 3, eps = 0.001)
)

print(kable(
  gender_test,
  digits = 3,
  caption = "Model 2L: Wald test of female-to-male IRR = 1"
))


# Interpretation using the actual fitted results.

percent_difference <- 100 * (gender_irr - 1)

cat(sprintf(
  paste0(
    "\nThe female-to-male ratio of expected left-ear width ",
    "is %.3f (95%% CI %.3f to %.3f).\n",
    "Holding the other predictors constant, expected width ",
    "for females is estimated to be %.2f%% %s than for males.\n",
    "Wald z = %.3f; p-value %s.\n"
  ),
  gender_irr,
  gender_lower,
  gender_upper,
  abs(percent_difference),
  ifelse(percent_difference >= 0, "higher", "lower"),
  gender_z,
  format.pval(gender_p, digits = 3, eps = 0.001)
))

if (gender_p < 0.05) {
  cat(
    "Reject H0 at the 5% level: there is evidence of an",
    " adjusted gender difference in expected left-ear width.\n"
  )
} else {
  cat(
    "Do not reject H0 at the 5% level: there is insufficient",
    " evidence of an adjusted gender difference.\n"
  )
}


#Test 2: Do adjusted predicted widths differ?

# Use the model 2L sample.
# Predict everyone once as Male and once as Female,
# keeping all other predictors unchanged.

used <- model.frame(m2L)

male_data <- used
female_data <- used

male_data$gender <- factor(
  rep("Male", nrow(used)),
  levels = levels(used$gender)
)

female_data$gender <- factor(
  rep("Female", nrow(used)),
  levels = levels(used$gender)
)

male_predictions <- predict(
  m2L,
  newdata = male_data,
  type = "response"
)

female_predictions <- predict(
  m2L,
  newdata = female_data,
  type = "response"
)

predicted_means <- data.frame(
  Gender = c("Male", "Female"),
  Adjusted_mean_width = c(
    mean(male_predictions),
    mean(female_predictions)
  )
)

print(kable(
  predicted_means,
  digits = 2,
  caption = "Model 2L: adjusted mean predicted left-ear width"
))


# H0: average predicted width for Female minus Male = 0.
# Account for uncertainty in the fitted coefficients.

prediction_test <- avg_comparisons(
  m2L,
  variables = list(gender = c("Male", "Female")),
  newdata = used,
  type = "response",
  comparison = "difference"
)

result <- as.data.frame(prediction_test)

prediction_table <- data.frame(
  Comparison = "Female minus Male",
  Difference = result$estimate,
  SE = result$std.error,
  Lower_95 = result$conf.low,
  Upper_95 = result$conf.high,
  z = result$statistic,
  p_value = format.pval(
    result$p.value,
    digits = 3,
    eps = 0.001
  )
)

print(kable(
  prediction_table,
  digits = 3,
  caption = "Model 2L: test of the adjusted predicted-width difference"
))


# Interpretation using the actual results.

cat(sprintf(
  paste0(
    "\nAdjusted mean predicted width:\n",
    "Male: %.2f\n",
    "Female: %.2f\n",
    "Female minus Male: %.3f (95%% CI %.3f to %.3f).\n",
    "Wald z = %.3f; p-value %s.\n"
  ),
  mean(male_predictions),
  mean(female_predictions),
  result$estimate,
  result$conf.low,
  result$conf.high,
  result$statistic,
  format.pval(result$p.value, digits = 3, eps = 0.001)
))

if (result$p.value < 0.05) {
  cat(
    "Reject H0 at the 5% level: adjusted mean predicted",
    " left-ear width differs between females and males.\n"
  )
} else {
  cat(
    "Do not reject H0 at the 5% level: there is insufficient",
    " evidence of a difference in adjusted predicted width.\n"
  )
}

# Both tests address the same underlying null in this model
# without gender interactions. Their Wald p-values may differ
# because they operate on different scales.
#
# These are ordinary, unweighted Poisson fits as requested.
# Their inference assumes the Poisson model and does not account
# for NHANES's complex survey design.


# References:
# NHANES demographics:
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/DEMO_I.htm
#
# NHANES audiometry:
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/AUX_I.htm
#
# R generalized linear models:
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/glm.html
#
# Prediction comparisons:
# https://marginaleffects.com/man/r/comparisons.html