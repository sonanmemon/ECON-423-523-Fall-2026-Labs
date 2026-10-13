
# Packages
library(tinytex)
library(tidyverse)
library(dslabs)
library(dplyr)
library(ggplot2)
library(tibble)
library(modelsummary)
library(broom)

library(fixest)


set.seed(1)

data <- tibble(x = runif(100, 0, 10),
               e = rnorm(100, 0, 15),
               y = 10 + 1.5 * x + e)


ols_coefficients <- function(x, y) {
  x_bar <- mean(x)
  y_bar <- mean(y)
  beta_1 <- sum((x - x_bar) * (y - y_bar)) / sum((x - x_bar)^2)
  beta_0 <- y_bar - beta_1 * x_bar
  return(list(intercept = beta_0, slope = beta_1))
}



result <- ols_coefficients(data$x, data$y)

result



intercept <- result$intercept
slope <- result$slope

# OLS 1: Scatterplot:

ggplot(data = data, aes(x = x, y = y)) +
  geom_point() +
  geom_abline(slope = slope, intercept = intercept)



# OLS 2: lm function (univariate)

result <- lm(data = data, y ~ x)

result$coefficients

summary(result)

# OLS 3: lm function (multivariate)

n = 100

set.seed(1)

data <- tibble(x = runif(n, 0, 10),
               z = runif(n, 5, 15),
               e = rnorm(n, 0, 15),
               y = 10 + 1.5 * x - 3 * z + e)

model1 <- lm(data = data, y ~ x)

model2 <- lm(data = data, y ~ x + z)

# Regression Table


modelsummary(
  list(
    "Regression 1" = model1,
    "Regression 2" = model2
  ),
  stars = TRUE,
  statistic = "std.error",
  output = "tinytable",
  title = "Results From Simulated Data",
  gof_omit = ".*"
)



# Standardisation of Data:

n = 100

set.seed(1)

data = tibble(
  e = rnorm(n, sd = 2),
  v = rnorm(n, sd = 1),
  x = runif(n, min = 0, max = 10),
  y = 8 - 3 * x + e,
  z = 20 - 0.3 * y + 3 * x + v
)




# Confidence Interval:

lm1 <- lm(data = data, z ~ y)
lm2 <- lm(data = data, z ~ y + x)

results <- rbind(tidy(lm1), tidy(lm2)) %>% 
  filter(term == "y")

results <- results %>% mutate(
  lower_bound = estimate - 1.96 * std.error,
  upper_bound = estimate + 1.96 * std.error,
  model = c("without x", "with x")
)

# Plot Confidence Intervals

ggplot(data = results, aes(x = model, y = estimate)) +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound),
                width = 0.15) +
  geom_point(size = 3) +
  geom_hline(yintercept = -0.3, color = "red",
             linetype = "dashed") +
  scale_y_continuous(
    breaks = c(-0.3),
    labels = c("-0.3")
  ) +
  theme_bw() +
  labs(y = "Estimate", x = "Model")



# Hypothesis Testing
# H₀: β = -0.3, Hₐ: β ≠ -0.3
# Test statistic = (β̂ - β) / std.error


library(car)

linearHypothesis(lm1, "y = -0.3")


linearHypothesis(lm2, "y = -0.3")

beta1 <- results$estimate[1]
st.err1 <- results$std.error[1]
test1 <- (beta1 - (-0.3)) / st.err1

beta2 <- results$estimate[2]
st.err2 <- results$std.error[2]
test2 <- (beta2 - (-0.3)) / st.err2

test1
test2

# Decision: Ifelse
ifelse(abs(test1) > 1.96,
       "Reject the null hypothesis",
       "Fail to reject the null hypothesis")



setwd("C:/Users/sonan/OneDrive/Documents/ECON-320-Fall-2025-GE")



data <- readRDS("running_data.rds")





descriptive_stats <- data.frame(
  Variable = names(data),
  Mean = sapply(data, mean, na.rm = TRUE),
  SD = sapply(data, sd, na.rm = TRUE)
)

descriptive_stats





# 5% two-sided test → 2.5% in each tail

t_crit <- qt(0.975, df = n - 2)

t_crit


# Estimate the full multiple regression model:

model_full <- lm(
  time_5k ~ miles_week + sleep_hours +
    resting_hr + age + cross_train + temp_outdoor,
  data = data
)

summary(model_full)

# Extract the t-statistic for temperature
t_stat <- summary(model_full)$coefficients["temp_outdoor", "t value"]

# Display the t-statistic
t_stat


# Display both values
t_stat
t_crit





linearHypothesis(model_full, "temp_outdoor = 0")

linearHypothesis(model_full, "temp_outdoor = 0.03")


# Extract F-statistic info safely
f_stat_vec <- summary(model_full)$fstatistic   # named numeric vector
f_stat_vec

f_value <- as.numeric(f_stat_vec["value"])   # numeric F-statistic
df1 <- as.numeric(f_stat_vec["numdf"])      # numerator df
df2 <- as.numeric(f_stat_vec["dendf"])      # denominator df

# Compute p-value
p_val <- pf(f_value, df1, df2, lower.tail = FALSE)



# Compute 5% critical value
f_crit <- qf(0.95, df1, df2)


f_value - f_crit



pf(
  summary(model_full)$fstatistic["value"],
  summary(model_full)$fstatistic["numdf"],
  summary(model_full)$fstatistic["dendf"],
  lower.tail = FALSE
)


linearHypothesis(
  model_full,
  c(
    "miles_week = 0",
    "sleep_hours = 0",
    "resting_hr = 0",
    "age = 0",
    "cross_train = 0",
    "temp_outdoor = 0"
  )
)


# Regression with interaction term
model_interaction <- lm(
  time_5k ~ miles_week * temp_outdoor +
    sleep_hours + resting_hr + age + cross_train,
  data = data
)

summary(model_interaction)




# Default IID standard errors

#m1 <- feols(time_5k ~ miles_week + temp_outdoor, data = data)

# Heteroskedasticity-robust standard errors

#m2 <- feols(time_5k ~ miles_week + temp_outdoor,
           # data = data, vcov = "hetero")

# Clustered standard errors
#m3 <- feols(time_5k ~ miles_week + temp_outdoor,
            #data = data, vcov = ~school_id)



# Individual and year fixed effects:

#m4 <- feols(
  #wage ~ education + experience | person_id + year,
  #data = data
#)

m5 <- feols(
  time_5k ~ miles_week * temp_outdoor + sleep_hours,
  data = data
)

#m6 <- feols(
  #time_5k ~ miles_week + temp_outdoor,
  #data = data,
  #weights = ~sample_weight
#)




m7 <- feols(
  time_5k ~ miles_week + temp_outdoor,
  data = data,
  subset = ~age >= 18
)


#m8 <- feols(
  #time_5k ~ temp_outdoor | miles_week ~ instrument,
  #data = data
#)



















