
# Packages
library(tinytex)
library(tidyverse)
library(dslabs)
library(dplyr)
library(ggplot2)
library(tibble)

library(modelsummary)

library(broom)

library(sandwich)

library(fixest)

library(lmtest)


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

n = 10000

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




n = 1000

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



data <- read.csv("nlsy79-2020.csv")




# Default IID standard errors



m1 <- feols(log_wealth ~ foreign_language, data = data)

# Heteroskedasticity-robust standard errors

m2 <- feols(log_wealth ~ foreign_language + sex + log_educ,
 
                       data = data, vcov = "hetero")

# or:


m2 <- lm(
  log_wealth ~ foreign_language + sex + log_educ,
  data = data
)

summary(m2)

# Heteroskedasticity-robust standard errors:

coeftest(m2, vcov. = vcovHC(m2, type = "HC1"))



# Clustered Standard Errors:


#m3 <- feols(log_wealth ~ foreign_language + sex + log_educ,
# data = data, vcov = ~country)



# Interaction Term:


m4 <- feols(log_wealth ~ foreign_language + sex + 
              sex*foreign_language,
            data = data,
)





# Individual and year fixed effects:

#m_panel <- feols(
#wage ~ education + experience | person_id + year,
#data = data,
#vcov = ~ person_id + year
#)


# Inverse, Probability Weights, Precision Weights 

# m_weights <- feols(
#time_5k ~ miles_week + temp_outdoor,
#data = data,
#weights = ~sample_weight
#)



m5 <- feols(
  log_wealth ~ foreign_language + sex,
  data = data,
  subset = ~log_wealth >= 10
)




# Homoskedastic vs Heteroskedastic Standard Errors:




library(ggplot2)
library(dplyr)
library(patchwork)

set.seed(1)

# ============================================
# 1. Simulate data
# ============================================

n <- 500

x <- runif(n, min = 0, max = 10)

# Homoskedastic errors: constant standard deviation
# Heteroskedastic errors: standard deviation increases with X

datar <- bind_rows(
  data.frame(
    x = x,
    y = 2 + 1.5 * x + rnorm(n, mean = 0, sd = 2),
    Type = "Homoskedasticity"
  ),
  data.frame(
    x = x,
    y = 2 + 1.5 * x +
      rnorm(n, mean = 0, sd = 0.3 + 0.5 * x),
    Type = "Heteroskedasticity"
  )
)


# ============================================
# 2. Conditional mean and variance bands
# ============================================

x_grid <- seq(0, 10, length.out = 300)

bands <- data.frame(
  x = rep(x_grid, 2),
  Type = rep(
    c("Homoskedasticity", "Heteroskedasticity"),
    each = length(x_grid)
  )
) %>%
  mutate(
    mean_y = 2 + 1.5 * x,
    sd_y = ifelse(
      Type == "Homoskedasticity",
      2,
      0.3 + 0.5 * x
    ),
    lower = mean_y - 1.96 * sd_y,
    upper = mean_y + 1.96 * sd_y
  )


# ============================================
# 3. Plot function
# ============================================

make_plot <- function(df, band_df, title, color) {
  
  ggplot() +
    
    # Shaded 95% conditional distribution band
    geom_ribbon(
      data = band_df,
      aes(x = x, ymin = lower, ymax = upper),
      fill = color,
      alpha = 0.15
    ) +
    
    # Upper distribution boundary
    geom_line(
      data = band_df,
      aes(x = x, y = upper),
      color = color,
      linetype = "dashed",
      linewidth = 0.8
    ) +
    
    # Lower distribution boundary
    geom_line(
      data = band_df,
      aes(x = x, y = lower),
      color = color,
      linetype = "dashed",
      linewidth = 0.8
    ) +
    
    # Simulated observations
    geom_point(
      data = df,
      aes(x = x, y = y),
      color = color,
      alpha = 0.40,
      size = 1.5
    ) +
    
    # True conditional mean
    geom_abline(
      intercept = 2,
      slope = 1.5,
      color = "black",
      linewidth = 1
    ) +
    
    labs(
      title = title,
      x = "Independent Variable (X)",
      y = "Dependent Variable (Y)"
    ) +
    
    coord_cartesian(
      xlim = c(0, 10),
      ylim = c(-5, 30)
    ) +
    
    theme_classic(base_size = 13) +
    
    theme(
      plot.title = element_text(
        face = "bold",
        hjust = 0.5,
        size = 13
      ),
      axis.title = element_text(face = "bold"),
      panel.grid = element_blank()
    )
}


# ============================================
# 4. Generate individual plots
# ============================================

homo <- make_plot(
  filter(datar, Type == "Homoskedasticity"),
  filter(bands, Type == "Homoskedasticity"),
  "Homoskedasticity: Constant Variance",
  "#2166AC"
)

hetero <- make_plot(
  filter(datar, Type == "Heteroskedasticity"),
  filter(bands, Type == "Heteroskedasticity"),
  "Heteroskedasticity: Increasing Variance",
  "#B2182B"
)




combined <- homo | hetero

print(combined)



# Bootstrapping:


# Bootstrapping is a statistical resampling method used to approximate 
# the sampling distribution of an estimator without requiring 
# repeated collection of new datasets.

# The central idea is straightforward: treat the observed sample as 
# an approximation of the population and repeatedly draw new samples 
# from it, with replacement.


library(ggplot2)
library(dplyr)
library(patchwork)

set.seed(1)

# ==============================================
#  Generate original sample
# ==============================================

n <- 500
B <- 2000

original <- rnorm(n, mean = 50, sd = 10)

original_mean <- mean(original)

print(original_mean)

# ==============================================
#  Draw one bootstrap sample
# ==============================================

bootstrap_indices <- sample(
  1:n,
  size = n,
  replace = TRUE
)

bootstrap_sample <- original[bootstrap_indices]

# ==============================================
#  Repeat bootstrap B times
# ==============================================

bootstrap_means <- replicate(
  B,
  mean(sample(original, size = n, replace = TRUE))
)

# Bootstrap standard error
boot_se <- sd(bootstrap_means)

# 95% percentile confidence interval
boot_ci <- quantile(
  bootstrap_means,
  probs = c(0.025, 0.975)
)


# ==============================================
#  Distribution of original sample
# ==============================================

p1 <- ggplot(
  data.frame(value = original),
  aes(x = value)
) +
  geom_density(
    fill = "#2166AC",
    color = "#2166AC",
    alpha = 0.35,
    linewidth = 1
  ) +
  geom_vline(
    xintercept = original_mean,
    color = "red",
    linewidth = 1.1
  ) +
  annotate(
    "text",
    x = original_mean,
    y = Inf,
    label = paste0("Mean = ", round(original_mean, 2)),
    vjust = 1.5,
    hjust = -0.1,
    color = "red",
    size = 3.5
  ) +
  labs(
    title = "1. Original Sample Distribution",
    subtitle = paste("Sample size =", n),
    x = "Observed Values",
    y = "Density"
  ) +
  theme_classic(base_size = 12)


# ==============================================
# 2. Distribution of one bootstrap sample
# ==============================================

bootstrap_mean <- mean(bootstrap_sample)

p2 <- ggplot(
  data.frame(value = bootstrap_sample),
  aes(x = value)
) +
  geom_density(
    fill = "#1B9E77",
    color = "#1B9E77",
    alpha = 0.35,
    linewidth = 1
  ) +
  geom_vline(
    xintercept = bootstrap_mean,
    color = "red",
    linewidth = 1.1
  ) +
  annotate(
    "text",
    x = bootstrap_mean,
    y = Inf,
    label = paste0("Mean = ", round(bootstrap_mean, 2)),
    vjust = 1.5,
    hjust = -0.1,
    color = "red",
    size = 3.5
  ) +
  labs(
    title = "2. Bootstrap Sample Distribution",
    subtitle = "One resample with replacement",
    x = "Resampled Values",
    y = "Density"
  ) +
  theme_classic(base_size = 12)


# ==============================================
#  Distribution of bootstrap sample means
# ==============================================

p3 <- ggplot(
  data.frame(bootstrap_mean = bootstrap_means),
  aes(x = bootstrap_mean)
) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 40,
    fill = "#7570B3",
    color = "white",
    alpha = 0.55
  ) +
  geom_density(
    color = "#54278F",
    linewidth = 1.1
  ) +
  geom_vline(
    xintercept = original_mean,
    color = "red",
    linetype = "dashed",
    linewidth = 1.1
  ) +
  geom_vline(
    xintercept = boot_ci,
    color = "darkgreen",
    linetype = "dotted",
    linewidth = 1
  ) +
  labs(
    title = "3. Bootstrap Sampling Distribution of the Mean",
    subtitle = paste(
      B, "bootstrap replications | SE =",
      round(boot_se, 3)
    ),
    x = "Bootstrap Sample Mean",
    y = "Density",
    caption = "Red: Original sample mean | Green: 95% percentile CI"
  ) +
  theme_classic(base_size = 12)


# ==============================================
# 4. Combine all three plots
# ==============================================

combined <- ((p1 | p2) / p3) +
  plot_layout(heights = c(1, 1.15)) +
  plot_annotation(
    title = "Illustration of the Bootstrap Method",
    subtitle = paste0(
      "Sampling with Replacement | Bootstrap SE = ",
      round(boot_se, 3),
      " | 95% CI = [",
      round(boot_ci[1], 2), ", ",
      round(boot_ci[2], 2), "]"
    )
  )

print(combined)

ggsave(
  "Bootstrap-Illustration.pdf",
  plot = combined,
  width = 11,
  height = 8
)



# Print numerical results
cat("Original mean:", original_mean, "\n")
cat("Bootstrap SE:", boot_se, "\n")
cat("95% Bootstrap CI:", boot_ci, "\n")












set.seed(1)

# ==========================================
#  Simulate heteroskedastic data
# ==========================================

n <- 500
B <- 2000

data <- data.frame(
  education = sample(8:20, n, replace = TRUE)
)

# Error variance increases with education

data$epsilon <- rnorm(
  n,
  mean = 0,
  sd = ifelse(
    data$education >= 17,
    8,
    0.5
  )
)



data$log_wage <- 1.5 + 0.08 * data$education +
  data$epsilon


# ==========================================
#  Conventional OLS and HC1 robust SE
# ==========================================

m_ols <- feols(
  log_wage ~ education,
  data = data,
  vcov = "iid"
)

m_robust <- feols(
  log_wage ~ education,
  data = data,
  vcov = "hetero"
)

beta_hat <- coef(m_ols)["education"]

se_ols <- se(m_ols)["education"]

se_robust <- se(m_robust)["education"]


# ==========================================
# 3. Pairs bootstrap
# ==========================================

boot_beta <- replicate(B, {
  
  idx <- sample(1:n, n, replace = TRUE)
  
  boot_data <- data[idx, ]
  
  coef(
    lm(log_wage ~ education, data = boot_data)
  )["education"]
  
})

se_boot <- sd(boot_beta)

# ==========================================
#  Compare three methods
# ==========================================

results <- data.frame(
  Method = c(
    "Conventional OLS",
    "HC1 Robust",
    "Pairs Bootstrap"
  ),
  Coefficient = rep(beta_hat, 3),
  SE = c(se_ols, se_robust, se_boot)
) %>%
  mutate(
    CI_Lower = Coefficient - 1.3* SE,
    CI_Upper = Coefficient + 1.3*SE
  )

knitr::kable(
  results,
  digits = 4,
  caption = "Comparison of Standard Errors under Heteroskedasticity"
)

plot = ggplot(
  results,
  aes(
    x = Coefficient,
    y = factor(Method, levels = rev(Method)),
    color = Method
  )
) +
  geom_errorbarh(
    aes(xmin = CI_Lower, xmax = CI_Upper),
    height = 0.18,
    linewidth = 1
  ) +
  geom_point(size = 3) +
  geom_vline(
    xintercept = 0.08,
    linetype = "dashed",
    color = "black"
  ) +
  labs(
    title = "Inference under Heteroskedasticity",
    subtitle = "Confidence Intervals: OLS, HC1, and Bootstrap",
    x = "Education Coefficient",
    y = NULL,
    caption = "Dashed line: True coefficient = 0.08"
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold")
  )


plot



data <- read.csv("nlsy79-2020.csv")




# IV:

m6 <- feols(
  log_wealth ~ sex + log_educ,
  data = data
)

m7 <- feols(
  log_wealth ~ sex | log_educ ~ foreign_language,
  data = data
)





















