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


# Bad Controls:



n <- 100

set.seed(1)

# Generate data in a tibble
data_sim = tibble(
  e1 = rnorm(n, sd = 3),
  x = runif(n, min = 0, max = 10),
  y = runif(n, min = 10, max = 20),
  a = 6 + 2*x - 1.5*y + e1)


# Example of a bad control:

data_sim$c <- data_sim$x + rnorm(nrow(data_sim), sd = 0.01)

data_sim$d <- data_sim$a + rnorm(nrow(data_sim), sd = 0.5)


# Correct Model:


lm1 = lm(data = data_sim, a ~ x + y)
summary(lm1)

# Bad Control: Creates Multi-Collinearity.


lm2 = lm(data = data_sim, a ~ c + y + x)

summary(lm2)

# Very bad: Soaks up variation in outcome variable: blocks causal
# pathways.



lm3 = lm(data = data_sim, a ~ d + y + x)

summary(lm3)

library(car)

# Testing a restriction: beta_{1} = beta_{2}

# Unrestricted model
unrestricted <- lm(a ~ y + x, data = data_sim)

# Test H0: beta_y = beta_x
linearHypothesis(unrestricted, "y = x")



data_sim <- data_sim %>%
  mutate(plus = y + x)

restricted = lm(data = data_sim, a ~ plus)
unrestricted = lm(data = data_sim, a ~ y + x)



res_r_sq = (unname(resid(restricted)))^2
res_u_sq = (unname(resid(unrestricted)))^2

rss_r = sum(res_r_sq)
rss_u = sum(res_u_sq)

q = 1
k = 2

F_stat <- ((rss_r - rss_u)/q)/(rss_u/(n - k - 1))




# Parameters

df1 <- 1
df2 <- 97
alpha <- 0.05
F_stat <- 5

# Critical value
F_critical <- qf(1 - alpha, df1, df2)



# Generate x values, starting above zero
x <- seq(0.001, 10, length.out = 1000)
y <- df(x, df1, df2)





# Plot the F-distribution
plot(x, y,
     type = "l",
     lwd = 2,
     col = "blue",
     main = "F-Distribution with Critical Value and F-Statistic",
     xlab = "F-statistic",
     ylab = "Density",
     xlim = c(0, 10),
     ylim = c(0, max(y, na.rm = TRUE)))


# Shade the rejection region (right tail)
x_shade <- seq(F_critical, 10, length.out = 500)
y_shade <- df(x_shade, df1, df2)

polygon(c(F_critical, x_shade, 10),
        c(0, y_shade, 0),
        col = rgb(1, 0, 0, 0.3),
        border = NA)

# Redraw the density curve over the shaded region
lines(x, y, lwd = 2, col = "blue")


# Add critical value and observed F-statistic
abline(v = F_critical, col = "red", lty = 2, lwd = 2)
abline(v = F_stat, col = "darkgreen", lty = 2, lwd = 2)



# Add legend
legend("topright",
       legend = c(
         paste0("Critical Value = ", round(F_critical, 3)),
         paste0("F-statistic = ", round(F_stat, 1))
       ),
       col = c("red", "darkgreen"),
       lty = 2,
       lwd = 2,
       bty = "n")





# ============================================================
# TWO-SIDED HYPOTHESIS TEST
# Type I Error, Type II Error, Power, and Confidence Level
# ============================================================


# Parameters
x <- seq(-4, 7, length.out = 3000)

mu0 <- 0
mu1 <- 2
sigma <- 1
alpha <- 0.05

# Critical values
critical_lower <- qnorm(alpha / 2, mean = mu0, sd = sigma)
critical_upper <- qnorm(1 - alpha / 2, mean = mu0, sd = sigma)

# Probability density functions
y0 <- dnorm(x, mean = mu0, sd = sigma)
y1 <- dnorm(x, mean = mu1, sd = sigma)

# Type II error and statistical power
beta <- pnorm(critical_upper, mean = mu1, sd = sigma) -
  pnorm(critical_lower, mean = mu1, sd = sigma)

power <- 1 - beta


# ============================================================
# Plot settings
# ============================================================

# Reserve space on right for external legend
par(mar = c(5, 4, 4, 11),
    xpd = FALSE)

plot(x, y0,
     type = "n",
     xlim = c(-4, 7),
     ylim = c(0, 0.48),
     xaxs = "i",
     yaxs = "i",
     xlab = "Test statistic",
     ylab = "Probability density",
     main = "Two-Sided Hypothesis Test",
     cex.main = 1.2)


# ============================================================
# Helper function for shaded areas
# ============================================================

shade_area <- function(xmin, xmax, mu, color,
                       transparency = 0.35) {
  
  xs <- seq(xmin, xmax, length.out = 500)
  ys <- dnorm(xs, mean = mu, sd = sigma)
  
  polygon(c(xmin, xs, xmax),
          c(0, ys, 0),
          col = adjustcolor(color, alpha.f = transparency),
          border = NA)
}


# ============================================================
# H0: Correct decision (1 - alpha)
# ============================================================

shade_area(critical_lower, critical_upper,
           mu0, "lightblue", 0.35)


# ============================================================
# H0: Type I Error (alpha)
# ============================================================

shade_area(min(x), critical_lower,
           mu0, "blue", 0.45)

shade_area(critical_upper, max(x),
           mu0, "blue", 0.45)


# ============================================================
# H1: Type II Error (beta)
# ============================================================

shade_area(critical_lower, critical_upper,
           mu1, "orange", 0.40)


# ============================================================
# H1: Statistical Power (1 - beta)
# ============================================================

shade_area(min(x), critical_lower,
           mu1, "darkgreen", 0.35)

shade_area(critical_upper, max(x),
           mu1, "darkgreen", 0.35)


# ============================================================
# Draw density curves
# ============================================================

lines(x, y0, col = "blue", lwd = 2.5)
lines(x, y1, col = "red", lwd = 2.5)


# ============================================================
# Critical values: Dashed lines ending exactly at zero
# ============================================================

segments(x0 = critical_lower,
         y0 = 0,
         x1 = critical_lower,
         y1 = 0.43,
         lty = 2,
         lwd = 1.8,
         col = "black")

segments(x0 = critical_upper,
         y0 = 0,
         x1 = critical_upper,
         y1 = 0.43,
         lty = 2,
         lwd = 1.8,
         col = "black")

segments(x0 = 0,
         y0 = 0,
         x1 = 0,
         y1 = 0.43,
         lty = 2,
         lwd = 1.8,
         col = "black")


# ============================================================
# Labels inside plot
# ============================================================

# Type I error: alpha/2 in each tail
text(-2.5, 0.045,
     expression(alpha/2),
     col = "blue",
     cex = 1.1)

text(2.8, 0.025,
     expression(alpha/2),
     col = "blue",
     cex = 1.1)


# Correct decision under H0
text(-0.7, 0.23,
     expression(1-alpha),
     col = "blue",
     cex = 1.2)


# Type II error
text(1.0, 0.13,
     expression(beta),
     col = "darkorange3",
     cex = 1.3)


# Statistical power
text(3.5, 0.12,
     expression(1-beta),
     col = "darkgreen",
     cex = 1.2)


# ============================================================
# Critical-value labels
# ============================================================

text(critical_lower, 0.45,
     labels = round(critical_lower, 2),
     pos = 4,
     cex = 0.85)

text(critical_upper, 0.45,
     labels = round(critical_upper, 2),
     pos = 4,
     cex = 0.85)



# Allow drawing in the margin
par(xpd = NA)

# Position relative to right edge of plot
legend("topright",
       inset = c(-0.48, 0.02),
       
       legend = c(
         expression(H[0] ~ ": Null distribution"),
         expression(H[1] ~ ": Alternative distribution"),
         expression(alpha ~ ": Type I error"),
         expression(beta ~ ": Type II error"),
         expression(1-beta ~ ": Statistical power")
       ),
       
       col = c(
         "blue",
         "red",
         "blue",
         "orange",
         "darkgreen"
       ),
       
       lty = c(1, 1, NA, NA, NA),
       lwd = c(2.5, 2.5, NA, NA, NA),
       
       pch = c(NA, NA, 15, 15, 15),
       
       pt.cex = 1.2,
       cex = 0.80,
       
       x.intersp = 0.8,
       y.intersp = 1.2,
       
       bty = "n")



cat("Alpha (Type I Error):",
    round(alpha, 4), "\n")

cat("1 - Alpha:",
    round(1 - alpha, 4), "\n")

cat("Beta (Type II Error):",
    round(beta, 4), "\n")

cat("1 - Beta (Power):",
    round(power, 4), "\n")



