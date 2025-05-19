library(tidyverse)
library(forecast)
library(lmtest)
library(patchwork)
library(MASS)

# Read in data
bpexample <- data.frame(
  age = c(
    27, 21, 22, 24, 25, 23, 20, 20, 29, 24,
    25, 28, 26, 38, 32, 33, 31, 34, 37, 38,
    33, 35, 30, 31, 37, 39, 46, 49, 40, 42,
    43, 46, 43, 44, 46, 47, 45, 49, 48, 40,
    42, 55, 54, 57, 52, 53, 56, 52, 50, 59,
    50, 52, 58, 57
  ),
  bp = c(
    73, 66, 63, 75, 71, 70, 65, 70, 79, 72,
    68, 67, 79, 91, 76, 69, 66, 73, 78, 87,
    76, 79, 73, 80, 68, 75, 89, 101, 70, 72,
    80, 83, 75, 71, 80, 96, 92, 80, 70, 90,
    85, 76, 71, 99, 86, 79, 92, 85, 71, 90,
    91, 100, 80, 109
  )
)

# Try OLS
bp_lm <- lm(bp ~ age, data = bpexample)
summary(bp_lm)
checkresiduals(bp_lm)

# https://www.statology.org/weighted-least-squares-in-r/
# Breusch-Pagan test
bptest(bp_lm)

# Look for relationship between SD of resid and X
resid_df <- data.frame(age = bpexample$age, abs_resid = abs(residuals(bp_lm)))
ggplot(resid_df, aes(age, abs_resid)) +
  geom_point() +
  theme_bw()

# Define weight
bp_weight <- 1 / lm(abs(bp_lm$residuals) ~ bp_lm$fitted.values)$fitted.values^2

# Fit WLS model
bp_wls <- lm(bp ~ age, data = bpexample, weights = bp_weight)
summary(bp_wls)
checkresiduals(bp_wls)
bptest(bp_wls)

################################################################################

# Create the data frame
toluca <- data.frame(
  lotsize = c(80, 30, 50, 90, 70, 60, 120, 80, 100, 50, 40, 70, 90, 20, 110, 100, 30, 50, 90, 110, 30, 90, 40, 80, 70),
  workhours = c(399, 121, 221, 376, 361, 224, 546, 352, 353, 157, 160, 252, 389, 113, 435, 420, 212, 268, 377, 421, 273, 468, 244, 342, 323)
)

# Create contaminated data
contam <- toluca |>
  mutate(workhours = ifelse(workhours > 500, workhours * 3, workhours))

# Plot side by side
g1 <- ggplot(toluca, aes(lotsize, workhours)) +
  geom_point() +
  theme_bw()
g2 <- ggplot(contam, aes(lotsize, workhours)) +
  geom_point() +
  theme_bw()
g1 + g2

# Look at shape of bisquare weighting curve
c <- 1.345
temp <- data.frame(u = seq(-2, 2, by = 0.2)) |>
  mutate(w = ifelse(abs(u) > c, 0, (1 - (u / c)^2)^2))
ggplot(temp, aes(u, w)) +
  geom_line() +
  theme_bw() +
  geom_vline(xintercept = -c, color = "red") +
  annotate("text", x = -1.5, y = 1, label = "-C", color = "red") +
  geom_vline(xintercept = c, color = "red") +
  annotate("text", x = 1.5, y = 1, label = "C", color = "red") +
  xlab("Standardized Residual") +
  ylab("Bisquare Weight") +
  ggtitle("Bisquare Weight Function") +
  theme(plot.title = element_text(hjust = 0.5))

# OLS regression on original data
toluca_lm <- lm(workhours ~ lotsize, data = toluca)
summary(toluca_lm)

# OLS regression on response-contaminated data
contam_lm <- lm(workhours ~ lotsize, data = contam)
summary(contam_lm)

# Robust regression on response-contaminated data
contam_rlm <- rlm(workhours ~ lotsize, data = contam)
summary(contam_rlm)

### To be continued...
