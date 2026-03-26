# frontend

# student chooses distribution
# student chooses type of area calculation, between, left, right, two-tailed
# probably a checkbox - with mixed challenge as an option - all random 1 dist
# depending on distribution chosen, change options in checkbox list
# like T doesn't get between, and chisq only has right tailed

# student chooses df (if applicable)

# backend

# determine the parameters for the 4 plots
# that includes area, limits

# between, left, right, two tailed
# small, medium large

# Normal

# toggle for T and Chisq with option df slider
# T - no between areas - df slider between 1 and 50
# Chisq - right tail only - df slider between 1 and 15

library(tidyverse)

area = runif(1, 0.1, 0.5)

lo = rnorm(1, -1, 0.2)
hi = qnorm(area + pnorm(lo))


left_tail = sample(c(T, F), 1)
if (left_tail) {
  lo = -3
  hi = qnorm(area + pnorm(lo))
} else {
  lo = qnorm(1 - area)
  hi = 3
}

# plot normal curve using stat_function and geom = area
ggplot(NULL) +
  stat_function(
    fun = dnorm,
    args = list(mean = 0, sd = 1),
    geom = "line",
    color = "black",
    xlim = c(-3, 3),
    size = 4
  ) +
  stat_function(
    fun = dnorm,
    args = list(mean = 0, sd = 1),
    geom = "area",
    fill = "blue",
    alpha = 0.5,
    xlim = c(lo, hi)
  ) +
  geom_text(
    aes(x = 2, y = 0.35, label = paste("area =", round(area, 2))),
    size = 5,
    color = "black"
  )

answer_a = pnorm(3) - pnorm(lo)
answer_a


filtered <- mtcars %>%
  filter(mpg > 20) %>%
  select(mpg)

plot(filtered$, filtered, type = "l", lwd = 2, xaxt="n", ylab="")

filtered$mpg


plot(
  filtered$mpg,
  filtered$mpg,
  type = "l",
  lwd = 2,
  xaxt = "n",
  xlab = num_variable_name,
  ylab = ""
)
axis(1, at = xaxis_ticks1, las = 2)
polygon(
  c(lw1, seq(lw1, up1, length = 200), up1),
  dnorm(c(0, seq(lw1, up1, length = 200), 0), mean = mu[1], sd = sd[1]),
  col = "grey"
)


dist_func <- dnorm
dist_args <- list(mean = 0, sd = 1)
limits <- c(-3, 3)
lower <- -2
upper <- 2
y_lower <- dnorm(-2)
y_upper <- dnorm(2)

new_plot <- ggplot(NULL) +
  stat_function(
    fun = dist_func,
    args = dist_args,
    geom = "area",
    fill = "darkgreen",
    alpha = 0.5,
    xlim = c(limits[1], lower)
  ) +
  stat_function(
    fun = dist_func,
    args = dist_args,
    geom = "area",
    fill = "darkgreen",
    alpha = 0.5,
    xlim = c(upper, limits[2])
  ) +
  # drawing the vertical lines
  geom_segment(
    aes(x = lower, xend = lower, y = 0, yend = y_lower),
    color = "darkgreen",
    size = 1.5
  ) +
  geom_segment(
    aes(x = upper, xend = upper, y = 0, yend = y_upper),
    color = "darkgreen",
    size = 1.5
  ) +
  # drawing the distribution line
  stat_function(
    fun = dist_func,
    args = dist_args,
    geom = "line",
    color = "black",
    xlim = limits,
    size = 1
  ) +
  labs(x = "", y = "") +
  geom_text(aes(
    x = -2,
    y = .35,
    label = paste("area = ", 0.2)
  ))
new_plot


assignments <- 6
bounds <- c(30, 45, 75, 90)

numbers <- sort(as.integer(runif(assignments, bounds[2], bounds[3])))

given_mean <- round(mean(numbers), 2)
given_med <- round(median(numbers), 2)
given_sd <- round(sd(numbers), 2)


generate_options <- function(bounds) {
  list(
    lower = as.integer(runif(1, bounds[1], bounds[2])),
    middle1 = as.integer(runif(1, bounds[2], bounds[3])),
    middle2 = as.integer(runif(1, bounds[2], bounds[3])),
    upper = as.integer(runif(1, bounds[3], bounds[4])),
    random = as.integer(runif(1, bounds[1], bounds[4]))
  )
}

options <- generate_options(bounds)
while (length(unique(unlist(options))) < 5) {
  options <- generate_options(bounds)
}
