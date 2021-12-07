
library(ggplot2)

TIMES <- 24*60

distData <- data.frame(
  "behave" = c(
    rep("0 - rest", TIMES),
    rep("1 - forage", TIMES),
    rep("2 - explore", TIMES)
  ),
  "values" = c(
    rnorm(n = TIMES, mean = 0, sd = 2),
    rnorm(n = TIMES, mean = 16, sd = 5),
    rnorm(n = TIMES, mean = 12, sd = 3)
  )
)

ggplot(distData) +
  geom_density(aes(x = values, fill = behave), alpha = 0.45)

distData <- data.frame(
  "behave" = c(
    rep("0 - rest", TIMES),
    rep("1 - forage", TIMES),
    rep("2 - explore", TIMES)
  ),
  "values" = c(
    cos( (1:TIMES) / 200),
    cos( (1:TIMES) / 200) * 0.1,
    cos( (1:TIMES) / 200) + 0.2
  ),
  "mins" = seq(1:TIMES*3)
)

ggplot(distData) +
  geom_line(aes(x = mins, y = values, colour = behave), alpha = 0.45) +
  scale_x_continuous(sec.axis = sec_axis(
    trans = ~ ./60,
    name = "hour",
    breaks = waiver(),
    labels = waiver(),
    guide = waiver()))

# from: Cornelissen, G. Cosinor-based rhythmometry. Theor Biol Med Model 11, 16
# (2014). https://doi.org/10.1186/1742-4682-11-16
# Y(t)=M+Acos(2πt/τ+ϕ)+e(t)

# M is the MESOR (Midline Statistic Of Rhythm, a rhythm-adjusted mean), A is the
# amplitude (a measure of half the extent of predictable variation within a
# cycle), ϕ is the acrophase (a measure of the time of overall high values
# recurring in each cycle), τ is the period (duration of one cycle), and e(t) is
# the error term (Figure 1).

Y(t) = M + βx + γz + e(t)

# this can be used regardless of units so need a handler to convert to minutes
# tau = 12 # months
tau = 24 # hours
A = 1
theta = 0

t = seq(1, 24, 24/60)

M = 0
β = A * cos(theta)
y = -A * sin(theta)
x = cos(2 * pi * t / tau)
z = sin(2 * pi * t / tau)

# Y(t) = M + β*x + y*z
# y = M + β*x + y*z

cycleData <- data.frame(
  "t" = t,
  y = M + β*x + y*z
)

ggplot(cycleData) +
  geom_hline(yintercept = M, linetype = 2, alpha = 0.5) +
  annotate("text", x = 0, y = M,
           label = paste0("M: ", M),
           hjust = 0, vjust = 1, fontface = 2) +

  annotate("errorbar", x = 0, y = M, xmin = 0, xmax = 0, ymin = M-A, ymax = M+A,
           linetype = 1, alpha = 0.5) +
  annotate("text", x = 0, y = M+A,
           label = paste0("A: ", A),
           hjust = 0, vjust = 1, fontface = 2) +

  geom_line(aes(x = t, y = y)) +
  annotate("text", x = max(t)/2, y = A,
           label = paste0("theta: ", theta,
                          "\ntau: ", tau),
           hjust = 0.5, vjust = 1, fontface = 2) +

  scale_x_continuous(breaks = seq(0, 48, 2))

