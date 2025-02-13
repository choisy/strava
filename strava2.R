activity_id <- 11622962583

if (0) {
  library(rStrava)
  library(dplyr)
  
  stoken <- httr::config(
    token = strava_oauth(
      app_name      = "sport_choisy",
      app_client_id = "123030",
      app_secret    = "94cd0e4bab4eacbc7dd3d3fb0536d0f33c26856e",
      app_scope     = "activity:read_all"
    )
  )
}

vel <- get_velocity(activity_id)

col_strava <- rgb(252, 82, 1, maxColorValue = 255)

get_athlete2 <- function(id) get_athlete(stoken, id)

get_activity2 <- function(id) {
  out <- get_activity_list(stoken, id)
  out[[1]] <- out[[1]][1:49]
  out
}

get_activity_streams2 <- function(act_data, ...) get_activity_streams(act_data, stoken, ...)

get_velocity <- function(id) {
  get_activity_streams2(get_activity2(id))$velocity_smooth
}

text2 <- function(x, y) {
  text(c(15, 32.5, 35.5, 38.5, (x + 40) / 2), rep(y, 5),
       c("D", "C", "B", "B+", "A"), col = col_strava)
}

polygon2 <- function(x, y, ...) {
  polygon(c(x[1], x, tail(x, 1)), c(0, y, 0), border = NA, ...)
}

btw <- function(x, int) {
  int[2] >= x & x >= int[1]
}

hist2 <- function(x, ...) {
  hist(x, main = NA, xlab = "speed (km/h)", ylab = "frequency", border = NA, ...)
}

########################################################################################

lev_lines <- seq(31, 40, 3)

add_level_lines <- function() abline(v = lev_lines, col = col_strava, lwd = 2)

hist3 <- function(x) {
  xmax <- max(x) + 3
  val <- hist2(x, seq(-2, xmax, 3))
  add_level_lines()
  text2(xmax, max(val$counts))
}

hist4 <- function(x) {
  xmax <- max(x) + 1
  val <- hist2(x, 0:xmax)
  add_level_lines()
  text2(xmax, max(val$counts))
}

hist5 <- function(x) {
  xmax <- max(x) + 3
  val <- hist2(x, n = 100)
  add_level_lines()
  text2(xmax, max(val$counts))
}

########################################################################################

plot_dens <- function(x) {
  d <- density(x, from = 0)
  with(d, plot(x, y, type = "n", main = NA, xlab = "speed (km/h)", ylab = NA, axes = FALSE))
  axis(1)
  with(d, polygon2(x, y, col = "grey"))
  d
}

add_lines <- function(d) {
  add_level_lines()
  abline(h = 0, col = "grey")
  text2(max(d$x), max(d$y))
  box(bty = "o")
}

########################################################################################

mode_val <- function(x, ...) {
  dens <- density(x, ...)
  dens$x[which.max(dens$y)]
}

mode2 <- function(x, ...) {
  d <- plot_dens(x)
  csp <- mode_val(x, ...)
  abline(v = csp, lwd = 2, lty = 2)
  add_lines(d)
}

########################################################################################

cs <- function(v, th, nb) {
  d <- density(v, n = nb)
  f <- function(t) d$x[range(which(d$y > t))]
  f(optimize(function(x) abs(diff(f(x)) - th), c(0, max(d$y)))$minimum)
}

cspeed <- function(x, th = 3, nb = 2048) {
  d <- plot_dens(x)
  val <- cs(vel, th, nb)
  sel <- d$x > val[1] & d$x < val[2]
  x <- d$x[sel]
  y <- d$y[sel]
  polygon2(x, y, col = "black")
  add_lines(d)
  sel <- purrr::map_lgl(lev_lines, btw, c(39, 42))
  if (any(sel)) return(sum(x > lev_lines[sel]) / length(x))
}

########################################################################################

# Normalized speed

# (1) 30-second moving average
# (2) raise to the power of 4
# (3) take the average
# (4) lower to the root of 4

ma <- function(x, n = 5) stats::filter(x, rep(1 / n, n), sides = 2)

nspeed <- function(x, w = 30, p = 4) mean(ma(vel, w)^p, na.rm = TRUE)^(1 / p)

nspeed(vel)

########################################################################################

# Speed curve

plot(cummean(rev(sort(vel))), type = "l")

cspeed <- function(x, p = .1) mean(sort(x)[-(1:round(length(x) * p))])

head(sort(vel)[-(1:round(length(vel) * .1))])

cspeed(vel)

mean(vel)
mean(vel[vel > 20])

########################################################################################



########################################################################################

#vel <- get_velocity(activity_id)
hist3(vel)
hist4(vel)
hist5(vel)
mode2(vel)
cspeed(vel)

