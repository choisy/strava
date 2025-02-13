library(dplyr)
library(purrr)
library(lubridate)

# install.packages('rStrava', repos = c('https://fawda123.r-universe.dev', 'https://cloud.r-project.org'))
library(rStrava)

############

# strava_oauth()
# get_athlete()
# get_activity_list()
# compile_activities()
# get_activity()
# get_activity_streams()


############

# Website: https://www.strava.testapp.com
# Authorisation Callback Domain: localhost

app_name <- "sport_choisy"                               # chosen by user
app_client_id  <- "123030"                               # an integer, assigned by Strava
app_secret <- "94cd0e4bab4eacbc7dd3d3fb0536d0f33c26856e" # an alphanumeric secret, assigned by Strava

# create the authentication token
stoken <- httr::config(token = strava_oauth(app_name, app_client_id, app_secret, app_scope = "activity:read_all"))

############

myinfo <- get_athlete(stoken, id = "34225766")
head(myinfo)

my_acts <- get_activity_list(stoken)
head(my_acts[[2]])

my_acts_df <- compile_activities(my_acts)

compile_activity(my_acts[[2]])

a <- get_activity(10940056952, stoken)

b <- compile_activity(a)

strms_data <- get_activity_streams(my_acts, stoken, id = 10940056952)

d <- density(strms_data$velocity_smooth)
plot(d)

cruising_speed <- function(x) {
  dens <- density(x)
  dens$x[which.max(dens$y)]
}

(cs <- cruising_speed(strms_data$velocity_smooth))
abline(v = cs)
abline(v = c(31, 34, 37, 40), col = "red")


cycling <- filter(my_acts_df, sport_type == "Ride")

ids <- cycling$id[1:5]

ids

cr_sp <- function(x) {
  strms_data <- get_activity_streams(my_acts, stoken, id = x)
  velocity <- strms_data$velocity_smooth
  cs <- cruising_speed(velocity)
  plot(density(velocity))
  abline(v = c(31, 34, 37, 40), col = "red")
  cs <- cruising_speed(velocity)
  abline(v = cs, col = "blue")
  cs
}

map_dbl(ids, cr_sp)

m <- get_activity_streams(my_acts, stoken, id = 10926783473)$velocity_smooth
plot(sort(m))
abline(v = length(m) / 2)
abline(h = median(m))

plot(density(m))
abline(v = c(31, 34, 37, 40), col = "red")
abline(v = cruising_speed(m), col = "blue")

cr_sp(10906581241)

####################

breaks <- c(0, seq(19, 52, 3), Inf)
m <- get_activity_streams(my_acts, stoken, id = 10906581241)$velocity_smooth
tbl <- table(cut(m, breaks, right = FALSE))
names(which.max(tbl))

####################

thu <- compile_activities(get_activity_list(stoken, after = today() - 1))




###################

plot_density <- function(x) {
  d <- density(x, from = 0)
  with(d, plot(x, y, type = "n"))
}
plot_density(strms_data$velocity_smooth, type = "n")

cr_sp2 <- function(aft = lubridate::today() - 1, acts = NULL, id = NULL, s = stoken) {
  alist <- compile_activities(get_activity_list(s, after = aft))
  vlcty <- get_activity_streams(alist, s, acts, id)$velocity_smooth
  cs <- cruising_speed(vlcty)
  opar <- par(mgp = c(1.5, .5, 0))
  plot(density(vlcty), main = NA, xlab = "speed (km/h)", ylab = "density")
  par(opar)
  abline(v = c(31, 34, 37, 40), col = "grey")
  cs <- cruising_speed(vlcty)
  abline(v = cs, col = "red")
  mtext(paste("cruising speed =", round(cs, 2), "km/h"), line = .2)
  mtext(paste("cruising speed in", names(which.max(table(cut(vlcty, c(0, seq(19, 52, 3), Inf), right = FALSE))))), line = 1.2)
}

cr_sp2()

###################

cr_sp3 <- function(aft = lubridate::today() - 1, acts = NULL, id = NULL, s = stoken) {
  alist <- compile_activities(get_activity_list(s, after = aft))
  vlcty <- get_activity_streams(alist, s, acts, id)$velocity_smooth
  cs <- cruising_speed(vlcty)
  d <- density(vlcty, from = 0)
  opar <- par(mgp = c(1.5, .5, 0))
  with(d, plot(x, y, type = "n", main = NA, xlab = "speed (km/h)", ylab = NA, axes = FALSE))
  axis(1)
  par(opar)
  with(d, polygon(c(x[1], x, tail(x, 1)), c(0, y, 0), border = NA, col = "grey"))
  abline(v = c(31, 34, 37, 40), lty = 2)
  abline(h = 0, col = "grey")
  cs <- cruising_speed(vlcty)
  abline(v = cs, col = "red", lwd = 2)
  box()
  breaks <- c(0, seq(19, 52, 3), Inf)
  hash <- setNames(c(rep("D", 5), "C", "B", "B+", rep("A", 5)),
                   levels(cut(15:60, breaks, right = FALSE)))
  ctgr <- names(which.max(table(cut(vlcty, breaks, right = FALSE))))
  mtext(paste("cruising speed =", round(cs, 2), "km/h"), line = .2)
  mtext(paste("cruising speed in", ctgr), line = 1.2)
  mtext(paste("level:", hash[ctgr]), line = 2.2)
}

cr_sp3()

