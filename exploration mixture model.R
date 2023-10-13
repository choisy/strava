read_ride <- function(x, time_zone = "Asia/Saigon") {
  tmp <- x |> 
    readFitFile() |> 
    records()
  
  nbcol <- tmp |>
    make_list() |> 
    map_int(ncol)
  
  tmp[which(nbcol == max(nbcol))] |>
    bind_rows() |> 
    arrange(timestamp) |> 
    mutate_at("timestamp", with_tz, time_zone) |> 
    mutate_at("speed", ms2kmh)
}

rcrds <- read_ride("~/Desktop/Sept_hommes_pr_C.fit")

sp <- rcrds[1220:4819, ]$speed
dens <- density(sp)
plot(dens, xlim = c(0, max(dens$x)), ylim = c(0, 1.03 * max(dens$y)),
     xaxs = "i", yaxs = "i",
     main = NA, xlab = "speed (km/h)", ylab = "density", col = 4, lwd = 3)
(asp <- mean(sp))
(csp <- cruising_speed(sp))
abline(v = c(asp, csp), col = 2:3, lwd = 2)


