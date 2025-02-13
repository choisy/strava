if (0) {
  library(rStrava)
  library(dplyr)
  library(purrr)
  library(magrittr)
  
#  stoken <- httr::config(
#    token = strava_oauth(
#      app_name      = "sport_choisy",
#      app_client_id = "123030",
#      app_secret    = "94cd0e4bab4eacbc7dd3d3fb0536d0f33c26856e",
#      app_scope     = "activity:read_all",
#      cache         = TRUE
#    )
#  )
  
  stoken <- httr::config(token = readRDS('.httr-oauth')[[1]])
  
  athl_num <- 34225766
  
  get_athlete2 <- function() get_athlete(stoken, athl_num)
  
  athl_fun2 <- function() athl_fun(athl_num, FALSE)
  
  get_activity_list2 <- function(id, ...) get_activity_list(stoken, id, ...)
  
  get_activity2 <- function(id) {
    out <- get_activity_list2(id)
    out[[1]] <- out[[1]][1:49]
    out
  }
  
  get_activity_streams2 <- function(act_data, ...) get_activity_streams(act_data, stoken, ...)
  
  keep49 <- function(x) {
    x[[1]] <- x[[1]][1:49]
    x
  }
  
  get_activity2 <- function(id) {
    keep49(get_activity_list(stoken, id))
  }
  
  get_activity_list3 <- function(ids, time = 901) {
    activities_list2 <<- c(activities_list2, map(ids, get_activity_list2))
    saveRDS(activities_list3, "activities_list3.RData")
    Sys.sleep(time)
  }
  
}



########################################################################################

# My basic data:
(my_info <- get_athlete2())

# Another version of my data:
athl_fun2()

# The list of all my activities with level of detail 2 (i.e. summary) (about 40"):
activities_list_level2  <- get_activity_list2()

# Converting the list into a data frame:
activities_df <- compile_activities(activities_list_level2)

# The IDs of all my activities:
activities_id <- activities_df$id

# Or, equivalently:
# activities_id <- map_dbl(activities_list_level2, magrittr::extract2, "id")

# The list of all my activities with level of detail 3 (i.e. detailed) (about "):
# It would take 50 more space on disk, i.e. about 1.5 GB
# activities_list_level3 <- map(activities_id, get_activity_list2)




n <- 50
nb <- length(activities_id) 
a <- nb %/% n
activities_id2 <- split(activities_id, c(rep(1:a, each = n), rep(a + 1, nb %% n)))
activities_list3 <- NULL
system.time(map(activities_id2[-c(1:3)], get_activity_list3))










system.time(b <- map(activities_id[51:100], get_activity_list2))


# Another version of the activities list:
#system.time(activities_list2b <- map(activities_id[1:30], get_activity_list, stoken = stoken))
#activities_list2c <- map(activities_list2b, keep49)
#a <- get_activity_streams2(activities_list2c[[1]])
#a$velocity_smooth










#activities_list1 <- get_activity_list(stoken, 11303496693)





activities_df2 <- activities_df %>% 
  select(-athlete.id, -athlete.resource_state, -commute, -flagged,
         -from_accepted_tag, -display_hide_heartrate_option, -has_kudoed,
         -heartrate_opt_out, -map.id, -map.resource_state,
         -map.summary_polyline, -resource_state, -upload_id_str,
         -average_cadence)

# Getting the stream data of all my activities for detailed info about each activity:

activities_streams <- (activities_list[1:2])

activities_streams <- (activities_list[[1]])

###
# plot_spdsplits()
# to recode: get_elevation_prof()
# to recode: get_heat_map()
