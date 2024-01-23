library(dplyr)
library(ggplot2)
library(lubridate)

################################################################################
################################################################################
################################################################################

### GENTLEMAN 1

water_1_df <- read.csv("data/1/water.csv",
                       header = TRUE, skip = 2, check.names = FALSE, stringsAsFactors = FALSE)

colnames(water_1_df) <- c("start_time", "amount", "custom", "update_time", 
                          "create_time", "time_offset", "deviceuuid", 
                          "unit_amount", "comment", "pkg_name", "datauuid")

water_1_df <- water_1_df %>% 
  select("start_time", "amount") %>% 
  mutate(time = ymd_hms(start_time),
         weekday = wday(time, label = TRUE, abbr = FALSE, locale = "en_US"),
         date = format(time, "%Y.%m.%d"),
         name = "Gentleman1"
  ) %>% 
  group_by(date, weekday, name) %>% 
  summarise(amount = sum(amount)) %>% 
  mutate(amount = ifelse(amount<1000, 1250, amount))

### GENTLEMAN 2

#woda spisywana recznie

water_2_df <- read.csv("data/2/water.csv",
                       header = FALSE, check.names = FALSE, stringsAsFactors = FALSE)

colnames(water_2_df) <- c("date", "weekday", "name", "amount")


### GENTLEMAN 3 

water_3_df <- read.csv("data/3/water.csv",
                       header = TRUE, skip = 2, check.names = FALSE, stringsAsFactors = FALSE)

colnames(water_3_df) <- c("start_time", "amount", "custom", "update_time",
                          "create_time", "time_offset", "deviceuuid",
                          "unit_amount", "comment", "pkg_name", "datauuid")

water_3_df <- water_3_df %>%
  select("start_time", "amount") %>%
  mutate(time = ymd_hms(start_time),
         day = day(time),
         weekday = wday(time, label = TRUE, abbr = FALSE, locale = "en_US"),
         hour = hour(time),
         dayHour = paste(day, hour, sep = "h"),
         date = format(time, "%Y.%m.%d"),
         name = "Gentleman3"
  ) %>% 
  group_by(date, weekday, name) %>% 
  summarise(amount = sum(amount))


################################################################################
################################################################################
################################################################################

### GENTLEMAN 1

step_1_df <- read.csv("data/1/steps.csv",
                      header = TRUE, skip = 2, check.names = FALSE, stringsAsFactors = FALSE)


colnames(step_1_df) <-  sub("^com.samsung.health.step_count\\.", "", c("duration", "version_code", "run_step", "walk_step", 
                                                                       "com.samsung.health.step_count.start_time", 
                                                                       "com.samsung.health.step_count.sample_position_type", 
                                                                       "com.samsung.health.step_count.custom", 
                                                                       "com.samsung.health.step_count.update_time", 
                                                                       "com.samsung.health.step_count.create_time", 
                                                                       "com.samsung.health.step_count.count", 
                                                                       "com.samsung.health.step_count.speed", 
                                                                       "com.samsung.health.step_count.distance", 
                                                                       "com.samsung.health.step_count.calorie", 
                                                                       "com.samsung.health.step_count.time_offset", 
                                                                       "com.samsung.health.step_count.deviceuuid", 
                                                                       "com.samsung.health.step_count.pkg_name", 
                                                                       "com.samsung.health.step_count.end_time", 
                                                                       "com.samsung.health.step_count.datauuid"))


step_1_df <- step_1_df %>% select("run_step", "walk_step", "start_time", "count", "speed", "distance") %>% 
  mutate(time = ymd_hms(start_time),
         weekday = wday(time, label = TRUE, abbr = FALSE, locale = "en_US"),
         dayHour = paste(day(time), hour(time), sep = "h"),
         date = format(time, "%Y.%m.%d"),
         name = "Gentleman1")

### GENTLEMAN 2

step_2_df <- read.csv("data/2/steps.csv",
                      header = TRUE, skip = 2, check.names = FALSE, stringsAsFactors = FALSE)


colnames(step_2_df) <-  sub("^com.samsung.health.step_count\\.", "", c("duration", "version_code", "run_step", "walk_step", 
                                                                       "com.samsung.health.step_count.start_time", 
                                                                       "com.samsung.health.step_count.sample_position_type", 
                                                                       "com.samsung.health.step_count.custom", 
                                                                       "com.samsung.health.step_count.update_time", 
                                                                       "com.samsung.health.step_count.create_time", 
                                                                       "com.samsung.health.step_count.count", 
                                                                       "com.samsung.health.step_count.speed", 
                                                                       "com.samsung.health.step_count.distance", 
                                                                       "com.samsung.health.step_count.calorie", 
                                                                       "com.samsung.health.step_count.time_offset", 
                                                                       "com.samsung.health.step_count.deviceuuid", 
                                                                       "com.samsung.health.step_count.pkg_name", 
                                                                       "com.samsung.health.step_count.end_time", 
                                                                       "com.samsung.health.step_count.datauuid"))


step_2_df <- step_2_df %>% select("run_step", "walk_step", "start_time", "count", "speed", "distance") %>% 
  mutate(time = ymd_hms(start_time),
         weekday = wday(time, label = TRUE, abbr = FALSE, locale = "en_US"),
         dayHour = paste(day(time), hour(time), sep = "h"),
         date = format(time, "%Y.%m.%d"),
         name = "Gentleman2")

### GENTLEMAN 3

step_3_df <- read.csv("data/3/steps.csv",
                      header = TRUE, skip = 2, check.names = FALSE, stringsAsFactors = FALSE)


colnames(step_3_df) <-  sub("^com.samsung.health.step_count\\.", "", c("duration", "version_code", "run_step", "walk_step", 
                                                                       "com.samsung.health.step_count.start_time", 
                                                                       "com.samsung.health.step_count.sample_position_type", 
                                                                       "com.samsung.health.step_count.custom", 
                                                                       "com.samsung.health.step_count.update_time", 
                                                                       "com.samsung.health.step_count.create_time", 
                                                                       "com.samsung.health.step_count.count", 
                                                                       "com.samsung.health.step_count.speed", 
                                                                       "com.samsung.health.step_count.distance", 
                                                                       "com.samsung.health.step_count.calorie", 
                                                                       "com.samsung.health.step_count.time_offset", 
                                                                       "com.samsung.health.step_count.deviceuuid", 
                                                                       "com.samsung.health.step_count.pkg_name", 
                                                                       "com.samsung.health.step_count.end_time", 
                                                                       "com.samsung.health.step_count.datauuid"))


step_3_df <- step_3_df %>% select("run_step", "walk_step", "start_time", "count", "speed", "distance") %>% 
  mutate(time = ymd_hms(start_time),
         weekday = wday(time, label = TRUE, abbr = FALSE, locale="en_US"),
         dayHour = paste(day(time), hour(time), sep = "h"),
         date = format(time, "%Y.%m.%d"),
         name = "Gentleman3")

################################################################################
################################################################################
################################################################################

### GENTLEMAN 1

sleep_1_df <- read.csv("data/1/sleep.csv",
                       header = TRUE, skip = 2, check.names = FALSE, stringsAsFactors = FALSE)

colnames(sleep_1_df) <- sub("^com\\.samsung\\.health\\.sleep\\.", "", c("original_efficiency", "mental_recovery", "factor_01", "factor_02",
                                                                        "factor_03", "factor_04", "factor_05", "factor_06", "factor_07", 
                                                                        "factor_08", "factor_09", "factor_10", "has_sleep_data", "combined_id", 
                                                                        "sleep_type", "data_version", "physical_recovery", "original_wake_up_time", 
                                                                        "movement_awakening", "original_bed_time", "goal_bed_time", "quality", 
                                                                        "extra_data", "goal_wake_up_time", "sleep_cycle", "efficiency", 
                                                                        "sleep_score", "sleep_duration", "com.samsung.health.sleep.create_sh_ver", 
                                                                        "com.samsung.health.sleep.start_time", "com.samsung.health.sleep.custom", 
                                                                        "com.samsung.health.sleep.modify_sh_ver", "com.samsung.health.sleep.update_time", 
                                                                        "com.samsung.health.sleep.create_time", "com.samsung.health.sleep.time_offset", 
                                                                        "com.samsung.health.sleep.deviceuuid", "com.samsung.health.sleep.comment", 
                                                                        "com.samsung.health.sleep.pkg_name", "com.samsung.health.sleep.end_time", 
                                                                        "com.samsung.health.sleep.datauuid"))
sleep_1_df <- sleep_1_df %>% 
  select("start_time", "end_time") %>% 
  mutate(start_time = ymd_hms(start_time),
         end_time = ymd_hms(end_time),
         duration = sprintf("%02d:%02d",
                            floor(as.numeric(difftime(end_time, start_time, units = "mins")) / 60),
                            floor(as.numeric(difftime(end_time, start_time, units = "mins")) %% 60)),
         day = day(end_time),
         weekday = wday(end_time, label = TRUE, abbr = FALSE, locale = "en_US"),
         name = "Gentleman1")

### GENTLEMAN 2

#spisywane recznie

sleep_2_df <- read.csv("data/2/sleep.csv",
                       header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)

### GENTLEMAN 3
sleep_3_df <- read.csv("data/3/sleep.csv",
                       header = TRUE, skip = 2, check.names = FALSE, stringsAsFactors = FALSE)

colnames(sleep_3_df) <- sub("^com\\.samsung\\.health\\.sleep\\.", "", c("original_efficiency", "mental_recovery", "factor_01", "factor_02",
                                                                        "factor_03", "factor_04", "factor_05", "factor_06", "factor_07",
                                                                        "factor_08", "factor_09", "factor_10", "has_sleep_data", "combined_id",
                                                                        "sleep_type", "data_version", "physical_recovery", "original_wake_up_time",
                                                                        "movement_awakening", "original_bed_time", "goal_bed_time", "quality",
                                                                        "extra_data", "goal_wake_up_time", "sleep_cycle", "efficiency",
                                                                        "sleep_score", "sleep_duration", "com.samsung.health.sleep.create_sh_ver",
                                                                        "com.samsung.health.sleep.start_time", "com.samsung.health.sleep.custom",
                                                                        "com.samsung.health.sleep.modify_sh_ver", "com.samsung.health.sleep.update_time",
                                                                        "com.samsung.health.sleep.create_time", "com.samsung.health.sleep.time_offset",
                                                                        "com.samsung.health.sleep.deviceuuid", "com.samsung.health.sleep.comment",
                                                                        "com.samsung.health.sleep.pkg_name", "com.samsung.health.sleep.end_time",
                                                                        "com.samsung.health.sleep.datauuid"))
sleep_3_df <- sleep_3_df %>%
  select("start_time", "end_time") %>%
  mutate(start_time = ymd_hms(start_time),
         end_time = ymd_hms(end_time),
         duration = sprintf("%02d:%02d",
                            floor(as.numeric(difftime(end_time, start_time, units = "mins")) / 60),
                            floor(as.numeric(difftime(end_time, start_time, units = "mins")) %% 60)),
         day = day(end_time),
         weekday = wday(end_time, label = TRUE, abbr = FALSE, locale = "en_US"),
         name = "Gentleman3")

