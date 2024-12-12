calculate_algorithm_results <- function(data_with_slnw) {

############ Calculate summarized outcomes by date for valid waking wear time ############

# Summarize data by date for SLNW == "Wake"
activitysummary <- data_with_slnw[SLNW == "Wake", .(
  off_min = sum(off_s, na.rm = TRUE)/60,
  sitting_min = sum(sitting_s, na.rm = TRUE)/60,
  standing_min = sum(standing_s, na.rm = TRUE)/60,
  walking_min = sum(walking_s, na.rm = TRUE)/60,
  cycling_min = sum(cycling_s, na.rm = TRUE)/60,
  high_intensity_min = sum(high_intensity_s, na.rm = TRUE)/60,
  activity_min = sum(activity_min, na.rm=T),
  wake_duration_min = max(Wake_duration, na.rm = TRUE),
  off_METmin = sum(off_METmin, na.rm = TRUE),
  sitting_METmin = sum(sitting_METmin, na.rm = TRUE),
  standing_METmin = sum(standing_METmin, na.rm = TRUE),
  walking_METmin = sum(walking_METmin, na.rm = TRUE),
  cycling_METmin = sum(cycling_METmin, na.rm = TRUE),
  high_intensity_METmin = sum(high_intensity_METmin, na.rm = TRUE),
  all_METmin = sum(all_METmin, na.rm = TRUE),
  activity_METmin = sum(activity_METmin, na.rm = TRUE),
  light_min = sum(standing_light_min, walking_light_min, cycling_light_min, high_intensity_light_min, na.rm = TRUE),
  mpa_min = sum(standing_mpa_min, walking_mpa_min, cycling_mpa_min, high_intensity_mpa_min, na.rm = TRUE),
  vpa_min = sum(standing_vpa_min, walking_vpa_min, cycling_vpa_min, high_intensity_vpa_min, na.rm = TRUE),
  mvpa_min = sum(standing_mpa_min, walking_mpa_min, cycling_mpa_min, high_intensity_mpa_min, standing_vpa_min, walking_vpa_min, cycling_vpa_min, high_intensity_vpa_min, na.rm = TRUE),
  duration_below_walkingMETs_min = sum(duration_below_walkingMETs_min, na.rm = TRUE),
  duration_above_walkingMETs_min = sum(duration_above_walkingMETs_min, na.rm=TRUE)
), by = .(date)]

# Calculate percentages
activitysummary[, `:=` (
  off_percent = off_min / wake_duration_min * 100,
  sitting_percent = sitting_min / wake_duration_min * 100,
  standing_percent = standing_min / wake_duration_min * 100,
  walking_percent = walking_min / wake_duration_min * 100,
  cycling_percent = cycling_min / wake_duration_min * 100,
  high_intensity_percent = high_intensity_min / wake_duration_min * 100,
  activity_percent = activity_min / wake_duration_min * 100,
  light_percent = light_min / wake_duration_min * 100,
  mpa_percent = mpa_min / wake_duration_min * 100,
  vpa_percent = vpa_min / wake_duration_min * 100,
  mvpa_percent = mvpa_min / wake_duration_min * 100,
  duration_below_walkingMETs_percent = duration_below_walkingMETs_min / wake_duration_min * 100,
  duration_above_walkingMETs_percent = duration_above_walkingMETs_min / wake_duration_min * 100
)]

# Compute mean METs for each activity
activitysummary[, `:=` (off_METs = off_METmin / off_min,
                        sitting_METs = sitting_METmin / sitting_min,
                        standing_METs = standing_METmin / standing_min,
                        walking_METs = walking_METmin / walking_min,
                        cycling_METs = cycling_METmin / cycling_min,
                        high_intensity_METs = high_intensity_METmin / high_intensity_min,
                        all_METs = all_METmin / wake_duration_min,
                        activity_METs = activity_METmin / wake_duration_min)]

print(activitysummary)

############ Calculate sitting and activity accumulation patterns ############

# Filter to include only valid waking wear time for analyses
data_bouts <- data_with_slnw[SLNW == "Wake"]

# Calculate activity_sum and sittingbout1_activitybout0 within a single data.table operation
data_bouts[, `:=` (
  activity_sum = rowSums(.SD, na.rm = TRUE),
  sittingbout1_activitybout0 = fifelse(sitting_s == 60, "1", fifelse(rowSums(.SD, na.rm = TRUE) > 30, "0", NA_character_))
), .SDcols = c("standing_s", "walking_s", "cycling_s", "high_intensity_s")]

# Optionally, immediately remove activity_sum if it's no longer needed, to keep the data table clean.
data_bouts[, activity_sum := NULL]

# Filter out rows that are not sitting or activity bouts. These are occasions where the sitting or activity duration do not match the bout criterion.
data_bouts <- data_bouts[!is.na(sittingbout1_activitybout0), ]

# Generate a running number for unique bouts, within each 'date'
data_bouts[, bout_no := rleid(sittingbout1_activitybout0), by = .(date)]

# Calculate active bouts
data_bouts[, active_bout := ifelse(all_METmin > 1.5, 1, 0)]

# Calculate duration for each bout
data_bouts_summarized <- data_bouts[, .(bout_dur_min = as.numeric(.N), 
                                        activity_dur_min = sum(active_bout)), 
                                    by = .(date, sittingbout1_activitybout0, bout_no)]


# Aggregate to calculate bout counts and durations

# For sitting bouts
sitting_bouts_summary <- data_bouts_summarized[sittingbout1_activitybout0 == "1", .(
  
  # Count unique bouts
  sitting_bouts_n = uniqueN(bout_no),
  
  # Count the number of bouts in each duration category
  sitting_bouts_below10min_n = sum(bout_dur_min < 10),
  sitting_bouts_10to30min_n = sum(bout_dur_min >= 10 & bout_dur_min <= 30),
  sitting_bouts_above30min_n = sum(bout_dur_min > 30),
  
  # Sum the duration at each duration category
  sitting_bouts_below10min_dur_min = sum(bout_dur_min[bout_dur_min < 10], na.rm = TRUE),
  sitting_bouts_10to30min_dur_min = sum(bout_dur_min[bout_dur_min >= 10 & bout_dur_min <= 30], na.rm = TRUE),
  sitting_bouts_above30min_dur_min = sum(bout_dur_min[bout_dur_min > 30], na.rm = TRUE),
  
  # Summarize sitting bout durations
  sitting_bout_dur_min_mean = mean(bout_dur_min, na.rm = TRUE),
  sitting_bout_dur_min_median = median(bout_dur_min, na.rm = TRUE),
  
  # Calculate active sitting duration
  active_sitting_dur_min = sum(activity_dur_min, na.rm = TRUE),
  active_sitting_dur_percent = sum(activity_dur_min, na.rm = TRUE) / sum(bout_dur_min, na.rm = TRUE) * 100
  
), by = .(date, sittingbout1_activitybout0)]

# Clean up temporary columns
sitting_bouts_summary[, c("sittingbout1_activitybout0", "date") := NULL]

# For activity bouts
activity_bouts_summary <- data_bouts_summarized[sittingbout1_activitybout0 == "0", .(
  
  # Count unique bouts
  activity_bouts_n = uniqueN(bout_no),
  
  # Count the number of bouts in each duration category
  activity_bouts_below10min_n = sum(bout_dur_min < 10),
  activity_bouts_10to30min_n = sum(bout_dur_min >= 10 & bout_dur_min <= 30),
  activity_bouts_above30min_n = sum(bout_dur_min > 30),
  
  # Sum the duration at each duration category
  activity_bouts_below10min_dur_min = sum(bout_dur_min[bout_dur_min < 10], na.rm = TRUE),
  activity_bouts_10to30min_dur_min = sum(bout_dur_min[bout_dur_min >= 10 & bout_dur_min <= 30], na.rm = TRUE),
  activity_bouts_above30min_dur_min = sum(bout_dur_min[bout_dur_min > 30], na.rm = TRUE),
  
  # Summarize activity bout durations
  activity_bout_dur_min_mean = mean(bout_dur_min, na.rm = TRUE),
  activity_bout_dur_min_median = median(bout_dur_min, na.rm = TRUE)
), by = .(date, sittingbout1_activitybout0)]

activity_bouts_summary[, c("sittingbout1_activitybout0", "date") := NULL]


############ Combine total activity and bout summaries ############

dayresultssummary <- cbind(activitysummary, sitting_bouts_summary, activity_bouts_summary)

############ Calculate weekly means ############

# Create new variables weekday and week_weekend
dayresultssummary[, c("weekday", "week_weekend") := list(weekdays(date), ifelse(weekdays(date) %in% c("Saturday", "Sunday"), "weekendday", "weekday"))]
setcolorder(dayresultssummary, c("date", "weekday", "week_weekend", names(dayresultssummary)[!(names(dayresultssummary) %in% c("date", "weekday", "week_weekend"))]))

# Convert date to character to enable adding mean row
dayresultssummary$date <- as.character(dayresultssummary$date)

# Calculate means for numeric and integer columns only
numeric_means <- lapply(dayresultssummary[, .SD, .SDcols = sapply(dayresultssummary, is.numeric)], mean, na.rm = TRUE)

# Prepare a list for the new row
new_row_mean <- as.list(dayresultssummary[1,]) # Take the structure of the first row

# Replace the values in the new row with the means for numeric columns and "mean" for character columns
for (col_name in names(new_row_mean)) {
  if (is.numeric(new_row_mean[[col_name]])) {
    new_row_mean[[col_name]] <- numeric_means[[col_name]]
  } else if (col_name == "date") {
    new_row_mean[[col_name]] <- "mean"
  } else if (col_name == "filename") {
    new_row_mean[[col_name]] <- filename
  } else {
    new_row_mean[[col_name]] <- "mean"
  }
}

# Add the new row to the original data.table
dayresultssummary <- rbind(dayresultssummary, new_row_mean, fill = TRUE)

# Weighted mean calculation
weights <- ifelse(dayresultssummary$week_weekend == "weekday", 5/7, 2/7)
numeric_columns <- sapply(dayresultssummary, is.numeric)

# Calculate weighted means for numeric columns
numeric_weighted_means <- sapply(names(dayresultssummary)[numeric_columns], function(col_name) {
  sum(dayresultssummary[[col_name]] * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE)
})

# Prepare a new row for the weighted means
new_row_weighted_mean <- new_row_mean # Start with the structure of the mean row

for (col_name in names(new_row_weighted_mean)) {
  if (is.numeric(new_row_weighted_mean[[col_name]])) {
    new_row_weighted_mean[[col_name]] <- numeric_weighted_means[[col_name]]
  } else if (col_name == "date") {
    new_row_weighted_mean[[col_name]] <- "weighted_mean"
  } else if (col_name == "filename") {
    new_row_weighted_mean[[col_name]] <- filename
  } else {
    new_row_weighted_mean[[col_name]] <- "weighted_mean"
  }
}


# Add the new row to the data.table
dayresultssummary <- rbind(dayresultssummary, new_row_weighted_mean, fill = TRUE)


############ Calculate usual bout durations ############

#Usual sitting bout duration

#Summarize data by bout length
sitting_bouts_summarized_arranged <- data_bouts_summarized %>% filter(sittingbout1_activitybout0 == "1") %>% group_by(bout_dur_min) %>% arrange((bout_dur_min)) %>% dplyr::summarize(bout_dur_min_sum = sum(bout_dur_min))

sitting_bouts_summarized_arranged2 <- sitting_bouts_summarized_arranged %>% dplyr::summarize(bout_dur_min = bout_dur_min, bout_durs_total_min = sum(bout_dur_min_sum), bout_durs_prop = bout_dur_min_sum/bout_durs_total_min, bout_durs_prop_sum = sum(bout_durs_prop), bout_durs_cumsum_min = cumsum(bout_durs_prop))

#First we define the function for the nonlinear model in order to iterate with several starting parameters
W50durs_sit_func <- function(bout_dur_min, sitW50dur_min, n) {
  I(bout_dur_min^n)/(I(bout_dur_min^n)+I(sitW50dur_min^n))
}

#Next we fit the model testing several starting parameters. This includes a catch function to return NA in case of error.
sitW50dur <- sitting_bouts_summarized_arranged2 %>%
  filter(n() > 5) %>%
  group_modify(~ {
    tryCatch(
      broom::tidy(
        nls.multstart::nls_multstart(
          bout_durs_cumsum_min ~ W50durs_sit_func(bout_dur_min, sitW50dur_min, n),
          data = .x,
          start_lower = c(sitW50dur_min = 0, n = 0.5),
          start_upper = c(sitW50dur_min = dayresultssummary$sitting_bout_dur_min_median[dayresultssummary$date == "mean"], n = 2),
          iter = 50,
          supp_errors = "Y"
        )
      ),
      error = function(e) tibble(term = NA, estimate = NA, std.error = NA, statistic = NA, p.value = NA)
    )
  })

# Extract the estimate for sitW50dur_min
sitW50dur_min_estimate <- sitW50dur$estimate[sitW50dur$term == "sitW50dur_min"]

# Add a new column to dayresultssummary initialized with NA
dayresultssummary[, "sitW50dur_min" := NA_real_]

# Update the value for the row where date is "mean" with the correct column name
dayresultssummary[date == "mean", "sitW50dur_min" := sitW50dur_min_estimate]


#Usual activity bout duration

#Summarize data by bout length
activity_bouts_summarized_arranged <- data_bouts_summarized %>% filter(sittingbout1_activitybout0 == "0") %>% group_by(bout_dur_min) %>% arrange((bout_dur_min)) %>% dplyr::summarize(bout_dur_min_sum = sum(bout_dur_min))

activity_bouts_summarized_arranged2 <- activity_bouts_summarized_arranged %>% dplyr::summarize(bout_dur_min = bout_dur_min, bout_durs_total_min = sum(bout_dur_min_sum), bout_durs_prop = bout_dur_min_sum/bout_durs_total_min, bout_durs_prop_sum = sum(bout_durs_prop), bout_durs_cumsum_min = cumsum(bout_durs_prop))

#First we define the function for the nonlinear model in order to iterate with several starting parameters
W50durs_act_func <- function(bout_dur_min, actW50dur_min, n) {
  I(bout_dur_min^n)/(I(bout_dur_min^n)+I(actW50dur_min^n))
}

#Next we fit the model testing several starting parameters. This includes a catch function to return NA in case of error.
actW50dur <- activity_bouts_summarized_arranged2 %>% filter(n()>5) %>% 
  group_modify(~ {
    tryCatch(broom::tidy(nls.multstart::nls_multstart(bout_durs_cumsum_min ~ W50durs_act_func(bout_dur_min, actW50dur_min, n),
                                                      data = .x,
                                                      start_lower = c(actW50dur_min=0, n=0.5),
                                                      start_upper = c(actW50dur_min=dayresultssummary$activity_bout_dur_min_median[dayresultssummary$date=="mean"], n=2),
                                                      iter = 50,
                                                      supp_errors = "Y"
    )
    ),
    error = function(e) tibble(term = NA, estimate = NA, std.error = NA, statistic = NA, p.value = NA)
    )
  })

# Extract the estimate for sitW50dur_min
actW50dur_min_estimate <- actW50dur$estimate[actW50dur$term == "actW50dur_min"]

# Add a new column to dayresultssummary initialized with NA
dayresultssummary[, "actW50dur_min" := NA_real_]

# Update the value for the row where date is "mean" with the correct column name
dayresultssummary[date == "mean", "actW50dur_min" := actW50dur_min_estimate]

return(dayresultssummary)

}