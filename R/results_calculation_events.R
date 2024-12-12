calculate_events_results <- function(data_with_METs, events) {

############ Calculate summarized outcomes by custom events ############

# Create a new column in data_with_METs to store the corresponding event
data_with_METs[, event := NA_character_]

# For each row in events, mark the corresponding rows in data_with_METs with the event
for (i in seq_len(nrow(events))) {
  data_with_METs[timestamp >= events$from[i] & timestamp <= events$to[i], event := events$event[i]]
}

#Filter out rows that do not belong to any events
data_with_METs_filtered <- data_with_METs[!is.na(event), ]

# Summarize data by event
activitysummary_events <- data_with_METs_filtered[, .(
  off_min = sum(off_s, na.rm = TRUE)/60,
  sitting_min = sum(sitting_s, na.rm = TRUE)/60,
  standing_min = sum(standing_s, na.rm = TRUE)/60,
  walking_min = sum(walking_s, na.rm = TRUE)/60,
  cycling_min = sum(cycling_s, na.rm = TRUE)/60,
  high_intensity_min = sum(high_intensity_s, na.rm = TRUE)/60,
  activity_min = sum(activity_min, na.rm=T),
  duration_min = .N,
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
), 
by = .(date, event)]

# Calculate percentages
activitysummary_events[, `:=` (
  off_percent = off_min / duration_min * 100,
  sitting_percent = sitting_min / duration_min * 100,
  standing_percent = standing_min / duration_min * 100,
  walking_percent = walking_min / duration_min * 100,
  cycling_percent = cycling_min / duration_min * 100,
  high_intensity_percent = high_intensity_min / duration_min * 100,
  activity_percent = activity_min / duration_min * 100,
  light_percent = light_min / duration_min * 100,
  mpa_percent = mpa_min / duration_min * 100,
  vpa_percent = vpa_min / duration_min * 100,
  mvpa_percent = mvpa_min / duration_min * 100,
  duration_below_walkingMETs_percent = duration_below_walkingMETs_min / duration_min * 100,
  duration_above_walkingMETs_percent = duration_above_walkingMETs_min / duration_min * 100
)]

# Compute mean METs for each activity
activitysummary_events[, `:=` (off_METs = off_METmin / off_min,
                               sitting_METs = sitting_METmin / sitting_min,
                               standing_METs = standing_METmin / standing_min,
                               walking_METs = walking_METmin / walking_min,
                               cycling_METs = cycling_METmin / cycling_min,
                               high_intensity_METs = high_intensity_METmin / high_intensity_min,
                               all_METs = all_METmin / duration_min,
                               activity_METs = activity_METmin / duration_min)]


############ Calculate sitting and activity accumulation patterns ############

# Calculate activity_sum and sittingbout1_activitybout0 within a single data.table operation
data_with_METs_filtered[, `:=` (
  activity_sum = rowSums(.SD, na.rm = TRUE),
  sittingbout1_activitybout0 = fifelse(sitting_s == 60, "1", fifelse(rowSums(.SD, na.rm = TRUE) > 30, "0", NA_character_))
), .SDcols = c("standing_s", "walking_s", "cycling_s", "high_intensity_s")]

# Optionally, immediately remove activity_sum if it's no longer needed, to keep the data table clean.
data_with_METs_filtered[, activity_sum := NULL]

# Filter out rows that are not sitting or activity bouts. These are occasions where the sitting or activity duration do not match the bout criterion.
data_with_METs_filtered_bouts <- data_with_METs_filtered[!is.na(sittingbout1_activitybout0), ]

# Generate a running number for unique bouts, within each 'date'
data_with_METs_filtered_bouts[, bout_no := rleid(sittingbout1_activitybout0), by = .(date, event)]

# Calculate duration for each bout
data_bouts_events_summarized <- data_with_METs_filtered_bouts[, .(
  bout_dur_min = .N
), by = .(date, event, sittingbout1_activitybout0, bout_no)]

# Aggregate to calculate bout counts and durations

# For sitting bouts
sitting_bouts_events_summary <- data_bouts_events_summarized[sittingbout1_activitybout0 == "1", .(
  
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
  sitting_bout_dur_min_mean = mean(as.numeric(bout_dur_min), na.rm = TRUE),
  sitting_bout_dur_min_median = median(as.numeric(bout_dur_min), na.rm = TRUE)
), by = .(date, event, sittingbout1_activitybout0)]

# Clean up temporary columns
sitting_bouts_events_summary[, c("sittingbout1_activitybout0", "date") := NULL]

# For activity bouts
activity_bouts_events_summary <- data_bouts_events_summarized[sittingbout1_activitybout0 == "0", .(
  
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
  activity_bout_dur_min_mean = mean(as.numeric(bout_dur_min), na.rm = TRUE),
  activity_bout_dur_min_median = median(as.numeric(bout_dur_min), na.rm = TRUE)
), by = .(date, event, sittingbout1_activitybout0)]

activity_bouts_events_summary[, c("sittingbout1_activitybout0", "date") := NULL]


############ Combine total activity and bout summaries ############

eventresultssummary <- cbind(activitysummary_events, sitting_bouts_events_summary, activity_bouts_events_summary)

return(eventresultssummary)

}
