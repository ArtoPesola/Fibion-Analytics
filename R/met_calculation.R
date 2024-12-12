calculate_METs <- function(prepared_data, parameters) {
  
  # Calculate METmin for each activity type
  all_types <- c("off", "sitting", "standing", "walking", "cycling", "high_intensity")
  activity_types <- c("standing", "walking", "cycling", "high_intensity")
  
  # Calculate METmin for all types
  for (activity in all_types) {
    
    met_col <- paste0(activity, "_met100")
    time_col <- paste0(activity, "_s")
    metmin_col <- paste0(activity, "_METmin")
    
    # Calculate MET minutes if not sitting
    prepared_data[, (metmin_col) := get(met_col) / 100 * get(time_col) / 60]
    
  }
  
  # Calculate light, mpa and vpa for activity types, excluding off and sitting
  for (activity in activity_types) {
    
    met_col <- paste0(activity, "_met100")
    time_col <- paste0(activity, "_s")
    metmin_col <- paste0(activity, "_METmin")
    light_col <- paste0(activity, "_light_min") # Light Activity, exclude sitting and off
    mpa_col <- paste0(activity, "_mpa_min") # Moderate Physical Activity, exclude sitting and off
    vpa_col <- paste0(activity, "_vpa_min") # Vigorous Physical Activity, exclude sitting and off
    
    # Use thresholds from parameters
    light_threshold <- parameters$light_threshold
    mpa_threshold_min <- parameters$mpa_threshold_min
    mpa_threshold_max <- parameters$mpa_threshold_max
    vpa_threshold <- parameters$vpa_threshold
    
    # Calculate Light Activity minutes if not sitting
    prepared_data[, (light_col) := fifelse(get(met_col)/100 < light_threshold, get(time_col) / 60, 0)]
    
    # Calculate MPA minutes if not sitting
    prepared_data[, (mpa_col) := fifelse(get(met_col)/100 >= mpa_threshold_min & get(met_col)/100 < mpa_threshold_max, get(time_col) / 60, 0)]
    
    # Calculate VPA minutes if not sitting
    prepared_data[, (vpa_col) := fifelse(get(met_col)/100 >= vpa_threshold, get(time_col) / 60, 0)]
    
  }
  
  
  # Calculate MET minutes for each activity type
  all_metmin_cols <- paste0(all_types, "_METmin")
  prepared_data[, all_METmin := rowSums(.SD, na.rm = TRUE), .SDcols = all_metmin_cols]
  
  # Calculate MET minutes for each activity type excluding 'sitting' and 'off'
  activity_metmin_cols <- paste0(activity_types, "_METmin")
  prepared_data[, activity_METmin := rowSums(.SD, na.rm = TRUE), .SDcols = activity_metmin_cols]
  
  # Calculate activity time
  all_activity_cols <- paste0(activity_types, "_s")
  prepared_data[, activity_min := rowSums(.SD, na.rm = TRUE)/60, .SDcols = all_activity_cols]
  
  # Compute overall walking METs threshold to use as a relative threshold
  walking_METthreshold <- sum(prepared_data$walking_METmin, na.rm = TRUE) / sum(prepared_data$walking_s / 60, na.rm = TRUE)
  
  #Calculate total activity duration and duration below and above walkingMETthreshold
  
  # Iterate over each activity type for row-wise calculation
  for (activity in activity_types) {
    met_col <- paste0(activity, "_met100")
    time_col <- paste0(activity, "_s")
    
    # Calculate durations row-wise. We need to first summarize METmin and then divide by time to account for different activity durations at different MET-levels
    prepared_data[, (paste0(activity, "_below_threshold_min")) := fifelse(get(met_col)/100 < walking_METthreshold, get(time_col) / 60, 0)]
    prepared_data[, (paste0(activity, "_above_threshold_min")) := fifelse(get(met_col)/100 >= walking_METthreshold, get(time_col) / 60, 0)]
    
  }
  
  # Accumulate the durations into the total columns, ignoring NA values
  prepared_data[, duration_below_walkingMETs_min := rowSums(.SD, na.rm = TRUE), .SDcols = patterns("_below_threshold_min")]
  prepared_data[, duration_above_walkingMETs_min := rowSums(.SD, na.rm = TRUE), .SDcols = patterns("_above_threshold_min")]
  
  data_with_METs <- prepared_data
  
  # Return the updated data.table
  return(data_with_METs)
}