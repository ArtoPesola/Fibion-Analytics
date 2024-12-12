identify_slnw <- function(data_with_METs, parameters) {
  
  ####### Step 1: Identify Off Segments as "SLNW" #######
  
  # Function to identify off and low variability segments
  identify_segments <- function(data_with_METs, off_window_length, break_window_length, activity_threshold, variability_threshold) {
    off_window_width <- off_window_length * 60  # Convert hours to minutes for the rolling window
    break_window_width <- break_window_length * 60  # Convert hours to minutes for the rolling window
    
    # Rolling sum of Off and Activity for the off_window_width
    data_with_METs[, off_sum := rollapplyr(Off, width = off_window_width, FUN = sum, fill = NA, align = "center")]
    data_with_METs[, activity_sum_next := shift(rollapplyr(Activity, width = break_window_width, FUN = sum, fill = NA, align = "center"), n = -1, fill = NA)]
    
    # Rolling variability of all_METmin for the off_window_width
    data_with_METs[, roll_variability := rollapplyr(all_METmin, width = off_window_width, FUN = function(x) max(x, na.rm = TRUE) - min(x, na.rm = TRUE), fill = NA, align = "center")]
    
    # Define off_segment and low_variability based on conditions
    data_with_METs[, off_segment := off_sum > 30 & (activity_sum_next / break_window_width <= activity_threshold * break_window_width)]
    data_with_METs[, low_variability := roll_variability < variability_threshold]
    
    return(data_with_METs)
  }
  
  # Apply the function to identify off and low variability segments
  data_with_METs <- identify_segments(
    data_with_METs, 
    parameters$off_window_length, 
    parameters$break_window_length, 
    parameters$activity_threshold, 
    parameters$variability_threshold
  )
  
  # Marking SLNW and Wake based on off and low variability segments
  data_with_METs[, SLNW := ifelse(off_segment | low_variability, "SLNW", "Wake")]
  
  # Clean up temporary columns
  data_with_METs[, c("off_sum", "activity_sum_next", "off_segment", "low_variability", "roll_variability") := NULL]
  
  
  ####### Step 2: Iteratively extend the SLNW window #######
  
  # Optimized Function to Identify and Update Surrounding Bouts
  identify_surrounding_bouts_optimized <- function(data_with_METs, sleep_window, stationary_dur, activity_threshold) {
    data_with_METs[, grp := cumsum(c(0L, diff(SLNW == "SLNW") != 0))]
    
    slnw_groups <- data_with_METs[SLNW == "SLNW", .(start_time = min(timestamp) - sleep_window, end_time = max(timestamp) + sleep_window), by = grp]
    
    for (k in 1:nrow(slnw_groups)) {
      indices <- which(data_with_METs$timestamp >= slnw_groups$start_time[k] & data_with_METs$timestamp <= slnw_groups$end_time[k])
      
      data_with_METs[indices, SLNW := ifelse(
        (Stationary >= (stationary_dur * 60 * 60)) |
          (SLNW == "SLNW") |
          ((Stationary >= (stationary_dur * 60 * 60 / 4)) & ((Activity / (.N * 60)) < activity_threshold)), 
        "SLNW", SLNW)]
    }
    
    data_with_METs[, grp := NULL]
    return(data_with_METs)
  }
  
  data_with_METs <- identify_surrounding_bouts_optimized(
    data_with_METs, 
    parameters$sleep_window, 
    parameters$stationary_dur, 
    parameters$activity_threshold
  )
  
  
  ####### Step 3: Drop short wake periods if they are preceded and followed by SLNW #######
  
  # Calculate segment IDs
  data_with_METs[, segment_id := rleid(SLNW)]
  
  # Summarize the duration of each segment and calculate the proportion of Activity time
  segment_summaries <- data_with_METs[, .(segment_duration = .N, 
                                    total_activity = sum(Activity)), by = .(segment_id, SLNW)]
  
  # Calculate the proportion of Activity time in each segment
  segment_summaries[, activity_ratio := total_activity / (segment_duration * 60)]  # Assuming each row represents 1 minute
  
  # Shift to get previous and next segment durations
  segment_summaries[, prev_duration := shift(segment_duration, n = 1, type = "lag", fill = 0)]
  segment_summaries[, next_duration := shift(segment_duration, n = 1, type = "lead", fill = 0)]
  
  # Join the segment summaries back to the original data
  data_with_METs <- data_with_METs[segment_summaries, on = .(segment_id, SLNW)]
  
  data_with_METs[, SLNW := ifelse(segment_duration < (parameters$short_wake_segment * 60) &
                              SLNW == "Wake" &
                              activity_ratio <= parameters$activity_threshold &  # Maximum 10% Activity time
                              prev_duration >= 10 &
                              next_duration >= 10, 
                            "SLNW", SLNW)]
  
  
  # Clean up temporary columns
  data_with_METs[, c("Activity", "Stationary", "segment_duration", "total_activity", "activity_ratio", "prev_duration", "next_duration", "segment_id") := NULL]
  
  
  ####### Step 4: Validate Day Length #######
  
  # Calculate SLNW duration per day
  data_with_METs[, Wake_duration := sum(SLNW == "Wake", na.rm = TRUE), by = .(date)]  # Assuming each row is 1 minute
  
  # Mark days as Short or Valid
  data_with_METs[, Wake_length_criteria := ifelse(Wake_duration < parameters$wake_criteria * 60, "Short", "Valid"), by = .(date)]
  
  # Mark SLNW segments for Short days
  data_with_METs[Wake_length_criteria == "Short", SLNW := "SLNW"]
  
  data_with_slnw <- data_with_METs
  
  return(data_with_slnw)
}
