prepare_data <- function(currentfile) {
  # Debugging: Print the file path
  message("Reading file: ", currentfile)
  
  # Verify if the file exists
  if (!file.exists(currentfile)) stop("File does not exist: ", currentfile)
  
  # Load the data
  data_table <- fread(currentfile, sep = ";", fill = TRUE)

  # Rename columns
  setnames(data_table, old = c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13"), 
           new = c("timestamp", "off_s", "sitting_s", "standing_s", "walking_s", "cycling_s", "high_intensity_s", "off_met100", "sitting_met100", "standing_met100", "walking_met100", "cycling_met100", "high_intensity_met100"))
  
  # Initialize metadata_table_row to integer(0) to handle cases where [METAdata_table] is not found
  metadata_row <- integer(0)
  
  # Now, try to identify the row where '[METADATA]' starts
  metadata_row <- which(data_table$timestamp == "[METADATA]")
  
  # If '[metadata_table]' is found, filter out all rows starting from that row
  if (length(metadata_row) > 0) {
    data_table <- data_table[1:(metadata_row - 1)]
  }
  
  # Ensure the timestamp format is okay
  data_table[, timestamp := as.POSIXct(timestamp)]
  
  # Extract the hours and minutes component
  data_table[, time := format(timestamp, "%H:%M")]
  
  # Extract the date component
  data_table[, date := as.Date(format(timestamp, "%Y-%m-%d"))]
  
  # Create new variables weekday and week_weekend
  data_table[, c("weekday", "week_weekend") := list(weekdays(date), ifelse(weekdays(date) %in% c("Saturday", "Sunday"), "weekendday", "weekday"))]
  
  
  setcolorder(data_table, c("timestamp", "date", "time", "weekday", "week_weekend", names(data_table)[!(names(data_table) %in% c("timestamp", "date", "time", "weekday", "week_weekend"))]))
  
  # Filter out rows that have more than 60 seconds of data_table
  # Convert columns ending in "_s" to numeric
  cols_to_convert <- names(data_table)[grepl("_s$", names(data_table))]
  data_table[, (cols_to_convert) := lapply(.SD, as.numeric), .SDcols = cols_to_convert]
  
  data_table[, rowsum_s := rowSums(.SD, na.rm = TRUE), .SDcols = patterns("_s$")]
  
  # Add activity summary variables
  data_table[, Off := off_s]
  data_table[, Stationary := sitting_s + standing_s]
  data_table[, Activity := walking_s + cycling_s + high_intensity_s]
  
  #Fill in the potential gaps in the timeline
  
  # Define the desired interval (1 minute)
  interval <- 60  # 60 minutes in seconds
  
  # Create a complete sequence of timestamps
  min_time <- min(data_table$timestamp)
  max_time <- max(data_table$timestamp)
  complete_timestamps <- seq(from = min_time, to = max_time, by = interval)
  
  # Create a new data_table.table with complete timestamps
  complete_data_table <- data.table(timestamp = complete_timestamps)
  
  # Merge with the original data_table and carry the last observation forward
  prepared_data <- data_table[complete_data_table, on = "timestamp", roll = "nearest"]
  
  return(prepared_data)
}
