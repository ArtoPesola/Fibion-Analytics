visualise_events_data <- function(data_with_METs, events, ID) {

  ############# Visualisation ############# 
  
  #Create algorithm timestamp dataframe
  # Create a column to identify changes in SLNW status
  data_with_METs[, event_change := ifelse(!is.na(event), rleid(event), NA)]
  
  # Summarize data to get the first and last timestamp of each SLNW_change segment
  data_with_METs_events <- data_with_METs %>%
    dplyr::filter(!is.na(event)) %>%  # Filter only events
    group_by(event_change, event, date) %>%
    dplyr::summarize(date = first(date),
                     from = first(timestamp),  # First timestamp within each group
                     to = last(timestamp)      # Last timestamp within each group
    ) %>%
    dplyr::filter(!is.na(event_change))  # Filter out NA SLNW_change if needed
  
  # Preparing data for the plot of activity types
  activity_data <- melt(data_with_METs, id.vars = c("date", "time", "timestamp"), measure.vars = c("off_s", "sitting_s", "standing_s", "walking_s", "cycling_s", "high_intensity_s"))
  
  # Ensure 'variable' is a character to avoid factor-related issues
  activity_data$variable <- as.character(activity_data$variable)
  
  # Rename
  activity_data <- activity_data %>%
    dplyr::mutate(variable = case_when(
      variable == "off_s" ~ "Off",
      variable == "sitting_s" ~ "Sitting",
      variable == "standing_s" ~ "Standing",
      variable == "walking_s" ~ "Walking",
      variable == "cycling_s" ~ "Cycling",
      variable == "high_intensity_s" ~ "High-intensity",
      TRUE ~ variable
    ))
  
  activity_data$variable <- factor(activity_data$variable, levels = c("Off", "Sitting", "Standing", "Walking", "Cycling", "High-intensity"))
  
  # Create the stacked bar plot with modifications for hours and minutes display
  plot_activity_stacked_events <- ggplot() +
    geom_col(data=activity_data, aes(x = timestamp, y = value, fill = variable)) +
    geom_segment(data = data_with_METs_events, aes(x = from, xend = to, y = -5, yend = -5, color = event)) +
    scale_x_datetime(labels = function(x) format(x, "%H:%M"), date_breaks = "2 hours") +  # Adjusted for hours and minutes
    labs(title = "Activity Overview on Valid Waking Hours", x = "Date and Time", y = "Duration within one minute window (s)") +
    theme_minimal() +
    theme(axis.text.x = element_text(hjust = 1), legend.position = "right") +
    facet_wrap(~date, ncol=1, scales="free_x") +
    scale_fill_manual(values = c(
      "Off" = "#F2F2F2",
      "Sitting" = "#EF4DB7",
      "Standing" = "#19D5FD",
      "Walking" = "#3399FF",
      "Cycling" = "#264FCD",
      "High-intensity" = "#333399"
    ))

# Name the plot
wd <- getwd()

file_name <- paste0(wd, "/Figures/Summaryplot_events_", ID, ".pdf")

# Save the plot
ggsave(filename = file_name, plot = plot_activity_stacked_events, width = 10, height = 30, units = "in", dpi = 300)

}