visualise_algorithm_data <- function(data_with_slnw, ID) {

############# Visualisation ############# 

#Create algorithm timestamp dataframe
# Create a column to identify changes in SLNW status
  data_with_slnw[, SLNW_change := ifelse(SLNW == "Wake", rleid(SLNW), NA)]

# Summarize data to get the first and last timestamp of each SLNW_change segment
data_slnw <- data_with_slnw %>%
  dplyr::filter(SLNW %in% c("Wake")) %>%  # Filter only relevant SLNW statuses
  group_by(SLNW_change, date) %>%
  dplyr::summarize(date = first(date),
                   from = first(timestamp),  # First timestamp within each group
                   to = last(timestamp)      # Last timestamp within each group
  ) %>%
  dplyr::filter(!is.na(SLNW_change))  # Filter out NA SLNW_change if needed

# Preparing data for the plot of activity types
activity_data <- melt(data_with_slnw, id.vars = c("date", "time", "timestamp"), measure.vars = c("off_s", "sitting_s", "standing_s", "walking_s", "cycling_s", "high_intensity_s"))

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

# Create a small dataframe for the "Waking period" line
waking_period_data <- data.frame(
  date = as.Date(data_slnw$from),
  from = data_slnw$from,
  to = data_slnw$to,
  y = -5,
  yend = -5,
  label = "Waking period"
)

# Plot with the manual color legend for the waking period
plot_activity_stacked <- ggplot() +
  geom_col(data = activity_data, aes(x = timestamp, y = value, fill = variable)) +
  geom_segment(
    data = waking_period_data,
    aes(x = from, xend = to, y = y, yend = yend, color = label),
    size = 1
  ) +
  scale_x_datetime(labels = function(x) format(x, "%H:%M"), date_breaks = "2 hours") +
  labs(
    title = "Activity Overview on Valid Waking Hours", 
    x = "Date and Time", 
    y = "Duration within one minute window (s)", 
    color = element_blank()
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1), legend.position = "right") +
  facet_wrap(~date, ncol = 1, scales = "free_x") +
  scale_fill_manual(values = c(
    "Off" = "#F2F2F2",
    "Sitting" = "#EF4DB7",
    "Standing" = "#19D5FD",
    "Walking" = "#3399FF",
    "Cycling" = "#264FCD",
    "High-intensity" = "#333399"
  )) +
  scale_color_manual(values = c("Waking period" = "red"))


# Name the plot
wd <- getwd()

file_name <- paste0(wd, "/Figures/Summaryplot_algorithm_", ID, ".pdf")

# Save the plot
ggsave(filename = file_name, plot = plot_activity_stacked, width = 10, height = 30, units = "in", dpi = 300)

}