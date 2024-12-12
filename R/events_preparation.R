prepare_events <- function(currentfile) {

############# Get the events file #############

#Get file info to match the events sheet  
file <- sub(".*data/", "", currentfile)

# Load the Excel file and match the sheet current file
events <- read_excel(path = file.path(getwd(), "Example data/events.xlsx"), sheet=file )

setDT(events)

# Make sure events timestamp is in PosixCT format
events <- events %>% 
  mutate(from = as.POSIXct(from, format = "%d.%m.%Y %H:%M"),
         to = as.POSIXct(to, format = "%d.%m.%Y %H:%M"))

eventstimezone <- attr(events$from, "tzone")

return(events)

}