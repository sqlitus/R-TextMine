# OVOT HISTORICAL BACKLOG REPORT #
# Using Incident Assignment History & State History

library(tidyverse); library(lubridate)
start_time <- Sys.time()
print(paste("Starting:", start_time))

# import all appropriately named files
path <- "\\\\cewp1650\\Chris Jabr Reports\\ONOW Exports\\INC History"
state_history_files <- list.files(path, "(?i)state_hist", full.names = TRUE)
assign_history_files <- list.files(path, "(?i)assign_hist", full.names = TRUE)

# merge state history
state_history <- data_frame()
for (i in 1:length(state_history_files)){
  data <- readxl::read_excel(state_history_files[i])
  # data$import_sheet <- str_extract(state_history_files[i], "(?<=/).*") # positive lookbehind
  state_history <- bind_rows(state_history, data)
}
state_history <- state_history %>% distinct()

# merge assignment history
assign_history <- data_frame()
for (i in 1:length(assign_history_files)){
  data <- readxl::read_excel(assign_history_files[i])
  # data$import_sheet <- str_extract(assign_history_files[i], "(?<=/).*") # positive lookbehind
  assign_history <- bind_rows(assign_history, data)
}
assign_history <- assign_history %>% distinct()

# set TZ of imported times to CST
state_history[c('Start','End')] <- force_tz(state_history[c('Start','End')], tzone = 'US/Central')
assign_history[c('Start','End')] <- force_tz(assign_history[c('Start','End')], tzone = 'US/Central')

# calendar table; datetime @ 8am
calendar_start <- date("2018-02-22")
calendar <- data_frame(
  date = seq.Date(from = calendar_start, to = today(), by = "days"),
  datetime = seq.POSIXt(from = as.POSIXct(paste(calendar_start, "08"), format = "%Y-%m-%d %H"), 
                        to = as.POSIXct(today()+1), 
                        by = "DSTday")
)

# get all distinct incidents from both datasets
distinct_incidents <- bind_rows(state_history %>% select(Number), assign_history %>% select(Number)) %>% distinct()

# construct daily list of open + OnePOS assigned tickets. use state history first since it's all tickets.
ovot <- data_frame()
for (i in 1:nrow(calendar)){
  insert_day <- distinct_incidents %>% mutate(datetime = calendar$datetime[i]) %>% 
    left_join(state_history, by = "Number") %>%
    filter(Start <= calendar$datetime[i] & (calendar$datetime[i] < End | is.na(End))) %>%
    left_join(assign_history, by = "Number") %>%
    filter(Start.y <= calendar$datetime[i] & (calendar$datetime[i] < End.y | is.na(End.y))) %>% 
    distinct()
  ovot <- bind_rows(ovot, insert_day)
}
ovot <- ovot %>% distinct()

# prune & output file
out <- ovot %>% select(Number, datetime, Status=Value.x, Team=Value.y)
writeLines(paste("Exporting file now at", Sys.time(),"\n Elapsed time:", Sys.time()-start_time))
write.csv(out, na = "", row.names = FALSE, paste0(path, "\\ovot.csv"))

# timing
writeLines(paste0("Start time: ", start_time, "\nEnd time: ", Sys.time(), "\nElapsed time: ", Sys.time() - start_time))


## add check for proper data size...
