# Scratchpad


# 1/12/2018 - Validations Data Analysis ----
library(readxl)
library(tidyverse)
library(tidytext)
library(stringr)

raw.data <- file.choose()
v.data <- readxl::read_excel(path = raw.data, sheet = "Data",trim_ws = TRUE)
ggplot(v.data, aes(Date)) + geom_bar()

# replace column names
names(v.data) <- str_replace_all(names(v.data), " |\\-", "\\.")

# unigrams
v.data %>% unnest_tokens(word, `POS.Readiness.Status`) %>% count(word, sort = TRUE) %>% View()

# bigrams
v.data %>% unnest_tokens(bigram, POS.Readiness.Status, token = "ngrams", n = 2) %>% count(bigram, sort = TRUE)

# filtered bigrams vs unfiltered
test <- v.data %>% unnest_tokens(bigram, POS.Readiness.Status, token = "ngrams", n = 2) %>%
  mutate(bigram.copy = bigram) %>%
  separate(bigram, c("word1","word2"), sep = " ")

test %>% count(word1, word2, sort = T)
test %>% count(bigram.copy, sort = T)


# 1/19/2018 test clipboard & text parsing ----

a <- readClipboard()
a
str(a)
grep("\\t", a) # metacharacters left in when data read from clipboard
a.t <- data_frame(text = a)
str(a.t)
a1 <- a.t %>% unnest_tokens(output = word, input = text) %>% count(word, sort = TRUE)
a2 <- a.t %>% unnest_tokens(output = word, input = text, format = c("text")) %>% count(word, sort = TRUE)
identical(a1, a2)

# custom words parser (n = 1 word) without filtering out single letter words
a3 <- a.t %>% unnest_tokens(word, text, token = "regex", pattern = "[ ]+") %>% count(word, sort = TRUE)
a3
a3$word[grep("(?i)source", a3$word)]

# r.1: replace entire cell w/ value; r.2: custom ngrams w/ filtering out metadata
a4 <- a.t %>% unnest_tokens(word, text, token = "regex", pattern = "[ ]+[^[:space:]]+[ ]+[^[:space:]]+") %>%
  mutate(word.r = replace(word, str_detect(word, "[[:cntrl:]]"), "")) %>%
  mutate(word.r.2 = str_replace_all(word, "[[:cntrl:]]", ""))
  count(word, sort = TRUE)
a4
View(a4)

# 1/29/2018 power point w/ ggplot & annotations ----
library(tidyverse)
library(officer)
library(rvg) # graphics into the ppt

setwd("C:\\Work\\Analysis\\OnePOS\\New Weekly Support Deck - 2017")
getwd()

myslide <- read_pptx("C:\\Work\\Analysis\\OnePOS\\New Weekly Support Deck - 2017\\01_31_2018 refresh.pptx")
layout_summary(myslide)
layout_properties(myslide)
slide_summary(myslide) # read from individual slide?

myslide %>%
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with_text(type = "title", str = "here is some Title text") %>%
    ph_with_vg(type = "body", code = print(aloha.top.down.p)) %>%
  add_slide(layout = "Title and Content", master = "1_Office Theme") %>%
    ph_with_text(type = "title", str = "phwithvgat") %>%
    ph_with_vg_at(code = print(aloha.top.down.p), left = .17, top = 1.2, width = 13, height = 6) %>%
    ph_with_text(type = "ftr", str = paste0("my text is ", max(aloha.top.down$n)))

print(myslide, target = "myNewPowerPoint.pptx")

# need to add footer text (properties)
# need to read slide (slide summary?)






#### sample filtering exercise ----
library(tidyverse)
t1 <- data_frame(col1 = c(1,2,3,4,5,6),
                 col2 = c("NE something something", "not nets", "nerds are funny", "help ne", "my ne region", "the cane"),
                 col3 = c("RM","NE","NE","PN","",""))

t1 %>% filter(col3 == "NE")
t1 %>% filter(grepl("NE", col2))
t1 %>% filter(grepl("(?i)NE", col2))
t1 %>% filter(grepl("(?i)\\WNE", col2))
t1 %>% filter(grepl("(?i)NE\\W", col2))
t1 %>% filter(grepl("(?i)\\WNE", col2))
t1 %>% filter(grepl("(?i)(^NE )|( NE$)", col2))
t1 %>% filter(grepl("(?i)\\bne\\b", col2))    # !!! searches for full word only


#### Brian regional IT analysis ----
library(readxl); library(tidyverse); library(tidytext)


r.data <- readxl::read_excel(path = file.choose(), sheet = "other",trim_ws = TRUE)
r.data$combined <- paste(r.data$Title, r.data$Description)
r.data$combined <- gsub("NULL|NA","", r.data$combined)
r.data$combined
r.data$`combined fields` <- NULL

# text analysis
r.data.words <- r.data %>% unnest_tokens(word, combined, token = "words", drop = FALSE) %>% distinct()
r.data.words %>% count(word, sort = TRUE) %>% top_n(5)

r.data %>% unnest_tokens(bigram, combined, token = "ngrams", n = 2, drop = FALSE) %>% distinct() %>%
  count(bigram, sort = TRUE)
r.data %>% unnest_tokens(trigram, combined, token = "ngrams", n = 3, drop = FALSE) %>% distinct() %>%
  count(trigram, sort = TRUE)
r.data %>% unnest_tokens(ngram, combined, token = "ngrams", n = 5, drop = FALSE) %>% distinct() %>%
  count(ngram, sort = TRUE)


r.data %>% unnest_tokens(trigram, combined, token = "ngrams", n = 3, drop = FALSE) %>% distinct() %>% View()




# level 3 analysis
r.other <- read.delim("clipboard")
View(r.other)
r.other <- readxl::read_excel(path = file.choose(), sheet = "Sheet3",trim_ws = TRUE)

r.other.words <- r.other %>% unnest_tokens(word, `combined fields`, token = "words", drop = FALSE) %>% distinct()
r.other.words %>% count(word, sort = TRUE) %>% top_n(25)
r.other.words %>% count(word, sort = TRUE) %>% anti_join(stop_words) %>% top_n(25)

r.other %>% unnest_tokens(bigram, `combined fields`, token = "ngrams", n = 2, drop = FALSE) %>% distinct() %>%
  count(bigram, sort = TRUE)
r.other %>% unnest_tokens(trigram, `combined fields`, token = "ngrams", n = 3, drop = FALSE) %>% distinct() %>%
  count(trigram, sort = TRUE)
r.other %>% unnest_tokens(ngram, `combined fields`, token = "ngrams", n = 5, drop = FALSE) %>% distinct() %>%
  count(ngram, sort = TRUE)


r.other %>% unnest_tokens(trigram, `combined fields`, token = "ngrams", n = 3, drop = FALSE) %>% distinct() %>% View()


r.other %>% filter(grepl("(?i)regional it support", r.other$`combined fields`)) %>% select(Title, Description) %>% slice(1)
grep






#### quick steph NE analysis - PHRASE ANALYSIS ----
library(tidyverse); library(tidytext); library(readxl)

scale_data <- readxl::read_excel(path = file.choose(), sheet = "data",trim_ws = TRUE, skip = 3)
View(scale_data)
str(scale_data)
dim(scale_data)
unique(scale_data$Smart_Region)
scale_data %>% filter(Smart_Region == "ne") %>% View()
table(scale_data$Smart_Region)
scale_data$Smart_Region <- toupper(scale_data$Smart_Region)
table(scale_data$Smart_Region) %>% str()
table(scale_data$Smart_Region) %>% sort(decreasing = T) %>% sum()
scale_data %>% filter(is.na(Smart_Region)) %>% View()

scale_data_2 <- scale_data
scale_data_2 <- scale_data_2 %>% mutate(Smart_Region = replace(Smart_Region, is.na(Smart_Region), "(blank)"))
table(scale_data$Smart_Region) %>% sum()
table(scale_data_2$Smart_Region) %>% sum()



# !!! phrase analysis: most sophisticated phrase analysis done yet #

# part 1: compile words w/ counts
scale_data_2 %>% unnest_tokens(ngram, Title, token = "ngrams", n = 2) %>% distinct() %>% count(ngram, sort = TRUE)
scale_data_2 %>% unnest_tokens(ngram, Title, token = "ngrams", n = 3) %>% distinct() %>% count(ngram, sort = TRUE)

# get top x words by frequency
scale_data_phrases <- scale_data_2 %>% unnest_tokens(phrase, Title) %>% distinct() %>% count(phrase, sort = TRUE) %>% 
  slice(1:100) %>% mutate(num_words = 1)


# append top x ngrams by frequency of 1:10 ngram length
for (i in 2:10){
  step_df <- scale_data_2 %>% unnest_tokens(phrase, Title, token = "ngrams", n = i) %>% 
    distinct() %>% count(phrase, sort = TRUE) %>% slice(1:100) %>% mutate(num_words = i)
  scale_data_phrases <- scale_data_phrases %>% bind_rows(step_df)
}


# create words dataset to attach calculations to original dimensions
scale_data_phrases_base <- scale_data_2 %>% unnest_tokens(phrase, Title, drop = FALSE) %>% distinct()
scale_data_full <- left_join(scale_data_phrases, scale_data_phrases_base, by = "phrase")
write.csv(scale_data_phrases_base, "scale_data_phrases_base.csv")


# part 2: calculate modified frequency
scale_data_phrases <- scale_data_phrases %>% mutate(adj_freq = n * num_words ^ (1 + num_words / 10))
scale_data_phrases <- scale_data_phrases %>% 
  # adjust frequency score (lower) if it is a SINGLE stopword
  mutate(adj_freq_stopwords = case_when(phrase %in% stop_words$word ~ adj_freq * .5, TRUE ~ adj_freq)) %>%
  # flag row if it is a SINGLE stopword
  mutate(stopword_adjusted = case_when(phrase %in% stop_words$word ~ TRUE)) %>%
  # flag row if it contains ANY stopwords
  mutate(full_stopword_found = case_when(grepl(paste(paste0("\\b",stop_words$word,"\\b"), collapse="|"), phrase)~"found stopword"))
  # count how many stopwords found ...
  # adj freq by how many stopwords found ...
  # flag if this phrase is fully contained in a higher-scoring phrase


scale_data_phrases %>% arrange(desc(adj_freq)) %>% View()




# data checking: double counted - tickets w/ words overlap - mentions word 'scale' and 'scales'
grep("(?i)\\bscale\\b", scale_data$Title) %>% length()
grep("(?i)\\bscales\\b", scale_data$Title) %>% length()

scale_data %>% filter(grepl("\\bscale\\b", scale_data$Title)) %>% View()
scale_data %>% filter(grepl("(?i)\\bscale\\b", scale_data$Title) & grepl("(?i)\\bscales\\b", scale_data$Title)) %>% View()





# part 3: visualize simple ngrams
scale_data_phrases

write.csv(scale_data_phrases, "scale_data_phrases.csv")








# function -- NOT WORKING
Phrase_df <- function(in_df, the_text_column = "Title", num_ngrams = 10, top_cut = 30){
  
  require(dplyr); require(lazyeval); require(tidytext)
  out_df <- in_df %>% unnest_tokens(output = ngram, input = the_text_column) %>% distinct() %>% 
    count(ngram, sort = TRUE) %>% slice(1:top_cut) %>% mutate(num_words = 1)
  
  for (i in 2:num_ngrams){
    step_df <- in_df %>% unnest_tokens(output = ngram, input = the_text_column, token = "ngrams", n = i) %>% distinct() %>% 
      count(ngram, sort = TRUE) %>% slice(1:top_cut) %>% mutate(num_words = i)
    out_df <- out_df %>% bind_rows(step_df)
  }
  return(out_df)
}

scale_data_phrases <- Phrase_df(scale_data)






#### 4/18/2018 - Text parsing - PHASE NUM in title ----
library(tidyverse)
ONOW_OnePOS_List <- readxl::read_excel(path = "\\\\cewp1650\\Chris Jabr Reports\\ONOW Exports\\incident.xlsx")
import_time <- date()

# FCF / FDR
ONOW_OnePOS_List$Cdate <- lubridate::date(ONOW_OnePOS_List$Created)
ONOW_OnePOS_List$Rdate <- lubridate::date(ONOW_OnePOS_List$Resolved)
ONOW_OnePOS_List$FCF <- case_when(ONOW_OnePOS_List$Cdate == ONOW_OnePOS_List$Rdate & ONOW_OnePOS_List$`Reassignment count` == 0~1,
                                   TRUE ~ 0)
ONOW_OnePOS_List$FDR <- case_when(ONOW_OnePOS_List$Cdate == ONOW_OnePOS_List$Rdate ~ 1, TRUE ~ 0)

sum(ONOW_OnePOS_List$FCF, na.rm = TRUE)
sum(ONOW_OnePOS_List$FDR, na.rm = TRUE)

# grab phase # from title & other useful fields Tracy mentioned
ONOW_OnePOS_List$Phase_Num = stringr::str_extract(ONOW_OnePOS_List$`Short description`, "\\d??[.]\\d[.]\\d")
ONOW_OnePOS_List$BU <- stringr::str_extract(ONOW_OnePOS_List$`Short description`, "\\b\\d\\d\\d\\d\\d\\b")
ONOW_OnePOS_List$Device_Name <- 
  stringr::str_extract(ONOW_OnePOS_List$`Short description`, "(?i)wfm\\s?\\d{5}\\s?[a-z]{3}\\s?\\d{2,3}") %>%
  stringr::str_replace_all("\\s", "") %>%
  toupper()
ONOW_OnePOS_List$Lane <- stringr::str_extract(
  ONOW_OnePOS_List$`Short description`, "(?i)(lane|reg|tab|pck|svr|aha)\\W{0,2}\\d{2,3}") %>%
  toupper() %>%
  stringr::str_replace_all("\\W", "") %>%
  stringr::str_replace("([A-Z])(\\d)", "\\1 \\2")
select(ONOW_OnePOS_List, -`Short description`, everything()) %>% View()

write.csv(ONOW_OnePOS_List, na = "", row.names = FALSE, "\\\\cewp1650\\Chris Jabr Reports\\ONOW Exports\\ONOW_OnePOS_Metrics.csv")



# test
x <- c("one two three", "four five six")
grep("one|four", x)
grep("one|our\\b", x)
grep("one|our", x)
grep("\\one\\b|\\bsix\\b", x)


# lanes vs regs
lanes <- ONOW_OnePOS_List %>% select(Lane) %>% filter(grepl("REG", Lane)) %>% distinct()
ONOW_OnePOS_List %>% filter(grepl("REG 06", Lane)) %>% View()




#### Latency analysis for L ----
library(tidyverse)
latency.data <- readxl::read_excel(
  path = "C:\\Work\\Requests\\Lori\\2018-05-01 Latency Issues\\2018-05-01 DATA LatencyReport.xlsx")
str(latency.data)


# deriving BU from short description's BU# and Machine#
latency.data$extracted_BU <- stringr::str_extract(latency.data$`Short description`, "\\b\\d\\d\\d\\d\\d\\b")
latency.data$compare_BU <- case_when(latency.data$BU == latency.data$extracted_BU ~ "same", TRUE ~ "diff")
latency.data$extracted_DeviceName <- 
  stringr::str_extract(latency.data$`Short description`, "(?i)wfm\\s?\\d{5}\\s?[a-z]{3}\\s?\\d{2,3}") %>%
  stringr::str_replace_all("\\s", "") %>% toupper()
latency.data$extracted_DeviceName_BU <- stringr::str_extract(latency.data$extracted_DeviceName, "\\d\\d\\d\\d\\d")
latency.data$derived_BU <- case_when(!is.na(latency.data$extracted_BU) ~ latency.data$extracted_BU,
                                     !is.na(latency.data$extracted_DeviceName_BU) ~ latency.data$extracted_DeviceName_BU,
                                     TRUE ~ latency.data$BU)


# cleaning excel imported dates: go-live times
wfm.schedule.data <- readxl::read_excel(
  path = "C:\\Work\\Requests\\Lori\\2018-05-01 Latency Issues\\WFM Schedule Breakdown - April 30 2018.xlsx")

latency.data.full <- left_join(latency.data, wfm.schedule.data, by = c("derived_BU" = "Business Unit"))
latency.data.full$`Go-Live_Converted` <- as.Date(as.numeric(latency.data.full$`Go-Live`), origin = "1899-12-30")


# extract reg/loc from short description, otherwise extract from original reg/loc field to accurately derive location
latency.data.full$extracted_Region <- stringr::str_extract(
  latency.data.full$`Short description`, "CE|FL|MA|MW|NA|NC|NE|PN|RM|SO|SP|SW|TS")
latency.data.full$derived_Region <- case_when(!is.na(latency.data.full$extracted_Region) ~ latency.data.full$extracted_Region,
                                              !is.na(latency.data.full$Region.y) ~ latency.data.full$Region.y,
                                              TRUE ~ latency.data.full$Region.x)
latency.data.full %>% select(-Region.y, everything()) %>% filter(Region.y != derived_Region | is.na(Region.y)) %>% View()

latency.data.full$extracted_Location <- stringr::str_extract(
  latency.data.full$`Short description`, "(CE|FL|MA|MW|NA|NC|NE|PN|RM|SO|SP|SW|TS)\\W{0,3}[A-z]{3}")
latency.data.full$extracted_Location_Loc <- stringr::str_sub(latency.data.full$extracted_Location, -3, -1)

latency.data.full$Site_Loc <- stringr::str_sub(latency.data.full$`Site List`, 3, 5)
latency.data.full$derived_Location <- case_when(!is.na(latency.data.full$extracted_Location_Loc) ~ 
                                                  latency.data.full$extracted_Location_Loc,
                                                !is.na(latency.data.full$Site_Loc) ~ latency.data.full$Site_Loc,
                                                TRUE ~ latency.data.full$`Store Abbreviation`)

# LEFT OFF: LATENCY DATA FULL DATASET. FOLLOW STEPS FOR ANALYSIS BELOW.
# ngram analysis of short description for common issues - then regex type 

library(tidytext)
latency.data.full %>% unnest_tokens(ngram, `Short description`, token = "ngrams", n = 1, drop = FALSE) %>% 
  distinct() %>% count(ngram, sort = TRUE)
latency.data.full %>% unnest_tokens(ngram, `Short description`, token = "ngrams", n = 2, drop = FALSE) %>% 
  distinct() %>% count(ngram, sort = TRUE)
latency.data.full %>% unnest_tokens(ngram, `Short description`, token = "ngrams", n = 3, drop = FALSE) %>% 
  distinct() %>% count(ngram, sort = TRUE)


# slice ticket count by region, location, site completion, go-live date, site type, state....etc
write.csv(latency.data.full, na = "", row.names = FALSE, "\\\\cewp1650\\Chris Jabr Reports\\Analysis\\latency_data.csv")

latency.data.full.export <- latency.data.full %>% 
  select(-c(`Go-Live`, extracted_BU, extracted_DeviceName, extracted_DeviceName_BU, BU, Region.y, 
            extracted_Region, compare_BU, Site_Loc, extracted_Location, extracted_Location_Loc), 
         "original_Region" = Region.x, "original_Store_Abbreviation" = `Store Abbreviation`)

write.csv(latency.data.full.export, na = "", row.names = FALSE, "\\\\cewp1650\\Chris Jabr Reports\\Analysis\\latency_data.csv")










## recreate an Extended Metrics export - with more robust R calculations ----
# FCF etc (asignee change counter, reassignment count, etc)
# also: approx. string matching categorization for incident typing...(latency issue, etc)

## text analysis
# approx string matching for spell checking & correcting words, before text aggregation analysis
# implement phrase algorithm & various measures for common topics. Need to research filtering out strings in a vector.

## historical OVOT reporting
# modeling? Times stuff sent to RS? feature engineer.


# Extended Metrics: Best practice metric calcualtion / feature engineering. 
# REQUIRED FIELDS: SHORT DESCRIPTION, BU, LOCATION
library(tidyverse)
OnePOS_Incidents_Import <- readxl::read_excel(path = "\\\\cewp1650\\Chris Jabr Reports\\ONOW Exports\\incident.xlsx")
inc_data <- OnePOS_Incidents_Import


inc_data$title_extracted_BU <- str_extract(inc_data$`Short description`, "\\b\\d\\d\\d\\d\\d\\b")
inc_data$title_extracted_DeviceName <- inc_data$`Short description` %>%
  str_extract("(?i)wfm\\s?\\d{5}\\s?[a-z]{3}\\s?\\d{2,3}") %>%
  str_replace_all("\\s", "") %>% toupper()
inc_data$title_extracted_DeviceName_BU <- str_extract(inc_data$title_extracted_DeviceName, "\\d\\d\\d\\d\\d")
inc_data$derived_BU <- case_when(
  !is.na(inc_data$title_extracted_BU) ~ inc_data$title_extracted_BU,
  !is.na(inc_data$title_extracted_DeviceName_BU) ~ inc_data$title_extracted_DeviceName_BU,
  TRUE ~ inc_data$BU)
inc_data$derived_BU_source <- case_when(
  !is.na(inc_data$title_extracted_BU) ~ "short description BU#",
  !is.na(inc_data$title_extracted_DeviceName_BU) ~ "short description device name",
  TRUE ~ "BU field")
inc_data$derived_Lane <- inc_data$`Short description` %>%
  str_extract("(?i)(lane|reg|tab|pck|svr|aha)\\W{0,2}\\d{2,3}") %>% toupper() %>%
  str_replace_all("\\W", "") %>%
  str_replace("([A-Z])(\\d)", "\\1 \\2") %>%
  str_replace("(?i)lane", "REG")
inc_data$derived_Phase_Num <- 
  str_extract(inc_data$`Short description`, "(\\d{1,2}½?|\\d[ ]?\\d/\\d)[ ]?[.]\\d[.]\\d")
inc_data$extracted_Location <- 
  str_extract(inc_data$`Short description`, "\\b(((?i)(CE|FL|MA|MW|NA|NC|NE|PN|RM|SP|SW|TS))|SO|365)\\b\\W{0,3}[A-z]{3}\\b") %>%
  toupper()
inc_data$derived_Location <- case_when(
  !is.na(inc_data$extracted_Location) ~ inc_data$extracted_Location, TRUE ~ inc_data$Location)
inc_data$derived_Location_source <- case_when(
  !is.na(inc_data$extracted_Location) ~ "short description", TRUE ~ "Location field")
inc_data$derived_Region <- str_sub(inc_data$derived_Location, 1, 2) %>% toupper()


# get rid of "builder" columns
inc_data$title_extracted_BU <- NULL
inc_data$title_extracted_DeviceName <- NULL
inc_data$title_extracted_DeviceName_BU <- NULL
inc_data$extracted_Location <- NULL


# just checking data
filter(inc_data, grepl("[a-z]", inc_data$derived_Location)) %>% View()
filter(inc_data, is.na(derived_Location)) %>% View()
filter(inc_data, derived_Region == "36") %>% View()
filter(inc_data, derived_Region == "TS") %>% View()
filter(inc_data, grepl("[a-z]", inc_data$derived_Location)) %>% View()
unique(inc_data$derived_Lane)[unique(inc_data$derived_Lane) %>% str_detect("REG")]
inc_data %>% distinct(derived_Lane)
inc_data %>% distinct(derived_Lane) %>% filter(grepl("AHA", derived_Lane))
table(inc_data$derived_Lane) %>% sort(decreasing = T)
filter(inc_data, derived_Lane == "REG 20") %>% View()


## Spell checking & text mining



# OVOT HISTORICAL BACKLOG REPORT ----
# Incident Assignment History
library(tidyverse); library(lubridate)
OnePOS_Assignments_Import <- readxl::read_excel(path = "\\\\cewp1650\\Chris Jabr Reports\\ONOW Exports\\incident_metric.xlsx")
OnePOS_Assignments_Import$Start <- force_tz(OnePOS_Assignments_Import$Start, "US/Central")
OnePOS_Assignments_Import$End <- force_tz(OnePOS_Assignments_Import$End, "US/Central")
OnePOS_Assignments_Import <- rename(OnePOS_Assignments_Import, Team = Value)
OnePOS_Assignments_Import <- select(OnePOS_Assignments_Import, -Field)
OnePOS_Assignments_Import <- rename(OnePOS_Assignments_Import, Start_Team = Start, End_Team = End)

# calendar table
calendar <- data_frame(date = seq.Date(min(OnePOS_Assignments_Import$Start) %>% date(),
                                       max(OnePOS_Assignments_Import$Start) %>% date(), by = "days"),
                       datetime = seq.POSIXt(as.POSIXct(paste(min(date(OnePOS_Assignments_Import$Start)), "08"), 
                                                        format = "%Y-%m-%d %H"), 
                         max(OnePOS_Assignments_Import$Start)+24*60*60, by = "DSTday"))

# active state history
inc_state_1 <- readxl::read_excel(path="\\\\cewp1650\\Chris Jabr Reports\\ONOW Exports\\inc_state_1.xlsx")
inc_state_2 <- readxl::read_excel(path="\\\\cewp1650\\Chris Jabr Reports\\ONOW Exports\\inc_state_2.xlsx")
inc_state_3 <- readxl::read_excel(path="\\\\cewp1650\\Chris Jabr Reports\\ONOW Exports\\inc_state_3.xlsx")
inc_state_history <- bind_rows(inc_state_1, inc_state_2, inc_state_3) %>% distinct()
inc_state_history$Start <- force_tz(inc_state_history$Start, "US/Central")
inc_state_history$End <- force_tz(inc_state_history$End, "US/Central")
inc_state_history <- rename(inc_state_history, State = Value, Start_State = Start, End_State = End)


## TABLE WITH OVOT
# get list of distinct incidents
ovot_incidents <- bind_rows(OnePOS_Assignments_Import %>% select(Number), inc_state_history %>% select(Number)) %>% distinct()

# loop each day, left join ticket list onto historical data, and insert results into result table
# NOTE: NULLS FOR ASSIGN. TEAM PROBABLY MEAN A NON-ONEPOS TEAM. ONLY ONEPOS ASSIGNMENTS WERE PULLED
ovot <- data_frame()
for (i in 1:nrow(calendar)){
  insert_day <- ovot_incidents %>% mutate(datetime = calendar$datetime[i]) %>% 
    left_join(inc_state_history, by = "Number") %>%
    filter(Start_State <= calendar$datetime[i] & (calendar$datetime[i] < End_State | is.na(End_State))) %>%
    left_join(OnePOS_Assignments_Import, by = "Number") %>%
    filter(Start_Team <= calendar$datetime[i] & (calendar$datetime[i] < End_Team | is.na(End_Team))) %>% 
    distinct()
  ovot <- bind_rows(ovot, insert_day)
}
# CHECK FOR DOUBLE ASSIGNMENTS ON DAYS. check w/o distinct
# double check timezone conversions ...prototype



write.csv(ovot, na = "", row.names = FALSE, "\\\\cewp1650\\Chris Jabr Reports\\ONOW Exports\\R Open Volume Over Time.csv")








# reference: timezones
OlsonNames()
force_tz(calendar$datetime[1], "US/Central")
with_tz(calendar$datetime[1], "US/Central")

# reference: aggregating values on a calendar
  # calendar: aggregate assignment total on date
  calendar_plus <- calendar
  calendar_plus$assigned_on_date <- NA
  for (i in 1:nrow(calendar_plus)){
    calendar_plus$assigned_on_date[i] <- OnePOS_Assignments_Import %>% 
      filter(Start_Team <= calendar_plus$datetime[i] & (calendar_plus$datetime[i] < End_Team | is.na(End_Team))) %>% nrow()
  }
  # calendar: aggregate active total on date
  calendar_plus$active_on_date <- NA
  for (i in 1:nrow(calendar_plus)){
    calendar_plus$active_on_date[i] <- inc_state_history %>% 
      filter(Start_State <= calendar_plus$datetime[i] & (calendar_plus$datetime[i] < End_State | is.na(End_State))) %>% nrow()
  }

# reference: bind dataframe rows & naming the source
a <- data_frame(num = c(1,2,3))
b <- data_frame(num = c(4,5))
bind_rows(b = b, a = a, .id = "source_dataframe")


  
  
## homeaway meetup #1 - ai and machine learning...
## dosh analysis
# cum amount vs days since first visit
# dosh trees. identifying healthy vs not-healthy trees.
# time delta.
# Q: how did you label the data? labeling the accounts as good vs bad? has transactions vs not?
# sensitivity vs specificity - true positive vs true negative
# Q: diff graphics engine ggplot?
# label data? labor intensive
# labeling almost manually necessary. algorithms do exist tho.
# all these grpahics with R ...network looking data. structure of this?
# austin deep learning ...austin big data 
# taylor - met at data science meetup.



# kaggle meetup #1: instacart machine learning. predicting future basket based on past purchases.
# github featureTools instacart-demo
# featuretools package in python. Similar for R?
# *** can you have a vector in a dataframe value...? [0, 1, 0]. you don't even need this - just have diff columns
# 

# reference: putting vectors/lists inside dataframes
test <- data_frame(id = c(1,2), name = c("bob", "mary"))
test
test$children <- list(c("bobette", "bobby"), c("marianne", "marietta"))
test
View(test)


# graph databases...
# stitch fix example ... multithreaded algorithm tour...collaborative filtering?
# iampatgrady on github
# numenta.org machine learning - high temporal memory neuron ... anomolous detection. htm model.
# google api text mining
# basically: use tools to create a bunch of calc columns for time series analysis ...
# python titanic notebook. guide thing. published notebook guide that shows the entire ML workflow...

# reference: string search inside of files
getwd()
dir()
readLines("Scratchpad.R")
readLines("Scratchpad.R") %>% str()
str_detect(readLines("Scratchpad.R"), "readLine")
readLines("Scratchpad.R") %>% attributes()
readLines("Scratchpad.R")[str_detect(readLines("Scratchpad.R"), "readLine")]


# reference: boolean logic operators ----
function() {print('hello'); TRUE }
FALSE && 5
FALSE & 5
5 | FALSE
5 || FALSE

# reference: subsetting df vs matrix
calendar[1:5] #error
calendar[1:5,]
str(calendar[1:5,])
library(tidyverse)
select(calendar, matches("date"))
select(calendar, matches("^date"))
matrix(c(1:12),3,4)
matrix(c(1:12),3,4)[1:5]