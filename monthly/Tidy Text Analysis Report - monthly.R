################################################
# Text mining - Extended Metrics CSV - TIDY TEXT - *** MONTHLY VERSION ***
# word counts & trending up words for OnePOS, Aloha 
# 9/28/2017
# (REFACTORING EM TM - TRENDING ABOVE AVERAGE)
################################################

setwd("C:\\Work\\Git\\Repos\\R-TextMine\\monthly")
subdir <- paste0("Monthly Text Analysis - ", Sys.Date())
dir.create(subdir)
setwd(file.path(getwd(), subdir))

#### Packages & Version Control ####

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# ipak("checkpoint")
# checkpoint("2017-08-24")
ipak(c("tidyverse", "ggplot2", "tm", "sqldf", "scales","chron", "tidytext", "tidyr","dplyr","stringr", "plotly",
       "wordcloud", "SnowballC", "RColorBrewer"))



#### Global Variables ####

startweek.2017 <- as.Date("2017-01-02")
last.monday <- (Sys.Date() - 7) + ( 1 - as.integer(format(Sys.Date(), format = "%u")))
last.sunday <- (Sys.Date() - 7) + ( 7 - as.integer(format(Sys.Date(), format = "%u")))
this.monday <- Sys.Date() + (1 - as.integer(format(Sys.Date(), format = "%u")))
num.weeks <- as.integer((this.monday - startweek.2017)/7)
two.mondays.ago <- (Sys.Date() - 14) + ( 1 - as.integer(format(Sys.Date(), format = "%u")))
four.mondays.ago <- (Sys.Date() - (7*4)) + ( 1 - as.integer(format(Sys.Date(), format = "%u")))

my.w <- 13
my.h <- 6

#### IMPORT & CLEAN DATA ####

em <- read.csv("\\\\cewp1650\\Chris Jabr Reports\\Extended Metrics 2017.csv", encoding = "UTF-8")
colnames(em)[1] <- "Id"
em$Title <- as.character(em$Title)
em$Smart_Region <- as.factor(em$Smart_Region)
em$Created_Date <- as.Date(em$Created_Date, "%m/%d/%Y")
em$Created_Week_Ending <- as.Date(em$Created_Week_Ending, "%m/%d/%Y")

options(scipen = 999)

#### PRE-PROCESSING: SUBSET 2017/ONEPOS, STOPWORD LISTS, UNIGRAM/BIGRAM WEEKLY CALCS ####

em.tidy <- em %>%
  filter(Created_Date >= startweek.2017 & Created_Date <= last.sunday & ONEPOS_LIST == 1) %>%
  select(Id, Created_Date, Created_Week, Created_Week_Ending, Smart_Region, Smart_Location, Title, LAST_SUPPORT_GROUP, Incident_Type)

# General Stopwords
general.stopwords <- data_frame(word = as.character(seq(0,10)))
regions <- data_frame(word = tolower(c("CE","SP","NE","NC","FL","PN","NA","SW","TS","MA","SO","RM","MW","UK")))

# unigram stopwords
word.blacklist <- data_frame(word = c("r10", "lane", "bus", "date", "ncr", "eod", "run", "lanes", "reg","aloha"))
stopwords.unigram <- stop_words %>% filter(lexicon == "SMART") %>% select(word) %>% bind_rows(word.blacklist, general.stopwords, regions)

# bigram stopwords - currently excluding some common terms ...
word.blacklist.bigram <- data_frame(word = c("r10", "bus", "date"))
stopwords.bigram <- stop_words %>% filter(lexicon == "snowball", !word %in% c("down","above","below","again", "after")) %>%
  bind_rows(word.blacklist.bigram)

# Custom Stemming List
synonyms.freeze <- c("freeze", "freezes", "freezing", "freezed", "froze", "frozen", "frozed")
synonyms.kiosk <- c("kiosk", "kiosks")
synonyms.validation <- c("validation","validations")

### tidy text - distinct UNIGRAMS per IR w/ calcs, maybe stem/remove stopwords
em.tidy.unigrams <- em.tidy %>%
  unnest_tokens(word, Title, drop = FALSE) %>%
  filter(!word %in% stopwords.unigram$word) %>%
  mutate(word = case_when(.$word %in% synonyms.freeze ~ "(FREEZE)", TRUE ~ .$word)) %>%
  # mutate(word = wordStem(word)) %>%
  distinct() %>% ### distinct removes multiples of same word from the same IR
  group_by(Created_Week_Ending, word) %>%
  mutate(word.week.total = n()) %>%
  group_by(word) %>%
  mutate(word.population.total = n()) %>%
  arrange(Created_Week_Ending) %>%
  ungroup() %>%
  mutate(word.week.avg = word.population.total/num.weeks, 
         word.week.trend = word.week.total / word.week.avg,
         word = toupper(word))

# unigram WoW calc - summary of term by week, lag; join back with week,word
unigram.wow <- em.tidy.unigrams %>%
  group_by(Created_Week_Ending) %>%
  count(word) %>%
  group_by(word) %>%
  mutate(word.wow = (n - lag(n, order_by = Created_Week_Ending)) / lag(n, order_by = Created_Week_Ending)) 

em.tidy.unigrams <- em.tidy.unigrams %>%
  inner_join(unigram.wow, by = c("Created_Week_Ending", "word")) %>% select(-n)



### distinct BIGRAMS per IR w/ calcs - leaving in stopwords - stemming not working well
em.tidy.bigrams <- em.tidy %>%
  unnest_tokens(bigram, Title, token = "ngrams", n = 2, drop = FALSE) %>%
  distinct() %>%
  separate(bigram, c("word1", "word2"), sep = " ", remove = TRUE) %>% ### ! stem here
  filter(!word1 %in% stopwords.bigram$word, !word2 %in% stopwords.bigram$word) %>% ### filter words
  unite(bigram, c(word1, word2), sep = " ") %>%
  group_by(Created_Week_Ending, bigram) %>%
  mutate(bigram.week.total = n()) %>%
  group_by(bigram) %>%
  mutate(bigram.population.total = n()) %>%
  ungroup() %>%
  mutate(bigram.week.avg = bigram.population.total/num.weeks, 
         bigram.week.trend = bigram.week.total / bigram.week.avg)




#### ALOHA: TOP X TICKET TYPES/ISSUES - ANALYSIS & VISUALIZATION ####

### top down keyword search & export ###
aloha.issues <- c("kiosk", "terminal", "printer", "oops", "down", "network", "spool", "tax", "calculat",
                  "sending|communicating", "menu")

# aloha w/ top 10 ticket types calc - L2W
em.aloha <- em %>%
  filter(Created_Date >= startweek.2017 & Created_Date <= last.sunday & ONEPOS_LIST == 1) %>%
  filter(LAST_SUPPORT_GROUP %in% c("Aloha Data Configuration", "Aloha Support Team", "Aloha Hardware Support")) %>%
  mutate(Aloha_Ticket_Type = toupper(str_extract(.$Title, paste0("(?i)(", aloha.issues, ")", collapse = "|")))) %>%
  mutate(Aloha_Ticket_Type = ifelse(is.na(Aloha_Ticket_Type), "(blank)", Aloha_Ticket_Type)) %>%
  mutate(Aloha_Ticket_Type = as.factor(case_when(is.na(.$Aloha_Ticket_Type) ~ "(blank)",
                                                 .$Aloha_Ticket_Type == "SENDING" | .$Aloha_Ticket_Type == "COMMUNICATING" ~ "SENDING/COMMUNICATING",
                                                 TRUE ~ .$Aloha_Ticket_Type)))
# top 10 top-down issues
aloha.top.down <- em.aloha %>%
  filter(Created_Date >= four.mondays.ago & Created_Date <= last.sunday) %>%
  count(Aloha_Ticket_Type, sort = TRUE) %>%
  top_n(10, wt = n) %>%
  mutate(week.top.10 = row_number()) %>%
  filter(week.top.10 <= 10) 

# plot
aloha.top.down.p <- aloha.top.down %>%
  ggplot(aes(reorder(Aloha_Ticket_Type, week.top.10), n, fill = Aloha_Ticket_Type, label = n)) +
  geom_bar(stat = "identity", color = "black") +
  theme_bw() +
  labs(x = "Aloha Incident Types", y = "Frequency", title = "Aloha Ticket Types by Last 4 Weeks") +
  geom_label() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

aloha.top.down.p
ggsave(paste0("Aloha Ticket Types by Last 4 Weeks - ", Sys.Date(), ".png"), width = my.w, height = my.h, units = ("in"))

# count how many incidents this captured...
test <- em.aloha %>% filter(Created_Date >= four.mondays.ago & Created_Date <= last.sunday)
unique(test$Id %>% length())

# write.csv(em.aloha,
#           file = paste("Aloha issues last 2 weeks - ", Sys.Date(), ".csv", sep = ""),
#           row.names = FALSE,
#           na = "")



#### ALOHA: TOP X UNIGRAMS - ANALYSIS & VISUALIZATION ####

### tidy text: aloha last 2 weeks - word frequencies ###
aloha.unigrams.last.2.weeks <- em.aloha %>%
  select(Id, Created_Date, Created_Week_Ending, LAST_SUPPORT_GROUP, Smart_Region, Smart_Location, Title) %>%
  filter(Created_Date >= four.mondays.ago & Created_Date <= last.sunday) %>%
  unnest_tokens(word, Title, drop = FALSE) %>%
  filter(!word %in% stopwords.unigram$word) %>% ### stopwords
  mutate(word = case_when(.$word %in% synonyms.kiosk ~ "(KIOSK)", TRUE ~ .$word)) %>%
  # mutate(word = wordStem(word)) %>% 
  distinct() %>%
  group_by(Created_Week_Ending, word) %>%
  mutate(word.week.total = n()) %>%
  ungroup() %>%
  mutate(word = toupper(word))

# calc
aloha.top.10 <- aloha.unigrams.last.2.weeks %>%
  count(word, sort = TRUE) %>%
  top_n(10, wt = n) %>%
  mutate(week.top.10 = row_number()) %>%
  filter(week.top.10 <= 10) 

# plot
aloha.top.10.p <- aloha.top.10 %>%
  ggplot(aes(reorder(word, week.top.10), n, fill = word, label = n)) +
  theme_bw() +
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Word", y = "Frequency", title = "Aloha Most Common Words by Last 4 Weeks") +
  geom_label() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

aloha.top.10.p
ggsave(paste0("Aloha Most Common Words by Last 4 Weeks ", Sys.Date(), ".png"), width = my.w, height = my.h, units = ("in"))

# IR list of the top terms...NOT PERFECT - PULLING ALL TICKETS WITH ANY MATCHING TOP 10 WORDS
ir.list.aloha.top.10.words.l2w <- em.aloha %>%
  filter(Created_Date >= four.mondays.ago & Created_Date <= last.sunday) %>%
  filter(grepl(paste0("(?i)(",unique(aloha.top.10$word),")", collapse = "|"), .$Title))





#### ONEPOS: TOP X TICKET TYPES/ISSUES - ANALYSIS & VISUALIZATION ####

### Top X Incident_Types - L2W; filter out blanks
top.x.ticket.types.l2w <- em.tidy %>%
  filter(Created_Date >= four.mondays.ago & Created_Date <= last.sunday) %>%
  count(Incident_Type, sort = TRUE) %>%
  mutate(week.ir.total = sum(n)) %>%
  filter(!(Incident_Type == "")) %>%
  top_n(10, wt = n) %>%
  mutate(week.top.10 = row_number()) %>%
  filter(week.top.10 <= 10) %>%
  ungroup() 

# Plot
top.x.ticket.types.l2w.plot <- top.x.ticket.types.l2w %>%
  ggplot(aes(reorder(Incident_Type, week.top.10), n, fill = Incident_Type, label = n)) +
  theme_bw() + 
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Incident Type", y = "# of IRs", title = "Top 10 Incident Types Last 4 Weeks") +
  theme(legend.position = "none") +
  geom_label() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

top.x.ticket.types.l2w.plot
ggsave(paste0("Top 10 Incident Types Last 4 Weeks ", Sys.Date(), ".png"), width = my.w, height = my.h, units = ("in"))


#### ONEPOS: TOP X UNIGRAMS - ANALYSIS & VISUALIZATION ####

### Top X unigrams - last Y weeks
top.x.unigrams.l2w <- em.tidy.unigrams %>%
  filter(Created_Date >= four.mondays.ago & Created_Date <= last.sunday) %>%
  # group_by(Created_Week_Ending) %>%
  count(word, sort = TRUE) %>%
  top_n(10, wt = n) %>%
  mutate(week.top.10 = row_number()) %>%
  filter(week.top.10 <= 10) %>%
  ungroup() 
# %>%
#   mutate(wordorder = nrow(.):1) %>%
#   mutate(facet.words = paste0(Created_Week_Ending, "__", word)) %>% #suffix word names for facet ordering
#   group_by(word) %>%
#   mutate(word.top10.freq = n())

# Plot
top.x.unigrams.l2w.plot <- top.x.unigrams.l2w %>%
  ggplot(aes(reorder(word, week.top.10), n, fill = word, label = n)) +
  theme_bw() + 
  geom_bar(stat = "identity", color = "black") +
  # facet_wrap(~paste("Week Ending", format(Created_Week_Ending, "%m-%d-%Y")), scales = "free_x") +
  labs(x = "Word", y = "Frequency", title = "Most Common Words Last 4 Weeks") +
  theme(legend.position = "none") +
  # scale_x_discrete(labels = function(x) gsub("^.+__", "", x)) +
  geom_label() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") #+ labs(caption = "test text")

top.x.unigrams.l2w.plot
ggsave(paste0("Most Common Words Last 4 Weeks ", Sys.Date(), ".png"), width = my.w, height = my.h, units = ("in"))



### Top X unigrams - YESTERDAY/TODAY
mydate <- Sys.Date()-1

top.x.unigrams.day <- em %>%
  filter(Created_Date >= mydate & Created_Date <= Sys.Date() & ONEPOS_LIST == 1) %>%
  unnest_tokens(word, Title, drop = FALSE) %>%
  filter(!word %in% stopwords.unigram$word) %>% ### ! stem here
  mutate(word = toupper(wordStem(word))) %>%
  distinct() %>% ### distinct removes multiples of same word from the same IR
  count(word, sort = TRUE) %>%
  top_n(20, wt = n) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 10)

# Plot
top.x.unigrams.day.p <- top.x.unigrams.day %>%
  ggplot(aes(reorder(word, rev(rank)), n, fill = word, label = n)) +
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Word", y = "Frequency", title = paste0("Most Common Words - ", format(mydate, format = "%m-%d"))) +
  coord_flip() +
  theme(legend.position = "none") +
  scale_x_discrete(labels = function(x) gsub("^.+__", "", x)) +
  geom_label()

top.x.unigrams.day.p  ## weirdly concats MPOS to MPO, only here


#### ONEPOS: TRENDING UP WORDS - ANALYSIS & VISUALIZATION ####

### BUBBLING UP WORDS - by week total vs week avg of entire dataset (& reconsolidate aggregates for plotting)
top.x.unigrams.bubbling.up.l2w <- em.tidy.unigrams %>%
  filter(Created_Date >= four.mondays.ago & Created_Date <= last.sunday) %>%
  group_by(Created_Week_Ending, word) %>%
  summarize(word.week.trend = mean(word.week.trend), word.week.avg = mean(word.week.avg),
            word.week.total = mean(word.week.total), word.population.total = mean(word.population.total), 
            num.weeks = mean(num.weeks)) %>%
  filter(word.week.total > 4) %>%
  arrange(Created_Week_Ending, desc(word.week.trend), desc(word.week.total)) %>%
  top_n(10, wt = word.week.trend) %>%
  mutate(week.top.10 = row_number()) %>%
  filter(week.top.10 <= 10) %>%
  ungroup() %>%
  mutate(wordorder = 1:nrow(.)) %>%
  mutate(ord.term = paste(Created_Week_Ending,"__", word, sep = ""))

# Plot
top.x.unigrams.bubbling.up.l2w.p <- top.x.unigrams.bubbling.up.l2w %>%
  ggplot(aes(x = reorder(ord.term, wordorder), y = word.week.total, color = word, label = paste0(round(word.week.total,0),"x"))) +
  geom_point(aes(alpha = word.week.trend, color = word), size = 11) +
  # geom_text(aes(label=paste0(round(word.week.trend, 0),"x")),color = "black", size = 3) +
  facet_wrap(~Created_Week_Ending, scales = "free_x") +
  labs(x = "Word", y = "Frequency", title = "Words Trending Up - last 2 weeks") +
  # theme(legend.position = "none") +
  scale_x_discrete(labels = function(x) gsub("^.+__", "", x)) +
  geom_label() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_size(guide = "none")
top.x.unigrams.bubbling.up.l2w.p


## maybe a word cloud better for bubbling up words ....

# word cloud ^ ---- should make this for a month
wordcloud(top.x.unigrams.bubbling.up.l2w$word, top.x.unigrams.bubbling.up.l2w$word.week.trend, c(3,.2),
          random.order = FALSE)

#### need to make color gradient



