################################################
# Text mining - Extended Metrics CSV - TIDY TEXT
# word counts & trending up words for OnePOS, Aloha 
# 9/28/2017
# (REFACTORING EM TM - TRENDING ABOVE AVERAGE)
################################################

setwd("\\\\cewp1650\\Chris Jabr Reports\\Text Analysis")

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
       "wordcloud", "SnowballC"))



#### Global Variables ####

startweek.2017 <- as.Date("2017-01-02")
last.monday <- (Sys.Date() - 7) + ( 1 - as.integer(format(Sys.Date(), format = "%u")))
last.sunday <- (Sys.Date() - 7) + ( 7 - as.integer(format(Sys.Date(), format = "%u")))
this.monday <- Sys.Date() + (1 - as.integer(format(Sys.Date(), format = "%u")))
num.weeks <- as.integer((this.monday - startweek.2017)/7)
two.mondays.ago <- (Sys.Date() - 14) + ( 1 - as.integer(format(Sys.Date(), format = "%u")))



#### IMPORT & CLEAN DATA ####

em <- read.csv("\\\\cewp1650\\Chris Jabr Reports\\Extended Metrics 2017.csv")
colnames(em)[1] <- "Id"
em$Title <- as.character(em$Title)
em$Smart_Region <- as.factor(em$Smart_Region)
em$Created_Date <- as.Date(em$Created_Date, "%m/%d/%Y")
em$Created_Week_Ending <- as.Date(em$Created_Week_Ending, "%m/%d/%Y")



#### SUBSET - ALL 2017 - ONEPOS IRS ####

em.tidy <- em %>%
  filter(Created_Date >= startweek.2017 & Created_Date <= last.sunday & ONEPOS_LIST == 1) %>%
  select(Id, Created_Date, Created_Week, Created_Week_Ending, Smart_Region, Smart_Location, Title, LAST_SUPPORT_GROUP)


# unigram stopwords
word.blacklist <- data_frame(word = c("r10", "lane", "bus", "date", "ncr", "eod", "run", "lanes", "reg","aloha"))
stopwords.unigram <- stop_words %>% filter(lexicon == "SMART") %>% select(word) %>% bind_rows(word.blacklist)

# bigram stopwords - currently excluding some common terms ...
word.blacklist.bigram <- data_frame(word = c("r10", "bus", "date"))
stopwords.bigram <- stop_words %>% filter(lexicon == "snowball", !word %in% c("down","above","below","again", "after")) %>%
  bind_rows(word.blacklist.bigram)


# tidy text - distinct UNIGRAMS per IR w/ calcs, maybe stem/remove stopwords
em.tidy.unigrams <- em.tidy %>%
  unnest_tokens(word, Title, drop = FALSE) %>%
  filter(!word %in% stopwords.unigram$word) %>% ### ! stem here
  distinct() %>%
  group_by(Created_Week_Ending, word) %>%
    mutate(word.week.total = n()) %>%
  group_by(word) %>%
    mutate(word.population.total = n()) %>%
    arrange(Created_Week_Ending) %>%
  ungroup() %>%
    mutate(word.week.avg = word.population.total/num.weeks, 
           word.week.trend = word.week.total / word.week.avg)

# unigram WoW calc - summary of term by week, lag; join back with week,word
unigram.wow <- em.tidy.unigrams %>%
  group_by(Created_Week_Ending) %>%
  count(word) %>%
  group_by(word) %>%
  mutate(word.wow = (n - lag(n, order_by = Created_Week_Ending)) / lag(n, order_by = Created_Week_Ending)) 

em.tidy.unigrams.w.wow <- em.tidy.unigrams %>%
  inner_join(unigram.wow, by = c("Created_Week_Ending", "word")) %>% select(-n)


# distinct BIGRAMS per IR w/ calcs - leaving in stopwords - stemming not working well
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



#### SUBSET - ALL 2017 - ALOHA ####

## top down keyword search & export
aloha.issues <- c("kiosk", "terminal", "printer", "oops", "down", "network", "spool", "tax", "calculat",
                  "sending|communicating", "menu")
em.aloha <- em %>%
  filter(Created_Date >= startweek.2017 & Created_Date <= last.sunday & ONEPOS_LIST == 1) %>%
  filter(LAST_SUPPORT_GROUP %in% c("Aloha Data Configuration", "Aloha Support Team", "Aloha Hardware Support")) %>%
  mutate(Aloha_Ticket_Type = toupper(str_extract(.$Title, paste0("(?i)(", aloha.issues, ")", collapse = "|")))) %>%
  mutate(Aloha_Ticket_Type = ifelse(.$Aloha_Ticket_Type == "SENDING" | .$Aloha_Ticket_Type == "COMMUNICATING", 
                                    "SENDING/COMMUNICATING", .$Aloha_Ticket_Type))
                                       
write.csv(em.aloha,
          file = paste("Aloha issues 2017 - ", Sys.Date(), ".csv", sep = ""),
          row.names = FALSE,
          na = "")


## aloha last 2 weeks word frequencies
aloha.last.2.weeks <- em.aloha %>%
  select(Id, Created_Date, Created_Week_Ending, LAST_SUPPORT_GROUP, Smart_Region, Smart_Location, Title) %>%
  filter(Created_Date >= two.mondays.ago & Created_Date <= last.sunday) %>%
  unnest_tokens(word, Title, drop = FALSE) %>%
  filter(!word %in% stopwords.unigram$word) %>% ### ! stem here
  mutate(word = wordStem(word)) %>% #### !!!! stemming
  distinct() %>%
  group_by(Created_Week_Ending, word) %>%
  mutate(word.week.total = n()) %>%
  ungroup()

write.csv(aloha.last.2.weeks,
          file = paste("Aloha words L2W - ", Sys.Date(), ".csv", sep = ""),
          row.names = FALSE,
          na = "")

# analysis - top 10 words by count each week
aloha.top.10 <- aloha.last.2.weeks %>%
  group_by(Created_Week_Ending) %>%
  count(word, sort = TRUE) %>%
  top_n(10, wt = n) %>%
  ungroup() %>%
  mutate(wordorder = nrow(.):1) %>%
  group_by(Created_Week_Ending, word) %>%  # begin ridiculous workaround for ordering words in facets correctly
  arrange(desc(n)) %>%
  ungroup() %>%
  mutate(ord.term = paste(Created_Week_Ending,"__",word, sep = "")) %>%
  group_by(word) %>%
  mutate(word.frequency = n()) ## for charting border thickness stuff ...

# plot - top 10 words by count each week
aloha.top.10.p <- aloha.top.10 %>%
  ggplot(aes(reorder(ord.term, wordorder), n, fill = word, label = n)) +
  geom_bar(stat = "identity", color = "black") +
  facet_wrap(~Created_Week_Ending, scales = "free_y") + # scales arg necessary for diff words
  labs(x = "Word", y = "Frequency", title = "Most Common Words by Week") +
  coord_flip() +
  theme(legend.position = "none") +
  scale_x_discrete(labels = function(x) gsub("^.+__", "", x)) +
  geom_label()
aloha.top.10.p

# save plot
ggsave(paste0("Aloha Most Common Words L2W - ", Sys.Date(), ".png"), width = 13, height = 6, units = ("in"),
       plot = aloha.top.10.p)




test <- aloha.top.10 %>%
  arrange(Created_Week_Ending, n)
test
# redo plot - col...
aloha.top.10 %>%
  ggplot(aes(reorder(ord.term, rev(wordorder)), n, fill = word, label = n)) +
  geom_bar(stat = "identity", color = "black") +
  facet_wrap(~Created_Week_Ending, scales = "free_x") + # scales arg necessary for diff words
  labs(x = "Word", y = "Frequency", title = "Most Common Words by Week") +
  # coord_flip() +
  theme(legend.position = "none") +
  scale_x_discrete(labels = function(x) gsub("^.+__", "", x)) +
  geom_label()
aloha.top.10.p




#### ANALYSIS & VISUALIZATION ####

options(scipen = 999)
x <- 10 # words
y <- 10 # weeks
last.y.mondays <- (Sys.Date() - 7*y) + ( 1 - as.integer(format(Sys.Date(), format = "%u")))


# Top X unigrams - last Y weeks
top.x.unigrams <- em.tidy.unigrams %>%
  group_by(Created_Week_Ending) %>%
    count(word, sort = TRUE) %>%
    top_n(20, wt = n) %>%
  ungroup() %>%
  mutate(wordorder = nrow(.):1) %>%
  group_by(Created_Week_Ending, word) %>%
    arrange(desc(n)) %>%
  ungroup() %>%
  mutate(facet.words = paste0(Created_Week_Ending, "__", word)) %>% #suffix word names for facet ordering
  group_by(word) %>%
    mutate(word.frequency = n())
# Plot
top.x.unigrams.plot <- top.x.unigrams %>%
  ggplot(aes(reorder(facet.words, wordorder), n, fill = word, label = n)) +
  geom_bar(stat = "identity", color = "black") +
  facet_wrap(~Created_Week_Ending, scales = "free_y") +
  labs(x = "Word", y = "Frequency", title = "Most Common Words - last X weeks") +
  coord_flip() +
  theme(legend.position = "none") +
  scale_x_discrete(labels = function(x) gsub("^.+__", "", x)) +
  geom_label()
top.x.unigrams.plot

## dataset - summary view ##
top.n.words.dataset <- em.tidy.words.all %>%
  group_by(Created_Week_Ending) %>%
  count(term, sort = TRUE) %>%
  top_n(20, wt = n) %>%
  ungroup() %>%
  mutate(wordorder = nrow(.):1) %>%
  group_by(Created_Week_Ending, term) %>%  # begin ridiculous workaround for ordering words in facets correctly
  arrange(desc(n)) %>%
  ungroup() %>%
  mutate(ord.term = paste(Created_Week_Ending,"__",term, sep = "")) %>%
  group_by(term) %>%
  mutate(word.frequency = n())

# plot - top 10 words summary view
top.n.words.dataset.p <- top.n.words.dataset %>%
  ggplot(aes(reorder(ord.term, wordorder), n, fill = term, label = n)) +
  geom_bar(stat = "identity", color = "black") +
  # facet_wrap(~Created_Week_Ending, scales = "free_y") + # scales arg necessary for diff words
  labs(x = "Word", y = "Frequency", title = "Most Common Words - 9/27") +
  coord_flip() +
  theme(legend.position = "none") +
  scale_x_discrete(labels = function(x) gsub("^.+__", "", x)) +
  geom_label()

# save plot
top.n.words.dataset.p
write.csv(em,
          file = paste("IRs - 9-27", Sys.Date(), ".csv", sep = ""),
          row.names = FALSE)



## subset dataset - last 2 weeks ##

em.last.2.weeks <- em.tidy.words.all %>%
  filter(Created_Date >= two.mondays.ago & Created_Date <= last.sunday) %>%
  # filter(word.week.total >= 5) %>%   ## filter out uncommon words?
  arrange(desc(word.trend)) %>%
  mutate(Created_Week_Ending = Created_Date + ( 7 - as.integer(format(Created_Date, format = "%u"))))

# analysis - top 10 words by count each week
top.10.words.weekly.count <- em.last.2.weeks %>%
  group_by(Created_Week_Ending) %>%
  count(term, sort = TRUE) %>%
  top_n(10, wt = n) %>%
  ungroup() %>%
  mutate(wordorder = nrow(.):1) %>%
  group_by(Created_Week_Ending, term) %>%  # begin ridiculous workaround for ordering words in facets correctly
  arrange(desc(n)) %>%
  ungroup() %>%
  mutate(ord.term = paste(Created_Week_Ending,"__",term, sep = "")) %>%
  group_by(term) %>%
  mutate(word.frequency = n())

# plot - top 10 words by count each week
top.10.words.weekly.count.p <- top.10.words.weekly.count %>%
  ggplot(aes(reorder(ord.term, wordorder), n, fill = term, label = n)) +
  geom_bar(stat = "identity", color = "black") +
  facet_wrap(~Created_Week_Ending, scales = "free_y") + # scales arg necessary for diff words
  labs(x = "Word", y = "Frequency", title = "Most Common Words by Week") +
  coord_flip() +
  theme(legend.position = "none") +
  scale_x_discrete(labels = function(x) gsub("^.+__", "", x)) +
  geom_label()

# save plot
top.10.words.weekly.count.p
ggsave(paste0("plot - Most Common Words - ", Sys.Date(), ".png"), width = 13, height = 6, units = ("in"))


### need to define this w/ Melissa & others ###
# analysis - top 10 words by trending above average 
top.10.words.trending.aa <- em.last.2.weeks %>%
  group_by(Created_Week_Ending, term) %>%
  summarize(word.trend = mean(word.trend), word.weekly.avg = mean(word.weekly.avg),
            word.week.total = mean(word.week.total), word.population.total = mean(word.population.total),
            word.population.total = mean(word.population.total), num.weeks = mean(num.weeks)) %>%
  arrange(desc(word.trend)) %>%
  top_n(10, wt = word.trend) %>%
  ungroup() %>%
  mutate(wordorder = nrow(.):1) %>%
  mutate(ord.term = paste(Created_Week_Ending,"__",term, sep = "")) %>%
  group_by(term) %>%
  mutate(word.frequency = n())

# plot - top 10 words by trending above average
top.10.words.trending.aa.p <- top.10.words.trending.aa %>% 
  ggplot(aes(reorder(ord.term, wordorder), word.trend, fill = term, label = round(word.trend,0)))+
  geom_bar(stat = "identity", color = "black") +
  facet_wrap(~Created_Week_Ending, scales = "free_y") + # scales arg necessary to hav diff words 
  labs(x = "Word", y = "Spike in Frequency", title = "Words Spiking in Frequency") +
  coord_flip() +
  theme(legend.position = "none") +
  scale_x_discrete(labels = function(x) gsub("^.+__", "", x)) +
  geom_label()
top.10.words.trending.aa.p

# word cloud ^ ---- should make this for a month
wordcloud.data <- top.10.words.trending.aa %>% 
  filter(Created_Week_Ending == max(top.10.words.trending.aa$Created_Week_Ending))
wordcloud(wordcloud.data$term, wordcloud.data$word.week.total)
wordcloud(top.10.words.trending.aa$term, top.10.words.trending.aa$word.week.total)  





## Aloha Analysis Dataset ##
em.aloha.last.2.weeks <- em.tidy.words.all %>%
  filter(Created_Date >= two.mondays.ago & Created_Date <= last.sunday) %>%
  arrange(desc(word.trend)) %>%
  mutate(Created_Week_Ending = Created_Date + ( 7 - as.integer(format(Created_Date, format = "%u"))))

# analysis - top 10 words by count each week
top.10.words.weekly.count <- em.last.2.weeks %>%
  group_by(Created_Week_Ending) %>%
  count(term, sort = TRUE) %>%
  top_n(10, wt = n) %>%
  ungroup() %>%
  mutate(wordorder = nrow(.):1) %>%
  group_by(Created_Week_Ending, term) %>%  # begin ridiculous workaround for ordering words in facets correctly
  arrange(desc(n)) %>%
  ungroup() %>%
  mutate(ord.term = paste(Created_Week_Ending,"__",term, sep = "")) %>%
  group_by(term) %>%
  mutate(word.frequency = n())   




# setwd("\\\\cewp1650\\Chris Jabr Reports\\Text Analysis")
# write.csv(em.last.2.weeks, 
#           file = paste("last 2 weeks - ", Sys.Date(), ".csv", sep = ""), 
#           row.names = FALSE)



#tidytext equivalent...
# consolidate stems
# mutate(word = if_else(word %in% c("emailing", "emails"), "email", word))
# test str replace ..
# handle *MSR from MSR ... ... regex gsub keeping certain punct.....

# plot - case when word is repeated, BOLD or something.
# outline color thicker based on freq of word...size, scale didn't work, ggplot aes vs geom aes...
