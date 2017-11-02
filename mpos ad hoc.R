################################################
# Ad hoc text mining for mpos incidents
# 11/2/2017
################################################

library(tidyverse)
library(stringr)
library(tidytext)

default.dir <- getwd(); 
sub.dir <- paste0("Text Analysis ", Sys.Date())
new.dir <- file.path(default.dir, sub.dir)

# make new folder
if (!dir.exists(new.dir)) dir.create(new.dir) else print("it exists. no folder made"); setwd(new.dir);

em <- read.csv("\\\\cewp1650\\Chris Jabr Reports\\_Misc\\Extended Metrics with descriptions.csv", encoding = "UTF-8")
colnames(em)[1] <- "Id"

# clean text
em$Title <- as.character(em$Title)
em$Description <- as.character(em$Description)
em$Resolution_Description <- as.character(em$Resolution_Description)

# clean dates
em$Created_Date <- as.Date(em$Created_Date, "%m/%d/%Y")
em$Created_Week_Ending <- as.Date(em$Created_Week_Ending, "%m/%d/%Y")
em$Resolved_Date <- as.Date(em$Resolved_Date, "%m/%d/%Y")
em$Resolved_Week_Ending <- as.Date(em$Resolved_Week_Ending, "%m/%d/%Y")
em$Closed_Date <- as.Date(em$closed_date_cst, "%m/%d/%Y")  ################################ watch out for column rename
em$Closed_Week_Ending <- as.Date(em$Closed_Week_Ending, "%m/%d/%Y")

em$closed_date_cst <- NULL


# subset
mpos.l2m <- em %>% filter(Created_Date >= as.Date("2017-09-04")) %>%
  filter(str_detect(.$Title, "(?i)mpos"))


# unigrams & plot
mpos.l2m.unigrams <- mpos.l2m %>% unnest_tokens(word, Title, drop = FALSE) %>% 
  filter(!word %in% stopwords.unigram$word) %>% filter(!word %in% c("mpos", "tab", "631")) %>%
  group_by(Created_Week_Ending) %>% count(word, sort = TRUE) %>% top_n(10, wt = n) %>% mutate(week.top.10 = row_number()) %>%
  filter(week.top.10 < 11) %>% ungroup() %>% mutate(wordorder = row_number()) %>%
  group_by(Created_Week_Ending, word) %>% arrange(desc(n)) %>%
  ungroup() %>% mutate(ord.term = paste0(Created_Week_Ending, "__", word))

  ggplot(mpos.l2m.unigrams, aes(x = reorder(ord.term, wordorder), y = n, label = n, fill = word)) + 
    geom_bar(stat = "identity") + 
    facet_wrap(~paste("Week Ending", format(Created_Week_Ending, "%m-%d-%Y")), scales = "free_x") +
    labs(x = "word", y = "Number of IRs containing word", title = "trending words in mpos tickets by IR Creation Week") +
    scale_x_discrete(labels = function(x) gsub("^.+__", "", x)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") 
  # ggsave(paste0("trending words in mpos tickets - ", Sys.Date(), ".bmp"), width = my.w, height = my.h, units = ("in"))
  
  