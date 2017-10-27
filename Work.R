################################################
# Import Extended Metrics
# 7/13/2017 - Plot Weekly or Monthly Totals
################################################

#### LOG ####
  function(further_analysis_tasks){
    # Text mining
      # em tm - lightweight - change to last 2 weeks, plot words, color code words to see diff
      # IRs trending higher than their typical average
    
    # Stores IRs Lanes
      # scatterplot all stores, by IRs created / Lanes / IRs Lane. Shiny/markdown/plotly
  }
  function(TM_PERSONALNOTES){
  # smarter import 
  # choose file
  # smart 1st column -> strip weird characters (substr(ï..))
  # choose what columns you want?
  # maybe convert all column names to lowercase
  # more consistent factoring...
  
  # smarter processing
  # gsub on corpus - not before
  # separate scripts -> source scripts for packages, import, processing (blacklist etc), etc
  # consolidate similar words!!!
  
  # choose export name + location
  
}
#### DATA - read em from sharefile, clean, transform, calculate columns ####
file_loc <- "C:\\Users\\2066074\\Documents\\Extended Metrics 2017.csv"
x <- read.csv(file_loc, header = TRUE)

# clean/transform
colnames(x)[1] <- "id"
x$Title <- as.character(x$Title)
x$Created_Date <- as.Date(x$Created_Date, "%m/%d/%Y")
x$Resolved_Date <- as.Date(x$Resolved_Date, "%m/%d/%Y")

# Created/Resolved Week / Month "variables" 
x$Created_Week_R <- as.Date(cut(x$Created_Date, breaks = "week"))
x$Created_Month_R <- as.Date(cut(x$Created_Date, breaks = "month"))
x$Resolved_Week_R <- as.Date(cut(x$Resolved_Date, breaks = "week"))
x$Resolved_Month_R <- as.Date(cut(x$Resolved_Date, breaks = "month"))

# month figure for coloring
x$created_month <- month


# slice - just OnePOS Incidents
x <- sqldf("select * from [x] where ONEPOS_LIST = 1")



#### Package Function ####
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
# ipak("checkpoint")
# checkpoint("2017-08-24")
ipak(c("ggplot2", "tm", "sqldf", "scales","chron", "tidytext", "tidyr","dplyr","plotly","tidyverse"))



#### 10/25/2017 - example ir type window function aggregates - for annotations ####


  # ir type - window function aggregates
test <- em.tidy %>%
  filter(Created_Date >= two.mondays.ago & Created_Date <= last.sunday) %>%
  group_by(Created_Week_Ending) %>%
  count(Incident_Type, sort = TRUE) %>% 
  mutate(week.ir.total = sum(n)) %>%  ## week total
  filter(!(Incident_Type == "")) %>%
  mutate(week.total.identified.1 = sum(n)) %>%  ## week total identified
  top_n(10, wt = n) %>%
  mutate(week.top10total.withties = sum(n)) %>% 
  mutate(week.top.10 = row_number()) %>% 
  filter(week.top.10 <= 10) %>%
  mutate(week.top.n.total = sum(n)) %>% # true top 10 count - after filtering to top 10 rows for week
  ungroup() %>%
  mutate(wordorder = nrow(.):1) %>%
  mutate(facet.words = paste0(Created_Week_Ending, "__", Incident_Type)) #suffix word names for facet ordering

  for (i in 1:5){
    print("test")
  }

#### 10/24/2017 - automate power point creation ####

install.packages("officer")
library(officer)
library(tidyverse)
library(stringr)

orig_pres <- read_pptx("C:\\Work\\Analysis\\OnePOS\\New Weekly Support Deck - 2017\\10_25_2017 chris refresh.pptx")

layout_summary(orig_pres) 

pp.theme <- layout_summary(orig_pres)$master %>% max()

my_pres <- orig_pres %>%
  add_slide(layout = "Title and Content", master = pp.theme)

my_pres <- my_pres %>% 
  ph_with_text(type = "title", str = "A title") %>%
  ph_with_text(type = "sldNum", str = "slide 1") %>%
  ph_with_text(str = "Hello world", type = "body")


my_pres <- my_pres %>% on_slide(3) %>% ph_with_text(type = "title", str = "redone title")

my_pres <- my_pres %>% add_slide(layout = "Title and Content", master = pp.theme)

# my_pres <- my_pres %>% 
#   ph_add_text(str = "A small red text!", style = text_prop ) # %>% 
#   ph_add_text(str = "Blue text first... ", pos = "before", style = text_blue_prop ) %>% 
#   ph_add_par(level = 2) %>%
#   ph_add_text(str = "additionnal paragraph")


print(my_pres, target = "C:\\Work\\Analysis\\OnePOS\\New Weekly Support Deck - 2017\\automate_test.pptx")



#### 9/20/2017 - date arithmatic - week ending; SQL LIKE equivalent (which) ####

library(lubridate)

test <- em %>% select(Id, Created_Date, Created_Week)
test$next.sunday <- as.Date(ceiling_date(test$Created_Date, unit = "week"))

test$cweek <- as.Date(cut(test$Created_Date, breaks = "week"))+1

test2 <- seq(as.Date("2016-12-25"), by = "days", length.out = 80)
test2 <- as.data.frame(test2)
test2$week.start <- as.Date(cut.Date(test2$test2, breaks = "week"))
test2$week.end <- as.Date(cut.Date(test2$test2, breaks = "week"))
test2$week.start.2 <- floor_date(test2$test2, unit = "week")
test2$week.end.2 <- ceiling_date(test2$test2, unit = "week")

# week ending formula
test2$next.monday <- (test2$test2 + 7) + ( 1 - as.integer(format(test2$test2, format = "%u")))
test2$next.sunday <- test2$test2 + ( 7 - as.integer(format(test2$test2, format = "%u")))


## SQL like equivalent
em.lastweek[which(str_detect(em.lastweek$term, "ST")),]


#### 9/15/2017 - REGEX, strings, the R book ####


grep("206", em$Title)
regexpr("206",em$Title[14])
regexpr("206", em$Title)
regexpr("Lane", em$Title)


substr(em$Title[14], regexpr("206", em$Title[14]), regexpr("206", em$Title[14]) + 2)
regmatches(em$Title, regexpr("206", em$Title[14]))

em %>% filter(grepl("206", em$Title)) %>% select(Title)
as.vector(em %>% filter(grepl("206", em$Title)) %>% select(Title))


## Regex Detection

regex.em <- em %>% filter(grepl("^C", em$LAST_ASSIGNED)) %>% arrange(LAST_ASSIGNED) %>% select(LAST_ASSIGNED)
regex.em <- em %>% filter(grepl(" *C", em$LAST_ASSIGNED)) %>% arrange(LAST_ASSIGNED) %>% select(LAST_ASSIGNED)
regex.em <- em %>% filter(grepl("y$", em$LAST_ASSIGNED)) %>% arrange(LAST_ASSIGNED) %>% select(LAST_ASSIGNED)
regex.em <- em %>% filter(grepl("[HJ]", em$LAST_ASSIGNED)) %>% arrange(LAST_ASSIGNED) %>% select(LAST_ASSIGNED)

# first letter
regex.em <- em %>% filter(grepl("^[HJ]", em$LAST_ASSIGNED)) %>% arrange(LAST_ASSIGNED) %>% select(LAST_ASSIGNED)

# find last letter, inverse(grepl), exclude last letter
regex.em <- em %>% filter(grepl("[a-zA-Z]$", em$LAST_ASSIGNED)) %>% arrange(LAST_ASSIGNED) %>% select(LAST_ASSIGNED)
regex.em <- em %>% filter(!grepl("[a-zA-Z]$", em$LAST_ASSIGNED)) %>% arrange(LAST_ASSIGNED) %>% select(LAST_ASSIGNED)
regex.em <- em %>% filter(grepl("[^a-zA-Z]$", em$LAST_ASSIGNED)) %>% arrange(LAST_ASSIGNED) %>% select(LAST_ASSIGNED)

# find H at beg of word. End of word.
regex.em <- em %>% filter(grepl("\\<[Hh]", em$LAST_ASSIGNED)) %>% arrange(LAST_ASSIGNED) %>% select(LAST_ASSIGNED)
regex.em <- em %>% filter(grepl("[Hh]\\>", em$LAST_ASSIGNED)) %>% arrange(LAST_ASSIGNED) %>% select(LAST_ASSIGNED)

# chars at position
regex.em <- em %>% filter(grepl("^.[eo]", em$LAST_ASSIGNED)) %>% arrange(LAST_ASSIGNED) %>% select(LAST_ASSIGNED)
regex.em <- em %>% filter(grepl("^.{1}[eo]", em$LAST_ASSIGNED)) %>% arrange(LAST_ASSIGNED) %>% select(LAST_ASSIGNED)

# match exactly X times, between X and Y times, at most/least X characters, 
regex.em <- em %>% filter(grepl("^.{5}[eo]", em$LAST_ASSIGNED)) %>% arrange(LAST_ASSIGNED) %>% select(LAST_ASSIGNED)
regex.em <- em %>% filter(grepl("^.{10,22}$", em$LAST_ASSIGNED)) %>% arrange(LAST_ASSIGNED) %>% select(LAST_ASSIGNED)
regex.em <- em %>% filter(grepl("^.{,10}$", em$LAST_ASSIGNED)) %>% arrange(LAST_ASSIGNED) %>% select(LAST_ASSIGNED)
regex.em <- em %>% filter(grepl("^.{28,}$", em$LAST_ASSIGNED)) %>% arrange(LAST_ASSIGNED) %>% select(LAST_ASSIGNED)


## Substitution - sub/gsub - replace 3 digit number with string, replace first characters... ##
gsub("[^0-9][0-9][0-9][0-9][^0-9]", "(NUMBERS WERE HERE)", em$Title)
gsub("^.", "(first letter)", em$Title)

## Location - regexpr/gregexpr - location of regex in vector
regexpr("[Ll][Aa][Nn][Ee]", em$Title)

# %in% - MUST BE PERFECT MATCH. regex / grep - for natural wildcard matching
which(em$Title %in% c("Office"))
em$Title[grep("Office", em$Title)]

# escaping characters
grepl("\\(", em$LAST_ASSIGNED)
em$LAST_ASSIGNED[-grep("\\(", em$LAST_ASSIGNED)]
gsub("[[:punct:]]", "(there was punct here", em$Incident_Type)
em$Incident_Type[grep("[[:punct:]]", em$Incident_Type)]

# OR matching
em$Title[grep("MSR|pass[^A-z]", em$Title)]



#### 9/13/2017 - ggplotly hover over info ####




ipak(c("ggplot2", "tm", "sqldf", "scales","chron", "tidytext", "tidyr","dplyr","plotly","tidyverse"))

function(ggplotly_REFERENCE){ref = "http://www.r-graph-gallery.com/2017/06/07/get-the-best-from-ggplotly/"}

p <- stores.2017 %>% ggplot(aes(IRs.Lane.Week, IRs.Week, color = Region, size = Num.Lanes))+geom_point()
ggplotly(p)

mytext=paste("Region: ", stores.2017$Region, "\n" , "Store: ", stores.2017$Store, "\n", 
             "IRs.Lane.Week = ", stores.2017$IRs.Lane.Week, "\n","IRs.Week = ", stores.2017$IRs.Week, sep="")    
pp=plotly_build(p)   
 ## problem here. nulls?  ## style( pp, text=mytext, hoverinfo = "text", traces = c(1, 2, 3) )
### region not displaying correctly???? mytext??? ###


#### 9/13/2017 - 9/20 - 9/30 word stemming 2.0 - consolidate worod stems w/ custom list ####



  # stemmed words list -> to combine to common term
  test.words <- c("freeze","freezes","freezing","froze","frozed","frozen",
                  "transact", "transaction", "TRANSACTIONS", "issue", "ISsueS", "issued", "issuer",
                  "Lane", "lanes", "after two   lanes  and a lane  ")
  test.remove.words <- "lane"
  
  library(hunspell)
  stemDocument(test.words) %>% str()
  
  
  test.hunspell.stem <- hunspell_stem(test.words)
  test.hunspell.stem[[1]] %>% str()
  test.hunspell.stem[1] %>% str()
  
  lapply(test.hunspell.stem, `[[`, 1)
  sapply(test.words, toupper) %>% str()
  lapply(test.words, toupper)
  
  # first element
  map(test.hunspell.stem, 1)
  # last element
  test.hunspell.stem.stems <- sapply(test.hunspell.stem, tail, 1)
  
  ## hunspell stemmer is dropping some words - don't use ##
  
  freeze.synonyms <- list(
    list(word="(freeze issues)", syns=c("freez","froze","frozen"))
  )
  
  replaceSynonymsFreeze <- content_transformer(function(x,syn=NULL){
    Reduce(function(a,b){
      gsub(paste0("\\b(", paste(b$syns, collapse = "|"),")\\b"), b$word, a)
    }, syn, x)
  })
  
  tidy.syns <- c("freez", "froze", "frozen")
  ## tidy method of above
  paste0("\\b(", paste(tidy.syns, collapse = "|"), ")\\b")
  
  test.tm <- Corpus(VectorSource(test.words))
  inspect(test.tm)
  test.tm <- tm_map(test.tm, tolower)
  inspect(test.tm)
  test.tm <- tm_map(test.tm, stemDocument)
  inspect(test.tm)
  test.tm <- tm_map(test.tm, replaceSynonymsFreeze, freeze.synonyms)
  inspect(test.tm)
  
  more.synonyms <- list(
    list(word="(issue issues)", syns=c("issu", "issuer"))
  )
  
  test.tm <- tm_map(test.tm, replaceSynonymsFreeze, more.synonyms)
  inspect(test.tm)

  ### also figure method for tidytext stem replacement ###

  test <- data.frame(words = test.words)
  test$words <- as.character(test$words)
  test
  test$words <- removeWords(test$words, test.remove.words)
  test
  # remove words before unnesting tokens (but after lowercase etc) for better performance prob.
  ## REMOVE WORDS AFTER STEMMING/LOWERCASE
  
  # TIDY TEXT PROCESS - tokenize (& tolower & stripwhitespace), then stem, then remove stopwords
  test.tidy <- test %>%
    unnest_tokens(word, words, drop = F) %>%
    mutate(word = stemDocument(word)) %>%
    mutate(word = removeWords(word, test.remove.words)) %>% ## will leave blanks
    filter(!(word == "")) # needed after removing words ^; NEED TO USE CUSTOM STEMMER
  test.tidy
  
  
# excellent stemming reference - including dictionary word corrections
# https://stackoverflow.com/questions/24443388/stemming-with-r-text-analysis




#### 9/13/2017 - file.choose; IRs/Store/Lane dataset  ####

filename <- file.choose()
stores.2017 <- read.csv(filename, header = TRUE)
### NOTE: NA REGION BEING READ AS "NA" NONVALUE. EITHER STRINGS.AS.FACTORS = F OR CUSTOM CONVERT NULLS
filename
basename(filename)
dirname(filename)

names(stores.2017)[3] <- "Count.IRs"
names(stores.2017)[4] <- "Region"
names(stores.2017)[6] <- "Num.Lanes"

#### 9/13/2017 - ggplot pt3 - Scatterplot stores by IRs/Lane ... ####
stores.2017 %>% ggplot(aes(IRs.Week, Num.Lanes))+geom_point()

# color palette for region
region.colors <- rainbow(length(levels(stores.2017$Region)))
names(region.colors) <- levels(stores.2017$Region)

# multivariate scatter plot
stores.2017 %>% ggplot(aes(IRs.Lane.Week, IRs.Week, color = Region, size = Num.Lanes))+geom_point()

# summary info
stores.summary <- stores.2017 %>%
  group_by(Region) %>%
  summarise(a.ilw = mean(IRs.Lane.Week)) %>%
  arrange(desc(a.ilw))

stores.miss.region <- stores.2017 %>%
  filter(is.na(Region) == T)



#### dynamic week formula ####

function(MORE_IMPROVEMENTS){
  
  # r formula to always get last business week
  last.monday <- (Sys.Date() - 7) + ( 1 - as.integer(format(Sys.Date(), format = "%u")))
  last.sunday <- (Sys.Date() - 7) + ( 7 - as.integer(format(Sys.Date(), format = "%u")))
  
  # FILENAME = Text Analysis - [min] to [max]
  paste("Text Analysis - ", last.monday, " to ", last.sunday, sep = "")
}
function(ggplot_references){
  # https://stackoverflow.com/questions/33613385/sort-bar-chart-by-sum-of-values-in-ggplot
  # stacked ordered bar - https://stackoverflow.com/questions/21596906/ggplot-how-to-limit-output-in-bar-plot-so-only-most-frequent-occurrences-are-sh
  
}


#### (old) em tm plotting - sorting and top 10 ####


ggplot(data=em.tidy.dtm.full, aes(x=reorder(term, flag, function(x){sum(x)}), y=flag)) +
  geom_bar(stat="summary", fun.y=sum) +
  coord_flip()

ggplot(data=em.tidy.dtm.full, aes(x=reorder(term, flag, sum), y=flag)) +
  geom_bar(stat="summary", fun.y=sum) #+
# coord_flip()


# aggregate 1 col grouped by another
aggregate(em.tidy.dtm.full$flag, by = list(em.tidy.dtm.full$term), FUN = sum)

aggregate(em.tidy.dtm.full$flag, list(em.tidy.dtm.full$term), sum)


# plot top x words; ggplot order
topwords <- sqldf("select term, count(*) as 'count' from [em.tidy.dtm.full] 
                  group by term order by count(*) desc limit 20")
ggplot(topwords, aes(reorder(term, -count), count))+geom_bar(stat = 'identity')





#### stopword list ####

full.word.exclusion.list <- c(word.blacklist, stopwords(kind = "SMART"))
write.csv(full.word.exclusion.list, 
          file = paste("stopwords - ", Sys.Date(), ".csv", sep = ""), 
          row.names = FALSE)







#### ggplot Part 1 - Basic Bar Plots; Colors ####
# Count
ggplot(x, aes(Created_Month_R))+
  geom_bar()

# Sum
ggplot(x, aes(Created_Month_R, Open_To_Resolve_Time__M_))+
  stat_summary(fun.y = "sum", geom = "bar")

# Avg
ggplot(x, aes(Created_Month_R, Open_To_Resolve_Time__M_))+
  stat_summary(fun.y = "mean", geom = "bar")


# test - label intervals
ggplot(x, aes(Created_Month_R, Open_To_Resolve_Time__M_))+
  stat_summary(fun.y = "sum", geom = "bar")+
  scale_x_date(date_breaks = "2 month")


## Custom colors

# border / fill
ggplot(x, aes(Created_Month_R))+
  geom_bar(color = "blue", fill = rgb(.1,.4,.5,.3))

# hue
ggplot(x, aes(Created_Month_R))+ # fill = as.factor(Created_Month_R)
  geom_bar()+scale_fill_hue(c = 11)


#### ggplot Part 2 - Stacked / Grouped bars ####
# Stacked Bar
ggplot(x, aes(x = Created_Week_R, fill = Priority))+
  geom_bar()

# Stacked Percent
ggplot(x, aes(x = Created_Week_R, fill = Priority))+
  geom_bar(position = "fill")

# Grouped Bar
ggplot(x, aes(x = Created_Week_R, fill = Priority))+
  geom_bar(position = "dodge")

# Add RcolorBrewer
ggplot(x, aes(x = Created_Week_R, fill = Priority))+
  geom_bar(position = "fill") + scale_fill_brewer(palette = "Pastel1")

# Faceting
ggplot(x, aes(x = Created_Week_R, fill = Priority))+
  geom_bar()+facet_wrap(~Last_SG_Grouping)



# test - plot avg resolve time by month, facet by support group
ggplot(x, aes(x = Created_Month_R, y = Open_To_Resolve_Time__M_, color = Created_Month_R))+
  stat_summary(fun.y = "mean", geom = "bar")+
  facet_wrap(~Last_SG_Grouping)



# text mine as corpus
# x.1 <- x[,c("id","Title")]
# corp <- Corpus(DataframeSource(x.1))
# dtm <- DocumentTermMatrix(corp)




