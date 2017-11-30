#### trending words up & down - return as list ####
test.WordTrendRateUp <- function(df, START_DATE, END_DATE, min_freq, top_x){
  
  # should be week start (Monday) and end (Sunday) for weekly analysis
  START_DATE <- as.Date(START_DATE)
  END_DATE <- as.Date(END_DATE)
  
  # dataset to return
  df.return <- df %>%
    filter(Created_Date >= START_DATE & Created_Date <= END_DATE) %>%
    filter(word.week.total >= min_freq) %>%
    arrange(Created_Week_Ending, desc(word.week.trend, desc(word.week.total)))
    
  # Calcs
  df.p <- df %>%
    filter(Created_Date >= START_DATE & Created_Date <= END_DATE) %>%
    group_by(Created_Week_Ending, word) %>%
    summarize(word.week.trend = mean(word.week.trend), word.week.avg = mean(word.week.avg),
              word.week.total = mean(word.week.total), word.population.total = mean(word.population.total), 
              num.weeks = mean(num.weeks)) %>%
    filter(word.week.total >= min_freq) %>%
    arrange(Created_Week_Ending, desc(word.week.trend, desc(word.week.total))) %>%
    top_n(top_x, wt = word.week.trend) %>%
    mutate(week.top.10 = row_number()) %>%
    filter(week.top.10 <= top_x) %>%
    arrange(Created_Week_Ending, desc(word.week.total), desc(word.week.trend)) %>% # reorder by freq. for plot
    ungroup() %>%
    mutate(wordorder = 1:nrow(.)) %>%
    mutate(ord.term = paste(Created_Week_Ending,"__", word, sep = ""))
  
  # Plot
  p <- df.p %>%
    ggplot(aes(x = reorder(ord.term, wordorder), y = word.week.total, color = word)) + # label = paste0(round(word.week.total,0),"x")
    theme_bw() +
    geom_point(aes(size = word.week.trend, color = word), alpha = .5) +
    geom_point(aes(x = reorder(ord.term, wordorder), y = word.week.avg, size = word.week.avg), alpha = .3) +
    # geom_text(aes(label=paste0(round(word.week.trend, 0),"x")),color = "black", size = 3) +
    facet_wrap(~Created_Week_Ending, scales = "free_x") +
    labs(x = "Word", y = "Frequency", title = "Words Trending Up", subtitle = paste("With",min_freq,"or more weekly occurrences")) +
    scale_x_discrete(labels = function(x) gsub("^.+__", "", x)) +
    # geom_label() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
    # guides(color = FALSE, size = guide_legend(title = "Trend Rate")) +
    scale_size_continuous(range = c(5, 25))
  # scale_size_area(breaks = pretty(df$word.week.trend, n = 5)) +
  # scale_y_continuous(breaks = pretty(df$word.week.total, n = 6))
  
  
  return(list(p, df.return))
}
# get element of list from function
test.WordTrendRateUp(em.tidy.unigrams, two.mondays.ago, last.sunday, min_freq = 5, top_x = 10)
test.df <- test.WordTrendRateUp(em.tidy.unigrams, two.mondays.ago, last.sunday, min_freq = 5, top_x = 10)[[2]]



#### char math: time to response ####

char.ttr <- read.delim("clipboard")

ggplot(char.ttr, aes(Time_to_Response__M_)) + geom_histogram(bins = 30) +
  geom_vline(xintercept = median(char.ttr$Time_to_Response__M_), color = "steelblue", size = 1) +
  geom_vline(xintercept = mean(char.ttr$Time_to_Response__M_), color = "firebrick", size = 1) +
  scale_x_continuous(breaks = pretty(char.ttr$Time_to_Response__M_, 12)) +
  ggplot2::annotate("text", x = mean(char.ttr$Time_to_Response__M_), y = 40, 
                    label = round(mean(char.ttr$Time_to_Response__M_)),
                    angle = 90,
                    vjust = 1,
                    size = 9)


ggplot(char.ttr, aes(Time_to_Response__M_)) + geom_boxplot()

# proportion histogram: showing percent of values at intervals...
ggplot(char.ttr, aes(Time_to_Response__M_)) + geom_histogram(aes(y = ..count../sum(..count..)))



#### text function words: clean, summarize, plot, annotate ####

# top x unigrams; optional faceting
test.function.words <- function(df, start.date, end.date, top.x.words, facet){

  start.date <- as.Date(start.date)
  end.date <- as.Date(end.date)
  
  df.filtered <- df %>% filter(Created_Date >= start.date & Created_Date <= end.date)
  df.summaries <- df.filtered %>% group_by_(facet) %>% summarise(week.ir.total = n_distinct(Id))
  
  df.prep <- df.filtered %>%
    group_by_(facet) %>%
    count(word, sort = TRUE) %>%
    top_n(top.x.words, wt = n) %>%
    mutate(week.top.10 = row_number()) %>%
    filter(week.top.10 <= top.x.words) %>%
    ungroup() %>%
    mutate(wordorder = nrow(.):1) %>%
    mutate(facet.words = paste0(facet, "__", word)) %>% #suffix word names for facet ordering # !! dynamic col referen
    group_by(word) %>%
    mutate(word.top10.freq = n()) %>%
    dplyr::left_join(y = df.summaries, by = "facet")
  
  # Plot
  p <- df.prep %>%
    ggplot(aes(reorder(facet.words, rev(wordorder)), n, fill = word, label = n)) +
    theme_bw() + 
    geom_bar(stat = "identity", color = "black") +
    labs(x = "Word", y = "Frequency", title = "Most Common Words - last 2 weeks") +
    theme(legend.position = "none") +
    scale_x_discrete(labels = function(x) gsub("^.+__", "", x)) +
    geom_label() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") #+ labs(caption = "test text")
  
    if (!missing(facet)){
      if (facet == "Created_Week_Ending"){
        p <- p + facet_wrap(~paste("Week Ending", format(Created_Week_Ending, "%m-%d-%Y"), week.ir.total), 
                            scales = "free_x")
      } else {
        p <- p + facet_wrap(as.formula(paste("~", facet)))        
      }
    }

    
  return(p)
}
test.function.words(em.tidy.unigrams, two.mondays.ago, last.sunday, 10, "Created_Week_Ending")






#### filter, plot, annotation function test ####

  ## join summary and plot dataset

  # onepos unigrams - summary, annotation calcs, plot
  a.uni.weektot <- em.tidy.unigrams %>% filter(Created_Date >= two.mondays.ago & Created_Date <= last.sunday) %>%
    group_by(Created_Week_Ending) %>% summarise(week.ir.total = n_distinct(Id))

  # main plot
  a.uni.1 <- em.tidy.unigrams %>%
    filter(Created_Date >= two.mondays.ago & Created_Date <= last.sunday) %>%
    group_by(Created_Week_Ending, word) %>%
    summarise(n = n()) %>% 
    arrange(desc(n)) %>%
    top_n(10, wt = n) %>%
    mutate(week.top.10 = row_number()) %>%
    filter(week.top.10 <= 10) %>%
    ungroup() %>%
    mutate(wordorder = nrow(.):1) %>%
    mutate(facet.words = paste0(Created_Week_Ending, "__", word)) %>% #suffix word names for facet ordering
    group_by(word) %>%
    mutate(word.top10.freq = n()) 
  
  # join summaries to plot
  a.uni.2 <- a.uni.1 %>% left_join(y = a.uni.weektot, by = "Created_Week_Ending")
  
  # get total distinct IRs in top 10 word list
  a.uni.3 <- a.uni.2 %>% left_join(y = em.tidy.unigrams, by = c("Created_Week_Ending", "word")) %>%
    group_by(Created_Week_Ending) %>%
    summarise(top.10.ir.count = n_distinct(Id))
  
  # join distinct IR total to plotting dataset
  a.uni.4 <- a.uni.2 %>% left_join(y = a.uni.3, by = "Created_Week_Ending")

# plot is fine
  a.uni.4 %>%
  ggplot(aes(reorder(facet.words, rev(wordorder)), n, fill = word, label = n)) +
  theme_bw() + 
  geom_bar(stat = "identity", color = "black") +
  facet_wrap(~paste("Week Ending", 
                    format(Created_Week_Ending, "%m-%d-%Y"), "\n Total IRs:",week.ir.total, 
                    "IRs in top 10:", top.10.ir.count, 
                    percent(round(top.10.ir.count / week.ir.total, 2))), scales = "free_x") +
  labs(x = "Word", y = "Frequency", title = "Most Common Words - last 2 weeks") +
  theme(legend.position = "none") +
  scale_x_discrete(labels = function(x) gsub("^.+__", "", x)) +
  geom_label() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") #+ labs(caption = "test text") +
  ggplot2::annotate("text", label = a.uni.weektot$week.ir.total[])


a.uni.2$Created_Week_Ending[]
a.uni.weektot$week.ir.total[]
a.uni.2$week.ir.total[1]




#### comparing dplyr summarise & count/tally####
# identical
mtcars %>% tally()
mtcars %>% summarise(n = n())

# identical grouped counts
mtcars %>% count(cyl)
mtcars %>% group_by(cyl) %>% summarise(n = n())


#### function local/global variable test ####
testFunction <- function(x){
  x = x + 1
  print(x)
  x = x / 2
  return(x)
}
testFunction(3)

#### add plots to pp; doesn't work yet; package updates? ####
library(officer)
library(rvg)
doc <- read_pptx()
doc <- add_slide(doc, "Title and Content", "Office Theme")
doc <- ph_with_vg(doc, code = barplot(1:5, col = 2:6), type = "body")
print(doc, target = "vg.pptx")






#### 11/1/2017 - plotting coordinates on a map? ####

# resource: https://stackoverflow.com/questions/23130604/plot-coordinates-on-map

# loading the required packages
library(ggplot2)
install.packages("ggmap")
library(ggmap)

# creating a sample data.frame with your lat/lon points
lon <- c(-38.31,-35.5)
lat <- c(40.96, 37.5)
df <- as.data.frame(cbind(lon,lat))

# getting the map
mapgilbert <- get_map(location = c(lon = mean(df$lon), lat = mean(df$lat)), zoom = 4,
                      maptype = "satellite", scale = 2)

# plotting the map with some points on it
ggmap(mapgilbert) +
  geom_point(data = df, aes(x = lon, y = lat, fill = "red", alpha = 0.8), size = 5, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)




#### 10/10/2017 - remove non-printable characters, personal stemming lists ####

a <- "Hey! \x8c\xe6 Maybe I can give some suggestions: \x8c\xe6"
a
gsub("[^[:print:]]","",a)

texts <- c("i am member of the XYZ association",
           "apply for our open associate position", 
           "xyz memorial lecture takes place on wednesday", 
           "vote for the most popular lecturer")

wordStem(texts)

test.synonyms <- list(word = c("freeze","freezes","freezing","freezed"))
  
  
  list(
  list(word="(freeze issues)", syns=c("freez","froze","frozen"))
)


#### 10/8/2017 - old code in tidy text analysis report ####

function(old_code){
  
  
  #### subset dataset - last 2 weeks ####
  
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
  
  
  
}


#### 9/30/2017 - custom 'get last x days|weeks' function
last.x.timespan <- function(x, timespan = "weeks"){
  if (timespan == "weeks") {
    time.units <- 7
  } else if (timespan == "days") {
      time.units <- 0
  }
  last.x.date <- (Sys.Date() - x*time.units) + ( 1 - as.integer(format(Sys.Date(), format = "%u")))
  return(last.x.date)
}
last.x.timespan(5,"weeks")
last.x.timespan(3,"weeks")
last.x.timespan(1,"weeks")
last.x.timespan(0,"weeks")
last.x.timespan(-1,"weeks")


#### 9/29/2017 - various text & inspection functions ####

  # find tickets that have a bigram repeated in the title
  test <- em.tidy.bigrams %>%
    group_by(Id, bigram) %>%
    mutate(id.bigram.total = n()) %>%
    arrange(desc(id.bigram.total))
  
  test$Title

  # find object size & format(print)
  object.size(em.tidy.unigrams)
  format(object.size(em.tidy.unigrams), units = "Mb")
  format(object.size(em.tidy.bigrams), units = "Mb")
  
  # get all titles with the word "to" appearing twice
  test <- em.tidy.text %>% filter(grepl(".* to .* to .*", em.tidy.text$Title))
  
  # search vector for regex char string
  stop_words$word[grepl("new", stop_words$word)]
  stop_words$word[grepl("^n", stop_words$word)]
  names(em)[grep("(?i)create", names(em))]
  test <- stop_words %>% filter(lexicon == "snowball", !word %in% c("down","above","below","again", "after"))


#### Text mining in R ch1.4 - text mining jane austin ####

library(dplyr, tidytext)
text_df <- data_frame(line = 1:4, text = text)
text_df
unnest_tokens(text_df, words, text)

library(janeaustenr)
library(dplyr)
library(stringr)

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

original_books









# 8/26/2017

# text mining resources
function(TM_RESOURCES){
  # https://stackoverflow.com/questions/25551514/termdocumentmatrix-errors-in-r
  # https://stackoverflow.com/questions/32225770/r-tm-removewords-function-not-removing-words
}



# need to learn more about: regex, tm mapping, etc

install.packages("swirl")


# 8/24/2017 notes

colnames(em)


# choose file with browser
myFile <- file.choose()
myData  <- read.table(myFile,header=TRUE)


# turn rownames into dataframe column
em.dtm.m.df <- data.frame(names = row.names(em.dtm.m), em.dtm.m)
identical(em$Id, em.dtm.m.df$names)










# text mining vignette ####

data("crude")
tdm <- TermDocumentMatrix(crude,
                          control = list(removePunctuation = TRUE,
                                         stopwords = TRUE))
dtm <- DocumentTermMatrix(crude,
                          control = list(weighting =
                                           function(x)
                                             weightTfIdf(x, normalize =
                                                           FALSE),
                                         stopwords = TRUE))
inspect(tdm[202:205, 1:5])
inspect(tdm[c("price", "prices", "texas"), c("127", "144", "191", "194")])
inspect(dtm[1:5, 273:276])

s <- SimpleCorpus(VectorSource(unlist(lapply(crude, as.character))))
m <- TermDocumentMatrix(s,
                        control = list(removeNumbers = TRUE,
                                       stopwords = TRUE,
                                       stemming = TRUE))
inspect(m[c("price", "texa"), c("127", "144", "191", "194")])










#### Testing Nulls, NAs ####
# 7/18/2017
# quick test of sqldf

x <- c(1,2,3)
y <- c('abc','def',NA)
as.data.frame(x,y)
?as.data.frame
?data.frame

t <- data.frame(x,y)

x.1 <- x
y.1 <- c('abc',NA,'def')

t.1 <- data.frame(x.1,y.1)
library(sqldf)
sqldf("select * from t where y is not null")


# missing fields in csv are represented as NA, not NULL
t.csv <- read.csv("C:\\Users\\2066074\\Documents\\test\\test_data_nulls_r.csv")

sqldf("select * from [t.csv] where td1 is not null")



# need to check evaluation of aggregates with nulls
# https://stackoverflow.com/questions/8859124/na-values-using-sqldf
mean(x)

a <- c(1,2,NULL,3)
mean(a)
mean(c(1,2,NA,3))


#### stuff ####










# Cbind example. Getting column flags and appending (cbinding) them to the orig dataframe

cbind(1,1:5)
cbind(c("a","b","c"),1:3)

d <- as.data.frame(cbind(c("a","b","c"),1:3))

sapply(d[2],function(i){switch(i, i==1, "is one",2="is two",3="is 3")})

flags <- sapply(d[2],function(i){ifelse(i==1, 1,0)})

cbind(d,flags)



# Synonym code
# cbind treats as matrix; data.frame as DF -> char will be factors.
df2 <- cbind(df.txt1[1:5],df2)
df2 <- data.frame(df.txt1[1:5],df2)




########################################################################
#### online examples
########################################################################

## Creating flags from column values, and replacing the orig columns
datwe <- data.frame(replicate(37,sample(c(1,2,99),10,rep=TRUE)))

#convert to Yes/No
res <- as.data.frame(
  sapply(datwe[,23:37], function(i)
    ifelse(i==1, "Yes",
           ifelse(i==2, "No",
                  ifelse(i==99,NA,"Name itttt")))))



#update dataframe
datwe <- cbind(datwe[, 1:22],res)

#output, just showing first 2 columns
datwe[,23:24]





## Word frequency by row

DATA 
dput(head(DATA))

freqs <- t(wfm(DATA$state, 1:nrow(DATA))) 
# Data frame with all word counts
df.txt1 <- data.frame(DATA, freqs, check.names = FALSE) 

# Data frame with top 9 word counts
ords <- rev(sort(colSums(freqs)))[1:9] #top 9 words 
top9 <- freqs[, names(ords)] #grab those columns from freqs
DATA.top9 <- data.frame(DATA, top9, check.names = FALSE) #put it together

########################################################################
#### END online examples
########################################################################



