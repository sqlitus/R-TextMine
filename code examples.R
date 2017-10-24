



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



