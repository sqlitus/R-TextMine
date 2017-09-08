ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
# ipak("checkpoint")
# checkpoint("2017-08-24")
ipak(c("ggplot2", "tm", "sqldf", "scales","chron", "tidytext", "tidyr"))

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


#### em tm plotting - sorting and top 10 ####


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



# 9/4 - text mining - tidy text ch1 ####

text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

text

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










# text mining vignette

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



