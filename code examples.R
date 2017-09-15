


  

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



