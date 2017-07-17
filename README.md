# R-TextMine
analyzing text documents by row 

packages <- c("mailR","rJava","qdap")

download.or.load <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,"Package"])]
  if(length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

download.or.load(packages)


DATA
dput(head(DATA))

freqs <- t(wfm(DATA$state, 1:nrow(DATA)))
freqs2 <- t(wfm(DATA$state))
df.txt1 <- data.frame(DATA, freqs, check.names = FALSE)
df.txt2 <- data.frame(DATA, freqs2, check.names = FALSE)


# top so-many-words
ords <- rev(sort(colSums(freqs)))[1:9]      #top 9 words
top9 <- freqs[, names(ords)]                #grab those columns from freqs  
data.frame(DATA, top9, check.names = FALSE) #put it together
