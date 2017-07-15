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


