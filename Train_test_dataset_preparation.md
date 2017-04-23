---
output: html_document
---
Johns Hopkins Coursera Capstone: Milestone Report
========================================================
author:Fan Zheng 
date: 2017-04-19
autosize: true

Executive Summary
========================================================
### This is part of the Capstone course of Coursera Data Science specialization by Johns Hopkins University and Swiftkey company. The goal for this project is to build a Shiny application that will predict the next word according to user's input of words.This milestone summary report includes: The information of downloaded files, initial statistical summary of files and procedure to sample the files, initial steps to clean up the files/corpus. Wordcloud and histogram of tri, bi and uni words are provided. I am going to propose a model that is going to provide the probability of the next word according to the user input.

Download the data and inital summary
========================================================
### The data is downloaded from the link provided by the course and unzipped to local disk.I will focus on en_US folder that contains three text files: en_US.blogs.txt (205M), en_US.news.txt(200M) and en_US.twitter.txt (163M). The r code below shows that for blogs, lines: 899288, words: 37334441; for news, lines: 1010242, words: 34372597; for twitter, lines: 2360148, words: 30373792. 


```r
library(tm)
library(stringi)
setwd("C:/Datascience/DatascienceJohnshopkins/Capstone/Data/Coursera-SwiftKey/final/en_US")
blogs <- readLines("en_US.blogs.txt")
news <- readLines("en_US.news.txt")
twitters <- readLines("en_US.twitter.txt")
stri_stats_general(blogs)
stri_stats_general(news)
stri_stats_general(twitters)
sum(stri_count(blogs,regex="\\S+"))
sum(stri_count(news,regex="\\S+"))
sum(stri_count(twitters,regex="\\S+"))
```
Sampling to generate subset of data 1: Generate combined data file
========================================================


```r
doc_blogs<-file("en_US.blogs.txt")
line_blogs<-readLines(doc_blogs)
combine<-file("combine_text.txt","w")
writeLines(line_blogs,combine)
close(doc_blogs)
doc_news<-file("en_US.news.txt","rb")
line_news<-readLines(doc_news)
writeLines(line_news,combine)
close(doc_news)
doc_twitter<-file("en_US.twitter.txt")
line_twitter<-readLines(doc_twitter)
writeLines(line_twitter,combine)
close(doc_twitter)
close(combine)
```

Sampling to generate subset of data 2: Sampling from combined file for train and test dataset
========================================================


```r
conn <- file("combine_text.txt", "r") # the plan is to select 5% of all pupulation, among the 5%
fulltext <- readLines(conn)           # select 0.1% as test and 99.9% as training text
nlines <- length(fulltext)
close(conn)
conn <- file("sample_combine.txt", "w")
selection <- rbinom(nlines, 1, 0.05)
for (i in 1:nlines) {
  if (selection[i]==1) { writeLines(fulltext[i],conn) }
}
close(conn)
conn <- file("sample_combine.txt", "r")
fulltext <- readLines(conn)
nlines <- length(fulltext)
close(conn)
conn <- file("test.txt", "w")
conn1<-file("train.txt","w")
selection <- rbinom(nlines, 1, 0.001)
for (i in 1:nlines) {
  if (selection[i]==1) { writeLines(fulltext[i],conn) }
  else {
    writeLines(fulltext[i],conn1)
  }
}
close(conn)
close(conn1)
```

Clean up the train text file and generate term document matrix for tri,bi and uni words. 
========================================================
### In the end I generate wordcloud to show the top words that have largest frequency. The text to term document matrix process takes a long time even with sampled text. So here we load the data previous saved for the wordcloud.
### The clean up part including replacing some special symbols with space, remove punctuation, change to lower case, take away all other symbols except character and number. Take away numbers and also very frequently used word such as a, the (they are called stop words). At last we remove all extraneous whitespaces.
### Then we use the function tokens_ngrams to generate tri, bi and uni term document matrix. In the end we can transfer the matrix to dataframe and plot with wordcloud.

```r
library(quanteda)
setwd("C:/Datascience/DatascienceJohnshopkins/Capstone/Data/Coursera-SwiftKey/final/en_US/combined/sampled")
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})
toNothing <- content_transformer(function(x, pattern) {return (gsub(pattern, "", x))})
encode<-content_transformer(function(x) {return (iconv(enc2utf8(x), sub = "byte"))})
docs1_train <- Corpus(DirSource("C:/Datascience/DatascienceJohnshopkins/Capstone/Data/Coursera-SwiftKey/Final/en_US/combined/sampled/train",encoding = "UTF-8")) 
# encoding is important to have consistent symbol to work on
docs1_train<-tm_map(docs1_train,toSpace, "-")
docs1_train<-tm_map(docs1_train,toNothing, "'")
docs1_train<-tm_map(docs1_train,toNothing, "'") # get rid different type of '
docs1_train<-tm_map(docs1_train, encode)
docs1_train<-tm_map(docs1_train,removePunctuation)
docs1_train <- tm_map(docs1_train,content_transformer(tolower))
keepEnglish <- content_transformer(function(x) {return (gsub("[^a-zA-Z0-9]"," ",x))})
docs1_train<-tm_map(docs1_train,keepEnglish)
docs1_train <- tm_map(docs1_train, removeNumbers)
# docs1_train <- tm_map(docs1_train, removeWords, stopwords("english"))
# for final apps we are going to keep stop words since in real world we are using stop words
docs1_train <- tm_map(docs1_train, stripWhitespace)
TrigramTokenizer <-function(x) unlist(lapply(tokens_ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
tdm_train_tri <- TermDocumentMatrix(docs1_train, control = list(tokenize = TrigramTokenizer))
BigramTokenizer <-function(x) unlist(lapply(tokens_ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
tdm_train_bi <- TermDocumentMatrix(docs1_train, control = list(tokenize = BigramTokenizer))
UnigramTokenizer <-function(x) unlist(lapply(tokens_ngrams(words(x), 1), paste, collapse = " "), use.names = FALSE)
#tdm_train_uni <- TermDocumentMatrix(docs1_train, control = list(tokenize = UnigramTokenizer))
# default remove less than 3-letter word here we want to keep them
tdm_train_uni <- TermDocumentMatrix(docs1_train, control = list(wordLengths=c(1,Inf),tokenize = UnigramTokenizer))
tdm_train_triMatrix<-as.matrix(tdm_train_tri)
fretri <- sort((rowSums(tdm_train_triMatrix)),decreasing=TRUE) # vector with term and its frequency
fretridf<-data.frame(word=names(fretri),freq=fretri) # transfer vector to data frame
wordcloud(fretridf$word,fretridf$freq,c(4,.2),max.words=100, random.order=FALSE, rot.per=0.1, colors=brewer.pal(10,"Dark2"))
tdm_train_biMatrix<-as.matrix(tdm_train_bi)
frebi <- sort((rowSums(tdm_train_biMatrix)),decreasing=TRUE)
frebidf<-data.frame(word=names(frebi),freq=frebi)
wordcloud(frebidf$word,frebidf$freq,c(4,.2),max.words=100, random.order=FALSE, rot.per=0.1,colors=brewer.pal(10,"Dark2"))
tdm_train_uniMatrix<-as.matrix(tdm_train_uni)
freuni <- sort((rowSums(tdm_train_uniMatrix)),decreasing=TRUE)
freunidf<-data.frame(word=names(freuni),freq=freuni)
# do the same for text file
wordcloud(freunidf$word,freunidf$freq,c(4,.2),max.words=100, random.order=FALSE, rot.per=0.1,colors=brewer.pal(10,"Dark2"))
```

Visulization of the tri, bi and uni text distribution and statistical plot with wordcloud and ggplot2.
========================================================


```r
library(wordcloud)
library(ggplot2)
setwd("C:/Datascience/DatascienceJohnshopkins/Capstone/Data/Coursera-SwiftKey/final/en_US")
fretridf <- readRDS("fretridfkeepstop.rds")
frebidf <- readRDS("frebidfkeepstop.rds")
freunidf <- readRDS("freunidfkeepstopkeep1letter.rds")
par(mfrow=c(1,3))
wordcloud(fretridf$word,fretridf$freq,c(4,.2),max.words=100, random.order=FALSE, rot.per=0.1, colors=brewer.pal(10,"Dark2"))
wordcloud(frebidf$word,frebidf$freq,c(4,.2),max.words=100, random.order=FALSE, rot.per=0.1,colors=brewer.pal(10,"Dark2"))
wordcloud(freunidf$word,freunidf$freq,c(8,.4),max.words=100, random.order=FALSE, rot.per=0.1,colors=brewer.pal(10,"Dark2"))
```

![plot of chunk unnamed-chunk-5](Capstone_MilestoneFinalforApps-figure/unnamed-chunk-5-1.png)

```r
library(ggplot2)
p1 <- ggplot(fretridf[1:10,], aes(word, freq))
p1 <- p1 + geom_bar(stat="identity")
p1 <- p1 + theme(axis.text.x=element_text(angle=45, hjust=1))
p2 <- ggplot(frebidf[1:10,], aes(word, freq))
p2 <- p2 + geom_bar(stat="identity")
p2 <- p2 + theme(axis.text.x=element_text(angle=45, hjust=1))
p3 <- ggplot(freunidf[1:10,], aes(word, freq))
p3 <- p3 + geom_bar(stat="identity")
p3 <- p3 + theme(axis.text.x=element_text(angle=45, hjust=1))
p1
```

![plot of chunk unnamed-chunk-5](Capstone_MilestoneFinalforApps-figure/unnamed-chunk-5-2.png)

```r
p2
```

![plot of chunk unnamed-chunk-5](Capstone_MilestoneFinalforApps-figure/unnamed-chunk-5-3.png)

```r
p3
```

![plot of chunk unnamed-chunk-5](Capstone_MilestoneFinalforApps-figure/unnamed-chunk-5-4.png)

Next step: Model proposal
========================================================
### When user inputs two words and if the two words are in the training set, it is easy to use the maximum likelyhood method to predict the next word, i.e., the word that has the largest frequency in 3-gram term document matrix. But the difficult part is what if the two words are not in the tranining data set. I plan to use the Katz's back-off model as a smoothing method such that when the two word do not show up in training data we "back-off" the search in the bi-gram training set. The final probability of the next word will take into account both the maximum likelyhood in the tri-gram model and the smoothing probability in the bi-gram model.
