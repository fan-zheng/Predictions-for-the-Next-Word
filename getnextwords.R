last2words <- function(str){
  
  return (c(word(trimws(str),-2),word(trimws(str),-1)))
}
getnextword <- function(strs,n){
  
  words<-last2words(strs)
  x<-words[1]
  y<-words[2]
  outputwords <- trigrams(x,y,n) 
  # trigrams searches in trigram for next words
  # if no word found in trigrams then recurses to bigram.
  return (outputwords)
}

trigrams <- function(x,y,n=5){
  # get words from trigram
  outputwords1 <- trigram[trigram$w1==x & trigram$w2==y,]
  if ((dim(outputwords1)[1]==0) | (is.na(x)==TRUE)) # if no words in trigram go to bigram
    return (bigrams(y,n))
  outputwords2<-outputwords1[order(-outputwords1$prob_tri),]
  if (nrow(outputwords2)>n)
    return (outputwords2$w3[1:n])
  count<-nrow(outputwords2)
  bwords<-bigrams(y,n-count)
  return (c(outputwords2$w3,bwords))
}

bigrams <- function(y,n=5){
  outputwords1 <- bigram[bigram$w1==y,]
  if(dim(outputwords1)[1]==0)
    return (unigrams(n))
  outputwords2<-outputwords1[order(-outputwords1$prob_bi),]
  if (nrow(outputwords2)>n)
    return (outputwords2$w2[1:n])
  count<-nrow(outputwords2)
  unWords<-unigrams(n-count)
  return (c(outputwords2$w2,unWords))
  
}

unigrams <- function(n){
  return (sample(unigram$w1,size = n))
}

fun<-function(strs,d,n) {
  
  words<-last2words(strs)
  x<-words[1]
  y<-words[2]
  obs_trigs<-trigram[trigram$w1==x & trigram$w2==y,]
  # in trigrams tri1 select rows that have word1 and word2 in w1, w2 and new df is obs_trigs
  obs_trigs$prob<-(obs_trigs$freq-d)/sum(obs_trigs$freq)
  # calculate probability for those that have word1 and word2 in obs_trigs
  #unobs_trig_tails<-unigram[!(unigram$w1%in%obs_trigs$w3),]
  # in unigrams keep only those words that are not in obs_trigs$w3, they are the ones not start with word1 
  # and word2 in unigrams no need here for later can get rid all of them from obs_bi
  # unobs_trig_tailsv<-uni1[!(uni1$w1%in%obs_trigs$w3),]$w1 
  # vector form
  obs_bigs<-bigram[bigram$w1==y,]
  # select all those start with word2 in bigrams
  obs_bigs$prob<-(obs_bigs$freq-d)/sum(obs_bigs$freq)
  # calculate probability of obs_bigrs
  alpha_big<-1-sum(obs_bigs$prob)
  # calculate alpha_big, that is the probability spill into unigrams
  
  
  unobs_trig_tails<-unigram[!(unigram$w1%in%obs_bigs$w2),]
  #update in unigrams that also removes the ones that in obs_bigs whose w1 is word1,including
  #the one shows up in trigram w1 is words, w2 is word2
  unobs_trig_tails$prob<-unobs_trig_tails$freq/sum(unobs_trig_tails$freq)*alpha_big
  # calculate the probabilities of the unigram that was not observed in either tramgrams or bigrams
  
  
  alpha_trig<-1-sum(obs_trigs$prob)
  # calcuate alpha_trig,that is the probability spill into bigrams
  
  obs_bigs_bionly<-obs_bigs[!(obs_bigs$w2 %in% obs_trigs$w3),]
  # in the observed bigs remove the ones that has w1 and w2 observed in trigrams since we need to combine
  # with unigrams to calculate probability
  obs_bigs_bionly<-subset(obs_bigs_bionly,select=c(freq,w2,prob))
  # before combine needs to only select the second word to be consistent with unigrams format 
  names(obs_bigs_bionly)[names(obs_bigs_bionly)=='w2']<-'w1'
  # before combine needs to change w2 name to w1 to be consistent with unigrams format
  unobs_trig_tails<-subset(unobs_trig_tails,select=c(freq,w1,prob))
  unobs_trigrams<-rbind(unobs_trig_tails,obs_bigs_bionly)
  # this unobs_trigrams combines all unigrams that not observed in trigrams in unigram format
  unobs_trigrams$prob<-unobs_trigrams$prob/sum(unobs_trigrams$prob)*alpha_trig
  # calculate the probabilities by useing alpha_trig
  obs_trigs_trionly<-subset(obs_trigs,select=c(freq,w3,prob))
  # again in order to combine modify obs_trigs select only w3 column
  names(obs_trigs_trionly)[names(obs_trigs_trionly)=='w3']<-'w1'
  # again in order to combine change name w3 to w1 
  probs_trigrams<-rbind(unobs_trigrams,obs_trigs_trionly)
  # in the end this gives the probabilities of all word in the format of unigram
  probs_trigrams <- probs_trigrams[order(-probs_trigrams$prob), ]
  # order decreasing of probs_trigrams
  return (probs_trigrams$w1[1:n])
}



