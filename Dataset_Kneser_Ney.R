library(dplyr)
library(tidyr)
# we start from tris, bis and unis where each contains words and frequency. each word is in different
# column labeled as w1, w2, w3
nbis=nrow(bis) # number of rows in bis, it is also the number of bigram types
bitest<-bis
c_normed_w2type<-bitest %>%
group_by(w2) %>%
summarise(number = n()/nbis)  #c_normed_w2type count for each w2 how many type of w1w2 and normalized by
# nbis, which is total number of bigram types,thus the result in number is uni_prob
unis_probdf<-merge(unis,c_normed_w2type,by.x="w1",by.y="w2",all.y =TRUE) # merge unis,c_normed_w2type basically gets the 
# uni_prob for each uni word, which is defined as for each w in uni, number of w1w types
# in bi gram and devide by total wxwy type in bi grams
w1type <- bis %>%
group_by(w1) %>%
summarise(number = n()) #w1type count for each w1 in w1w2 bis, how many w1 types for each w1,
# which is different from c(w1), c(w1) count total number of w1
com1<-merge(bis,unis_probdf,by.x="w1",by.y="w1")
names(com1)[names(com1)=='freq.y']<-'Cnt_w1'# combine bis and unis_probdf label Cnt_w1 which is c(w1)
com1<-subset(com1,select=c(w1,w2,freq.x,Cnt_w1)) # freq.x is the count for each w1w2 as originally
# in bis
unis_probdf1<-unis_probdf
names(unis_probdf1)[names(unis_probdf1)=='w1']<-'w2' # change w1 to w2 name so naturally merge with com1,
# since we want to merge on the last word
com2test<-merge(com1,unis_probdf1)
names(com2test)[names(com2test)=='number']<-'prob_uni' # formally label the prob_uni
com2test<-subset(com2test,select=c(w2,w1,freq.x,Cnt_w1,prob_uni)) # here w2 is last word
# w1 is first word, freq.x is count of w1w2 combination
com3<-merge(com2test,w1type) # merge com2test, w1type to get the w1 types with different w2 follow w1
# in bis
names(com3)[names(com3)=='number']<-'Cnt_w1type'
com3$prob_bi<-(com3$freq.x-0.75)/com3$Cnt_w1+0.75/com3$Cnt_w1*com3$Cnt_w1type*com3$prob_uni
# calculate each w1w2 probability
saveRDS(com3, file="com.rds")
# save data frame of bi gram look up table.
com4<-merge(tris,com3,by.x=c('w1','w2'),by.y=c('w1','w2'))
names(com4)[names(com4)=='freq']<-'freq_w1w2w3'
names(com4)[names(com4)=='freq.x']<-'freq_w1w2'
# merge tris data frame with bigram look up table, label names for clarity
w1w2type <- tris %>%
group_by(w1,w2) %>%
summarise(number = n()) # similar to w1type, this time count the type of w1,w2 combination
com5<-merge(com4,w1w2type,by.x=c('w1','w2'),by.y=c('w1','w2'))
names(com5)[names(com5)=='number']<-'Cnt_w1w2type'
# similar to previous step, get the w1w2 type count for w1w2 combination for tri grams
com5$prob_tri<-(com5$freq_w1w2w3-0.75)/com5$freq_w1w2+0.75/com5$freq_w1w2*com5$Cnt_w1w2type*com5$prob_bi
# using Kneser-Ney formula to calculate pribability of tri-words
saveRDS(com5, "com_tri.rds")
# save data frame of tri gram look up table.
com_uni<-unis_probdf
names(com_uni)[names(com_uni)=='number']<-'prob_uni'
saveRDS(com_uni, "com_uni.rds")
# save data frame of tri gram look up table.
com_uni_order<-com_uni[order(-com_uni$prob_uni),][1:50,]
saveRDS(com_uni_order, "com_uni50.rds")
# save data frame of tri gram look up table for top 50 words only.
uni_final<-subset(com_uni_order,select = c(w1, prob_uni))
bi_final<-subset(com3,select = c(w1, w2, prob_bi))
tri_final<-subset(com5,select = c(w1, w2, w3, prob_tri))
save(tri_final,bi_final,uni_final, file = "data_ngrams.Rdata")