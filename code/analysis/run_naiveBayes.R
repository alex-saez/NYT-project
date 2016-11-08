
library(tm)

setwd("/Users/Alex/Dropbox/NYT project/")

source('./code/analysis/naiveBayes_train_dtm.R')
source('./code/analysis/naiveBayes_predict_dtm.R')
source('./code/analysis/naiveBayes_predict_doc.R')
source('./code/analysis/remove_uncommon_words.R')

# get data:
load("./output/database_Oct14.RData")

# data = database[grepl('e',database$content),] # only articles with content
# remove(database)

train_ind = sample(nrow(data), round(0.8*nrow(data)))


# define response vector
top_prop = 1 # the upper quantile of ME to consider
me_scores = data$most_emailed
me = me_scores[me_scores>0]
y = me_scores >= sort(me, decreasing = TRUE)[floor(top_prop*length(me))]


# Train NB ---------------------------------------------

# training dtm:
corp = VCorpus(VectorSource(data$content[train_ind]))
dtm_Tr = DocumentTermMatrix(corp, control = list(stemming = FALSE,
                                                 bound = list(local = c(1, Inf)),
                                                 tokenize = "MC",
                                                 removePunctuation = TRUE,
                                                 removeNumbers = TRUE,
                                                 stopwords = TRUE,
                                                 minWordLength = 3))

# load('./data/dtm_Tr.RData')

dtm_Tr = remove_uncommon_words(dtm_Tr,10,1000)


mus = 10^c(-2,-1,0,1)
NBs = list()
for(j in 1:length(mus)){
  # NBs[[j]] = naiveBayes_train_dtm(dtm_Tr, y[train_ind], mus[j], data$section[train_ind])
  NBs[[j]] = naiveBayes_train_dtm(dtm_Tr, y[train_ind], mus[j])
}


# Test ----------------------------------------------------

# test dtm:
corp = VCorpus(VectorSource(data$content[-train_ind]))
dtm_Te = DocumentTermMatrix(corp, control = list(stemming = FALSE,
                                                 bound = list(local = c(1, Inf)),
                                                 tokenize = "MC",
                                                 removePunctuation = TRUE,
                                                 removeNumbers = TRUE,
                                                 stopwords = TRUE,
                                                 minWordLength = 3))

# load('./data/dtm_Te.RData')

dtm_Te = remove_uncommon_words(dtm_Te,10,2)

y_Te = y[-train_ind] # test response vector

acc = vector()
TP = vector()
TN = vector()
FP = vector()
FN = vector()
for(j in 1:length(mus)){

  # log_ps = naiveBayes_predict_dtm(NBs[[j]], dtm_Te, data$section[-train_ind])
  log_ps = naiveBayes_predict_dtm(NBs[[j]], dtm_Te)
  yhat = log_ps$log_p_y > log_ps$log_p_noty

  acc[j] = mean(yhat == y_Te)
  TP[j] = sum(yhat & y_Te)/sum(y_Te)
  TN[j] = sum(!yhat & !y_Te)/sum(!y_Te)
  FP[j] = sum(yhat & !y_Te)/sum(!y_Te)
  FN[j] = sum(!yhat & y_Te)/sum(y_Te)
}


plot(1:length(mus),acc, type='l', ylim=c(0,1),xaxt="n", xlab='mu', ylab='')
axis(1, at=(1:length(mus)), labels=as.character(mus))
par(new=T) 
plot(1:length(mus),TP, type='l', col='red', ylim=c(0,1), axes = FALSE, xlab='', ylab='')
par(new=T) 
plot(1:length(mus),TN, type='l', col='blue', ylim=c(0,1), axes = FALSE, xlab='', ylab='')
par(new=T) 
plot(1:length(mus),FP, type='l', lty = 2, col='red', ylim=c(0,1), axes = FALSE, xlab='', ylab='')
par(new=T) 
plot(1:length(mus),FN, type='l', lty = 2, col='blue', ylim=c(0,1), axes = FALSE, xlab='', ylab='')

# prob of y given yhat:
p.y = mean(y_Te)
p.y.given.yhat = p.y*TP/(p.y*TP + (1-p.y)*FP)


# print ratio p(y|yhat)/p(y)
print(TP/(p.y*TP + (1-p.y)*FP))



############################################################################################################

source('./code/analysis/naiveBayes_train_dtm.R')
source('./code/analysis/naiveBayes_predict_doc.R')

load("./data/dtm_unstemmed_Oct14.RData")
load("./data/database_Oct14.RData")


y_me = data$most_emailed >0
y_ms = data$most_shared >0
y_mv = data$most_viewed >0


NB_me = naiveBayes_train_dtm(dtm, y_me, mu=1)
NB_ms = naiveBayes_train_dtm(dtm, y_ms, mu=1)
NB_mv = naiveBayes_train_dtm(dtm, y_mv, mu=1)

NB = list(NB_me=NB_me, NB_ms=NB_ms, NB_mv=NB_mv)
save(NB, file='./output/naive Bayes/NB_Oct14.RData')

all_log_p_me = data.frame(log_p_y=NULL, log_p_noty=NULL)
all_log_p_ms = data.frame(log_p_y=NULL, log_p_noty=NULL)
all_log_p_mv = data.frame(log_p_y=NULL, log_p_noty=NULL)
for(i in 1:nrow(data)){
  print(i)
  all_log_p_me = rbind(all_log_p_me, naiveBayes_predict_doc(NB_me, data$content[i]))
  all_log_p_ms = rbind(all_log_p_ms, naiveBayes_predict_doc(NB_ms, data$content[i]))
  all_log_p_mv = rbind(all_log_p_mv, naiveBayes_predict_doc(NB_mv, data$content[i]))
}

all_log_p = list(ME=all_log_p_me, MS=all_log_p_ms, MV=all_log_p_mv)
save(all_log_p, file='./output/naive Bayes/all_log_p.RData')

hist(all_log_ps$log_p_y - all_log_ps$log_p_noty, 100)
d = all_log_ps$log_p_y - all_log_ps$log_p_noty
d = d[d>-100 & d<100]
hist(d,100)





