
library(tm)

setwd("/Users/Alex/Dropbox/NYT project/")

source('./code/analysis/dtm_naiveBayes_train.R')
source('./code/analysis/dtm_naiveBayes_predict.R')
source('./code/analysis/naiveBayes_predict.R')
source('./code/analysis/remove_uncommon_words.R')

# # get data:
# load("./data/database.RData")
# 
# dat = database[grepl('e',database$content),] # only articles with content
# remove(database)
# 
# train_ind = sample(nrow(dat), round(0.8*nrow(dat)))


# define response vector
top_prop = .1 # the upper quantile of ME to consider
me_scores = dat$most_emailed
me = me_scores[me_scores>0]
y = me_scores >= sort(me, decreasing = TRUE)[floor(top_prop*length(me))]


# Train NB ---------------------------------------------

# training dtm:
# corp = VCorpus(VectorSource(dat$content[train_ind]))
# dtm_Tr = DocumentTermMatrix(corp, control = list(stemming = TRUE,
#                                                  bound = list(local = c(1, Inf)), 
#                                                  tokenize = "MC",
#                                                  removePunctuation = TRUE, 
#                                                  removeNumbers = TRUE,
#                                                  stopwords = TRUE, 
#                                                  minWordLength = 3))

# load('./data/dtm_Tr.RData')

dtm_Tr = remove_uncommon_words(dtm_Tr,10,1000)


mus = c(.001,.01,.1,1,10)
NBs = list()
for(j in 1:length(mus)){
  NBs[[j]] = dtm_naiveBayes_train(dtm_Tr, y[train_ind], mus[j], dat$section[train_ind])
}


# Test ----------------------------------------------------

# test dtm:
# corp = VCorpus(VectorSource(dat$content[-train_ind]))
# dtm_Te = DocumentTermMatrix(corp, control = list(stemming = TRUE,
#                                                  bound = list(local = c(1, Inf)), 
#                                                  tokenize = "MC",
#                                                  removePunctuation = TRUE, 
#                                                  removeNumbers = TRUE,
#                                                  stopwords = TRUE, 
#                                                  minWordLength = 3))

# load('./data/dtm_Te.RData')

#dtm_Te = remove_uncommon_words(dtm_Te,10,2)

y_Te = y[-train_ind] # test response vector

acc = vector()
TP = vector()
TN = vector()
FP = vector()
FN = vector()
for(j in 1:length(mus)){

  yhat = dtm_naiveBayes_predict(NBs[[j]], dtm_Te, dat$section[-train_ind])

  TP[j] = sum(yhat & y_Te)/sum(y_Te)
  TN[j] = sum(!yhat & !y_Te)/sum(!y_Te)
  FP[j] = sum(yhat & !y_Te)/sum(!y_Te)
  FN[j] = sum(!yhat & y_Te)/sum(y_Te)
}

# prob of y given yhat:
p.y = mean(y_Te)
acc = p.y*TP/(p.y*TP + (1-p.y)*FP)

plot(1:length(mus),acc, type='l', ylim=c(0,1),xaxt="n", xlab='mu', ylab='')
axis(1, at=(1:5), labels=c('.001','.01','.1','1','10'))
par(new=T) 
plot(1:length(mus),TP, type='l', col='red', ylim=c(0,1), axes = FALSE, xlab='', ylab='')
par(new=T) 
plot(1:length(mus),TN, type='l', col='blue', ylim=c(0,1), axes = FALSE, xlab='', ylab='')
par(new=T) 
plot(1:length(mus),FP, type='l', lty = 2, col='red', ylim=c(0,1), axes = FALSE, xlab='', ylab='')
par(new=T) 
plot(1:length(mus),FN, type='l', lty = 2, col='blue', ylim=c(0,1), axes = FALSE, xlab='', ylab='')

# print ratio p(y|yhat)/p(y)
print(TP/(p.y*TP + (1-p.y)*FP))




# barplot(c(TP[1], TN[1], FP[1], FN[1]), ylim=c(0,1), col=c('red','blue','pink','cyan'))
# axis(1, at=(1:4), labels=c('TP','TN','FP','FN'))


#   yhat = logical(dtm_Te$nrow)
#   for(k in 1:dtm_Te$nrow){
#     doc = table(dtm_Te$dimnames$Terms[dtm_Te$j[dtm_Te$i==k]])
#     yhat[k] = naiveBayes_predict(NBs[[j]], doc)
#   }

