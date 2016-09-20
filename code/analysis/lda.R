library(topicmodels)

setwd("/Users/Alex/Dropbox/NYT project/")
load("./data/dtm.RData")

date()
topicmodel = LDA(dtm, k=50, method = "Gibbs")
date()


terms = terms(topicmodel, 100)
terms = as.data.frame(terms)
write.csv(terms,"./data/topic_models/topic_terms_dtm2_100_Gibbs_burn1000.csv", row.names=FALSE)

# use topic model to estimate the topics of new data:------------------------------------

dict = dtm$dimnames$Terms # get dictionary
doc = VCorpus(VectorSource(data$content[seq(16446,16843,by=2)])) # new data
x = DocumentTermMatrix(doc, control = list(dictionary = dict)) # dtm using training dictionary

a = posterior(topicmodel, newdata = x)
plot(a$topics[1,])
order(a$topics[1,], decreasing=T)

p_G50 = perplexity(topicmodel_Gibbs50, newdata = x)
p_V50 = perplexity(topicmodel_VEM50, newdata = x)
p_G75 = perplexity(topicmodel_Gibbs75, newdata = x)
p_V75 = perplexity(topicmodel_VEM75, newdata = x)
p_G100 = perplexity(topicmodel_Gibbs100, newdata = x)
p_V100 = perplexity(topicmodel_VEM100, newdata = x)
p_G125 = perplexity(topicmodel_Gibbs125, newdata = x)
p_V125 = perplexity(topicmodel_VEM125, newdata = x)
p_G150 = perplexity(topicmodel_Gibbs150, newdata = x)
p_V150 = perplexity(topicmodel_VEM150, newdata = x)
p_G200 = perplexity(topicmodel_Gibbs200, newdata = x)
p_V200 = perplexity(topicmodel_VEM200, newdata = x)
peplexities = matrix(c(p_G50,p_V50,p_G75,p_V75,p_G100,p_V100,p_G125,p_V125,p_G150,p_V150,p_G200,p_V200),ncol=2,byrow=T)
matplot(c(50,75,100,125,150,200), peplexities, type="b",pch='o',lty=1, xlab='Number of topics', ylab = 'Perplexity')

