
library(tm)
library(ggplot2)

setwd("/Users/Alex/Dropbox/NYT project/")
load("./data/database.RData")

data = database[grepl('e',database$content),] # only articles with content

corp = VCorpus(VectorSource(data$content))

dtm = DocumentTermMatrix(corp, control = list(stemming = FALSE,
                                              bound = list(local = c(1, Inf)), 
                                              tokenize = "MC",
                                              removePunctuation = TRUE, 
                                              removeNumbers = TRUE,
                                              stopwords = TRUE, 
                                              minWordLength = 3))

# compute word frequency
terms = dtm$dimnames$Terms[dtm$j]
word_counts = tapply(dtm$v, terms, sum)
sort(word_counts, decreasing=TRUE)[1:20]


# define most-emailed
top_prop = 1 # the upper quantile of ME to consider
me_scores = data$most_emailed
me = me_scores[me_scores>0]
me_ind = which(me_scores %in% sort(me, decreasing = TRUE)[1:(top_prop*length(me))])


# compute word frequency in ME
word_counts_me = tapply(dtm$v[dtm$i %in% me_ind], terms[dtm$i %in% me_ind], sum)
non_me_terms = setdiff(terms, names(word_counts_me)) # so that # terms is same as in word_counts
non_me_terms = array(0, dim=length(non_me_terms), dimnames=list(non_me_terms))
word_counts_me = c(word_counts_me, non_me_terms)
word_counts_me = word_counts_me[order(names(word_counts_me))]
sort(word_counts_me, decreasing=TRUE)[1:20]


# compare word frequencies between ME and ALL-------------------------------------------------------
term_freq = word_counts/sum(word_counts)
term_freq_me = word_counts_me/sum(word_counts_me)
rel_freq_me = term_freq_me-term_freq
names(sort(rel_freq_me,decreasing = TRUE)[1:100])

# mutual information:-------------------------------------------------------
source('./code/analysis/dtm_mi.R')
I = dtm_mi(dtm, me_ind)
names(sort(I[rel_freq_me>0],decreasing = TRUE)[1:100])

intersect(names(sort(rel_freq_me[ind_gt_10],decreasing = TRUE)[1:100]), names(sort(I[ind_gt_10 & rel_freq_me>0],decreasing = TRUE)[1:100]))


# find most emailed sections-------------------------------------------------------
sections = unique(data$section)
sections = sections[!is.na(sections)]
sec_count_me = vector()
sec_count = vector()
for(s in sections){
  sec_count_me = c(sec_count_me, sum(s==data$section[me_ind], na.rm=T))
  sec_count = c(sec_count, sum(s==data$section, na.rm=T))
}
names(sec_count_me)=sections
names(sec_count)=sections

# keep only sections with >10 me articles
sec_count = sec_count[sec_count_me>=10]
sec_count_me = sec_count_me[sec_count_me>=10]

p_sec_me = (sec_count_me/sum(sec_count_me))/(sec_count/sum(sec_count)) # freq of section in me normalized by overall freq
sort(p_sec_me, decreasing=T)

# plot 
p_sec_me = sort(p_sec_me)
b = qplot(x=1:length(p_sec_me), y=p_sec_me) + xlab("") + ylab("Relative frequency")  + scale_x_discrete(labels=c("",names(p_sec_me)))
b = b + geom_hline(yintercept=1)
b = b + geom_bar(stat="identity", fill = "#993333", width=.5) + coord_flip()  
b = b + theme(axis.text.y = element_text(face="bold", color="#993333", size=14))
b + theme(axis.title.x = element_text(face="bold", color="#777777", size=16))



# find most emailed tags----------------------------------------------------------
tags = unique(unlist(data$tags))
tags = tags[!is.na(tags)]
tag_count_me = vector()
tag_count = vector()
for(t in tags){
  tag_count_me = c(tag_count_me, sum(grepl(t, data$tags[me_ind])))
  tag_count= c(tag_count, sum(grepl(t, data$tags)))
}
names(tag_count_me)=tags
names(tag_count)=tags

# keep only tags with >10 me articles
tag_count = tag_count[tag_count_me>=10]
tag_count_me = tag_count_me[tag_count_me>=10]

p_tag_me = (tag_count_me/sum(tag_count_me))/(tag_count/sum(tag_count)) # freq of tag in me normalized by overall freq
sort(p_tag_me, decreasing=T)

# plot 
p_tag_me = sort(p_tag_me)
b = qplot(x=1:length(p_tag_me), y=p_tag_me) + xlab("") + ylab("Relative frequency")  + scale_x_discrete(labels=c("",names(p_tag_me)))
b = b + geom_hline(yintercept=1)
b = b + geom_bar(stat="identity", fill = "#993333", width=.5) + coord_flip()  
b = b + theme(axis.text.y = element_text(face="bold", color="#993333", size=14))
b + theme(axis.title.x = element_text(face="bold", color="#777777", size=16))


# find most emailed topics----------------------------------------------------------
load("./data/topic_models/topicmodel_dtm2_100_Gibbs.RData")
topicnames = read.csv("./data/topic_models/topic_names.csv") # load topic names
topicmat = as.data.frame(topicmodel@gamma)
names(topicmat) = names(topicnames)
topic_me_ind = me_ind[1:nrow(topicmat)]

me_topic_prop = (colSums(topicmat[topic_me_ind,])/sum(topicmat[topic_me_ind,])) / (colSums(topicmat)/sum(topicmat))
names(sort(me_topic_prop,decreasing = TRUE))

# M.I. for topics:
source('./code/analysis/dtm_mi.R')
I = dtm_mi(topicmat, topic_me_ind)
names(sort(I,decreasing = TRUE))


# find most emailed authors----------------------------------------------------------
authors = unique(unlist(data$author))
authors = authors[!is.na(authors)]
author_count_me = vector()
author_count = vector()
for(t in authors){
  author_count_me = c(author_count_me, sum(grepl(t, data$author[me_ind])))
  author_count= c(author_count, sum(grepl(t, data$author)))
}
names(author_count_me)=authors
names(author_count)=authors

# keep only authors with >10 me articles
author_count = author_count[author_count_me>=10]
author_count_me = author_count_me[author_count_me>=10]

p_author_me = (author_count_me/sum(author_count_me))-(author_count/sum(author_count)) # freq of tag in me normalized by overall freq
sort(p_author_me, decreasing=T)

# plot 
p_author_me = sort(p_author_me)
b = qplot(x=1:length(p_author_me), y=p_author_me) + xlab("") + ylab("Relative frequency")  + scale_x_discrete(labels=c("",names(p_author_me)))
b = b + geom_hline(yintercept=1)
b = b + geom_bar(stat="identity", fill = "#993333", width=.5) + coord_flip()  
b = b + theme(axis.text.y = element_text(face="bold", color="#993333", size=14))
b + theme(axis.title.x = element_text(face="bold", color="#777777", size=16))


# predict me----------------------------------------------------------
n = nrow(data)
a = as.data.frame(as.matrix(dtm))
train_ind = sample(n,round(.01*n))
a_train = a[train_ind,]
a_test = a[-train_ind,] 

y = rep(0,n)
y[me_ind] = 1
y = as.factor(y)
y_train = y[train_ind]
y_test = y[-train_ind] 

NB = naiveBayes(a_train, y_train, laplace=1/ncol(a_train))
y_hat = predict(NB, a_test)

sum(y_test==y_hat)/length(y_test)
