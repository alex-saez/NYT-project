# make prediction for a single document using Naive Bayes
# INPUT:
#     NB : list returned by dtm_naiveBayes_train.R
#     doc: a character string with the content of an article
#     section: (char) NYT section of the article
# OUTPUT: 
#     list with the two log probabilities of belonging to popular or not

naiveBayes_predict_doc = function(NB, doc, section=NA)
{
  library(tm)
  
  if(section %in% names(NB$p.y.section))
    prior = NB$p.y.section[section]
  else
    prior = NB$p.y
  
  doc = iconv(doc, "UTF-8", "UTF-8")
  doc = PlainTextDocument(doc)
  doc = termFreq(doc, control = list(stemming = FALSE,
                                     bound = list(local = c(1, Inf)), 
                                     tokenize = "MC",
                                     tolower = TRUE,
                                     removePunctuation = TRUE, 
                                     removeNumbers = TRUE,
                                     stopwords = TRUE, 
                                     minWordLength = 3))
  
  
  log_posterior_y = (log(prior) + sum(doc * NB$log.p.X.given.y[names(doc)], na.rm = TRUE)) /sum(doc)
  log_posterior_noty = (log(1-prior) + sum(doc * NB$log.p.X.given.noty[names(doc)], na.rm = TRUE)) /sum(doc)
  
  #y_hat = log_posterior_y>log_posterior_noty
  
  return(list(log_p_y = log_posterior_y, 
              log_p_noty = log_posterior_noty))
}


