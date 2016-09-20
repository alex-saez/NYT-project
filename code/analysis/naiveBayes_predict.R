# make prediction for a single document using Naive Bayes list returned by dtm_naiveBayes_train.R
# NB : list returned by dtm_naiveBayes_train.R
# doc: vector of counts per word for a single document. words must specified as dimnames
# section: (char) section of the document
#
# Returns single prediction

naiveBayes_predict = function(NB, doc, section=NA)
{
  
  if(section %in% names(NB$p.y.section))
    prior = NB$p.y.section[section]
  else
    prior = NB$p.y
  
  log_posterior_y = log(prior) + sum(doc * NB$log.p.X.given.y[names(doc)], na.rm = TRUE)
  log_posterior_noty = log(1-prior) + sum(doc * NB$log.p.X.given.noty[names(doc)], na.rm = TRUE)
  
  y_hat = log_posterior_y>log_posterior_noty
  
  return(y_hat)
}


