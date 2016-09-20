# make prediction for a set of docs in a dtm using Naive Bayes list returned by dtm_naiveBayes_train.R
# NB : list returned by dtm_naiveBayes_train.R
# dtm: dtm of documents, in simple triplet matrix format
# sections: character vector with the section of each document
#
# Returns vector of predictions

dtm_naiveBayes_predict = function(NB, dtm, sections)
{

  if(missing(sections))
    sections = rep(NA, dtm$nrow)

  if(any(class(dtm) == "simple_triplet_matrix"))
    dtm = as.array(dtm)
  
  dtm = dtm[, dimnames(dtm)$Terms %in% names(NB$log.p.X.given.y)]
  
  other_terms = setdiff(names(NB$log.p.X.given.y), dimnames(dtm)$Terms)
  other_terms = array(0, dim=c(nrow(dtm),length(other_terms)), dimnames = list(as.character(1:nrow(dtm)),other_terms))
  dtm = cbind(dtm, other_terms)
  remove(other_terms)
  dtm = dtm[,order(dimnames(dtm)[[2]])]
  
  # set prior to p(y|section)
  prior = NB$p.y.section[sections]
  # if section not recognized, set prior to p(y)
  prior[!(sections %in% names(NB$p.y.section))] = NB$p.y
  
  log_posterior_y = log(prior) + dtm %*% NB$log.p.X.given.y
  log_posterior_noty = log(1-prior) + dtm %*% NB$log.p.X.given.noty
  
  y_hat = as.logical(log_posterior_y>log_posterior_noty)
  
  return(y_hat)
}


