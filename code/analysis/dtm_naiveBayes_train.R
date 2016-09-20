# train Naive Bayes to classify docs given:
# dtm: document-term matrix in either regular (sparse) representation or as a dense, simple-triplet matrix
# y: response variable, as logical vector of length nrow(dtm)
# mu: Laplace smoothing, in multiples of 1/dtm$ncol. Optional, default=0
# sections: char vector of length nrow(dtm) with the section for each document. Optional
#
# Returns list with the priors p(y) and p(y|section), and the 2 log-likelihoods log(p(X|y)) and log(p(X|not.y))

dtm_naiveBayes_train = function(dtm, y, mu=0, sections){
  
  if(any(class(dtm) == "matrix")){
    
    n = nrow(dtm)
    d = ncol(dtm)
    
    n.X.given.y = colSums(dtm[y,])
    n.X.given.noty = colSums(dtm[!y,])
    
  }
  
  else if(any(class(dtm) == "simple_triplet_matrix")){
    
    n = dtm$nrow
    d = dtm$ncol
    
    y_ind = which(y)
    terms = dtm$dimnames$Terms[dtm$j]
    
    
    n.X.given.y = tapply(dtm$v[dtm$i %in% y_ind], terms[dtm$i %in% y_ind], sum)
    other_terms = setdiff(terms, names(n.X.given.y))
    other_terms = array(0, dim=length(other_terms), dimnames = list(other_terms))
    n.X.given.y = c(n.X.given.y, other_terms)
    n.X.given.y = n.X.given.y[order(names(n.X.given.y))]
    
    n.X.given.noty = tapply(dtm$v[!(dtm$i %in% y_ind)], terms[!(dtm$i %in% y_ind)], sum)
    other_terms = setdiff(terms, names(n.X.given.noty))
    other_terms = array(0, dim=length(other_terms), dimnames = list(other_terms))
    n.X.given.noty = c(n.X.given.noty, other_terms)
    n.X.given.noty = n.X.given.noty[order(names(n.X.given.noty))]

  }
  
  else
    stop("dtm must be either a matrix or a simple-triplet mtrix")
  
  
  if(length(y) != n) 
    stop("Length of y must equal number of documents")
  

  # compute general prior
  p.y = sum(y)/n
  
  # compute section-based priors
  if(!missing(sections)){
    p.y.section = tapply(y, sections, sum)/table(sections)
    p.y.section = as.numeric(p.y.section)
    attr(p.y.section, "names") = sort(unique(sections))
  }
  else
    p.y.section = NULL
  
  # compute log-posterior
  log.p.X.given.y = log(n.X.given.y + mu) - log(sum(n.X.given.y) + mu*d)
  log.p.X.given.noty = log(n.X.given.noty + mu) - log(sum(n.X.given.noty) + mu*d)
  
  
  return(list(p.y = p.y, 
              p.y.section = p.y.section,
              log.p.X.given.y = log.p.X.given.y, 
              log.p.X.given.noty = log.p.X.given.noty))

}



