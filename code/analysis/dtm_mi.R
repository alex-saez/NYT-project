# Compute mutual information between a document indicator variable y and the different terms in a document-term matrix
# Accepts DTM's in either regular (sparse) representation or as a dense, simple-triplet matrix

dtm_mi = function(dtm, y){
  
  if(any(class(dtm) == "matrix")){
    
    if(length(y)!=nrow(dtm)) 
      stop("Length of y must equal number of documents")
    
    y = as.logical(y)
    
    p.y = sum(y)/length(y)
    p.X = colSums(dtm)/sum(dtm)
    p.X.given.y = colSums(dtm[y,])/sum(dtm[y,])
    p.X.given.noty = colSums(dtm[!y,])/sum(dtm[!y,])
    
  }
  
  else if(any(class(dtm) == "simple_triplet_matrix")){
    
    if(any(duplicated(y)) | any(!(y %in% (1:dtm$nrow)))) 
      stop("y must be a vector of unique document indices")
    
    terms = dtm$dimnames$Terms[dtm$j]
    
    p.y = length(y)/dtm$nrow
    p.X = tapply(dtm$v, terms, sum) / sum(dtm$v)

    p.X.given.y = tapply(dtm$v[dtm$i %in% y], terms[dtm$i %in% y], sum)
    p.X.given.y = p.X.given.y / sum(p.X.given.y)
    other_terms = setdiff(terms, names(p.X.given.y))
    other_terms = array(0, dim=length(other_terms), dimnames = list(other_terms))
    p.X.given.y = c(p.X.given.y, other_terms)
    p.X.given.y = p.X.given.y[order(names(p.X.given.y))]
    
    p.X.given.noty = tapply(dtm$v[!(dtm$i %in% y)], terms[!(dtm$i %in% y)], sum)
    p.X.given.noty = p.X.given.noty / sum(p.X.given.noty)
    other_terms = setdiff(terms, names(p.X.given.noty))
    other_terms = array(0, dim=length(other_terms), dimnames = list(other_terms))
    p.X.given.noty = c(p.X.given.noty, other_terms)
    p.X.given.noty = p.X.given.noty[order(names(p.X.given.noty))]

  }
  
  else
    stop("dtm must be either a matrix or a simple-triplet mtrix")
  
  I = p.X.given.y * p.y * log(p.X.given.y/p.X) + 
    p.X.given.noty * (1-p.y) * log(p.X.given.noty/p.X) + 
    (1-p.X.given.y) * p.y * log((1-p.X.given.y)/(1-p.X)) + 
    (1-p.X.given.noty) * (1-p.y) * log((1-p.X.given.noty)/(1-p.X))
  
  return(I)
}