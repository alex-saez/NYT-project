# Find most indicative words for a document to belong in a specific category
#
# dtm: document-term matrix, in simple-triplet format
# y: vector of row indices of documents of interest (e.g. 'most emailed')
# len: number of most important terms to display. defaut 10
# method: either frequency change ('freq_diff', default) or mutual information ('mi')
# tail: whether to show most ('top', default) or least ('bottom') important terms

important_terms = function(dtm, y, len=10, method="freq_diff", tail="top"){
  
  if(!any(class(dtm) == "simple_triplet_matrix"))
    stop("dtm must be a simple triplet matrix")
  
  if(any(duplicated(y)) | any(!(y %in% (1:dtm$nrow)))) 
    stop("y must be a vector of unique document indices")
  
  
  # compute word frequency
  terms = dtm$dimnames$Terms[dtm$j]
  word_counts = tapply(dtm$v, terms, sum)
  
  # compute word frequency in docs of interest
  word_counts_y = tapply(dtm$v[dtm$i %in% y], terms[dtm$i %in% y], sum)
  non_y_terms = setdiff(terms, names(word_counts_y)) # so that # terms is same as in word_counts
  non_y_terms = array(0, dim=length(non_y_terms), dimnames=list(non_y_terms))
  word_counts_y = c(word_counts_y, non_y_terms)
  word_counts_y = word_counts_y[order(names(word_counts_y))]
  
  # compare word frequencies between docs of interest and ALL
  term_freq = word_counts/sum(word_counts)
  term_freq_y = word_counts_y/sum(word_counts_y)
  rel_freq_y = term_freq_y-term_freq
  
  if(method != "freq_diff"){   # mutual information
    source('./code/analysis/dtm_mi.R')
    I = dtm_mi(dtm, y)
  }
  
  # prepare output:
  if(method == "freq_diff"){
    if(tail == "top")
      output = names(sort(rel_freq_y, decreasing = TRUE)[1:len])
    else
      output = names(sort(rel_freq_y, decreasing = FALSE)[1:len])
  }
  
  else{
    if(tail == "top")
      output = names(sort(I[rel_freq_y>0],decreasing = TRUE)[1:len])
    else
      output = names(sort(I[rel_freq_y<0],decreasing = TRUE)[1:len])
  }

  return(output)
}