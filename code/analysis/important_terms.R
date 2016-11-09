
important_terms = function(dtm, y, len=10, method="freq_diff", tail="top", n_doc_min=5){
  # Find most indicative words for a document to belong in a specific category
  # INPUT:
  #     dtm: document-term matrix, in simple-triplet format
  #     y: vector of row indices of documents of interest (e.g. 'most emailed')
  #     len: number of most important terms to display. defaut 10
  #     method: either frequency change ('freq_diff', default) or mutual information ('mi')
  #     tail: whether to show most ('top', default) or least ('bottom') important terms
  #     n_doc_min: min number of documents of interest a word should appear in. Default =5
  # OUTPUT:
  #     named vector of words and their over/sub-representation metric, whether MI or freq diff
  
  if(!any(class(dtm) == "simple_triplet_matrix"))
    stop("dtm must be a simple triplet matrix")
  
  if(any(duplicated(y)) | any(!(y %in% (1:dtm$nrow)))) 
    stop("y must be a vector of unique document indices")
  
  
  # compute word frequency
  terms = dtm$dimnames$Terms[dtm$j]
  word_counts = tapply(dtm$v, terms, sum)
  
  # compute word frequency in docs of interest ...
  i_y = dtm$i[dtm$i %in% y]
  j_y = dtm$j[dtm$i %in% y]
  v_y = dtm$v[dtm$i %in% y]
  terms_y = terms[dtm$i %in% y]
  # ...with requirement of word presence in at least n_doc_min docs of interest
  n_doc_y_per_term = tapply(i_y, j_y, function(x){length(unique(x))}) 
  term_ind_crit_ok = as.integer(names(n_doc_y_per_term[n_doc_y_per_term >= n_doc_min]))
  indkeep = j_y %in% term_ind_crit_ok
  word_counts_y = tapply(v_y[indkeep], terms_y[indkeep], sum)
  
  # equalize list of words to match those in word_counts (non-y list)
  non_y_terms = setdiff(terms, names(word_counts_y)) 
  non_y_terms = array(0, dim=length(non_y_terms), dimnames=list(non_y_terms))
  word_counts_y = c(word_counts_y, non_y_terms)
  word_counts_y = word_counts_y[order(names(word_counts_y))]
  
  # compare word frequencies between docs of interest and ALL
  term_freq = word_counts/sum(word_counts)
  term_freq_y = word_counts_y/sum(word_counts_y)
  rel_freq_y = term_freq_y - term_freq
  
  if(method != "freq_diff"){   # mutual information
    source('../code/analysis/dtm_mi.R')
    I = dtm_mi(dtm, y)
  }
  
  # prepare output:
  if(method == "freq_diff"){
    if(tail == "top")
      output = sort(rel_freq_y, decreasing = TRUE)[1:len]
    else
      output = sort(rel_freq_y, decreasing = FALSE)[1:len]
  }
  
  else{
    if(tail == "top")
      output = sort(I[rel_freq_y>0],decreasing = TRUE)[1:len]
    else
      output = sort(I[rel_freq_y<0],decreasing = TRUE)[1:len]
  }

  return(output)
}