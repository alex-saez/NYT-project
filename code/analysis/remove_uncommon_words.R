# allows to remove dtm elements corresponding to words that appear unfrequently, 
# either in terms of total # of occurrences or of # of documents that contain them

remove_uncommon_words = function(dtm, nmin, ndocmin=1){
  
  # select words with at least nmin occurrences total:
  ind1 = tapply(dtm$v, dtm$j, sum) >= nmin
  
  # select words present in atleast ndocmin documents:
  ind2 = tapply(dtm$i, dtm$j, function(x){length(unique(x))}) >= ndocmin
  
  # entries of the matrix corresponding to words of interest:
  indkeep = dtm$j %in% (1:dtm$ncol)[ind1 & ind2]
  
  dtm$i = dtm$i[indkeep]
  dtm$j = dtm$j[indkeep]
  dtm$v = dtm$v[indkeep]

  # map old word ids to new ones:
  new_word_ids = rep(NA, dtm$ncol)
  new_word_ids[(1:dtm$ncol)[ind1 & ind2]] = 1:sum(ind1 & ind2)
  dtm$j = new_word_ids[dtm$j]
  
  # update ncol and Terms fields too:
  dtm$ncol = sum(ind1 & ind2)
  dtm$dimnames$Terms = dtm$dimnames$Terms[ind1 & ind2]
  
  return(dtm)
}