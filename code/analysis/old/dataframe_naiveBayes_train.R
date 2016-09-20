
dataframe_naiveBayes_train = function(data, y, mu){
  
  library(tm)
  source('./code/analysis/remove_uncommon_words.R')
  source('./code/analysis/dtm_naiveBayes_train.R')
  
  corp = VCorpus(VectorSource(data$content))

  # training dtm:
  dtm = DocumentTermMatrix(corp, control = list(stemming = TRUE,
                                                   bound = list(local = c(1, Inf)), 
                                                   tokenize = "MC",
                                                   removePunctuation = TRUE, 
                                                   removeNumbers = TRUE,
                                                   stopwords = TRUE, 
                                                   minWordLength = 3))
  
  dtm = remove_uncommon_words(dtm,10,1)
  
  p_y_section = tapply(y, data$section, sum)/table(data$section)
  p_y_section = as.numeric(p_y_section)
  attr(p_y_section, "names") = sort(unique(data$section))
  
  # Train NB
  NB = dtm_naiveBayes_train(dtm, which(y), mu)

  NB = c(list(p.y.section=p_y_section),NB)
  NB = NB[c(2,1,3,4)]
  
  return(NB)
}



