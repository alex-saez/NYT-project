library(shiny)
library(wordcloud)
load("../data/ind_popular_Oct14.RData")
load("../data/dtm_unstemmed_Oct14.RData")
# load("../data/mutual_info_Oct14.RData")


# compute word frequency
terms = dtm$dimnames$Terms[dtm$j]
word_counts = tapply(dtm$v, terms, sum)


shinyServer(function(input, output) {
  
  # read type of popular
  pop_score = reactive({
   switch(input$popular, 
          'ME' = popular$most_emailed, 
          'MV' = popular$most_viewed, 
          'MS' = popular$most_shared 
   )
 }) 
  
  
 # indices for most-popular articles:
 top_pop_ind = reactive({
   pop = pop_score()
   pop = pop[pop>0]
   which(pop_score() %in% sort(pop, decreasing = TRUE)[1:(input$top_prop*length(pop)/100)])
 })
 

  # make the wordcloud drawing predictable during a session:
  wordcloud_rep <- repeatable(wordcloud)

  output$wordcloud = renderPlot({
    
    #rel_freq_pop = important_terms(dtm, top_pop_ind(), len=100, method="mi", tail="top")
      
    # compute word count in popular articles
    word_counts_pop = tapply(dtm$v[dtm$i %in% top_pop_ind()], terms[dtm$i %in% top_pop_ind()], sum)
    non_pop_terms = setdiff(terms, names(word_counts_pop)) # so that # terms is same as in word_counts
    non_pop_terms = array(0, dim=length(non_pop_terms), dimnames=list(non_pop_terms))
    word_counts_pop = c(word_counts_pop, non_pop_terms)
    word_counts_pop = word_counts_pop[order(names(word_counts_pop))]

    # compare word frequencies between POP and ALL:
    term_freq = word_counts/sum(word_counts)
    term_freq_pop = word_counts_pop/sum(word_counts_pop)
    rel_freq_pop = term_freq_pop - term_freq

    wordcloud_rep(names(rel_freq_pop), rel_freq_pop, scale = c(4,0.5),
                  max.words = input$num_words,
                  colors = brewer.pal(9, "Set1"))
  })
  
  
})