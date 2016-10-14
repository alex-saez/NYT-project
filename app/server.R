library(shiny)
library(wordcloud)
load("../data/database_Oct14.RData")
load("../data/dtm_Oct14.RData")

terms = dtm$dimnames$Terms[dtm$j]


shinyServer(function(input, output) {
  
  # Define a reactive expression for the document term matrix
  
 pop_score = reactive({
   switch(input$popular, 
          'ME' = data$most_emailed, 
          'MV' = data$most_viewed, 
          'MS' = data$most_shared 
   )
 }) 
  
  
 top_pop = reactive({
   # define most-popular
   pop = pop_score()
   pop = pop[pop>0]
   which(pop_score() %in% sort(pop, decreasing = TRUE)[1:(input$top_prop*length(pop)/100)])
 })
 

  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)

  output$plot = renderPlot({
    
    # compute word frequency in popular articles
    word_counts_pop = tapply(dtm$v[dtm$i %in% top_pop()], terms[dtm$i %in% top_pop()], sum)

    wordcloud_rep(names(word_counts_pop), word_counts_pop, scale=c(4,0.5),
                  min.freq = 1, max.words=100,
                  colors=brewer.pal(8, "Dark2"))
  })
  
  
  
  output$print = renderPrint({head(names(word_counts_pop))})
  
})