library(shiny)
library(shinythemes)
library(wordcloud)
library(rvest)
library(tm)
library(ggplot2)
source("../code/analysis/naiveBayes_predict_doc.R")
source("../code/analysis/important_terms.R")
source("../code/parse_link.R")
source("../code/parse_article.R")
load("../output/ind_popular_Oct14.RData")
load("../output/dtm_unstemmed_Oct14.RData")
load("../output/naive Bayes/NB_Oct14.RData")
load("../output/naive Bayes/all_log_p.RData")


# compute word frequency
terms = dtm$dimnames$Terms[dtm$j]
word_counts = tapply(dtm$v, terms, sum)

shinyServer(function(input, output) {
  
  ################################  FIRST TAB #########################################
  
  # read type of popular for word analysis
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
    
    indicative_terms = important_terms(dtm, top_pop_ind(), len=100, n_doc_min=5)
    
    wordcloud_rep(names(indicative_terms), indicative_terms, scale = c(4,0.5),
                  max.words = input$num_words,
                  colors = brewer.pal(8, "Dark2"))
  })
  
  
  ################################  SECOND TAB #########################################
  
  # get content to analyze, either from url or from text box
  content = reactive({
    
    if(!is.null(input$doc) && grepl('[:alpha:]', input$doc))
      return(input$doc)
    
    else{
      doc = parse_article(input$url)
      return(doc$content)
    }
    

  })

  # calculate score (log difference of p's) of sample article
  score = reactive({
    NB = switch(input$popular2, 
           'ME' = NB$NB_me, 
           'MV' = NB$NB_mv, 
           'MS' = NB$NB_ms 
    )
    posterior = naiveBayes_predict_doc(NB, content())
    score = posterior$log_p_y - posterior$log_p_noty
    if(score > 100) score=100
    if(score < -100) score=-100
    return(score)
  }) 
  
  # select population of posteriors based on type of popular
  log_p = reactive({
    switch(input$popular2, 
           'ME' = all_log_p$ME, 
           'MV' = all_log_p$MV, 
           'MS' = all_log_p$MS 
    )
  }) 
  

  output$popular_score = renderPlot({
    
    diffs = log_p()$log_p_y - log_p()$log_p_noty
    diffs = diffs[!is.na(diffs)]
    
    score_x = score()
    percentile = 100*mean(score_x>diffs, na.rm=TRUE)
    percentile = paste(as.character(round(percentile)), '%', sep='')
    
    xmin = -.5
    xmax = .5
    if(score_x < xmin) score_x = xmin
    if(score_x > xmax) score_x = xmax
    diffs = diffs[diffs>xmin & diffs<xmax]
    
    # plot:
    ggplot(NULL,aes(diffs))  +
      ggtitle("How this article scores relative to database") + 
      geom_histogram(bins=100, color="white") +
      geom_vline(xintercept = 0, col='white', lwd=1, lty=2) +
      theme(title=element_text(size=18),
            axis.text=element_text(size=14),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position="none",
            panel.background=element_blank()) + 
      geom_point(aes(x=score_x, y=0), shape=23, color="black", fill='red', size=11, stroke=2) + 
      annotate("text", x=score_x, y=0, label=percentile, size=4, color='white')
  })
  
})
