library(shiny)


type_popular = list("Most emailed articles" = "ME",
               "Most viewed articles" = "MV",
               "Most shared articles" = "MS")

shinyUI(
  fluidPage(
    
      # Application title
      titlePanel("Overrepresented words in popular New York Times articles"),
      
      sidebarLayout(
        # Sidebar with a slider and selection inputs
        sidebarPanel(
          
          selectInput("popular", 
                      h4("Type of popular article:"),
                      choices = type_popular),
                    
          hr(),
          
          sliderInput("top_prop",
                      h4("Top percentile of popular (lower = more popular):"),
                      min = 1,  max = 100, value = 100),
          
          hr(),
          
          sliderInput("num_words",
                      tags$small("Number of words to display:"),
                       min = 10,  max = 100, value = 30),
          
          submitButton("Update")
          
        ),

        # Show Word Cloud
        mainPanel(
          plotOutput("wordcloud", width = "100%"), 
          tags$small("Note: This plot displays words whose frequency in popular articles is much higher that their baseline frequency.
                     It is based on all articles published in The New York Times from March 15th to October 14th 2016.  The New
                     York Times compiles three daily lists of popular articles: Mose Emailed, Most Viewed and Most Shared. For each of 
                     these categories, each article received a score according to its rank within the list and the duration it remained 
                     on the list. The slider allows to select a top percentile of these scores.")
        )
        

      )
      
      
    )
)