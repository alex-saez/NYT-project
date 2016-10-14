library(shiny)


type_popular = list("Most emailed articles" = "ME",
               "Most viewed articles" = "MV",
               "Most shared articles" = "MS")

shinyUI(
  fluidPage(
    
      # Application title
      titlePanel("Word Analysis"),
      
      sidebarLayout(
        # Sidebar with a slider and selection inputs
        sidebarPanel(
          
          selectInput("popular", "Type of popular article:",
                      choices = type_popular),
                    
          hr(),
          
          sliderInput("top_prop",
                      "How popular (define top percentile):",
                      min = 1,  max = 100, value = 100), 
          
          submitButton("Update")
          
        ),

        # Show Word Cloud
        mainPanel(
          plotOutput("plot")
        )
        
        # mainPanel(
        #   textOutput("print")
        # )
        
      )
      
      
    )
)