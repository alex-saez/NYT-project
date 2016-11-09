
type_popular = list("Most emailed articles" = "ME",
                    "Most viewed articles" = "MV",
                    "Most shared articles" = "MS")

sample_url = "http://www.nytimes.com/2016/10/02/style/how-to-express-sympathy.html"

shinyUI(
  
  navbarPage(tags$em("New York Times Project"), 
             id="nav",
             theme = shinytheme('flatly'),
             
             ################################  FIRST TAB #########################################
             
             tabPanel(tags$em("Word Analysis"),
                      titlePanel("Overrepresented words in popular New York Times articles"),
                      sidebarLayout(
                        sidebarPanel( # control panel
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
                        
                        mainPanel( # word cloud panel
                          plotOutput("wordcloud", width = "100%"), 
                          hr(),
                          tags$small("Note: This plot displays words whose frequency in popular articles is much higher that their baseline frequency.
                     It is based on all articles published in The New York Times from March 15th to October 14th 2016.  The New
                     York Times compiles three daily lists of popular articles: Most Emailed, Most Viewed and Most Shared. For each of 
                     these categories, each article received a score according to its rank within the list and the duration it remained 
                     on the list. The slider allows to select a top percentile of these scores.")
                        )
                      )
             ),
             
             
             
             ################################  sECOND TAB #########################################
             tabPanel(tags$em("Score an article"),
                      titlePanel("Score an article"),
                      sidebarLayout(
                        sidebarPanel( # control panel
                          textInput('url', 'Enter URL:', value=sample_url, placeholder=sample_url),
                          textAreaInput('doc', 'Or enter text:', height = '400px', value=NULL),
                          selectInput("popular2", "Type of popular article:", choices=type_popular),
                          submitButton("Update")
                        ),
                        
                        mainPanel( # popularity score distribution
                          plotOutput("popular_score", width = "100%"), 
                          hr(),
                          tags$small("Note: Score is defined as log odds ratio for an article to belong in the category of most popular articles 
                                     selected on the left. Shown in grey is the (truncated) distribution of scores for all articles in the database
                                     (~42,000). Scores greater than zero correspond to articles more likely than not to be among the most popular.
                                     The red diamond indicates where the current article scores and its percentile in the database (e.g. 61% 
                                     means that the article has a score higher than that of 61% of all articles in the database)")
                        )
                      )
             )
             
  )
)