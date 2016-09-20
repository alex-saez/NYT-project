
library(rvest)

setwd("/Users/Alex/Dropbox/NYT project/")

source("./code/parse_link.R")
source("./code/parse_article.R")
source("./code/convert_date.R")
source("./code/clean_special.R")
source("./code/check_link.R")
source("./code/check_article.R")
source("./code/add_article.R")
source("./code/get_curr_date.R")
source("./code/add_to_log.R")
source("./code/label_popular.R")


##################################################################

scrape_pages <- function(database=list()){
  
  pages = c("http://www.nytimes.com/pages/world/",  
            "http://www.nytimes.com/pages/business/",  
            "http://www.nytimes.com/pages/technology/",  
            "http://www.nytimes.com/pages/sports/",  
            "http://www.nytimes.com/pages/arts/",  
            "http://www.nytimes.com/pages/realestate/",  
            "http://www.nytimes.com/pages/opinion/",  
            "http://www.nytimes.com/pages/dining/")
  
  for(section in pages){
    cat("\n")
    
    page = html(section)
    links = html_attr(html_nodes(page, "h1 a, h2 a, h3 a"),"href")
    
    if(length(links)==0)
      add_to_log(paste("NO ARTICLES FOUND in:", section))    
    
    for(l in 1:length(links)){
      sectionname = strsplit(section, "/")[[1]][5]
      cat("\r", sectionname, ": ", l, "  of ", length(links))
      
      database = add_article(database, links[l])
    }
  }
  
  return(database)
  
}

##################################################################

scrape_sections <- function(database=list()){
  
  sections = c("http://www.nytimes.com/section/us/",  
               "http://www.nytimes.com/section/nyregion/",  
               "http://www.nytimes.com/section/science/",  
               "http://www.nytimes.com/section/health/",  
               "http://www.nytimes.com/section/fashion/",  
               "http://www.nytimes.com/section/travel/",  
               "http://www.nytimes.com/section/magazine/",  
               "http://www.nytimes.com/section/obituaries/",  
               "http://www.nytimes.com/section/t-magazine/",
               "http://www.nytimes.com/section/upshot/", 
               "http://www.nytimes.com/section/books/")
  
  for(section in sections){
    cat("\n")
    
    page = html(section)
    links = html_attr(html_nodes(page, ".headline a, #latest-panel .story-body a"),"href")

    if(length(links)==0)
      add_to_log(paste("NO ARTICLES FOUND in:", section))    
    
    for(l in 1:length(links)){
      sectionname = strsplit(section, "/")[[1]][5]
      cat("\r", sectionname, ": ", l, "  of ", length(links))
      
      database = add_article(database, links[l])
    }
  }

  return(database)
  
}


##################################################################

get_popular <- function(database=list()){
  # most emailed:
  cat("\n")
  section = "http://www.nytimes.com/services/xml/rss/nyt/pop_top.xml"
  page = html(section)
  links = html_text(html_nodes(page, css = "guid"))
  if(length(links)==0) stop(c("Unable to get Most Emailed list"))    
  for(i in 1:length(links)){    
    cat("\r", "Most Emailed:",i,"articles of",length(links))
    importance = 100*(length(links)+1-i)/length(links)
    database = label_popular(database, links[i], "most emailed", importance)
  }

  # most shared:
  cat("\n")
  section = "http://rss.nytimes.com/services/xml/rss/nyt/MostShared.xml"
  page = html(section)
  links = html_text(html_nodes(page, css = "guid"))
  if(length(links)==0) stop(c("Unable to get Most Shared list"))    
  for(i in 1:length(links)){    
    cat("\r", "Most Shared:",i,"articles of",length(links))
    importance = 100*(length(links)+1-i)/length(links)
    database = label_popular(database, links[i], "most shared", importance)
  }

  # most viewed:
  cat("\n")
  section = "http://rss.nytimes.com/services/xml/rss/nyt/MostViewed.xml"
  page = html(section)
  links = html_text(html_nodes(page, css = "guid"))
  if(length(links)==0) stop(c("Unable to get Most Viewed list"))    
  for(i in 1:length(links)){    
    cat("\r", "Most Viewed:",i,"articles of",length(links))
    importance = 100*(length(links)+1-i)/length(links)
    database = label_popular(database, links[i], "most viewed", importance)
  }
  
  return(database)
  
}

##################################################################
##################################################################

load("./data/database.RData")

n_prev = nrow(database)

add_to_log(paste(Sys.time()))

database = scrape_pages(database)
database = scrape_sections(database)
database = get_popular(database)

add_to_log(paste(Sys.time(), nrow(database)-n_prev, "articles added", 
                 "\n----------------------------------------------------------------------------------------"))

save(database, file="./data/database.RData")




# database = data.frame(id = numeric(),
#                       url = numeric(),
#                       title = numeric(),
#                       author = numeric(),
#                       section = numeric(),
#                       tags = numeric(),
#                       date = numeric(),
#                       content = numeric(), 
#                       date_in = numeric(), 
#                       most_emailed = numeric(), 
#                       most_shared = numeric(), 
#                       most_viewed = numeric())



