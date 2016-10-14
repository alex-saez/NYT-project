
add_article <- function(database, link, tags=character()){
  
  link = gsub('http://http://', 'http://', link)
  
  if(grepl("?",link)) # trim link
    link = strsplit(link, "?", fixed = TRUE)[[1]][1]
  
  
  if(!check_link(link)){
    # add_to_log(paste("EXCLUDED:", link))
  }
  else{ # if not excluded
    
    article = parse_article(link, tags)      
    
    if(!(article$url %in% database$url)){  # if not in the database
      
      check_article(article) # displays warning messages if fields are missing
      
      ind = nrow(database) + 1
      
      database[ind,]$id = article$id
      database[ind,]$url = article$url
      database[ind,]$title = article$title
      database[ind,]$author = article$author
      database[ind,]$section = article$section
      database[ind,]$tags = article$tags
      database[ind,]$date = article$date
      database[ind,]$content = article$content
      # new fields:
      database[ind,]$date_in = get_curr_date()
      database[ind,]$most_emailed = 0
      database[ind,]$most_shared = 0
      database[ind,]$most_viewed = 0
      
    }
  }
  
  return(database)
  
}

