
label_popular <- function(database, link, type, importance){
  
  if(!(type %in% c("most emailed","most shared","most viewed")))
    stop("Wrong category of popular article")
  
  link_info = parse_link(link)
  url = link_info$url
  
  if(url %in% database$url){
    
    ind = which(url==database$url)
    
    if(type=="most emailed")
      database$most_emailed[ind] = database$most_emailed[ind] + importance
    
    if(type=="most shared")
      database$most_shared[ind] = database$most_shared[ind] + importance
    
    if(type=="most viewed")
      database$most_viewed[ind] = database$most_viewed[ind] + importance
  }
  
  else{ # if not in the database
    add_to_log(paste("NOT FOUND", type,"Importance=", round(importance), link))
  }
  
  return(database)
  
}

