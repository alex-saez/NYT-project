

check_article <- function(article){
  
  for(i in c(1:3,5,7:8))   
    if(nchar(article[[i]])==0 || length(article[[i]])==0 || is.na(article[[i]]))
      add_to_log(paste("MISSING",names(article[i]), "in:", article$url)) 
      
  if(nchar(article$author)==0 || length(article$author)==0 || is.na(article$author))
    if(length(intersect(article$tags, c("letter", "letters", "interactive")))==0 &&
       !grepl("/realestate/homes-for-sale-", article$url) &&
       !grepl("/nyregion/things-to-do-", article$url))
      add_to_log(paste("MISSING author in:", article$url))    
  
}

