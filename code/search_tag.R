
# returs logical vector with where the tag was found in database

search_tag = function(database, tag){
  tag_found = logical(nrow(database))
  for(i in 1:nrow(database)){
    tag_found[i] = tag %in% database$tags[i][[1]]
  }
  
  return(tag_found)
}