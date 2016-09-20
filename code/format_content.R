

format_content <- function(content){
  content = iconv(content, "UTF-8", "latin1")
  content = gsub("'s","",content)
  content = gsub("' "," ",content)
  content = gsub("''","",content)
  content = gsub("'","XxXx",content)
  content = gsub("-"," ",content)
  content = removePunctuation(content)
  content = removeNumbers(content)
  content = gsub("XxXx","'",content)
  
  return(content)
}

