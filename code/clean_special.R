
clean_special <- function(content){
  #content = iconv(content, "UTF-8", "latin1")
  content = gsub("\n","",content)
  content = gsub("\t","",content)
  content = gsub("\r","",content)
  
  content = content[content!="Advertisement"]
  content = content[-1]
  
  return(content)
}


