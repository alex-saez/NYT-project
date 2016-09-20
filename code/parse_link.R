
parse_link <- function(link, tags = character()){  
  
  if(grepl("?",link))
    link = strsplit(link, "?", fixed = TRUE)[[1]][1]
  if(grepl(".html",link))
    url = paste(strsplit(link, ".html")[[1]][1], ".html",sep = '')
  else url=link

  link_parts = strsplit(link, "/")[[1]]

  id = tail(link_parts,1)
  id = strsplit(id, ".html")[[1]][1]
  id = tolower(id)
    
  if(link_parts[4]=="interactive"){
    tags = c(tags, "interactive")
    if(!is.na(as.double(link_parts[5])) && as.double(link_parts[5])>1950 && is.na(as.double(link_parts[6]))){ 
      section = link_parts[6]
    }
    else{
      section = ifelse(length(link_parts)>7, link_parts[8], NA)
      if(length(link_parts)>9) tags = c(tags, link_parts[9]) 
    }
  }
  
  else if(link_parts[4]=="reuters"){
    tags = c(tags, "reuters")
    section = link_parts[8]
    if(length(link_parts)>9) tags = c(tags, link_parts[9]) 
  }
  
  else if(link_parts[4]=="aponline"){
    tags = c(tags, "aponline")
    section = link_parts[8]
    if(length(link_parts)>9) tags = c(tags, link_parts[9]) 
  }
  
  
  else if(strsplit(link_parts[3],".", fixed=TRUE)[[1]][2]=="blogs"){
    id = link_parts[7]
    section = "blogs"
    tags = c(tags, strsplit(link_parts[3],".", fixed=TRUE)[[1]][1])
    url = link
  }
  
  else if(link_parts[5]=="first-draft"){
    id = link_parts[9]
    section = "blogs"
    tags = c(tags, c("politics", "first-draft"))
  }
  
  else if(length(link_parts)>6 && link_parts[7]=="style"){
    section = "fashion"
    if(length(link_parts)>8) tags = c(tags, link_parts[8]) 
    tags = c(tags, "style")
  }
  
  
  else{
      section = link_parts[7]
      if(length(link_parts)>8) tags = c(tags, link_parts[8]) 
  }
  
  tags = gsub("-","",tags)

  
  return(list(id=id, 
              url=url, 
              section=section, 
              tags=unique(tags))
         )
}

