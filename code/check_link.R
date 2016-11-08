

check_link <- function(link){
  
  islinkgood= TRUE
  
  if(is.na(link)) islinkgood= FALSE

  if(length(link)>1) islinkgood= FALSE
  
  
  # exclude links not strating in http:
  if(islinkgood && substr(link, 1, 4) != "http") islinkgood= FALSE
  
  
  
  # ##   TEMP  #########################################
  # link_parts = strsplit(link, "/")[[1]]
  # if(islinkgood && strsplit(link_parts[3],".", fixed=TRUE)[[1]][2]=="blogs")
  #   islinkgood= FALSE
  # 
  # ###########################################
  
  
  # exclude links to a "page not found":
  if(islinkgood && html_session(link)$response$status_code==404) islinkgood= FALSE
  
  # exclude links to a "page unavailable":
  if(islinkgood && html_session(link)$response$status_code==410) islinkgood= FALSE
  
  
  
  link_parts = strsplit(link, "/")[[1]]
  
  
  # exclude links with not enough fields (usually not articles):
  if(islinkgood && length(link_parts)<5) islinkgood= FALSE
  
  # exclude other sites:
  if(islinkgood && !grepl("nytimes.com",link_parts[3])) islinkgood= FALSE
  
  # exclude videos:
  if(islinkgood && link_parts[4]=="video") islinkgood= FALSE
  
  # exclude slideshows:
  if(islinkgood && grepl("slideshow",link_parts[4])) islinkgood= FALSE
  
  # exclude room-for-debate:
  if(islinkgood & link_parts[4]=="roomfordebate") islinkgood= FALSE
  
  # exclude the-strip:
  if(islinkgood & grepl("thestrip",link_parts[4])) islinkgood= FALSE
  
  # exclude projects:
  if(islinkgood && link_parts[4]=="projects") islinkgood= FALSE
  if(islinkgood && length(link_parts)>4 & link_parts[5]=="projects") islinkgood= FALSE
  
  # exclude congress votes:
  if(islinkgood & link_parts[4]=="congress") islinkgood= FALSE
  
  # exclude live:
  if(islinkgood & link_parts[4]=="live") islinkgood= FALSE
  
  # exclude recipes:
  if(islinkgood && link_parts[4]=="recipes") islinkgood= FALSE
  
  # exclude indexes:
  if(islinkgood && link_parts[4]=="indexes") islinkgood= FALSE
  
  # exclude spotlight (T-Magazine):
  if(islinkgood && link_parts[4]=="spotlight") islinkgood= FALSE
  
  # exclude column:
  if(islinkgood & link_parts[4]=="column") islinkgood= FALSE
  
  # exclude community:
  if(islinkgood && link_parts[4]=="community") islinkgood= FALSE
  
  # exclude news-event:
  if(islinkgood & link_parts[4]=="news-event") islinkgood= FALSE
  
  # exclude newsletters:
  if(islinkgood & link_parts[4]=="newsletters") islinkgood= FALSE
  
  # exclude cooking:
  if(islinkgood && grepl("cooking",link_parts[3])) islinkgood= FALSE

  
  
  # exclude links to other main pages:
  if(islinkgood && link_parts[4]=="pages") islinkgood= FALSE
  if(islinkgood && link_parts[4]=="section") islinkgood= FALSE
  
  # exclude T-Magazine past issues links:
  if(islinkgood && link_parts[length(link_parts)]=="past-issues.html") islinkgood= FALSE
  
  # exclude Oscars ballot link:
  if(islinkgood && grepl("oscars.nytimes.com/2016/ballot", link)) islinkgood= FALSE
  
  
#   a = strsplit(link_parts[3],".", fixed=TRUE)
#   ind = which(a[[1]]=="nytimes")
#   if(islinkgood && a[[1]][ind-1] != "www")
#     add_to_log(paste("SUSPECT LINK: ",link))

  return(islinkgood)
  
    
}

