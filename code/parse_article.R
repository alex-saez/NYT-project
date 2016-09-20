
parse_article <- function(link, tags=character()){
  
  useless_tags = c("n.y./region",
                   "u.s.",
                   "world",
                   "science",
                   "technology",
                   "magazine",
                   "theater",
                   "movies",
                   "health",
                   "theopinionpages",
                   "opinion",
                   "art&design",
                   "theupshot",
                   "sundayreview", 
                   "travel", 
                   "books", 
                   "timesinsider",
                   "tmagazine",
                   "realestate",
                   "paulkrugman",
                   "rossdouthat",
                   "thepubliceditorsjournal", 
                   "energy&environment")

  
  # extract info from link 
  link_info = parse_link(link, tags)
  id = link_info$id
  url = link_info$url
  section = link_info$section
  tags = link_info$tags

  # extract info from page:
  
  page = html_session(link)
  
  title = html_text(html_nodes(page,"#headline"))
  author = html_text(html_nodes(page,".byline-author, .simple-byline-author"))
  date = html_text(html_nodes(page,".dateline, .simple-byline-date"))
  content = html_text(html_nodes(page,".story-content"))
  if(length(content)>1) content = content[-length(content)]
  
  # get "byline" label, can be author or tag
  if(length(html_nodes(page,".simple-byline-column"))!=0){
    byline = html_text(html_nodes(page,".simple-byline-column"))
    if(length(html_nodes(page,".simple-byline-date"))!=0){
      date = html_text(html_nodes(page,".simple-byline-date"))
      byline = gsub(date,"",byline)
    }
    byline = gsub("\n","",byline)
    byline = gsub("  ","",byline)
    byline = byline[1]
    
    if(length(author)==0){ # if there's no author, use this as author
      author = byline
    }
    else{
      extra_tag = tolower(byline) # otherwise, add it as an extra tag
      extra_tag = gsub(" ","",extra_tag)
      tags = c(tags, extra_tag)
      }
  }
  
  
  if(length(html_nodes(page,".kicker a"))!=0){
    extra_tag = html_text(html_nodes(page,xpath = "//span[@class='kicker-label']//a[@href]"))
    extra_tag = gsub("\n","",extra_tag)
    extra_tag = gsub(" ","",extra_tag)
    extra_tag = tolower(extra_tag[1])
    tags = c(tags, extra_tag)
  }
  
  if(length(html_nodes(page,"h3.kicker>span.pipe"))!=0){
    kicker = html_text(html_nodes(page,"h3.kicker"))
    extra_tag = strsplit(kicker,"|", fixed=TRUE)[[1]][2]
    extra_tag = gsub("\n","",extra_tag)
    extra_tag = gsub(" ","",extra_tag)
    extra_tag = tolower(extra_tag[1])
    tags = c(tags, extra_tag)
  }
  
    
  if(any(tags=="opinionator")){
    opinionator_tag = html_text(html_nodes(page,xpath = "//span[@class='kicker']//a[@href]"))
    opinionator_tag = gsub("\n","",opinionator_tag)
    opinionator_tag = gsub(" ","",opinionator_tag)
    tags = c(tags, tolower(opinionator_tag))
  }
  
  if(any(tags=="interactive")){
    title = html_text(html_nodes(page,".story-heading"))
    author = html_text(html_nodes(page,".byline-author"))
    date = html_text(html_nodes(page,".dateline"))
    content = html_text(html_nodes(page,"p")) 
    content = clean_special(content)
  }
  
  
  if(!is.na(section) && section=="blogs"){
    title = html_text(html_nodes(page,".entry-title"))    
    author = html_text(html_nodes(page,xpath = "//span[@class='fn']"))
    date = html_text(html_nodes(page,".dateline"))
    content = html_text(html_nodes(page,".story-body-text")) 
    #content = clean_special(content)
    if(any(tags=="krugman")) author = "paul krugman"
    if(any(tags=="douthat")) author = "ross douthat"
    
  }
  
  title = iconv(title[1], "UTF-8", "UTF-8")  
  title = tolower(title)
  author = iconv(author, "UTF-8", "UTF-8")  
  author = tolower(author)  
  date = tolower(date[1])
  tags = gsub("-","",tags)
  tags = gsub("â€™","",tags)
  tags = tags[!is.element(tags, useless_tags)]
  tags = tags[!grepl("@",tags)]
  content = paste(content, collapse=" ")
  content = iconv(content, "UTF-8", "UTF-8")
  content = tolower(content)
  
  
  return(list(id=id,
              url=url, 
              title=title, 
              author=list(unique(author)),
              section=section, 
              tags=list(unique(tags)), 
              date=date, 
              content=content)
         )
}

