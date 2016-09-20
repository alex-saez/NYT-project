

get_comments <- function(link){
  page = html_session(link)
  comment_count = html_text(html_nodes(page, xpath = "//span[@class='count']"))
  comment_count = html_text(html_nodes(page, ".button-text"))
}