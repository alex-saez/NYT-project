

get_curr_date <- function(){
  d = Sys.Date()
  d = as.character(d)
  d = strsplit(d,"-")
  d = as.numeric(d[[1]])
  d = convert_date(d)
  return(d)
}
