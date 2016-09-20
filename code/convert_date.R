

convert_date <- function(date){
  
  if(!any(is.na(date))){
    
    if(length(date)!=3 | date[1]<2000 | date[1]>2029 | date[2]>12 | date[3]>31) 
      stop("date is not in the form c(YYYY,MM,DD)")
    
    year_days = rep(365,30)
    year_days[seq(1,30,4)]=366
    month_days = c(31,28,31,30,31,30,31,31,30,31,30,31)
    
    # define date of article as number of days since Dec. 31th 1999
    date_code = sum(year_days[0:(date[1]-1-1999)]) + sum(month_days[0:(date[2]-1)]) + date[3]
    
    # if this is leap year
    if(year_days[date[1]-1999]==366 && date[2]>2) date_code = date_code + 1
  }
  else date_code= NA
  
  return(date_code)
  
}

