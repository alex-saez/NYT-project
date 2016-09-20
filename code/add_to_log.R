
add_to_log <- function(message){
  filename = "./data/log.txt"
  connection = file(description=filename, open="at")
  newline = paste("\n", message)
  cat(newline, file=connection, append=TRUE)
  close(connection)
}
  