#' @export
lfltMeta <- function() {
  read_csv(system.file("meta.csv", package = "lfltmagic"))
}


#' @export
lfltWhich <- function(d){
  meta <- lfltMeta()
  if ("data.frame" %in% class(d)) {
    guessedctypes <- guessCtypes(d, as_string = TRUE) # TODO possibleFtypes
  } else {
    guessedctypes <- paste(d, collapse = "-")
  }
  meta %>% filter(ctypes == guessedctypes)
}


#' @export
lfltCtypes <- function(ctypesIn){
  meta <- lfltMeta()
  meta %>% filter(ctypes == ctypesIn)
}

#' @export
lfltNames <- function(){
  lfltMeta()$name
}
