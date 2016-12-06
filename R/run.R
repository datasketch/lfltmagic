
#' @export
run_lflt <- function(d,lfltname){
  #lfltname <- "lflt_waffle."

  if(validateD(d,lfltname))
    do.call(lfltname,list(d))
  else
    stop("D did not validate")
}

validateD <- function(d,lfltname){
  guessFtype(d) %in% lfltFtype(lfltname)
}
