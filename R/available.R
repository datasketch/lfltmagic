
#' @export
lfltWhich <- function(d){
  pf <- lfltFtype()
  ftype <- guessFtype(d) # TODO possibleFtypes
  names(keep(pf, ~ ftype %in% .))
}


#' @export
lfltvizList <- function(){
  db <- Rd_db("lfltmagic")
  meta <- unname(map_chr(db, tools:::.Rd_get_name))
  keep(meta, ~ grepl("^lflt_*$",.))
}


#' @export
lfltFtype <- function(lflt = NULL){
  db <- Rd_db("ciudatos")
  db <- db[grepl("^lflt_.*$",names(db))]
  meta <- lapply(db, tools:::.Rd_get_section, "section")
  cleanFtypeDoc <- function(ftype){
    ftype <- as.character(ftype[[2]][[2]])
    strsplit(gsub(" |\n","",ftype),",")[[1]]
  }
  meta <- lapply(meta,cleanFtypeDoc)
  names(meta) <- gsub(".Rd","",names(meta))
  if(!is.null(lflt)) return(meta[[lflt]])
  meta
}



#' #' @export
#' lfltList <- function(type = NULL,wrongNames = FALSE){
#'   #http://stackoverflow.com/questions/7495685/how-to-access-the-help-documentation-rd-source-files-in-r
#'   db <- Rd_db("lfltmagic")
#'   meta <- unname(map_chr(db, tools:::.Rd_get_name))
#'   meta <- meta[!grepl("lflt_test_docs",meta)]
#'   if(wrongNames) return(keep(meta, ~ !grepl("^lflt_.*\\.$",.)))
#'   lflts <- keep(meta, ~ grepl("^lflt_.*\\.$",.))
#'   if(!is.null(type))
#'     return(lflts[grepl(type,lflts)])
#'   lflts
#' }
#'
#' #' @export
#' lfltFtype <- function(lflt = NULL){
#'   db <- Rd_db("lfltmagic")
#'   meta <- map(db, tools:::.Rd_get_section, "section")
#'   meta <- meta[!grepl("lflt_test_docs",names(meta))]
#'   #ftype <- meta$lflt_test_docs.Rd
#'   safe_cleanFtypeDoc <- safely(lfltmagic:::cleanFtypeDoc)
#'   parsedMeta <- map(meta,safe_cleanFtypeDoc)
#'   results <- parsedMeta %>% map(~.$result)
#'   errors <- parsedMeta %>% map(~.$error) %>% purrr::discard(is.null)
#'   names(results) <- gsub(".Rd","",names(results))
#'   names(errors) <- gsub(".Rd","",names(errors))
#'   if(!is_empty(errors))
#'     stop("Something wrong with ftypes for:\n",paste(names(errors),collapse = "\n  "))
#'   if(!is.null(lflt)) return(results[[lflt]])
#'   results
#' }
#'
#'
#' cleanFtypeDoc <- function(ftype){
#'   sectionName <- as.character(ftype[[1]][[1]])
#'   if(sectionName != "ftypes") stop("No section name ftype")
#'   ftype <- as.character(ftype[[2]][[2]])
#'   strsplit(gsub(" |\n","",ftype),",")[[1]]
#' }
