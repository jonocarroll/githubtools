# This is the secret sauce.  It overrides tools:::htmlify in Rd2HTML so that
# it doesn't replace characters with HTML escapes.
# courtesy of @noamross
no_htmlify <- function() {
  my_htmlify <- function(x) return(x)
  R2Hframe = grep("^Rd2HTML", sapply(sys.calls(), function(a) paste(deparse(a), collapse = "\n")))
  if(length(R2Hframe) != 0) {
    assign("htmlify", my_htmlify, envir = sys.frame(which = max(R2Hframe)))
  }
}


html_raw <- function(text) {
  no_htmlify()
  return(text)
}

html_file <- function(filename) {
  no_htmlify()
  return(paste(readLines(system.file("doc", filename, package=packageName())), collapse = "\n"))
}

change_stylesheet <- function(sheet) {
  R2Hframe = grep("^Rd2HTML", sapply(sys.calls(), function(a) paste(deparse(a), collapse = "\n")))
  if(length(R2Hframe) != 0) {
    assign("stylesheet", file.path("..", "doc", sheet), envir = sys.frame(which = max(R2Hframe)))
  }
}

add_stylesheet <- function(sheet) {
  no_htmlify()
  if(!R.utils::isUrl(sheet)) {
    sheet = file.path("..", "doc", sheet)
  }
  return(paste0('<link rel="stylesheet" type="text/css" href="', sheet, '">'))
}

get_fn_name <- function() {
  R2Hframe = grep("^Rd2HTML", sapply(sys.calls(), function(a) paste(deparse(a), collapse = "\n")))
  #return(ls(envir = sys.frame(which = max(R2Hframe))))
  return(get("Rd", envir = sys.frame(which = max(R2Hframe)))[[2L]][[1L]])
}
