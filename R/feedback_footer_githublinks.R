#' @importFrom RCurl getURL
github_overlay = function(repo, file=NULL) {
  message("in footer")
  no_htmlify()
  # fn_name = get_fn_name()
  stylesheet = paste(readLines(system.file("doc", 'footer_style.css', package="htmlhelp")), collapse = "\n")
  # connection = !("try-error" %in% class(try(getURL('rating-widget.com'), silent=TRUE)))
  # script = ifelse(connection,
                  # paste(readLines(system.file("doc", 'rating-script.html', package="htmlhelp")), collapse = "\n"), '')
  script = ""
  
  repo = paste0('http://github.com/', repo)
  if(is.null(file)) {
    file = repo
  } else {
    file = paste0(repo, '/blob/master/', file)
  }
  new_issue = URLencode(paste0(repo, '/issues/new?body=', 'Feedback on `', 'SOMENAME', '()`.'))
  # rating = ifelse(connection, paste0('<div class="rw-ui-container" data-title="', packageName(), "-", fn_name, '"></div>'),
                  # '<div class="rw-ui-container">Internet connection required for ratings.</div>')
  rating = ""
  footer = sprintf('<div id="footer">
                   <span id="lift-anchor"><a>^</a></span>
                   %s
                   <span id="gh-links">
                   <img src="figures/GitHub-Mark-Light-64px.png">
                   <a href ="%s">Send feedback</a>
                   <a href="%s">Edit the function or doc</a>  </span>
                   </div>', rating, new_issue, file)

  return(paste('<style>', stylesheet, '</style>', script, footer, sep="\n"))

}

# get_stuff <- function() {
#   no_htmlify()
#   R2Hframe = grep("^Rd2HTML", sapply(sys.calls(), function(a) paste(deparse(a), collapse = "\n")))
#   #return(paste(ls(envir = sys.frame(which = max(R2Hframe))), collapse = "<br/>"))
#   return(get("out", envir = sys.frame(which = max(R2Hframe))))
# }
