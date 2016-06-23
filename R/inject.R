#' @export
install_github <- function(repo, username = NULL, ref = "master", subdir = NULL, 
                           auth_token = devtools:::github_pat(quiet), host = "api.github.com", 
                           force = TRUE, quiet = FALSE, ...) {
  
  message("Warning: this function has the potential to do damage to your R setup. 
It interferes with the devtools install process and injects HTML
into the help files.

*** DO NOT USE THIS FUNCTION ON PRODUCTION/MISSION CRITICAL SETUPS ***

Best results are obtained by starting with a fresh R session.

Refer to http://github.com/jonocarroll/githubtools for further disclaimers.
")
  
  continueYN <- readline(prompt = "If you are okay with continuing, please type YES and hit Enter.  ")
  
  waive_blame <- tolower(continueYN)
  stopifnot(waive_blame == "yes")
  
  remotes <- lapply(repo, devtools:::github_remote, username = username, 
                    ref = ref, subdir = subdir, auth_token = auth_token, 
                    host = host)
  if (!isTRUE(force)) {
    remotes <- Filter(function(x) devtools:::different_sha(x, quiet = quiet),
                      remotes)
  }
  githubtools::install_remotes(remotes, quiet = quiet, ...)
}

#' @export
install_remotes <- function(remotes, ...) {
  invisible(vapply(remotes, githubtools::install_remote, ..., FUN.VALUE = logical(1)))
}

#' @export
install_remote <- function(remote, ..., quiet=FALSE) {
  ## hijack devtools:::install_remote to inject some HTML into help files
  
  stopifnot(devtools:::is.remote(remote))
  
  if (any(grepl(remote$repo, installed.packages()[,1]))) utils::remove.packages(remote$repo)
  
  bundle <- devtools:::remote_download(remote, quiet = FALSE) # quiet = FALSE to force re-install
  on.exit(unlink(bundle), add = TRUE)
  source <- devtools:::source_pkg(bundle, subdir = remote$subdir)
  on.exit(unlink(source, recursive = TRUE), add = TRUE)
  metadata <- devtools:::remote_metadata(remote, bundle, source)
  
  message("*** INJECTING HTML CODE INTO HELP FILE ***")
  allrfiles <- dir(file.path(source, "R"), full.names = TRUE)
  
  for (ifile in allrfiles) {
    
    cat(paste0("injecting to ",basename(ifile), "\n"))
    
    injection <- paste0("#' \\if{html}{\\Sexpr[stage=render, results=text]{githubtools:::feedback_footer(", 
                        "'",remote$username,"/",remote$repo,"',",
                        "'R/",basename(ifile),"')}}")
    
    rcontent <- file(ifile, "r")
    
    allLines <- readLines(rcontent, n = -1)
    
    ## find roxygen functions
    ## hooking into @export works if there aren't examples, 
    ## otherwise the injection is treated as an example.
    # returnLines <- which(grepl("#'[ ]+@export", allLines))
    # STILL FAILS IF AFTER @inheritParams or @import
    ## NEED A BETTER INJECTION POINT
    roxyBlocks <- which(grepl("^#'", allLines))
    runs <- split(roxyBlocks, cumsum(seq_along(roxyBlocks) %in% (which(diff(roxyBlocks)>1)+1)))
    # runs = split(seq_along(roxyBlocks), cumsum(c(0, diff(roxyBlocks) > 1)))
    # cat(paste0("runs has length ",length(runs)))
    if (length(runs) > 0) {
      # roxyLines <- lapply(runs, function(x) allLines[roxyBlocks[x]])
      roxyLines <- lapply(runs, function(x) allLines[x])
      for (iblock in seq_along(runs)) {
        if (length(roxyLines[[iblock]]) > 5) { ## skip over helper files
          # exampleLine <- which(grepl("^#'[ ]+@examples", roxyLines[[iblock]]))
          exportLine <- which(grepl("^#'[ ]+@export", roxyLines[[iblock]])) ## check that the fn is exported
          if (length(exportLine) != 0) {
              injectLine <- 2 ## just after the one-line title... should be safe(r)
              allLines[runs[[iblock]][injectLine]] <- paste0("#'\n", injection, "\n#'\n", allLines[runs[[iblock]][injectLine]])
            # if (length(exampleLine) == 0) {
            #   injectLine <- exportLine 
            #   allLines[runs[[iblock]][injectLine]] <- paste0("#'\n#'\n", injection, "\n#'\n", allLines[runs[[iblock]][injectLine]])
            # # } else if (exportLine < exampleLine) {
            # #   injectLine <- exportLine
            # #   allLines[runs[[iblock]][injectLine]] <- paste0("#'\n", injection, "\n#'\n", allLines[runs[[iblock]][injectLine]])
            # } else {
            #   injectLine <- min(exampleLine)
            #   allLines[runs[[iblock]][injectLine]] <- paste0("#'\n#'\n", injection, "\n#'\n", allLines[runs[[iblock]][injectLine]])
            # }
          }
        }
      }
    }
    # returnLines <- which(grepl("#'[ ]+@export", allLines))
    
    ## write out the file, but inject the footer code before @export
    
    # savedLines <- allLines  
    # message(paste0("*** INJECTING HTML CODE INTO ",basename(ifile)," HELP FILE ***"))
    # allLines[returnLines] <- paste0(allLines[returnLines], "\n\n", injection, "\n\n")
    # allLines[returnLines] <- paste0("#'\n", injection, "\n#'\n", allLines[returnLines])
    
    cat(allLines, file = ifile, sep = "\n")
    cat(allLines, file = paste0("/home/jono/tmpR/",basename(ifile)), sep = "\n")
    
    close(rcontent)
    
  }
  
  cat(source)
  
  file.copy(paste0(source,"/NAMESPACE"), "~/tmpR/")
  
  message("*** REBUILDING HELP FILES WITH INJECTED CODE ***")
  devtools::document(pkg = source)
  message("DOCUMENTED.")
  retCode <- devtools:::install(source, ..., quiet = quiet, metadata = metadata)
  
  ## re-write the documentation
  # devtools::document(pkg = as.package(remote$repo))
  
  # install(source, ..., quiet = quiet, metadata = metadata)
  return(invisible(retCode))
}