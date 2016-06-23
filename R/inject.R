#' Install a GitHub package with injected HTML in the help file 
#' 
#' Behaviour is otherwise identical to \code{\link[devtools]{install_github}} except 
#' that some HTML code is carefully inserted in the roxygen2 header. Processing of the 
#' roxygen2 code into a .Rd \code{\link[utils]{help}} file is also hijacked and HTML 
#' sanitisation is deactivated (for that call only). The injected HTML (static, not 
#' user-changeable for now) overlays a pull-up tab at the bottom of HTML help files 
#' (such as viewed in RStudio) with some context of the GitHub package, such as links 
#' to the source, issues page, version, and author.
#' 
#' @details Warning
#' \strong{This function has potential to make damaging changes to your R library, and 
#' should not be executed on production or mission-critical setups.} You are invited to carefully 
#' scrutinize the source code \url{http://github.com/jonocarroll/githubtools} to ensure that 
#' nothing malicious is being done here. 
#' 
#' @section Limitations:
#' This function is not currently able to install GitHub packages that it itself depends on. Doing so 
#' results in failure to re-load the namespace and that's not good. This of course means that it can't 
#' self-document with the injected HTML.
#' 
#' The full consequences of changing the default parameters has not been explored. Most of the code for 
#' this function calls devtools functions, but there is no guarantee attached to any of it.
#' 
#' @section If something goes wrong:
#' If you do find a bug that causes something to go wrong, please file an Issue on GitHub. Some steps to 
#' try and remedy the failure that I've found to work include
#' \itemize{
#'   \item Restarting the R session and trying again,
#'   \item Manually removing the offending package with (\code{utils::\link[utils]{remove.packages}}),
#'   \item Manually deleting the library folder for the offending package, 
#'   \item Installing the GitHub or CRAN version of the package with the standard tools, 
#'   (i.e. \code{utils::\link[utils]{install.packages}} or \code{devtools::\link[devtools]{install_github}}).
#' }
#' 
#' @inheritParams devtools::install_github
#'
#' @references \url{http://github.com/jonocarroll/githubtools}
#'
#' @examples
#' \dontrun{
#' install_github("jonocarroll/butteRfly")
#' } 
#'
#' @export
install_github <- function(repo, username = NULL, ref = "master", subdir = NULL, 
                           auth_token = devtools:::github_pat(quiet), host = "api.github.com", 
                           force = TRUE, quiet = FALSE, ...) {
  
  ## prevent attempts to remove/re-install a package that githubtools is itself dependent on
  ## e.g. removing ggplot2 screws up loading the githubtools namespace
  ghtDeps <- gtools::getDependencies("githubtools")
  reqRepo <- sub(".*/", "", repo)
  if (reqRepo %in% ghtDeps) stop("Not currently able to remove/re-install a package that githubtools depends on.")
  
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
    
    # cat(paste0("injecting to ",basename(ifile), "\n"))
    
    injection <- paste0("#' \\if{html}{\\Sexpr[stage=render, results=text]{githubtools:::github_overlay(", 
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
    
    close(rcontent)
    
  }
  
  # cat(source)
  
  ## add the GitHub logo to the package help
  manfigdir <- file.path(source, "man/figures")
  if (!dir.exists(manfigdir)) dir.create(manfigdir)
  file.copy(from = system.file("extdata", 'GitHub-Mark-Light-64px.png', package = "githubtools"),
            to   = manfigdir)
  
  message("*** REBUILDING HELP FILES WITH INJECTED CODE ***")
  devtools::document(pkg = source)
  # message("DOCUMENTED.")
  retCode <- devtools:::install(source, ..., quiet = quiet, metadata = metadata)
  
  ## re-write the documentation
  # devtools::document(pkg = as.package(remote$repo))
  
  # install(source, ..., quiet = quiet, metadata = metadata)
  return(invisible(retCode))
}