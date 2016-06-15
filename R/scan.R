#' Generate a HTML Page of GitHub Tile Charts
#'
#' @param pkg packages to include in the page
#' @param img.dir where the images are stored
#' @param viewer.pane (unused) logical. Use the inbuilt viewer pane?
#'
#' @return location of the stored .html file
#' 
#' \feedbackfooter{'jonocarroll/githubtools'}{'R/scan.R'}{TRUE}
#' 
#' @export
#' 
build_html <- function(pkg=NULL, img.dir=".", viewer.pane=FALSE) {
  
  img.dir <- normalizePath(img.dir, mustWork=FALSE)
  
  if(is.null(pkg)) {  
    img_files <- dir(img.dir, pattern="*.png", full.names=TRUE)
  } else {
    all_img_files <- dir(img.dir, pattern=".png", full.names=TRUE)
    img_matches <- vapply(X=sub("/","~",pkg), FUN=grepl, FUN.VALUE=logical(length(all_img_files)), all_img_files)
    img_files <- all_img_files[apply(img_matches, 1, any)]
  }
  gh_html <- paste0('<html>\n   <head><h1>Generated: ',lubridate::today(),'</h1></head>\n   <body>
                    <table cellspacing="0" border="0" style="margin:0; padding:0;">\n')
  for(i in img_files) {
    gh_html <- paste0(gh_html, '      <tr>
                      <td><img style="width:100%; max-height:600px;" src="file://',i,'"</img></td>
                      </tr>\n')
  }
  gh_html <- paste(gh_html, '      </table>\n   </body>\n</html>')
  myViewer <- getOption("viewer")
  if(!viewer.pane) {
    htmlfile <- paste0(img.dir,"/all_gh_img.html")
    message(paste0("Storing ",htmlfile," then loading it."))
    write(gh_html, file=htmlfile)
    myViewer(url=htmlfile)
  } else {
    warning("This currently isn't working. Maybe try a pull-request with a solution?")
    # tmpfile <- tempfile(pattern="all_gh_img.html", fileext=".html")
    # write(gh_html, file=tmpfile)
    # myViewer(tmpfile, height=400)
  }
  # viewer  <- getOption("viewer")
  return(htmlfile)
  
}

# Make automated paging till response is empty
# https://github.com/cscheid/rgithub/issues/30#issuecomment-150354560
#' Automatically roll-over a search when paginated
#'
#' @param f call to execute
#' @param max.pages max number of pages to traverse (exits earlier if fail or complete)
#'
#' @return evaluates the call and returns the content
#' 
#' \feedbackfooter{'jonocarroll/githubtools'}{'R/scan.R'}{TRUE}
#' 
#' @export
#'
auto.page <- function(f, max.pages=10) {
  
  f_call <- substitute(f)
  stopifnot(is.call(f_call))
  
  i <- 1
  req <- list()
  result_lst <- list()
  
  repeat {
    
    # message(paste0("obtaining page ",i))
    message(".", appendLF=FALSE)
    
    # Specify the page to download
    f_call$page <- i
    
    req <- tryCatch({
      eval(f_call, parent.frame())}, 
      error = function(e) {
        stop(e)
      })
    if(inherits(req, "try-error")) stop("something went wrong with the scrape (autopage)")
    
    # Last page has empty content
    if (length(req$content) <= 0) break
    
    result_lst[[i]] <- req$content
    i <- i + 1
    
    # only get max max.pages pages
    if(i > max.pages) {
      message(paste0(" truncating at ",max.pages*30L," commits."), appendLF=TRUE)
      break
    }
  }
  
  result_req <- req
  result_req$content <- unlist(result_lst, recursive = FALSE)
  
  (result_req)
}

#' Obtain commit stats for one or more GitHub repos
#'
#' @param pkg character vector of packages to analyse (uses all if not set/\code{NULL})
#' @param img.dir where to store the generated images
#' @param max.pages maximum number of paginated results (30 commits per page) to scan
#' @param ViewHTML logical. If \code{TRUE}, save the images to \code{img.dir} for loading into a 
#'  HTML page, otherwise sequentially view the \code{ggplot} objects.
#'
#' @return vector of repositories analysed (as author/repos)
#' 
#' \feedbackfooter{'jonocarroll/githubtools'}{'R/scan.R'}{TRUE}
#' 
#' @import github
#' @import magrittr
#' @import dplyr
#' @import ggplot2
#' 
#' @export
#'
check_all_github <- function(pkg=NULL, img.dir=".", max.pages=10, ViewHTML=TRUE) {
  
  img.loc <- normalizePath(img.dir, mustWork = FALSE)
  
  # equivalent to 
  # gh_list <- view_all_sources(github.only=FALSE)
  # but intermediate values required
  all_inst <- installed.packages()
  pkg_list <- devtools::session_info(rownames(all_inst))$packages
  gh_list  <- pkg_list[grepl("Github",pkg_list$source), ]
  
  gh_pkg_loc          <- dplyr::add_rownames(data.frame(lib = all_inst[, 2][names(all_inst[, 2]) %in% gh_list$package]), "package")
  gh_pkg_loc$full_lib <- apply(gh_pkg_loc[,c("lib", "package")], 1, paste, collapse = "/")
  
  gh_list$repo    <- sub("Github \\((.*?)@.*","\\1", gh_list$source)
  gh_list$author  <- sub("(^.*?)\\/.*","\\1",        gh_list$repo)
  gh_list$repodir <- sub(".*?\\/(.*)","\\1",         gh_list$repo)
  gh_list$age     <- lubridate::today() - as.Date(gh_list$date)
  
  ## logic to determine if each pkg is 
  ## a) an installed package via author/repo; installed &  fullname
  ## b) an installed package via just repo;   installed & !fullname
  ## c) an external package via author/repo; !installed &  fullname
  ## d) an external package via repo;        !installed & !fullname - can't work with this
  if (!is.null(pkg)) {
    inst.det <- data.frame(pkg, installed = rep(NA, length(pkg)), fullname = rep(NA, length(pkg)))
    for (j in seq_along(pkg)) {
      if (pkg[j] %in% gh_list$repo) {
        inst.det[j,"installed"] <- TRUE
        inst.det[j,"fullname"]  <- TRUE
      } else if (pkg[j] %in% gh_list$repodir) {
        inst.det[j,"installed"] <- TRUE
        inst.det[j,"fullname"]  <- FALSE
      } else {
        message(paste0(pkg[j]," could not be found in your library, assuming you're just curious."))
        inst.det[j,"installed"] <- FALSE
        if (grepl("/", pkg[j])) {
          inst.det[j,"fullname"] <- TRUE
        } else {
          stop(paste0(pkg[j]," doesn't appear to be the full name of a repo and no package of that name is in your library. Nothing more I can do."))
        }
      }
    }
  } else {
    message("Scanning all installed packages")
    inst.det <- data.frame(pkg = gh_list$repo, installed = TRUE, fullname = TRUE)
    pkg <- gh_list$repo
  }
  
  ## grrr... testing against character(0) is a bad idea. just do the full logic
  if (any(inst.det$installed)) {
    if (length(pkg[inst.det$installed & inst.det$fullname]) > 0 & 
        length(pkg[inst.det$installed & !inst.det$fullname]) > 0) {
      gh_list <- gh_list[is.element(gh_list$repo, pkg[inst.det$installed & inst.det$fullname]) | 
                           is.element(gh_list$repodir, pkg[inst.det$installed & !inst.det$fullname]), ]
    } else if (length(pkg[inst.det$installed & inst.det$fullname]) > 0 & 
               length(pkg[inst.det$installed & !inst.det$fullname]) == 0) { 
      gh_list <- gh_list[is.element(gh_list$repo, pkg[inst.det$installed & inst.det$fullname]), ]
    } else if (length(pkg[inst.det$installed & inst.det$fullname]) == 0 & 
               length(pkg[inst.det$installed & !inst.det$fullname]) > 0) { 
      gh_list <- gh_list[is.element(gh_list$repodir, pkg[inst.det$installed & !inst.det$fullname]), ]
    } else if (length(pkg[inst.det$installed & inst.det$fullname]) == 0 & 
               length(pkg[inst.det$installed & !inst.det$fullname]) == 0) { 
      stop("I can do nothing more with this.")
    }
  } else {
    gh_list <- gh_list[0,]
  }
  
  if (any(!inst.det$installed)) {
    gh_list <- rbind(gh_list, 
                     data.frame(package = sub(".*/", "", pkg[!inst.det$installed & inst.det$fullname]),
                                `*`     = NA,
                                version = NA,
                                date    = NA,
                                source  = NA,
                                repo    = pkg[!inst.det$installed & inst.det$fullname],
                                author  = sub("/.*", "", pkg[!inst.det$installed & inst.det$fullname]),
                                repodir = sub(".*/", "", pkg[!inst.det$installed & inst.det$fullname]),
                                age     = NA, 
                                check.names = FALSE, stringsAsFactors = FALSE)
    )
  }
  
  full_list <- gh_list
  
  if (dir.exists(img.loc)) {
    message(paste0("Found ", img.loc, ", saving images there."))
  } else {
    message(paste0("Could not find directory ", img.loc, ", attempting to create it."))
    tryCatch(dir.create(img.loc), 
             error = function(e) stop(e), 
             finally = message("Directory created, saving images there.")
    )
  }
  
  github_setup()
  
  for (i in 1:nrow(full_list)) {
    
    this.pkg.installed <- inst.det$installed[i]
    this.full          <- full_list[full_list$repo == inst.det$pkg[i] | full_list$repodir == inst.det$pkg[i], ]
    
    message(paste0("Obtaining stats for ", inst.det$pkg[i], " "), appendLF = FALSE)
    
    year_ago <- format(lubridate::today() - lubridate::days(365), "%Y-%m-%dT%H:%M:%SZ")
    ghres    <- auto.page(github::get.repository.commits(this.full$author, this.full$repodir, since = year_ago), 
                          max.pages = max.pages)
    if (!ghres$ok) stop("something went wrong with the scrape (returned !ok)")
    commit_dates <- unlist(lapply(lapply(lapply(ghres$content, "[[", "commit"), "[[", "author"), "[[", "date"))
    
    contribsDF        <- data.frame(commit_dates, commits = 1, stringsAsFactors = FALSE)
    contribsDF$date   <- as.Date(contribsDF$commit_dates, format = "%Y-%m-%dT%H:%M:%SZ")
    contribsDF_agg    <- contribsDF %>% group_by(date) %>% summarise(nCommits = n()) %>% 
      merge(data.frame(date = seq(min(contribsDF$date), max(contribsDF$date), "days")), all = TRUE)
    contribsDF_agg[is.na(contribsDF_agg)] <- 0
    
    gh_data <- prepare_for_github_chart(data_agg = contribsDF_agg, primaryData = "nCommits")
    
    ## add a red tile for the date this package was installed (if it was)
    if (this.pkg.installed) gh_data$data[gh_data$data$date == this.full$date, "t.fill"] <- 7
    
    gg <- create_github_chart(gh_data, user, network = "GitHub")
    
    if (this.pkg.installed) {
      gg <- gg + labs(title = paste0(this.full$repo, " -- ", as.integer(this.full$age, units = "days"), " days old")) 
    } else {
      gg <- gg + labs(title = paste0(this.full$repo, " -- ", " (not installed locally)")) 
    }
    
    if (!ViewHTML) {
      message(paste0("\nPlotting ", this.full$repo), appendLF = TRUE)
      print(gg)
      if (nrow(full_list) > 1) grDevices::devAskNewPage(ask = TRUE)
    } else {
      ggsave(gg, file = paste0(file.path(img.loc,sub("/","~", this.full$repo)), ".png"), height = 2, width = 10)
    }
    
    message("", appendLF = TRUE)
    
  }
  
  return(full_list$repo)
  
}

#' Scan and analyse GitHub R packages
#'
#' @param pkg package to check (local or external)
#' @param img.dir where to store generated images 
#' @param ViewHTML logical. If \code{TRUE}, load the relevant images from \code{img.dir} into a 
#'  HTML page.
#'
#' @return NULL (used for the side effect of generating a .html file in \code{img.dir})
#' 
#' \feedbackfooter{'jonocarroll/githubtools'}{'R/scan.R'}{TRUE}
#' 
#' @export
#'
scan_gh_pkgs <- function(pkg=NULL, img.dir=".", max.commits=200, ViewHTML=TRUE) {
  
  npages <- ceiling(max.commits %/% 30L + (max.commits %% 30L)/30L)
  pkg_list <- check_all_github(pkg, img.dir, max.pages=npages, ViewHTML=ViewHTML)
  if(ViewHTML) build_html(pkg_list, img.dir)
  
}

#' View Data Related to All Installed Packages
#' 
#' This may return a large \code{data.frame} depending on how many packages
#' you have installed (not just loaded).
#'
#' @return data.frame of installed package information
#' 
#' \feedbackfooter{'jonocarroll/githubtools'}{'R/scan.R'}{TRUE}
#' 
#' @export
#'
view_all_sources <- function(github.only=FALSE) {
  
  # all_inst <- utils::installed.packages()
  # pkg_list <- devtools::session_info(rownames(all_inst))$packages
  
  all_inst <- installed.packages()
  pkg_list <- devtools::session_info(rownames(all_inst))$packages
  gh_list  <- pkg_list[grepl("Github",pkg_list$source), ]
  
  if(!github.only) return(pkg_list)
  if(github.only)  return(gh_list)
  
}