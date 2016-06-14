build_html <- function(img.dir=".") {
  
  img_files <- dir(img.dir, pattern="*.png", full.names=TRUE)
  gh_html <- paste0('<html>\n   <head><h1>Generated: ',lubridate::today(),'</h1></head>\n   <body>
                    <table cellspacing="0" border="0" style="margin:0; padding:0;">\n')
  for(i in img_files) {
    gh_html <- paste0(gh_html, '      <tr>
                      <td><img style="width:100%; max-height:600px;" src="',i,'"</img></td>
                      </tr>\n')
  }
  gh_html <- paste(gh_html, '      </table>\n   </body>\n</html>')
  write(gh_html, file=paste0(img.dir,"/all_gh_img.html"))
  viewer  <- getOption("viewer")
  viewer(url=paste0(img.dir,"/all_gh_img.html"))
}

# Make automated paging till response is empty
# https://github.com/cscheid/rgithub/issues/30#issuecomment-150354560
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
    if(i > max.pages) break
  }
  
  result_req <- req
  result_req$content <- unlist(result_lst, recursive = FALSE)
  
  (result_req)
}

#' Obtain commit stats for one or more GitHub repos
#'
#' @param pkg
#' @param img.dir 
#'
#' @return
#' 
#' @import github
#' @import magrittr
#' @import dplyr
#' @import ggplot2
#' 
#' @export
#'
#' @examples
check_all_github <- function(pkg=NULL, img.dir=".") {
  
  img.loc <- normalizePath(img.dir, mustWork=FALSE)
  
  all_inst <- installed.packages()
  pkg_list <- devtools::session_info(rownames(all_inst))$packages
  gh_list  <- pkg_list[grepl("Github",pkg_list$source), ]
  
  gh_pkg_loc          <- dplyr::add_rownames(data.frame(lib=all_inst[,2][names(all_inst[,2]) %in% gh_list$package]), "package")
  gh_pkg_loc$full_lib <- apply(gh_pkg_loc[,c("lib", "package")], 1, paste, collapse="/")
  
  gh_list$repo    <- sub("Github \\((.*?)@.*","\\1", gh_list$source)
  gh_list$author  <- sub("(^.*?)\\/.*","\\1",        gh_list$repo)
  gh_list$repodir <- sub(".*?\\/(.*)","\\1",         gh_list$repo)
  gh_list$age     <- lubridate::today() - as.Date(gh_list$date)
  
  ## logic to determine if each pkg is 
  ## a) an installed package via author/repo; installed &  fullname
  ## b) an installed package via just repo;   installed & !fullname
  ## c) an external package via author/repo; !installed &  fullname
  ## d) an external package via repo;        !installed & !fullname - can't work with this
  if(!is.null(pkg)) {
    installed <- rep(NA, length(pkg))
    fullname  <- rep(NA, length(pkg))
    for(j in seq_along(pkg)) {
      if (pkg[j] %in% gh_list$repo) {
        installed[j] <- TRUE
        fullname[j]  <- TRUE
      } else if (pkg[j] %in% gh_list$repodir) {
        installed[j] <- TRUE
        fullname[j]  <- FALSE 
      } else {
        message(paste0(pkg[j]," could not be found in your library, assuming you're just curious."))
        installed[j] <- FALSE
        if (grepl("/",pkg[j])){
          fullname[j] <- TRUE
        } else {
          stop(paste0(pkg[j]," doesn't appear to be the full name of a repo and no package of that name is in your library. Nothing more I can do."))
        }
      }
    }
  } else {
    message("using all")
    installed <- rep(TRUE, nrow(gh_list))
    fullname  <- rep(TRUE, nrow(gh_list))
    # pkg <- gh_list$repo
  }
  
  # print(pkg)
  # print(installed)
  # print(fullname)
  
  ## grrr... testing against character(0) is a bad idea. just do the full logic
  if(any(installed)) {
    if (length(pkg[installed & fullname])>0 & length(pkg[installed & !fullname])>0) {
      gh_list <- gh_list[pkg[installed & fullname] == gh_list$repo | pkg[installed & !fullname] == gh_list$repodir, ]
    } else if (length(pkg[installed & fullname])>0 & length(pkg[installed & !fullname])==0) { 
      gh_list <- gh_list[pkg[installed & fullname] == gh_list$repo, ]
    } else if (length(pkg[installed & fullname])==0 & length(pkg[installed & !fullname])>0) { 
      gh_list <- gh_list[pkg[installed & !fullname] == gh_list$repodir, ]
    } else if (length(pkg[installed & fullname])==0 & length(pkg[installed & !fullname])==0) { 
      stop("I can do nothing more with this.")
    }
  } else {
    # message("none installed")
    gh_list <- gh_list[0,]
  }
  
  # print(gh_list)
  
  if(any(!installed)) {
    # message("found non-installed packages")
    gh_list <- rbind(gh_list, data.frame(package=sub(".*/","",pkg[!installed & fullname]),
                                         `*`=NA,
                                         version=NA,
                                         date=NA,
                                         source=NA,
                                         repo=pkg[!installed & fullname],
                                         author=sub("/.*","",pkg[!installed & fullname]),
                                         repodir=sub(".*/","",pkg[!installed & fullname]),
                                         age=NA, check.names=FALSE)
    )}
  
  # full_list <- merge(gh_list, gh_pkg_loc, by="package")
  full_list <- gh_list
  
  # print(full_list)
  # print(nrow(gh_list))
  # stop("deliberate")
  
  # img.loc <- path.expand(img.dir)
  # img.loc <- img.dir
  if(dir.exists(img.loc)) {
    message(paste0("Found ",img.loc,", saving images there."))
  } else {
    message(paste0("Could not find directory ",img.loc,", attempting to create it."))
    tryCatch(dir.create(img.loc), error=function(e) stop(e), finally=message("Directory created, saving images there."))
  }
  
  for(i in 1:nrow(full_list)) {
    
    message(paste0("Obtaining stats for ", full_list$repo[i], " "), appendLF=FALSE)
    
    year_ago <- format(lubridate::today() - lubridate::days(365), "%Y-%m-%dT%H:%M:%SZ")
    ghres     <- auto.page(github::get.repository.commits(full_list$author[i],full_list$repodir[i],since=year_ago))
    if(!ghres$ok) stop("something went wrong with the scrape (returned !ok)")
    commit_dates <- unlist(lapply(lapply(lapply(ghres$content, "[[", "commit"), "[[", "author"), "[[", "date"))
    
    contribsDF        <- data.frame(commit_dates, commits=1, stringsAsFactors=FALSE)
    contribsDF$c.date <- as.Date(contribsDF$commit_dates, format="%Y-%m-%dT%H:%M:%SZ")
    contribsDF_agg    <- contribsDF %>% group_by(c.date) %>% summarise(nCommits=n()) %>% merge(data.frame(c.date=seq(min(contribsDF$c.date),max(contribsDF$c.date),"days")), all=TRUE)
    contribsDF_agg[is.na(contribsDF_agg)] <- 0
    
    
    if (min(contribsDF_agg$c.date) <= lubridate::today() - lubridate::years(1)) {
      ## restrict to the last year
      contribsDF_agg <- contribsDF_agg %>% filter(c.date > lubridate::today() - lubridate::years(1))
    } else {
      ## extend to the last year (e.g. 3200 limit reached or too few)
      contribsDF_agg %<>% merge(data.frame(c.date=seq(lubridate::today() - lubridate::years(1), 
                                                      min(contribsDF_agg$c.date), "days"), nCommits=-1), all=TRUE)
    }
    if (max(contribsDF_agg$c.date) <= lubridate::today()) {
      ## add data up to today()
      contribsDF_agg %<>% merge(data.frame(c.date=seq(max(contribsDF_agg$c.date), lubridate::today(), "days"), nCommits=-1), all=TRUE)
    }
    
    contribsDF_agg$c.fill <- as.character(cut(contribsDF_agg$nCommits, breaks=c(-1,0,1,5,10,20,1e5), 
                                              right=FALSE, labels=c("#bbbbbb", "#eeeeee","#d6e685","#1e6823","#8cc665","#44a340")))
    contribsDF_agg[contribsDF_agg$c.date==full_list$date[i],"c.fill"] <- "#ff0000"
    
    ## split into weeks
    contribsDF_agg$c.week  <- cut(contribsDF_agg$c.date, breaks="week", start.on.monday=FALSE, labels=FALSE)
    contribsDF_agg$c.month <- lubridate::month(contribsDF_agg$c.date, abbr=TRUE, label=TRUE)
    contribsDF_agg$c.day   <- as.integer(lubridate::wday(contribsDF_agg$c.date))
    contribsDF_agg$id      <- rownames(contribsDF_agg)
    
    ## unique values of month
    rl <- rle(as.character(contribsDF_agg$c.month))
    month.pos <- contribsDF_agg$c.week[cumsum(rl$lengths)]
    month.pos <- month.pos[-length(month.pos)]
    month.lab <- rl$values[-1]
    
    gg <- ggplot(contribsDF_agg, aes(x=c.week, y=c.day))
    gg <- gg + geom_tile(fill=contribsDF_agg$c.fill, color="white", size=0.75)
    gg <- gg + scale_fill_manual(values=contribsDF_agg$c.fill, guide=FALSE)
    gg <- gg + scale_x_continuous(limits=c(0,max(contribsDF_agg$c.week)+1), breaks=month.pos, labels=month.lab)
    gg <- gg + scale_y_reverse(breaks=seq(1,7,1), labels=c("","M","","W","","F",""))
    gg <- gg + theme_minimal()
    gg <- gg + theme(panel.grid.major=element_blank(),
                     panel.grid.minor=element_blank())
    gg <- gg + labs(x="", y="", title=paste0(full_list[i,"repo"]," -- ",as.integer(full_list[i,"age"],units="days")," days old"))
    gg <- gg + coord_fixed(ratio=1)
    
    ggsave(gg, file=paste0(file.path(img.loc,sub("/","~",full_list[i,"repo"])),".png"), height=2, width=10)
    
  }
  
}

scan_gh_pkgs <- function(img.dir=".") {
  
  check_all_github(img.dir)
  build_html(img.dir)
  
}

view_all_sources <- function() {
  
  all_inst <- utils::installed.packages()
  pkg_list <- devtools::session_info(rownames(all_inst))$packages
  
  return(pkg_list)
  
}