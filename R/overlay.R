#' @importFrom RCurl getURL
github_overlay = function(repo, file=NULL) {
  githubtools:::no_htmlify()
  stylesheet = paste(readLines(system.file("doc", 'footer_style.css', package = "githubtools")), collapse = "\n")
  repolink = paste0('http://github.com/', repo)
  if (is.null(file)) {
    file = repolink
  } else {
    file = paste0(repolink, '/blob/master/', file)
  }
  # new_issue = URLencode(paste0(repo, '/issues/new?body=', 'Feedback on `', 'SOMENAME', '()`.'))
  issue <- URLencode(paste0(repolink, '/issues/'))
  # gh_image <- system.file("extdata", 'GitHub-Mark-Light-64px.png', package = "githubtools")
  gh_image <- "figures/GitHub-Mark-Light-64px.png"
  footer = sprintf('<div style="clear: both;" id="footer">
                      <span id="lift-anchor"><a>^</a></span>
                      <span id="gh-links">
                         <img src="%s">
                         <a href="%s">View Source Code</a>
                         <a href="%s">Known Issues</a>
                         <a href="%s">Installed from: %s</a>
                      </span>
                   </div>', gh_image, repolink, file, issue, repo)

  return(paste('<style>', stylesheet, '</style>', footer, sep = "\n"))

                         # <img src="figures/GitHub-Mark-Light-64px.png">
}
