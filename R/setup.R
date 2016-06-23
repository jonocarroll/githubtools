#' Perform GitHub OAuth setup.
#'
##' #@param config_file location of the configuration file, default ~/.githuburlcheckr
#'
##' #@param echo if TRUE print the credentials read from the file to the console.
#'
#' @return a github context object that is used in every github API call issued by the github package.
#'
#' @import github
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ctx <- github_setup()
#' }
# gith_setup <- function(config_file="~/.githuburlcheckr", echo=FALSE) {}
github_setup <- function() {
  
  # if(file.exists(config_file)) {
  #   config <- read.dcf(config_file, fields=c("CLIENT_ID", "CLIENT_SECRET"))
  #   Sys.setenv(GITHUB_CLIENT_ID = config[, "CLIENT_ID"])
  #   Sys.setenv(GITHUB_CLIENT_SECRET = config[, "CLIENT_SECRET"])
  #
  #   if(echo) {
  #     print(jsonlite::toJSON(as.list(Sys.getenv(c("GITHUB_CLIENT_ID",
  #                                                 "GITHUB_CLIENT_SECRET"))), pretty = TRUE))
  #   }
  #
  #   ctx <- github::interactive.login(client_id=Sys.getenv("GITHUB_CLIENT_ID"),
  #                                    client_secret=Sys.getenv("GITHUB_CLIENT_SECRET"),
  #                                    scopes=c("public_repo", "gist", "user"))
  
  ctx <- github::interactive.login(scopes=c("public_repo", "gist", "user"))
  
  # } else {
  #   warning("PLEASE ADD A ~/.githuburlcheckr FILE WITH CONFIGURATION\n
  #           CLIENT_ID: <YOURCLIENTID>\n
  #           CLIENT_SECRET: <YOURCLIENTSECRET>\n
  #           AWS_DEFAULT_REGION: <YOURDEFAULTREGION>")
  #
  # }
  
  return(ctx)
  
}
