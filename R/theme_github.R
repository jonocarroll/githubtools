#' ggplot2 theme based on the GitHub contribution tile chart
#'
#' Style a tile chart similar to those on \emph{GitHub} profiles.
#'
#' \code{theme_github} implements the standard green tile chart as 
#' seen on GitHub profiles. Alternatively, uses a different color scheme
#' in the style of another popular site, e.g. twitter, stackoverflow.
#'
#'
#' @return An object of class \code{\link{theme}}.
#'
#' @export
#'
#' @examples
#' library("ggplot2")
#' ## get data
#' p <- ggplot(commit_data) + geom_tile(aes(x=c.week, y=c.day)) + theme_github()
#'
#' ## Twitter colors
#' p + theme_github(alt="Twitter")
#' 
theme_github <- function() {
  
  # gg <- ggplot(contribsDF_agg, aes(x=c.week, y=c.day))
  # gg <- gg + geom_tile(fill=contribsDF_agg$c.fill, color="white", size=0.75)
  # gg <- gg + scale_fill_manual(values=contribsDF_agg$c.fill, guide=FALSE)
  # gg <- gg + scale_x_continuous(limits=c(0,max(contribsDF_agg$c.week)+1), breaks=month.pos, labels=month.lab)
  # gg <- gg + scale_y_reverse(breaks=seq(1,7,1), labels=c("","M","","W","","F",""))
  # gg <- gg + theme_minimal()
  # gg <- gg + theme(panel.grid.major=element_blank(),
  #                  panel.grid.minor=element_blank())
  # gg <- gg + labs(x="", y="", title=paste0(full_list[i,"repo"]," -- ",as.integer(full_list[i,"age"],units="days")," days old"))
  # gg <- gg + coord_fixed(ratio=1)
  
  ret <-
    theme_minimal() +
    theme(panel.grid.major=element_blank())
  # ret <- ret + scale_fill_manual(values=contribsDF_aret$c.fill, guide=FALSE)
  # ret <- ret + scale_x_continuous(limits=c(0,54), breaks=month.pos, labels=month.lab)
  ret <- ret + theme(panel.grid.major=element_blank(),
                     panel.grid.minor=element_blank(),
                     # axis.text.x=element_blank(),
                     axis.title=element_blank(),
                     plot.title=element_text(size=14))
  # ret <- ret + theme(aspect.ratio=1)
  # ret <- ret + labs(x="", y="", title=paste0(full_list[i,"repo"]," -- ",as.integer(full_list[i,"age"],units="days")," days old"))
  ret <- ret + theme(complete = TRUE)
  
  ret
}

#' @rdname scale_social
#' @export
scale_fill_social <- function(network=c("GitHub", "Twitter", "StackOverflow"), ...) {
  # discrete_scale("fill", "social", social_pal(network = network), breaks=c(-1,0,1,5,10,20,1e5), guide=FALSE, ...)
  discrete_scale("fill", "social", social_pal(network = network), guide=FALSE, drop=FALSE, ...)
}

#' Social networking color palettes (discrete)
#'
#' The hues in each palette are:
#' \itemize{
#' \item GitHub greens
#' \item Twitter blues
#' \item StackOverflow oranges
#' }
#'
#' @param network which palette to use
#'
#' @rdname scale_social
#' @export
#' @examples
#' library(scales)
#' show_col(social_pal("GitHub")(6))
#' show_col(social_pal("Twitter")(6))
#' show_col(social_pal("StackOverflow")(6))
social_pal <- function(network=c("GitHub", "Twitter", "StackOverflow")) {
  # function(n) {
  # assert_that(n > 0)
  # assert_that(n <= 6)
  function(n) {
  i <- switch(tolower(network),
              github = c("#bbbbbb","#eeeeee","#d6e685","#1e6823","#8cc665","#44a340","#ff0000"),
              twitter = c("#bbbbbb","#eeeeee","#9ecae1","#6baed6","#4292c6","#2171b5","#ff0000"),
              stackoverflow = c("#bbbbbb","#eeeeee","#fdae6b","#fd8d3c","#f16913","#d94801","#ff0000"))
  # unname(colors[i][seq_len(n)])
  }
  # return(i)
}
