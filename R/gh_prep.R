
#' Prepare Daily Data Ready for Use in a GitHub-Style Chart
#'
#' @param data_agg aggregated daily data
#' @param primaryData name of the primary data column to plot daily
#' @param secondaryData name of the secondary data column to label each daily tile (optional).
#'
#' @return a list object ready for \link{\code{create_github_chart}}.
#'
#' @export
#'
prepare_for_github_chart <- function(data_agg, primaryData, secondaryData="dummy") {
  
  ## if the secondary column wasn't added, make sure it's there now
  if (secondaryData == "dummy") { 
    data_agg$dummy <- -1 
    # data_agg[secondaryData <- data_agg$dummy
  }
  data_agg["primaryData"]   <- data_agg[primaryData]
  data_agg["secondaryData"] <- data_agg[secondaryData]
  if (!all(c("date", primaryData, secondaryData) %in% names(data_agg))) stop("Columns missing from input.")
  
  if (min(data_agg$date) <= lubridate::today() - lubridate::years(1)) {
    ## restrict to the last year
    data_agg <- data_agg %>% filter(date > lubridate::today() - lubridate::years(1))
  } else {
    ## extend to the last year
    data_agg %<>% 
      merge(data.frame(date = seq(lubridate::today() - lubridate::years(1), min(data_agg$date), "days"), 
                       primaryData = -1, 
                       secondaryData = -1), 
            all = TRUE)
  }
  if (max(data_agg$date) <= lubridate::today()) {
    ## add data up to today()
    data_agg %<>% merge(data.frame(date = seq(max(data_agg$date), lubridate::today(), "days"), 
                                   primaryData = -1, 
                                   secondaryData = -1), 
                        all = TRUE)
  }
  
  data_agg$secondaryTF <- NA_character_
  data_agg[data_agg["secondaryData"] > 0, "secondaryTF"] <- data_agg[data_agg["secondaryData"] > 0, "secondaryData"]
  data_agg[!data_agg["secondaryData"] > 0, "secondaryTF"] <- ""
  
  data_agg$t.fill <- cut(unlist(data_agg["primaryData"]), breaks = c(-1,0,1,5,10,20,1e5,1e6), right = FALSE, labels = 1:7)
  
  ## split into weeks
  data_agg$c.week  <- cut(data_agg$date, breaks = "week", start.on.monday = FALSE, labels = FALSE)
  data_agg$c.month <- lubridate::month(data_agg$date, abbr = TRUE, label = TRUE)
  data_agg$c.day   <- as.integer(lubridate::wday(data_agg$date))
  
  ## unique values of month
  rl        <- rle(as.character(data_agg$c.month))
  month.pos <- data_agg$c.week[cumsum(rl$lengths)]
  month.pos <- month.pos[-length(month.pos)]
  month.lab <- rl$values[-1]  
  
  gh_object <- list(data          = data_agg, 
                    month.pos     = month.pos, 
                    month.lab     = month.lab, 
                    primaryData   = primaryData, 
                    secondaryData = secondaryData)
  
  return(gh_object)
  
}



#' Create a GitHub-style Tile Chart with Social Network Color Palette
#'
#' @param gh_data data prepared with \link{\code{prepare_for_github_chart}}
#' @param user user ID to add to the subtitle
#' @param network which color palette to use, styled after a network (GitHub, Twitter, or StackOverflow). 
#'  Case insensitive, but will be also be used in the title.
#'
#' @return a ggplot2 object for printing
#' 
#' @export
#'
create_github_chart <- function(gh_data, user, network = c("GitHub", "Twitter", "StackOverflow")) {
  
  gg <- ggplot(gh_data$data, aes(x = c.week, y = c.day, label = gh_data$primaryData))
  gg <- gg + geom_tile(aes(fill = gh_data$data$t.fill), color = "white", size = 0.75)
  gg <- gg + geom_text(aes(label = gh_data$data$secondaryTF), size = 3, col = "grey20")
  gg <- gg + scale_x_continuous(limits = c(0, max(gh_data$data$c.week) + 1), breaks = gh_data$month.pos, labels = gh_data$month.lab)
  gg <- gg + scale_y_reverse(breaks = seq(1,7,1), labels = c("","M","","W","","F",""))
  gg <- gg + theme_github()
  gg <- gg + scale_fill_social(network)
  gg <- gg + labs(x = "", y = "")
  gg <- gg + coord_fixed(ratio = 1)
  return(gg)
}
