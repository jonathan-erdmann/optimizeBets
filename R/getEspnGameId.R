#' @export
getEspnGameId <- function(iDate, iSport = "ncaam") {

    base_url <- "https://www.espn.com/"

    if (iSport %in% "ncaam") {

        base_url <- paste0(base_url,"mens-college-basketball/schedule/_/date/")

    } else if (iSport %in% "nba") {

        base_url <- paste0(base_url,"nba/schedule/_/date/")

    } else {

        base_url <- paste0(base_url,"nfl/schedule/_/date/")

    }

    target_url <- paste0(base_url, str_replace_all(iDate, "-", ""))

    print(target_url)

    html <- read_html(target_url)

    links <- html %>% html_nodes("a") %>% html_attr("href")
    game_id <- str_replace(links[str_which(links, "gameId")], ".*=", "")
    game_id <- as.numeric(game_id[str_which(game_id, "[a-z]", negate = TRUE)])

    return(game_id)

}
