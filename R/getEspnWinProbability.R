#' @export
getEspnWinProbability <- function(iGameId, iSport = "ncaam") {

    base_url <- "https://www.espn.com/"

    if (iSport %in% "ncaam") {

        base_url <- paste0(base_url,"mens-college-basketball/game?gameId=")

    } else if (iSport %in% "nba") {

        base_url <- paste0(base_url,"nba/game?gameId=")

    } else {

        base_url <- paste0(base_url,"nfl/game?gameId=")

    }

    returnGameProbability <- NULL

    for (game in iGameId) {

        target_url <- paste0(base_url, game)

        print(target_url)

        html <- read_html(target_url)

        homeTeam <- html %>% html_nodes(".home-team") %>% html_text()
        awayTeam <- html %>% html_nodes(".away-team") %>% html_text()

        homeWinPct <- as.numeric(html %>% html_nodes(".value-home") %>% html_text() %>% str_replace("%", "")) / 100
        awayWinPct <- 1 - homeWinPct

        if (length(homeWinPct) > 0) {

            gameProbability <- data.frame(c(homeTeam, awayTeam), c(awayWinPct, homeWinPct))
            colnames(gameProbability) <- c("team", "probability")
            returnGameProbability <- rbind(returnGameProbability, gameProbability)

        }

    }

    return(returnGameProbability)

}
