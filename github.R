# Nikita Ushakov | ID: 15313300
# Load the API packages
install.packages(c("jsonlite", "httpuv", "httr", "magrittr", "plotly", "dplyr"))
library(jsonlite)
library(httpuv)
library(httr)
library(magrittr)
library(dplyr)
detach(package:plotly, unload=T)

# Save base enpoint as variable
url_git <- 'https://api.github.com/users/'

# Set and get my github token 
token <- config(token = oauth2.0_token(oauth_endpoints("github"), 
                              oauth_app(appname = "RGitHub_Access", 
                              key = "6141c2c391718b210db8", 
                              secret = "d9248f312fbd73f3cbbf3f4cdb54807fbfc6118d")))

# Returns a dataframe with information on the Current Users Followers 
getFollowers <- function(username)
{
    i <- 1
    x <- 1
    followersDF <- data.frame()
    
    while(x != 0)
    {
        followers           <- GET(paste0(url_git, username, "/followers?per_page=100&page=", i), token)
        followersContent    <- content(followers)
        currentFollowersDF  <- lapply(followersContent, function(x) 
        {
            df <- data.frame(user = x$login, userID = x$id, followersURL = x$followers_url, followingURL = x$following_url)
        }) %>% bind_rows()

        i <- i + 1
        x <- length(followersContent)
        followersDF <- rbind(followersDF, currentFollowersDF)
    }

    return(followersDF)
}
