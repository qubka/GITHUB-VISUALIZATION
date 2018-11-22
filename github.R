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

# Returns a dataframe with information on the Current Users Repositories
getRepositories <- function(username)
{
    i <- 1
    x <- 1
    repositoriesDF <- data.frame()
    
    while(x != 0)
    {
        repositories            <- GET(paste0(url_git, username, "/repos?per_page=100&page=", i), token)
        repositoriesContent     <- content(repositories)
        currentRepositoriesDF   <- lapply(repositoriesContent, function(x) 
        {
            df <- data.frame(repo = x$name, id = x$id, commits = x$git_commits_url, language = x$languages)
        }) %>% bind_rows()
        
        i <- i + 1
        x <- length(repositoriesContent)
        repositoriesDF <- rbind(repositoriesDF, currentRepositoriesDF)
    }
    
    return(repositoriesDF)
}

# Returns a dataframe with the language used in each of the users repository
getLanguages <- function(username)
{
    repositoriesDF      <- GET(paste0(url_git, username, "/repos?per_page=100"), token)
    repositoriesContent <- content(repositoriesDF)
    
    i <- 1
    languageDF <- data.frame()
    numberOfRepositories <- length(repositoriesContent)
    
    for(i in 1:numberOfRepositories)
    {
        repositoriesLanguage  <- repositoriesContent[[i]]$language
        repositoriesName      <- repositoriesContent[[i]]$name
        
        if(is_null(repositoriesLanguage))
        {
            currentLanguageDF <- data.frame(repo = repositoriesName, language = "No language specified")
        }
        else
        {
            currentLanguageDF <- data.frame(repo = repositoriesName, language = repositoriesLanguage)
        }
        
        i <- i + 1
        languageDF <- rbind(languageDF, currentLanguageDF)
    }
    
    return(languageDF)
}

# Returns a pie chart which depicts the languages information for the current user
languagesVisualization <- function(username)
{
    z   <- getLanguages(username)
    x   <- data.frame(table(z$language))

    pie <-  plot_ly(data =x, labels = ~Var1, values = ~Freq, type = 'pie') %>% 
            layout(title = paste('Languages used by Github User', username), xaxis = list(showgrid = F, zeroline = F, showticklabels = F), yaxis = list(showgrid = F, zeroline = F, showticklabels = F))

    return(pie)
}

# Returns a dataframe giving the number of followers and number of repos a user has
getFollowersInformation <- function(username)
{
    followersDF <- getFollowers(username)
    numberOfFollowers <- length(followersDF$userID)
    followersUsernames <- followersDF$user
    dataDF <- data.frame()

    for(i in 1:numberOfFollowers)
    {
        userName             <- followersUsernames[i]
        repository           <- getRepositories(userName)
        followers            <- getFollowers(userName) 
        numberOfRepositories <- length(repository$repository)
        numberOfFollowers    <- length(followers$user)
        newRow               <- data.frame(userName, numberOfRepositories, numberOfFollowers)
        dataDF               <- rbind(dataDF, newRow)
        
        i <- i + 1;
    }
    
    return(dataDF)
}
