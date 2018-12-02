# Nikita Ushakov | ID: 15313300
# Load the API packages
#install.packages(c("jsonlite", "httpuv", "httr", "magrittr", "plotly", "dplyr"))
require("jsonlite")
require("httpuv")
require("httr")
require("magrittr")
require("dplyr")
require("rlang")
detach(plotly, unload=T)

# Save base enpoint as variable
url_git <- 'https://api.github.com/users/'

# Set and get my github token 
token <- config(token = oauth2.0_token(oauth_endpoints("github"), 
                        oauth_app(appname = "Git_API", 
                        key = "e1dc9916e527c5bf68f7", 
                        secret = "1547dc09069d0e1eef0a3cc650fb2d9f9df5a5cc")))

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
        
        currentFollowersDF <- data.frame()
        tryCatch({
            currentFollowersDF  <- lapply(followersContent, function(x) 
            {
                df <- data.frame(user = x$login, userID = x$id, followersURL = x$followers_url, followingURL = x$following_url, stringsAsFactors=F)
            }) %>% bind_rows()
        }, error = function(cond) { 
            print(followersContent)
            message(cond) 
            Sys.sleep(3600.0) # API rate limit exceeded for user ID
            return(followersDF)
        } , warning = function(cond) {
            print(followersContent)
            message(cond) 
            Sys.sleep(3600.0) # API rate limit exceeded for user ID
            return(followersDF)
        })
        
        i <- i + 1
        x <- length(followersContent)
        if(i > 50) x <- 0 # Avoid big amount of folowers
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
        
        currentRepositoriesDF <- data.frame()
        tryCatch({
            currentRepositoriesDF   <- lapply(repositoriesContent, function(x) 
            {
                df <- data.frame(repo = x$name, id = x$id, commits = x$git_commits_url, language = x$languages, stringsAsFactors=F)
            }) %>% bind_rows()
        }, error = function(cond) {
            print(repositoriesContent)
            message(cond)
            Sys.sleep(3600.0) # API rate limit exceeded for user ID
            return(repositoriesDF)
        } , warning = function(cond) {
            print(repositoriesContent)
            message(cond) 
            Sys.sleep(3600.0) # API rate limit exceeded for user ID
            return(repositoriesDF)
        })
        
        i <- i + 1
        x <- length(repositoriesContent)
        if(i > 50) x <- 0 # Avoid big amount of folowers
        repositoriesDF <- rbind(repositoriesDF, currentRepositoriesDF)
    }
    
    return(repositoriesDF)
}

# Returns a number of folowers and repositories on the Current User
getNumberRepositoriesAndFollowers <- function(username)
{
    followers            <- GET(paste0(url_git, username), token)
    followersContent     <- content(followers)

    numberOfFolowers     <- 0
    numberOfRepositories <- 0
    currentAmountDF <- data.frame(reponum = 0, folowersnum = 0)
    tryCatch({
        numberOfFolowers     <- followersContent$followers
        numberOfRepositories <- followersContent$public_repos
    }, error = function(cond) {
        message(cond)
        Sys.sleep(3600.0) # API rate limit exceeded for user ID
        return(currentAmountDF)
    } , warning = function(cond) {
        message(cond) 
        Sys.sleep(3600.0) # API rate limit exceeded for user ID
        return(currentAmountDF)
    })
    
    currentAmountDF <- data.frame(reponum = numberOfFolowers, folowersnum = numberOfRepositories)
    return(currentAmountDF)
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
            currentLanguageDF <- data.frame(repo = repositoriesName, language = "No language specified", stringsAsFactors=F)
        }
        else
        {
            currentLanguageDF <- data.frame(repo = repositoriesName, language = repositoriesLanguage, stringsAsFactors=F)
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
    x   <- data.frame(table(z$language), stringsAsFactors=F)

    pie <-  plot_ly(data = x, labels = ~Var1, values = ~Freq, type = 'pie') %>% layout(title = paste('Languages used by Github User :', username))

    return(pie)
}

# Returns a dataframe giving the number of followers and number of repos a user has
getFollowersInformation <- function(username)
{
    followersDF <- getFollowers(username)
    numberOfFollowers <- length(followersDF$userID)
    followersUsernames <- followersDF$user
    dataDF <- data.frame()

    #cat("Amount of folowers: ", numberOfFollowers, "\n")
    
    for(i in 1:numberOfFollowers)
    {
        userName              <- followersUsernames[[i]]
        if(!length(userName)) break
        userDF                <- getNumberRepositoriesAndFollowers(userName)
        numberOfRepositories  <- userDF$reponum
        numberOfFollowers     <- userDF$folowersnum
        bindDF                <- data.frame(userName, numberOfRepositories, numberOfFollowers, stringsAsFactors=F)
        dataDF                <- rbind(dataDF, bindDF)
        
        i <- i + 1;
    }
    
    return(dataDF)
}

# Load another important plotting package
require(plotly)

# Generate data for followers and repository starting at user 'paulmillr'
currentUser         <- "paulmillr"
x                   <- getFollowers(currentUser)
followersUsernames  <- x$user
numberOfFollowers   <- length(x$userID)
info                <- getFollowersInformation(currentUser)

i <- 1
size <- nrow(info)
while(size < 10000)
{
    current <- followersUsernames[[i]]
    #cat("Main start", current, "\n")
    bind    <- getFollowersInformation(current)
    info    <- rbind(bind, info)
    i       <- i + 1
    if(i > size-1) break
}
info <- distinct(info)

# Use plotly to graph the relationship between a users number of followers and repositories 
scatter <-  plot_ly(data = info, x = ~numberOfFollowers, y = ~numberOfRepositories, type = "scatter", mode = "markers",
            text = ~paste("User: ", userName, '<br>Followers: ', numberOfFollowers, '<br>Repository:', numberOfRepositories),
            marker = list(size = 10, color = 'rgba(255, 182, 193, .9)',
            line = list(color = 'rgba(152, 0, 0, .8)', width = 2))) %>%
            layout(title = 'Relationship between Followers and Repositories', yaxis = list(zeroline = F), xaxis = list(zeroline = F),
            plot_bgcolor='rgba(204, 255, 229, 0.2)')
scatter

# Extracting data for users with over 1000 followers or repositories
mostFollowers       <- info[which(info$numberOfFollowers >= 1000), ]
mostFollowers$code  <- 1
mostRepos           <- info[which(info$numberOfRepositories >= 1000), ]
mostRepos$code      <- 0
combined <- rbind(mostFollowers, mostRepos)
scatter2 <- plot_ly(data = combined, x = ~numberOfFollowers, y = ~numberOfRepositories, color = ~code, colors = "Set1", type = "scatter", mode = "markers",
            text = ~paste("User: ", userName, '<br>Followers: ', numberOfFollowers, '<br>Repository:', numberOfRepositories)) %>%
            layout(title = 'Most Followers and Repositories', yaxis = list(zeroline = F), xaxis = list(zeroline = F),
            plot_bgcolor='rgba(204, 255, 229, 0.2)')
scatter2

# Language information about user
pie <- languagesVisualization(currentUser)
pie

# Save in CSV file
write.csv(info, file="followers.csv")