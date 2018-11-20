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

