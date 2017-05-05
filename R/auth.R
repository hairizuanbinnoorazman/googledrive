.driveEnv <- new.env(parent = emptyenv())
.driveEnv$Token <- NULL

# Set token to environment
set_token <- function(value) {
  .driveEnv$Token <- value
  return(value)
}

# Get token from environment
get_token <- function() {
  .driveEnv$Token
}

#' Authorize R package to access Google Drive API
#' @description This is a function to authorize the R package to access the Googleslides API. If no
#' client_id and client_secret is provided, the package would provide predefined values in the package.
#'
#' Do take note that you should not be using the predefined values when using this package in proper
#' applications but to define your own client_id and client_secrets from Google Console.
#' @importFrom httr oauth_app oauth_endpoints oauth2.0_token
#' @param client_id OAuth client ID. This is obtained from Google API Credentials
#' @param client_secret OAuth client secret. This is obtained from Google API Credentials
#' @export
authorize <- function(client_id = getOption("drive.client.id"),
                      client_secret = getOption("drive.client.secret")){
  app <- oauth_app(appname = "googledrive", key = client_id, secret = client_secret)
  endpoint <- oauth_endpoints("google")
  token <- oauth2.0_token(endpoint = endpoint, app = app,
                          scope = c("https://www.googleapis.com/auth/drive"))
  set_token(token)
  return(invisible(token))
}
