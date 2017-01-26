#' Generate endpoint for the Google Drive API
#' Temporarily available
#' @export
get_endpoint <- function(typeOfEndpoint = "drive.endpoint.about", fileId = NULL){
  url = getOption(typeOfEndpoint)
  # If there is no parameter within URL, it is supposed to return the url with no changes
  if(!grepl('{', url, fixed = TRUE)){
    return(url)
  }
  if(grepl('{fileId}', url, fixed = TRUE)){
    return(gsub('{fileId}', fileId, url, fixed = TRUE))
  }
}
