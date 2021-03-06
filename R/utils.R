#' Generate endpoint for the Google Drive API
#' @keywords internal
get_endpoint <- function(typeOfEndpoint = "drive.endpoint.about", fileId = NULL, permissionId = NULL){
  url = getOption(typeOfEndpoint)
  # If there is no parameter within URL, it is supposed to return the url with no changes
  if(!grepl('{', url, fixed = TRUE)){
    return(url)
  }
  if(grepl('{fileId}', url, fixed = TRUE)){
    return(gsub('{fileId}', fileId, url, fixed = TRUE))
  }
  if(grepl('{permissionId}', url, fixed = TRUE)){
    modifiedUrl <- gsub('{permissionId}', permissionId, url, fixed = TRUE)
    modifiedUrl <- gsub('{fileId}', fileId, modifiedUrl, fixed = TRUE)
    return(modifiedUrl)
  }
}
