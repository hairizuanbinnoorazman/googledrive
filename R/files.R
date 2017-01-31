#' Get the list of files from Googledrive
#' @description Utilizes the list_files Google Drive API endpoint to retrieves files and folder details.
#' This function provides the raw functionality of full access to the list_files API endpoint but is not
#' the recommended way of accessing the API. It would be better to use the higher level functions provided
#' within this package to access the files/folders/data that you need.
#' @param q Optional. A query for filtering the file results. See the "Search for Files" guide for supported syntax.
#' @param pageSize Optional. The maximum number of files to return per page. Acceptable values are 1 to 1000,
#' inclusive. (Default: 100)
#' @param pageToken Optional. The token for continuing a previous list request on the next page. This should be
#' set to the value of 'nextPageToken' from the previous response.
#' @param orderBy Optional. A comma-separated list of sort keys. Valid keys are 'createdTime', 'folder',
#' 'modifiedByMeTime', 'modifiedTime', 'name', 'quotaBytesUsed', 'recency', 'sharedWithMeTime',
#' 'starred', and 'viewedByMeTime'. Each key sorts ascending by default, but may be reversed with the
#' 'desc' modifier. Example usage: ?orderBy=folder,modifiedTime desc,name.
#' @param spaces Optional. A comma-separated list of spaces to query within the corpus. Supported
#' values are 'drive', 'appDataFolder' and 'photos'.
#' @param corpus Optional. The source of files to list. Acceptable values are domain and user
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @export
base_list_files <- function(q = NULL, pageSize = NULL, pageToken = NULL,
                       orderBy = NULL, spaces = NULL, corpus = NULL){
  # Get endpoint url
  url <- get_endpoint("drive.endpoint.files.list")
  # Get token
  token <- get_token()
  config <- httr::config(token=token)
  # List of query parameters
  query_params = list()
  query_params['q'] = q
  query_params['pageSize'] = pageSize
  query_params['pageToken'] = pageToken
  query_params['orderBy'] = orderBy
  query_params['spaces'] = spaces
  query_params['corpus'] = corpus
  # Modify slides
  result <- httr::GET(url, config = config, accept_json(), query = query_params, encode = "json")
  # Process results
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)
  # If endpoint return url status other than 200, return error message
  if(httr::status_code(result) != 200){
    stop(result_list$error$message)
  }
  return(result_list)
}

#' Get list of files from a folder
#' @param id ID of the drive folder
#' @inheritParams base_list_files
#' @export
#' @examples
#' \dontrun{
#' library(googledrive)
#' authorize()
#' # Folder id is 0XXXXXXXX
#' list_files_in_folder('0XXXXXXXX')
#' }
list_files_in_folder <- function(id = NULL, pageSize = NULL, pageToken = NULL,
                                  orderBy = NULL, spaces = NULL, corpus = NULL){
  q = paste0("'", id, "' in parents and mimeType != 'application/vnd.google-apps.folder' and trashed = false")
  output <- base_list_files(q, pageSize, pageToken, orderBy, spaces, corpus)
  return(output)
}

#' Get list of folders from a folder
#' @param id ID of the drive folder
#' @inheritParams base_list_files
#' @export
#' @examples
#' \dontrun{
#' library(googledrive)
#' authorize()
#' # Folder id is 0XXXXXXXX
#' list_folders_in_folder('0XXXXXXXX')
#' }
list_folders_in_folder <- function(id = NULL, pageSize = NULL, pageToken = NULL,
                                 orderBy = NULL, spaces = NULL, corpus = NULL){
  q = paste0("'", id, "' in parents and mimeType = 'application/vnd.google-apps.folder' and trashed = false")
  output <- base_list_files(q, pageSize, pageToken, orderBy, spaces, corpus)
  return(output)
}


