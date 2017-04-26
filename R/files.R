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
#' @importFrom httr config accept_json content GET
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
  # GET Request
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

#' Get file via name
#' @param filename Name of the file in the Drive folder
#' @param matchType Either exact or contains or not_equal
#' @param id FolderID to search in. 'all' is also accepted which would mean to search the whole of user's drive
#' @inheritParams base_list_files
#' @export
#' @examples
#' \dontrun{
#' library(googledrive)
#' authorize()
#' # Folder id is 0XXXXXXXX
#' get_file_by_name('some_file_name', 'exact', '0XXXXXXXX')
#' }
get_file_by_name <- function(filename, matchType, id = NULL, pageSize = NULL, pageToken = NULL,
                             orderBy = NULL, spaces = NULL, corpus = NULL){
  if(id == 'all'){
    if(matchType == 'not_equal'){
      q = paste0("'", id, "' in parents and mimeType != 'application/vnd.google-apps.folder' and trashed = false and not name contains '", filename, "'")
      output <- base_list_files(q, pageSize, pageToken, orderBy, spaces, corpus)
      return(output)
    }
    if(matchType == 'exact'){
      q = paste0("'", id, "' in parents and mimeType != 'application/vnd.google-apps.folder' and trashed = false and name = '", filename, "'")
      output <- base_list_files(q, pageSize, pageToken, orderBy, spaces, corpus)
      return(output)
    }
    if(matchType == 'contains'){
      q = paste0("'", id, "' in parents and mimeType != 'application/vnd.google-apps.folder' and trashed = false and name contains '", filename, "'")
      output <- base_list_files(q, pageSize, pageToken, orderBy, spaces, corpus)
      return(output)
    }
  } else {
    if(matchType == 'not_equal'){
      q = paste0("mimeType != 'application/vnd.google-apps.folder' and trashed = false and not name contains '", filename, "'")
      output <- base_list_files(q, pageSize, pageToken, orderBy, spaces, corpus)
      return(output)
    }
    if(matchType == 'exact'){
      q = paste0("mimeType != 'application/vnd.google-apps.folder' and trashed = false and name = '", filename, "'")
      output <- base_list_files(q, pageSize, pageToken, orderBy, spaces, corpus)
      return(output)
    }
    if(matchType == 'contains'){
      q = paste0("mimeType != 'application/vnd.google-apps.folder' and trashed = false and name contains '", filename, "'")
      output <- base_list_files(q, pageSize, pageToken, orderBy, spaces, corpus)
      return(output)
    }
  }
}

#' Copy a file in Google Drive
#' @param fileID ID of the file in Google Drive
#' @param folderID ID of the folder to store the copied file
#' @param fileName The name of the file. This is not necessarily unique within a folder.
#' @export
copy_file <- function(fileID, folderID = NULL, fileName = NULL){
  # Get endpoint url
  url <- get_endpoint("drive.endpoint.files.copy", fileID)
  # Get token
  token <- get_token()
  config <- httr::config(token=token)
  # List of query parameters
  body_params = list()
  body_params['name'] = fileName
  body_params['parents'] = list(list(folderID))
  # POST Request
  result <- httr::POST(url, config = config, accept_json(), body = body_params, encode = "json")
  # Process results
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)
  # If endpoint return url status other than 200, return error message
  if(httr::status_code(result) != 200){
    stop(result_list$error$message)
  }
  return(result_list)
}


#' Delete a file in Google Drive
#' @description Permanently deletes a file owned by the user without moving it to the trash.
#' @param fileID ID of the file in Google Drive
#' @export
delete_file <- function(fileID){
  # Get endpoint url
  url <- get_endpoint("drive.endpoint.files.delete", fileID)
  # Get token
  token <- get_token()
  config <- httr::config(token=token)
  # DELETE Request
  result <- httr::DELETE(url, config = config, accept_json(), encode = "json")
  # Process results
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)
  # If endpoint return url status other than 200, return error message
  if(httr::status_code(result) != 200){
    stop(result_list$error$message)
  }
  return(result_list)
}


#' Download non-Google docs resources from Google Drive
#' @description The function will acknowledge that the file is safe to download. Do be careful when
#' downloading files from the web (even if its on your own Google Drive folder)
#'
#' If you are downloading images, it would be recommended for you to download the imager package
#' for quick image manipulation and saving. You would need to convert the image from a 3 dimensional array
#' to a 4 dimensional array in that case
#' @param fileID ID of the file in Google Drive
#' @export
#' @examples
#' \dontrun{
#' library(googledrive)
#' authorize()
#' file <- get_file('0XXXXXXXXXXXXXXXXc')
#'
#' library(imager)
#' dim(file) # Check dimensions of the file dataset
#' dim(file) <- c(400, 320, 1, 3) # Example dimensions for color image (x, y, z, c)
#' save.image(file, "file.jpg")
#' }
get_file <- function(fileID){
  # Get endpoint url
  url <- get_endpoint("drive.endpoint.files.get", fileID)
  # Get token
  token <- get_token()
  config <- httr::config(token=token)
  # List of query parameters
  query_params = list()
  query_params['alt'] = 'media'
  query_params['acknowledgeAbuse'] = FALSE
  # GET Request
  result <- httr::GET(url, config = config, query = query_params, encode = "json")
  # Process results
  result_content <- content(result)
  # result_list <- fromJSON(result_content)
  # If endpoint return url status other than 200, return error message
  # if(httr::status_code(result) != 200){
  #  stop(result_list$error$message)
  #}
  #return(result_list)
  return(result_content)
}


#' Upload file to Google Drive
#' @export
#' @importFrom httr config accept_json content POST upload_file
#' @importFrom jsonlite fromJSON
upload_file <- function(fileName){
  # Get endpoint url
  url <- get_endpoint("drive.endpoint.upload.files.create")
  # Get token
  token <- get_token()
  config <- httr::config(token=token)
  # List of query parameters
  query_params = list()
  query_params['uploadType'] = 'media'
  # Body parameters
  body_params = httr::upload_file(fileName)
  # POST Request
  result <- httr::POST(url, config = config, query = query_params, body = body_params)
  # Process results
  result_content <- content(result)
  return(result_content)
}
