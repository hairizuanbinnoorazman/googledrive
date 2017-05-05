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
base_list_files <- function(q = NULL, page_size = NULL, page_token = NULL,
                       order_by = NULL, spaces = NULL, corpus = NULL){
  # Get endpoint url
  url <- get_endpoint("drive.endpoint.files.list")
  # Get token
  token <- get_token()
  config <- httr::config(token=token)
  # List of query parameters
  query_params = list()
  query_params['q'] = q
  query_params['pageSize'] = page_size
  query_params['pageToken'] = page_token
  query_params['orderBy'] = order_by
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
#'
#' # Folder id is 0XXXXXXXX
#' list_files_in_folder('0XXXXXXXX')
#'
#' # If id is not specified, list of files would be obtained from root Google drive folder
#' list_files_in_folder()
#' }
list_files_in_folder <- function(id = NULL, page_size = NULL, page_token = NULL,
                                  order_by = NULL, spaces = NULL, corpus = NULL){
  if (is.null(id)){
    q = paste0("'root' in parents and mimeType != 'application/vnd.google-apps.folder' and trashed = false")
  } else {
    q = paste0("'", id, "' in parents and mimeType != 'application/vnd.google-apps.folder' and trashed = false")
  }
  output <- base_list_files(q, page_size, page_token, order_by, spaces, corpus)
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
#'
#' # Folder id is 0XXXXXXXX
#' list_folders_in_folder('0XXXXXXXX')
#'
#' If id is not specified, list of files would be obtained from root Google drive folder
#' list_folders_in_folder()
#' }
list_folders_in_folder <- function(id = NULL, page_size = NULL, page_token = NULL,
                                 order_by = NULL, spaces = NULL, corpus = NULL){
  if (is.null(id)){
    q = "'root' in parents and mimeType = 'application/vnd.google-apps.folder' and trashed = false"
  } else {
    q = paste0("'", id, "' in parents and mimeType = 'application/vnd.google-apps.folder' and trashed = false")
  }
  output <- base_list_files(q, page_size, page_token, order_by, spaces, corpus)
  return(output)
}

#' Get file via name
#' @description Allows you to pull the a list of file names that match the names that you provide to the file.
#' Due to Google Drive's nature to allow multiple files with the same name to coexist in the same folder, you may
#' be able to obtain a list of files even if you put matchType exact
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
get_file_by_name <- function(file_name, match_type, id = 'root', page_size = NULL, page_token = NULL,
                             order_by = NULL, spaces = NULL, corpus = NULL){
  if(match_type == 'not_equal'){
    q = paste0("'", id, "' in parents and mimeType != 'application/vnd.google-apps.folder' and trashed = false and not name contains '", file_name, "'")
    output <- base_list_files(q, page_size, page_token, order_by, spaces, corpus)
    return(output)
  }
  if(match_type == 'exact'){
    q = paste0("'", id, "' in parents and mimeType != 'application/vnd.google-apps.folder' and trashed = false and name = '", file_name, "'")
    output <- base_list_files(q, page_size, page_token, order_by, spaces, corpus)
    return(output)
  }
  if(match_type == 'contains'){
    q = paste0("'", id, "' in parents and mimeType != 'application/vnd.google-apps.folder' and trashed = false and name contains '", file_name, "'")
    output <- base_list_files(q, page_size, page_token, order_by, spaces, corpus)
    return(output)
  }
}

#' Copy a file in Google Drive
#' @param fileID ID of the file in Google Drive
#' @param folderID ID of the folder to store the copied file
#' @param fileName The name to be given to the copied file. This does not need to be unique within a folder.
#' @importFrom httr config accept_json content POST
#' @importFrom jsonlite fromJSON
#' @export
copy_file <- function(file_id, folder_id = 'root', file_name = NULL){
  # Get endpoint url
  url <- get_endpoint("drive.endpoint.files.copy", fileID)
  # Get token
  token <- get_token()
  config <- httr::config(token=token)
  # List of query parameters
  body_params = list()
  body_params['name'] = file_name
  body_params['parents'] = list(list(folder_idD))
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
#' @importFrom assertthat assert_that
#' @importFrom httr config accept_json content DELETE
#' @importFrom jsonlite fromJSON
#' @export
delete_file <- function(file_id){
  assert_that(is.character(file_id))
  # Get endpoint url
  url <- get_endpoint("drive.endpoint.files.delete", file_id)
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
#'
#' This function is temporarily disabled as there are issues to be resolved
#' @param fileID ID of the file in Google Drive
#' @examples
#' \dontrun{
#' library(googledrive)
#' authorize()
#' file <- get_file('0XXXXXXXXXXXXXXXXc')
#'
#' # Example with image
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
  result_content <- content(result, type = "raw")
  return(result_content)
}


#' Upload file to Google Drive
#' @description Allows you to uploads files into Google Drive. During the uploading process, you would
#' not be able to define other metadata that concerns the file. Utilize other functions to edit the
#' file metadata
#' @importFrom assertthat assert_that
#' @importFrom httr config accept_json content POST upload_file
#' @importFrom jsonlite fromJSON
#' @export
upload_file <- function(file_name){
  assert_that(is.character(file_name))
  # Get endpoint url
  url <- get_endpoint("drive.endpoint.upload.files.create")
  # Get token
  token <- get_token()
  config <- httr::config(token=token)
  # List of query parameters
  query_params = list()
  query_params['uploadType'] = 'media'
  # Body parameters
  body_params = httr::upload_file(file_name)
  # POST Request
  result <- httr::POST(url, config = config, query = query_params, body = body_params)
  # Process results
  result_content <- content(result)
  return(result_content)
}


#' Add or remove file from folders
#' @description Allows you to move file around. Due to the nature of Google drive which allows multiple
#' files with the same names to coexist in the same folder, we can technically "hook" files into the
#' folders. At the same time, there is a convenient feature of being able to "hook" a file into
#' multiple folders at the same time which basically means a file can be in 2 folders at once.
#' @param fileId The ID of the file.
#' @param addFolders A character vector of folder Ids
#' @param removeFolders A character vector of folder Ids
#' @importFrom httr config accept_json content PATCH
#' @importFrom jsonlite fromJSON
#' @export
move_file <- function(fileId, add_folders = NULL, remove_folders = NULL){
  # Get endpoint url
  url <- get_endpoint("drive.endpoint.files.update", fileId)
  # Get token
  token <- get_token()
  config <- httr::config(token=token)
  # List of query parameters
  query_params = list()
  query_params['addParents'] = paste0(add_folders, collapse = ",")
  query_params['removeParents'] = paste0(remove_folders, collapse = ",")
  # PATCH Request
  result <- httr::PATCH(url, config = config, query = query_params)
  # Process results
  result_content <- content(result)
  return(result_content)
}


#' Update file metadata properties
#' @description Allows you to update the name and description of the file
#' @param fileId The ID of the file.
#' @param name Name of the file. Overwrites the file name on Google Drive
#' @param description Description of the file. Overwrites the description on Google Drive
#' @importFrom httr config accept_json content PATCH upload_file content_type_json
#' @importFrom jsonlite fromJSON toJSON
#' @export
update_file_metadata <- function(file_id, name = NULL, description = NULL, starred = NULL, trashed = NULL){
  # Get endpoint url
  url <- get_endpoint("drive.endpoint.files.update", file_id)
  # Get token
  token <- get_token()
  config <- httr::config(token=token)
  # List of query parameters
  body_params = list()
  body_params['name'] = name
  body_params['description'] = description
  body_params['starred'] = starred
  body_params['trashed'] = trashed
  # PATCH Request
  result <- httr::PATCH(url, config = config, content_type_json(), body = as.character(toJSON(body_params, auto_unbox=TRUE)))
  # Process results
  result_content <- content(result)
  return(result_content)
}
