#' Get the list of files from Googledrive
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @export
list_files <- function(q = NULL, pageSize = NULL, pageToken = NULL,
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
