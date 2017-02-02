.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Please use predefined Credentials only for the testing requests. To obtain your own Credentials see help(authorize).")
}

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.drive <- list(
    drive.client.id = "10709400262-28lpv43nui21l1172cup6kh68blvkllq.apps.googleusercontent.com",
    drive.client.secret = "FADkDoE_H0B7j-VytEjTbgaU",

    # About me Representation
    drive.endpoint.about = "https://www.googleapis.com/drive/v3/about",

    # Files Resource Representation
    drive.endpoint.files.copy = "https://www.googleapis.com/drive/v3/files/{fileId}/copy",
    drive.endpoint.upload.files.create = "https://www.googleapis.com/upload/drive/v3/files",
    drive.endpoint.files.delete = "https://www.googleapis.com/drive/v3/files/{fileId}",
    drive.endpoint.files.generateIds = "https://www.googleapis.com/drive/v3/files/generateIds",
    drive.endpoint.files.get = "https://www.googleapis.com/drive/v3/files/{fileId}",
    drive.endpoint.files.list = "https://www.googleapis.com/drive/v3/files",

    # Permissions Resource Representation
    drive.endpoint.permissions.create = "https://www.googleapis.com/drive/v3/files/{fileId}/permissions",
    drive.endpoint.permissions.delete = "https://www.googleapis.com/drive/v3/files/{fileId}/permissions/{permissionId}",
    drive.endpoint.permissions.get = "https://www.googleapis.com/drive/v3/files/{fileId}/permissions/{permissionId}",
    drive.endpoint.permissions.list = "https://www.googleapis.com/drive/v3/files/{fileId}/permissions"
  )
  toset <- !(names(op.drive) %in% names(op))
  if (any(toset)) options(op.drive[toset])

  invisible()
}
