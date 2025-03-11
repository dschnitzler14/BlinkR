
upload_and_overwrite <- function(temp_file, pathname, name) {
  
  existing_file <- drive_ls(path = pathname) %>%
    dplyr::filter(name == name)
  
  if (nrow(existing_file) > 0) {
    drive_rm(as_id(existing_file$id))
  }
  
  drive_upload(
    media = temp_file,
    path = pathname,
    name = name
  )
}

upload_and_overwrite(temp_file = "myfile.txt", pathname = "MyFolder", name = "myfile.txt")
