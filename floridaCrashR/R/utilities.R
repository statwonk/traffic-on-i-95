import_data_files_into_r <- function(type = NULL) {
  if(is.null(type)) {
    stop("Must be of type event, passenger, or dot")
  }
  d <- list()
  event_files <- list.files(".")[grepl("event", list.files(".")) & grepl("rds", list.files("."))]
  for(i in 1:length(event_files)) {
    d[[i]] <- readRDS(event_files[i])
    for(j in 1:length(d[[i]])) {
      if(is.factor(d[[i]][ , j])) {
        d[[i]][ , j] <- as.character(d[[i]][ , j])
      }
    }
    print(i)
  }

  return(d)
}

return_raw_files <- function() {
  data <- XML::xmlParse("s3list.xml") # please email cpeter9@gmail.com for this file.
  xml_data <- XML::xmlToList(data)
  d <- unlist(do.call(rbind, xml_data[-c(1:5)]))
  d <- d[grepl(".txt", d) | grepl(".csv", d)]

  return(d)
}

saveRDSfast <- function (object, file = "", ascii = FALSE,
                         version = NULL, compress = TRUE,
                         refhook = NULL) {
  if (is.character(file)) {
    if (file == "")
      stop("'file' must be non-empty string")
    mode <- if (ascii)
      "w"
    else "wb"
    con <- if (identical(compress, "bzip2"))
      bzfile(file, mode, compression = 1)
    else if (identical(compress, "xz"))
      xzfile(file, mode, compress = 1)
    else if (compress)
      gzfile(file, mode, compress = 1)
    else file(file, mode)
    on.exit(close(con))
  }
  else if (inherits(file, "connection")) {
    if (!missing(compress))
      warning("'compress' is ignored unless 'file' is a file name")
    con <- file
  }
  else stop("bad 'file' argument")
  .Internal(serializeToConn(object, con, ascii, version, refhook))
}
