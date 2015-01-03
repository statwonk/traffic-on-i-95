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
