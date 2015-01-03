source("utilities.R")
d <- return_raw_files()

for(i in 1:45 { # d[45] is through 2009
  df <- fread(getURL(paste0("http://s3.amazonaws.com/fl-crash/", d[i])),
              verbose = T,
              header = T)
  partial_name <- substr(d[i], nchar("2005/non_redacted_") + 1, nchar(d[i]))
  partial_name <- substr(partial_name,
                         1,
                         nchar(partial_name) -
                           nchar("_20130326.txt"))
  full_name <- paste0(substr(partial_name, 1, 4), substr(partial_name, nchar("2005_20051231_"), nchar(partial_name)))
  saveRDS(df, file = paste0(full_name, ".rds"))
  print(paste(i, "out of", length(d)))
}

for(i in 46:49) { # d[45] is through 2009
  df <- fread(getURL(paste0("http://s3.amazonaws.com/fl-crash/", d[i])),
              verbose = T,
              header = T)
  partial_name <- substr(d[i], nchar("2005/oldform/non_redacted_") + 1, nchar(d[i]))
  partial_name <- substr(partial_name,
                         1, nchar(partial_name) - nchar("_20130326.txt"))
  full_name <- paste0(substr(partial_name, 1, 4), substr(partial_name, nchar("2005_20051231_"), nchar(partial_name)))
  saveRDS(df, file = paste0(full_name, ".rds"))
  print(paste(i, "out of", length(d)))
}

write.table(paste0("http://s3.amazonaws.com/fl-crash/", d[50:length(d)]), file = "new_files.txt",
            row.names = F, quote = F)

# wget -vi new_files.txt
d <- list.files(".")[grepl('2011', list.files(".")) | grepl('2012', list.files(".")) | grepl('2013', list.files("."))]
d <- d[grepl(".csv", d)]
# d <- d[grepl("2011_2011", d)]
d <- list.files(".")[grepl('2010', list.files(".")) &
                       grepl('.txt', list.files("."))]

for(i in 1:length(d)) { # d[45] is through 2009
  df <- read.table(d[i],
                   header = T, quote = "",
                   sep = ",", fill = NA, row.names = NULL)
  partial_name <- substr(d[i], nchar("non_redacted_") + 1, nchar(d[i]))
  full_name <- paste0(substr(partial_name, 1, 4),
                      "_",
                      substr(partial_name,
                             nchar("2010_20101231_p"),
                             nchar(partial_name)- nchar("_20110914.csv")))
  saveRDSfast(df, file = paste0(full_name, ".rds"))
  print(paste(i, "out of", length(d)))
}
