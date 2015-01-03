devtools::install("floridaCrashR")
devtools::load_all("floridaCrashR")
library(floridaCrashR)

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

# 2010 is a bit funky in that FDOT split their data format
# in this year, the first halt matches from 2005, and the later
# through 2013 (at least).

d_2010_a <- fread(getURL("http://s3.amazonaws.com/fl-crash/2010/oldform/non_redacted_2010_20101231_event_20110304.txt"),
                  verbose = T,
                  header = T)
d_2010_b <- fread(getURL("http://s3.amazonaws.com/fl-crash/2010/newform/non_redacted_2010_20101231_event_20110914.csv"),
                  verbose = T,
                  header = T)
saveRDSfast(d_2010_a, "2010_event_a.rds")
saveRDSfast(d_2010_b, "2010_event_b.rds")

d_2010_a <- fread(getURL("http://s3.amazonaws.com/fl-crash/2010/oldform/non_redacted_2010_20101231_dot_20110304.txt"),
                  verbose = T,
                  header = T)
saveRDSfast(d_2010_a, "2010_dot_a.rds") # this is necessary to join with events in order to find the location
# of crashes.

# wget -vi new_files.txt
d <- list.files(".")[grepl('2011', list.files(".")) | grepl('2012', list.files(".")) | grepl('2013', list.files("."))]
d <- d[grepl(".csv", d)]

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




