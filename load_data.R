library(XML)
data <- xmlParse("s3list.xml")
xml_data <- xmlToList(data)

d <- unlist(do.call(rbind, xml_data[-c(1:5)]))
d <- d[grepl(".txt", d) | grepl(".csv", d)]

library(RCurl); library(data.table)
for(i in 1:45 { # d[45] is through 2009
  df <- fread(getURL(paste0("http://s3.amazonaws.com/fl-crash/", d[i])),
              verbose = T,
              header = T)
  partial_name <- substr(d[i], nchar("2005/non_redacted_") + 1, nchar(d[i]))
  partial_name <- substr(partial_name,
                         1, nchar(partial_name) - nchar("_20130326.txt"))
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


saveRDSfast <- function (object, file = "", ascii = FALSE, version = NULL, compress = TRUE,
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

library(data.table)
d <- list()
event_files <- list.files(".")[grepl("event", list.files(".")) & grepl("rds", list.files("."))]
for(i in 1:length(event_files)) {
  d[[i]] <- readRDS(event_files[i])
  print(i)
}

# Group 1
table(names(d[[1]]) == names(d[[2]]))
table(names(d[[1]]) == names(d[[3]]))
table(names(d[[1]]) == names(d[[4]]))
table(names(d[[1]]) == names(d[[5]]))

g1 <- do.call(rbind, lapply(1:5, function(x) { d[[x]] }))

g2 <- d[[6]]
# Group 2
table(names(d[[7]]) == names(d[[8]]))
table(names(d[[7]]) == names(d[[9]]))
g3 <- do.call(rbind, lapply(7:9, function(x) {
  d[[x]]$crash_date_time <- as.character(d[[x]]$crash_date_time)
  d[[x]]
  }))

library(dplyr)

g1 <- g1[ , c("Crash Date", "Hour", "Minute", "AMPM"), with = F]
setnames(g1, 1:4, c("crash_date", "hour", "min", "ampm"))
g1 <- tbl_df(as.data.frame(g1))
g1 <- g1 %>%
  mutate(ampm = ifelse(ampm == "P", "PM", "AM")) %>%
  mutate(time = as.POSIXct(paste0(crash_date, " ",
                                  hour, ":",
                                  min, ":00 ",
                                  ampm),
                           format = "%m/%d/%Y %I:%M:00 %p",
                           tz = "EST")) %>%
  select(crash_at = time) %>%
  mutate(counter = 1)

g2 <- as.data.table(g2)
g2 <- g2[ , c("Crash.Date", "Hour", "Minute", "AMPM"), with = F]
setnames(g2, 1:4, c("crash_date", "hour", "min", "ampm"))
g2 <- tbl_df(as.data.frame(g2))
g2 <- g2 %>%
  mutate(ampm = ifelse(ampm == "P", "PM", "AM")) %>%
  mutate(time = as.POSIXct(paste0(crash_date, " ",
                                  hour, ":",
                                  min, ":00 ",
                                  ampm),
                           format = "%m/%d/%Y %I:%M:00 %p",
                           tz = "EST")) %>%
  select(crash_at = time) %>%
  mutate(counter = 1)

g3 <- as.data.table(g3)
g3 <- g3[ , c("crash_date_time"), with = F]
setnames(g3, 1, "crash_at")
g3[, counter := 1,]
g3[ , crash_at := paste0(crash_at, ":00"), ]
g3 <- g3[nchar(crash_at) == nchar("2011-01-01 00:00:00")]
g3[ , crash_at := as.POSIXct(crash_at,
                          format = "%Y-%m-%d %H:%M:%S",
                          tz = "EST", origin = "1970-01-01"), ]

g1 <- as.data.table(as.data.frame(g1))
g2 <- as.data.table(as.data.frame(g2))
g3 <- as.data.table(as.data.frame(g3))

g <- rbind(g1, g2, g3)

# clean up
rm(g1, g2, g3, d)

g[ ,crash_at := format(crash_at, "%Y-%m-%d"),]
setkey(g, crash_at)
g <- g[ , list(crashes = sum(counter)), by = "crash_at"]
g <- g[!is.na(crash_at) & as.integer(substr(crash_at, 1, 4)) >= 2005]

input <- tbl_df(as.data.frame(g))
input <- input %>% mutate(crash_at = as.POSIXct(crash_at, tz = "EST"))
# Fill in dropped dates, if any
input <- left_join(tbl_df(data.frame(crash_at = seq(min(input$crash_at),
                                                    max(input$crash_at),
                                                    by = "days"))),
                   input) %>%
  mutate(crashes = ifelse(is.na(crashes), 0, crashes))

library(ggplot2); library(ggthemes)
ggplot(input,
       aes(x = crash_at,
           y = crashes)) +
  geom_point(alpha = 0.3, size = 4) +
  geom_smooth(method = "gam",
              formula = y ~ s(x, bs = "ps", k = 100),
              colour = "red", size = 5, alpha = 0.8, fill = "red") +
  theme_bw(base_size = 25) +
  ggtitle("Crashes on Florida Roads") +
  ylab("Crashes per day") +
  xlab("") +
  # scale_y_continuous(breaks = seq(0, 25, 2.5)) +
  theme(panel.grid.major.y = element_line(colour = "black", size = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 50, face = "bold"),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30, face = "bold", vjust = 0.75),
        plot.title = element_text(size = 50, face = "bold"))

library(ggplot2); library(ggthemes)
ggplot(input,
       aes(x = crash_date,
           y = fatalities)) +
  # geom_point(alpha = 0.3, size = 4) +
  geom_smooth(method = "gam",
              formula = y ~ s(x, bs = "ps", k = 100),
              colour = "red", size = 5, alpha = 0.5, fill = "red") +
  theme_bw(base_size = 25) +
  ggtitle("Fatalities on Florida Roads") +
  ylab("Fatalities per day") +
  xlab("") +
  scale_y_continuous(breaks = seq(0, 25, 1)) +
  theme(panel.grid.major.y = element_line(colour = "black", size = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 50, face = "bold"),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30, face = "bold", vjust = 0.75),
        plot.title = element_text(size = 50, face = "bold")) +
  coord_cartesian(ylim = c(5, 12.5))









