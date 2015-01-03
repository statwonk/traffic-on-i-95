source("utilities.R")
d <- list()
event_files <- list.files(".")[grepl("event", list.files(".")) & grepl("rds", list.files("."))]
for(i in 1:length(event_files)) {
  d[[i]] <- readRDS(event_files[i])
  print(i)
}

g1 <- as.data.table(as.data.frame(do.call(rbind, lapply(1:6, function(x) { d[[x]] }))))
g2 <- as.data.table(do.call(rbind, lapply(7:10, function(x) {
  d[[x]]$crash_date_time <- as.character(d[[x]]$crash_date_time)
  d[[x]]
})))

g1_roads <- list()
road_files <- list.files(".")[grepl("dot", list.files(".")) & grepl("rds", list.files("."))]
for(i in 1:length(road_files)) {
  d[[i]] <- readRDS(road_files[i])
  print(i)
}
g1_roads <- as.data.table(as.data.frame(do.call(rbind, lapply(1:6, function(x) { d[[x]] }))))
setkey(g1, `Report Number`); setkey(g1_roads, `Report Number`)
g1_roads <- unique(g1_roads)
g1 <- merge(g1, g1_roads[ ,c("Report Number", "On") , with = F], by = "Report Number", all.x = T)

g1[, miami_dade := `County Code` == 1 | grepl("dade", `County Name`, ignore.case = T), ]
g2[, miami_dade := county_code == 1 | grepl("dade", county_name, ignore.case = T), ]

g1[, i95 := grepl("INTERSTATE 95", On, ignore.case = T) |
     grepl("INTERSTATE 95", On, ignore.case = T) |
     grepl("I95", On, ignore.case = T) |
     grepl("I 95", On, ignore.case = T), ]
g2[, i95 := grepl("INTERSTATE 95", on_street_road_highway, ignore.case = T) |
     grepl("I-95", on_street_road_highway, ignore.case = T) |
     grepl("I95", on_street_road_highway, ignore.case = T) |
     grepl("I 95", on_street_road_highway, ignore.case = T), ]

g1 <- g1[ , c("Crash Date", "Hour", "Minute", "AMPM", "miami_dade", "i95"), with = F]
setnames(g1, 1:6, c("crash_date", "hour", "min", "ampm", "miami_dade", "i95"))
g1 <- tbl_df(as.data.frame(g1))
g1 <- g1 %>%
  mutate(ampm = ifelse(ampm == "P", "PM", "AM")) %>%
  mutate(time = as.POSIXct(paste0(crash_date, " ",
                                  hour, ":",
                                  min, ":00 ",
                                  ampm),
                           format = "%m/%d/%Y %I:%M:00 %p",
                           tz = "EST")) %>%
  select(crash_at = time, miami_dade, i95) %>%
  mutate(counter = 1)

g2 <- as.data.table(g2)
g2 <- g2[ , c("crash_date_time", "miami_dade", "i95"), with = F]
setnames(g2, 1, "crash_at")
g2[, counter := 1,]
g2[ , crash_at := paste0(crash_at, ":00"), ]
g2 <- g2[nchar(crash_at) == nchar("2011-01-01 00:00:00")]
g2[ , crash_at := as.POSIXct(crash_at,
                             format = "%Y-%m-%d %H:%M:%S",
                             tz = "EST", origin = "1970-01-01"), ]

g <- rbind(as.data.table(as.data.frame(g1)),
           as.data.table(as.data.frame(g2)))
# clean up
rm(g1, g2, d)

g[ ,crash_at := format(crash_at, "%Y-%m-%d"),]
setkey(g, miami_dade, i95, crash_at)
g <- g[ , list(crashes = sum(counter)), by = c("miami_dade", "i95", "crash_at")]
g <- g[!is.na(crash_at) & as.integer(substr(crash_at, 1, 4)) >= 2005]

input <- tbl_df(as.data.frame(g))
input <- input %>% mutate(crash_at = as.POSIXct(crash_at, tz = "EST"))
# Fill in dropped dates, if any
input <- left_join(tbl_df(data.frame(crash_at = seq(min(input$crash_at),
                                                    max(input$crash_at),
                                                    by = "days"))),
                   input) %>%
  mutate(crashes = ifelse(is.na(crashes), 0, crashes),
         miami_dade = ifelse(!is.na(miami_dade),
                             ifelse(miami_dade == T,
                                    "Miami-Dade", "Rest of Florida"), "Rest of Florida"))

md <- input %>%
  filter(miami_dade == "Miami-Dade",
         i95 == T)

write.csv(md,
          "crashes.csv")

ggplot(input %>%
         mutate(i95 = ifelse(i95, "Interstate 95", "Other road")),
       aes(x = crash_at,
           y = crashes)) +
  geom_point(alpha = 0.3, size = 4) +
  geom_smooth(method = "gam",
              formula = y ~ s(x, bs = "ps", k = 10),
              colour = "red", size = 5, alpha = 0.8, fill = "red") +
  theme_bw(base_size = 25) +
  ggtitle("Crashes on Florida Roads") +
  ylab("Crashes per day") +
  xlab("") +
  # scale_y_continuous(breaks = seq(0, 25, 2.5)) +
  theme(panel.grid.major.y = element_line(colour = "black", size = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30, face = "bold", vjust = 0.75),
        plot.title = element_text(size = 50, face = "bold")) +
  facet_wrap(i95 ~ miami_dade, scales = "free_y")

input <- input %>%
  arrange(crash_at, miami_dade) %>%
  group_by(miami_dade) %>%
  mutate(crashes_jan_2008 = mean(head(crashes[crash_at >= as.POSIXct('2008-01-01')], 31))) %>%
  mutate(crash_growth_from_2008 = crashes / crashes_jan_2008)

ggplot(input,
       aes(x = crash_at,
           y = crash_growth_from_2008,
           colour = factor(miami_dade),
           fill = factor(miami_dade))) +
  #   geom_point(alpha = 0.25, size = 3) +
  geom_smooth(method = "gam",
              formula = y ~ s(x, bs = "ps", k = 200),
              size = 2, alpha = 0) +
  scale_fill_solarized(guide = "none") +
  scale_colour_solarized(name = "", guide = guide_legend(override.aes = list(size = 10))) +
  scale_x_datetime(breaks = date_breaks("1 year"),
                   labels = date_format("%Y")) +
  scale_y_continuous(labels = percent, breaks = seq(0, 10, 0.5)) +
  theme_bw(base_size = 25) +
  ggtitle("Crashes on Florida Roads") +
  ylab("Percentage growth") +
  xlab("") +
  theme(panel.grid.major.y = element_line(colour = "black", size = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30, face = "bold", vjust = 0.75),
        plot.title = element_text(size = 50, face = "bold"),
        legend.position = "top") +
  expand_limits(x = as.POSIXct("2004-11-01"))
