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
saveRDSfast(d_2010_a, "2010_dot_a.rds")
