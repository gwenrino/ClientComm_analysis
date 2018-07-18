remoter::client("localhost", port = 55556)

library(RPostgreSQL)
library(lubridate)
library(tidyverse)
Sys.setenv(TZ='GMT')

m <- dbDriver("PostgreSQL")

con <- DBI::dbConnect(m, RPostgreSQL::PostgreSQL(), 
                      user = "gwen", password = "p994a8b8d6bd1b5cb1abb83f0de9eefb31bacc1a1d036b7ac28540c3e031b1e7c", 
                      dbname = "d17uaffbp2f6v6", host = "ec2-54-88-173-12.compute-1.amazonaws.com", port = 5432)

sort(dbListTables(con))

surveys <- data.frame(dbGetQuery(con, "select s.client_id, s.user_id, srl.survey_response_id, sr.text
                                 from surveys s
                                 left join survey_response_links srl on srl.survey_id = s.id
                                 left join survey_responses sr on sr.id = srl.survey_response_id
                                 order by s.client_id asc"))

messages <- data.frame(dbGetQuery(con, "select rr.client_id, rr.user_id, m.id, m.body, m.number_from, m.number_to, m.inbound, m.twilio_status, m.read, m.created_at, m.send_at, m.sent, m.reporting_relationship_id, m.original_reporting_relationship_id,
                                  c.created_at as client_created_at
                                  from messages m
                                  inner join reporting_relationships rr on rr.id = m.reporting_relationship_id
                                  inner join clients c on c.id = rr.id
                                  LEFT JOIN attachments a ON m.id = a.message_id
                                  order by c.id asc"))

# Solving the time zone problem
messages$created_at <- as.POSIXlt(messages$created_at, tz = "America/New_York")
messages$send_at <- as.POSIXlt(messages$send_at, tz = "America/New_York")
messages$client_created_at <- as.POSIXlt(messages$client_created_at, tz = "America/New_York")

messages$created_at_backup <- as.character(messages$created_at)
messages$send_at_backup <- as.character(messages$send_at)
messages$client_created_at_backup <- as.character(messages$client_created_at)

messages[grepl("Srour", messages$body),] # check magic time zone message

save.image("/home/gwen/remote.Rdata")

write.csv(messages, "/home/gwen/messages.csv", row.names = FALSE)
write.csv(surveys, "/home/gwen/surveys.csv", row.names = FALSE)
