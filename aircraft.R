library(rvest)
library(dplyr)

url <- "https://en.wikipedia.org/wiki/List_of_aircraft_accidents_and_incidents_resulting_in_at_least_50_fatalities"

tables <- url %>%
    read_html() %>%
    html_nodes(xpath='//*[@id="mw-content-text"]/table[2]') %>%
    html_table(fill=TRUE)

data <- tables[[1]]

# fix names
names(data) <- c("Deaths.T", "Deaths.C", "Deaths.P", "Deaths.G", "Deaths.N", "T",
              "Incident", "Aircraft", "Location", "Phase", "Airport", "Distance", "Date")

# delete the first row
data <- data[-1, ]
head(data)
