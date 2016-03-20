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
# head(data)

make_countries <- function(x) {
    items <- strsplit(x, ",")
    counries <- c()

    for (item in items) {
        country <- gsub(" ", "", item[length(item)])
        counries <- c(counries, country)
    }

    return(counries)
}

empty_as_na <- function(x) {
    return(ifelse(as.character(x) != "", x, NA))
}

# cleanup data
data <- data %>%
    mutate(Phase = gsub("\n.*$", "", Phase)) %>%
    mutate(Phase = gsub("[0-9]+", "", Phase)) %>%
    mutate(Phase = gsub("\\[\\]", "", Phase)) %>%
    mutate_each(funs(empty_as_na)) %>%
    mutate(Country = make_countries(Location))

accidents_per_country <- data %>%
    group_by(Country) %>%
    summarise(total = n()) %>%
    arrange(desc(total))
