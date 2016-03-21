library(rvest)
library(dplyr)

url <- "https://en.wikipedia.org/wiki/List_of_aircraft_accidents_and_incidents_resulting_in_at_least_50_fatalities"

tables <- url %>%
    read_html() %>%
    html_nodes(xpath='//*[@id="mw-content-text"]/table[2]') %>%
    html_table(fill=TRUE)

data <- tables[[1]]

# fix names
names(data) <- c("Deaths.T", "Deaths.C", "Deaths.P", "Deaths.G", "Deaths.N", "Type",
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

extract_integer <- function(x) {
    x <- gsub("\n.*$", "", x)
    return(as.integer(sub(".*?([0-9]+).*", "\\1", x, perl=TRUE)))
}

# cleanup and massage data
data <- data %>%
    mutate(Phase = gsub("\n.*$", "", Phase)) %>%
    mutate(Phase = gsub("[0-9]+", "", Phase)) %>%
    mutate(Phase = gsub("\\[\\]", "", Phase)) %>%
    mutate(Deaths.G = as.vector(sapply(Deaths.G, extract_integer))) %>%
    mutate(Deaths.T = as.vector(sapply(Deaths.T, extract_integer))) %>%
    mutate_each(funs(empty_as_na)) %>%
    mutate(Type = as.factor(Type),
           Phase = as.factor(Phase)) %>%
    mutate(Deaths.C = as.integer(Deaths.C),
           Deaths.P = as.integer(Deaths.P)) %>%
    mutate(Country = make_countries(Location))

accidents_per_country <- data %>%
    group_by(Country) %>%
    summarise(accidents = n(), passengers = sum(Deaths.P), all = sum(Deaths.T)) %>%
    arrange(desc(accidents))

accidents_per_country_com <- data %>%
    filter(Type == "COM") %>%
    group_by(Country) %>%
    summarise(accidents = n(), passengers = sum(Deaths.P), all = sum(Deaths.T)) %>%
    arrange(desc(accidents))
