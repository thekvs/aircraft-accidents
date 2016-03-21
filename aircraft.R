library(rvest)
library(dplyr)
library(date)

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

dates <- date.mdy(as.date(data$Date))

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
    mutate(Day = as.integer(dates$day),
           Month = as.integer(dates$month),
           Year = as.integer(dates$year)) %>%
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

# статистика по странам по авариям во время посадки, взлета или набора высоты
accidents_per_country2 <- data %>%
    filter(Phase %in% c("LDG", "TOF", "ICL") & Type == "COM") %>%
    group_by(Country) %>%
    summarise(accidents = n(), deaths_passengers = sum(Deaths.P), deaths_all = sum(Deaths.T)) %>%
    arrange(desc(accidents))

# статистика для России по годам по авариям во время посадки, взлета или набора высоты
accidents_ru_by_year <- data %>%
    filter(Country == "Russia") %>%
    filter(Phase %in% c("LDG", "TOF", "ICL") & Type == "COM") %>%
    group_by(Year) %>%
    summarise(accidents = n(), deaths_passengers = sum(Deaths.P), deaths_all = sum(Deaths.T)) %>%
    arrange(desc(deaths_all))

# статистика для США по годам по авариям во время посадки, взлета или набора высоты
accidents_us_by_year <- data %>%
    filter(Country == "US") %>%
    filter(Phase %in% c("LDG", "TOF", "ICL") & Type == "COM") %>%
    group_by(Year) %>%
    summarise(accidents = n(), deaths_passengers = sum(Deaths.P), deaths_all = sum(Deaths.T)) %>%
    arrange(desc(deaths_all))

most_deadly_years <- data %>%
    filter(Type == "COM") %>%
    group_by(Year) %>%
    summarise(accidents = n(), deaths_passengers = sum(Deaths.P), deaths_all = sum(Deaths.T)) %>%
    arrange(desc(deaths_all))

most_deadly_months <- data %>%
    filter(Type == "COM") %>%
    group_by(Month) %>%
    summarise(accidents = n(), deaths_passengers = sum(Deaths.P), deaths_all = sum(Deaths.T)) %>%
    arrange(desc(deaths_all))
