# Aircraft accidents
Konstantin Sorokin  


Для анализа использовались данные, приведенные на [странице](https://en.wikipedia.org/wiki/List_of_aircraft_accidents_and_incidents_resulting_in_at_least_50_fatalities) сайта WikiPedia.

Подключаем используемые в этом примере библиотеки. Враппер suppressMessages() используется
чтобы подавить ненужную в этом случае отладочную печать при загрузке библиотек.

```r
suppressMessages(library(rvest))
suppressMessages(library(dplyr))
suppressMessages(library(date))
suppressMessages(library(knitr))
```

Определяем разные вспомогательные функции, которые нам понадобятся в дальнейшем.

```r
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
```

Скачиваем страницу с сайта Википедии и вытаскиваем оттуда нужную нам таблицу. Выражение XPath
предварительно находим с помощью какого-нибудь девелоперского расширения для браузера. В данном
случае использовался Chrome.

```r
url <- "https://en.wikipedia.org/wiki/List_of_aircraft_accidents_and_incidents_resulting_in_at_least_50_fatalities"

tables <- url %>%
    read_html() %>%
    html_nodes(xpath='//*[@id="mw-content-text"]/table[2]') %>%
    html_table(fill=TRUE)

data <- tables[[1]]
```

Исправляем имена колонок.

```r
names(data) <- c("Deaths.T", "Deaths.C", "Deaths.P", "Deaths.G", "Deaths.N", "Type",
              "Incident", "Aircraft", "Location", "Phase", "Airport", "Distance", "Date")
```

Удаляем первую строку т.к. она содержит мусор.

```r
data <- data[-1, ]
```

Чистим и преобразовываем данные.

```r
dates <- date.mdy(as.date(data$Date))

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
```

Первым делом рассчитаем количество аварий и жертв в них, сгруппировав данные по странам.

```r
accidents_per_country <- data %>%
    group_by(Country) %>%
    summarise(Accidents = n(),
              Deaths.Passengers = sum(Deaths.P),
              Deaths.Total = sum(Deaths.T)) %>%
    arrange(desc(Accidents))

kable(head(accidents_per_country, 20), format="markdown")
```



|Country     | Accidents| Deaths.Passengers| Deaths.Total|
|:-----------|---------:|-----------------:|------------:|
|US          |        69|              5819|         9058|
|Russia      |        49|              3785|         4296|
|China       |        15|              1166|         1379|
|France      |        14|              1562|         1669|
|India       |        14|              1499|         1641|
|Indonesia   |        13|              1214|         1486|
|Colombia    |        12|               910|         1031|
|Spain       |        12|              1786|         1892|
|Brazil      |        11|               860|          962|
|Iran        |        11|              1243|         1528|
|Nigeria     |        11|              1153|         1309|
|Japan       |        10|              1433|         1572|
|Ukraine     |        10|              1007|         1163|
|Turkey      |         9|               578|          732|
|Italy       |         8|               552|          642|
|Morocco     |         8|               639|          695|
|Peru        |         8|               623|          688|
|Canada      |         7|               852|          905|
|SaudiArabia |         7|               978|         1049|
|UK          |         7|               592|          706|

То же самое, что и в предыдущем примере, но теперь только для коммерческих полетов.

```r
accidents_per_country_com <- data %>%
    filter(Type == "COM") %>%
    group_by(Country) %>%
    summarise(Accidents = n(),
              Deaths.Passengers = sum(Deaths.P),
              Deaths.Total = sum(Deaths.T)) %>%
    arrange(desc(Accidents))

kable(head(accidents_per_country_com, 20), format="markdown")
```



|Country   | Accidents| Deaths.Passengers| Deaths.Total|
|:---------|---------:|-----------------:|------------:|
|US        |        62|              5429|         5873|
|Russia    |        45|              3532|         3966|
|France    |        13|              1473|         1574|
|China     |        12|              1036|         1133|
|India     |        12|              1337|         1465|
|Spain     |        12|              1786|         1892|
|Brazil    |        11|               860|          962|
|Colombia  |        10|               809|          871|
|Indonesia |        10|               984|         1109|
|Nigeria   |        10|              1001|         1151|
|Japan     |         8|              1311|         1385|
|Peru      |         8|               623|          688|
|Ukraine   |         8|               724|          788|
|Morocco   |         7|               568|          615|
|Turkey    |         7|               453|          588|
|Argentina |         6|               316|          349|
|Canada    |         6|               806|          853|
|Thailand  |         6|               578|          643|
|Ecuador   |         5|               274|          368|
|Egypt     |         5|               433|          476|

Статистика по странам по авариям во время посадки, взлета или набора высоты для коммерческих
перевозчиков.

```r
accidents_per_country2 <- data %>%
    filter(Phase %in% c("LDG", "TOF", "ICL") & Type == "COM") %>%
    group_by(Country) %>%
    summarise(Accidents = n(),
              Deaths.Passengers = sum(Deaths.P),
              Deaths.Total = sum(Deaths.T)) %>%
    arrange(desc(Accidents))

kable(head(accidents_per_country2, 20), format="markdown")
```



|Country    | Accidents| Deaths.Passengers| Deaths.Total|
|:----------|---------:|-----------------:|------------:|
|Russia     |        11|               944|         1022|
|US         |         9|               986|         1051|
|China      |         4|               253|          275|
|Brazil     |         3|               310|          350|
|France     |         3|               267|          297|
|Nigeria    |         3|               363|          380|
|Spain      |         3|               343|          359|
|Angola     |         2|               263|          271|
|Canada     |         2|               348|          365|
|DRCongo    |         2|                70|          311|
|Ecuador    |         2|                56|          133|
|India      |         2|               205|          214|
|UK         |         2|               162|          173|
|Algeria    |         1|                96|          102|
|Argentina  |         1|                60|           65|
|Azerbaijan |         1|                50|           52|
|Belarus    |         1|                55|           58|
|Benin      |         1|               136|          141|
|Bolivia    |         1|                 0|           91|
|Cameroon   |         1|               101|          111|

Статистика по странам по авариям из-за внешнего воздействия на самолёт (взрыв на борту, ракета),
сортировку производим по общему количеству жертв.

```r
accidents_per_country3 <- data %>%
    filter(Type %in% c("INB", "EXG", "EXS")) %>%
    group_by(Country) %>%
    summarise(Accidents = n(),
              Deaths.Passengers = sum(Deaths.P),
              Deaths.Total = sum(Deaths.T)) %>%
    arrange(desc(Deaths.Total))

kable(head(accidents_per_country3, 20), format="markdown")
```



|Country            | Accidents| Deaths.Passengers| Deaths.Total|
|:------------------|---------:|-----------------:|------------:|
|Ireland            |         1|               307|          329|
|Ukraine            |         1|               283|          298|
|Iran               |         1|               274|          290|
|UK                 |         1|               243|          270|
|SovietUnion        |         1|               246|          269|
|Egypt              |         1|               217|          224|
|Niger              |         1|               156|          170|
|Turkey             |         2|               125|          144|
|Vietnam            |         2|               124|          139|
|Burma              |         1|               104|          115|
|UnitedArabEmirates |         1|               107|          112|
|Abkhazia           |         1|               100|          108|
|Israel             |         1|               100|          108|
|Colombia           |         1|               101|          107|
|SriLanka           |         2|                97|          107|
|Afghanistan        |         2|                47|          105|
|France             |         1|                89|           95|
|Greece             |         1|                79|           88|
|Russia             |         1|                76|           84|
|Italy              |         1|                77|           81|

Статистика для России по годам по авариям во время посадки, взлета или набора высоты для
коммерческих перевозчиков. Сортировку производим по общему количеству жертв.

```r
accidents_ru_by_year <- data %>%
    filter(Country == "Russia") %>%
    filter(Phase %in% c("LDG", "TOF", "ICL") & Type == "COM") %>%
    group_by(Year) %>%
    summarise(Accidents = n(),
              Deaths.Passengers = sum(Deaths.P),
              Deaths.Total = sum(Deaths.T)) %>%
    arrange(desc(Deaths.Total))

kable(head(accidents_ru_by_year, 20), format="markdown")
```



| Year| Accidents| Deaths.Passengers| Deaths.Total|
|----:|---------:|-----------------:|------------:|
| 1984|         1|               169|          178|
| 1972|         2|               153|          170|
| 2006|         1|               120|          125|
| 1973|         1|               100|          108|
| 1967|         1|                99|          107|
| 1982|         1|                82|           90|
| 1986|         1|                66|           70|
| 1976|         1|                56|           62|
| 2016|         1|                55|           62|
| 2013|         1|                44|           50|

Статистика для США по годам по авариям во время посадки, взлета или набора высоты для
коммерческих перевозчиков. Сортировку производим по общему количеству жертв.

```r
accidents_us_by_year <- data %>%
    filter(Country == "US") %>%
    filter(Phase %in% c("LDG", "TOF", "ICL") & Type == "COM") %>%
    group_by(Year) %>%
    summarise(Accidents = n(),
              Deaths.Passengers = sum(Deaths.P),
              Deaths.Total = sum(Deaths.T)) %>%
    arrange(desc(Deaths.Total))

kable(head(accidents_us_by_year, 20), format="markdown")
```



| Year| Accidents| Deaths.Passengers| Deaths.Total|
|----:|---------:|-----------------:|------------:|
| 1979|         1|               258|          273|
| 1982|         2|               208|          231|
| 1987|         1|               148|          156|
| 1989|         1|               110|          111|
| 1962|         1|                87|           95|
| 1985|         1|                64|           70|
| 1960|         1|                59|           62|
| 1949|         1|                52|           53|

Статистика по годам в которых было больше всего жертв аварий по всем странам.

```r
most_deadly_years <- data %>%
    group_by(Year) %>%
    summarise(Accidents = n(),
              Deaths.Passengers = sum(Deaths.P),
              Deaths.Total = sum(Deaths.T)) %>%
    arrange(desc(Deaths.Total))

kable(head(most_deadly_years, 20), format="markdown")
```



| Year| Accidents| Deaths.Passengers| Deaths.Total|
|----:|---------:|-----------------:|------------:|
| 2001|         7|               754|         3495|
| 1985|        14|              1940|         2110|
| 1996|        13|              1592|         1961|
| 1972|        15|              1533|         1654|
| 1973|        18|              1508|         1626|
| 1974|        13|              1362|         1453|
| 1989|        13|              1234|         1368|
| 1976|        16|              1109|         1356|
| 1979|        10|              1146|         1235|
| 1992|        11|              1112|         1199|
| 1977|         8|              1117|         1187|
| 1988|         8|              1017|         1164|
| 1969|        13|               809|         1084|
| 1997|        10|               853|         1064|
| 1980|         8|               983|         1053|
| 1962|        11|               946|         1048|
| 1983|         8|               973|         1047|
| 1994|         9|               951|         1026|
| 1971|        13|               939|         1013|
| 2002|         8|               748|          976|

Статистика по месяцам в которых было больше всего жертв аварий по всем странам по всем полетам.

```r
most_deadly_months <- data %>%
    group_by(Month) %>%
    summarise(Accidents = n(),
              Deaths.Passengers = sum(Deaths.P),
              Deaths.Total = sum(Deaths.T)) %>%
    arrange(desc(Deaths.Total))

kable(head(most_deadly_months, 12), format="markdown")
```



| Month| Accidents| Deaths.Passengers| Deaths.Total|
|-----:|---------:|-----------------:|------------:|
|     9|        53|              4716|         8058|
|     7|        57|              5480|         6053|
|     8|        51|              4703|         5407|
|    12|        54|              4649|         5297|
|    11|        51|              4478|         4950|
|     3|        41|              3901|         4380|
|     6|        39|              3467|         4005|
|     2|        42|              3521|         3915|
|    10|        40|              3367|         3803|
|     5|        32|              3022|         3466|
|     1|        37|              2827|         3453|
|     4|        30|              2401|         2784|
