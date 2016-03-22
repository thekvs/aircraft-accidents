---
title: "Aircraft accidents"
output:
  html_document:
  md_document:
    variant: markdown_github
---


Для анализа использовались данные, приведенный на [странице](https://en.wikipedia.org/wiki/List_of_aircraft_accidents_and_incidents_resulting_in_at_least_50_fatalities) сайта WikiPedia.

Подключаем используемые в этом примере библиотеки. Враппер suppressMessages() используется,
чтобы подавить ненужную в этом случае отладочную печать при загрузке библиотек.

```r
suppressMessages(library(rvest))
suppressMessages(library(dplyr))
suppressMessages(library(date))
```

Определяем разные вспомогательные функции, которые нам понадабятся в дальнейшем.

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

Скачиваем страницу с сайта Википедии и вытаскиваем оттуда нужную нам таблицу. Выражения XPath
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

Первым делом рассчитаем количество аварий и жертв в них сгруппировав данные по странам.

```r
accidents_per_country <- data %>%
    group_by(Country) %>%
    summarise(accidents = n(), deaths_passengers = sum(Deaths.P), deaths_all = sum(Deaths.T)) %>%
    arrange(desc(accidents))

head(accidents_per_country, 20)
```

```
## Source: local data frame [20 x 4]
## 
##        Country accidents deaths_passengers deaths_all
##          (chr)     (int)             (int)      (int)
## 1           US        69              5819       9058
## 2       Russia        49              3785       4296
## 3        China        15              1166       1379
## 4       France        14              1562       1669
## 5        India        14              1499       1641
## 6    Indonesia        13              1214       1486
## 7     Colombia        12               910       1031
## 8        Spain        12              1786       1892
## 9       Brazil        11               860        962
## 10        Iran        11              1243       1528
## 11     Nigeria        11              1153       1309
## 12       Japan        10              1433       1572
## 13     Ukraine        10              1007       1163
## 14      Turkey         9               578        732
## 15       Italy         8               552        642
## 16     Morocco         8               639        695
## 17        Peru         8               623        688
## 18      Canada         7               852        905
## 19 SaudiArabia         7               978       1049
## 20          UK         7               592        706
```

То же самое, что и в предыдущем примере, но теперь только для коммерческих полетов.

```r
accidents_per_country_com <- data %>%
    filter(Type == "COM") %>%
    group_by(Country) %>%
    summarise(accidents = n(), deaths_passengers = sum(Deaths.P), deaths_all = sum(Deaths.T)) %>%
    arrange(desc(accidents))

head(accidents_per_country_com, 20)
```

```
## Source: local data frame [20 x 4]
## 
##      Country accidents deaths_passengers deaths_all
##        (chr)     (int)             (int)      (int)
## 1         US        62              5429       5873
## 2     Russia        45              3532       3966
## 3     France        13              1473       1574
## 4      China        12              1036       1133
## 5      India        12              1337       1465
## 6      Spain        12              1786       1892
## 7     Brazil        11               860        962
## 8   Colombia        10               809        871
## 9  Indonesia        10               984       1109
## 10   Nigeria        10              1001       1151
## 11     Japan         8              1311       1385
## 12      Peru         8               623        688
## 13   Ukraine         8               724        788
## 14   Morocco         7               568        615
## 15    Turkey         7               453        588
## 16 Argentina         6               316        349
## 17    Canada         6               806        853
## 18  Thailand         6               578        643
## 19   Ecuador         5               274        368
## 20     Egypt         5               433        476
```

Статистика по странам по авариям во время посадки, взлета или набора высоты для коммерческих
перевозчиков.

```r
accidents_per_country2 <- data %>%
    filter(Phase %in% c("LDG", "TOF", "ICL") & Type == "COM") %>%
    group_by(Country) %>%
    summarise(accidents = n(), deaths_passengers = sum(Deaths.P), deaths_all = sum(Deaths.T)) %>%
    arrange(desc(accidents))

head(accidents_per_country2, 20)
```

```
## Source: local data frame [20 x 4]
## 
##       Country accidents deaths_passengers deaths_all
##         (chr)     (int)             (int)      (int)
## 1      Russia        11               944       1022
## 2          US         9               986       1051
## 3       China         4               253        275
## 4      Brazil         3               310        350
## 5      France         3               267        297
## 6     Nigeria         3               363        380
## 7       Spain         3               343        359
## 8      Angola         2               263        271
## 9      Canada         2               348        365
## 10    DRCongo         2                70        311
## 11    Ecuador         2                56        133
## 12      India         2               205        214
## 13         UK         2               162        173
## 14    Algeria         1                96        102
## 15  Argentina         1                60         65
## 16 Azerbaijan         1                50         52
## 17    Belarus         1                55         58
## 18      Benin         1               136        141
## 19    Bolivia         1                 0         91
## 20   Cameroon         1               101        111
```

Статистика по странам по авариям из-за внешнего воздействия на самолёт (взрыв на борту, ракета),
сортировку производим по общему количеству жертв.

```r
accidents_per_country3 <- data %>%
    filter(Type %in% c("INB", "EXG", "EXS")) %>%
    group_by(Country) %>%
    summarise(accidents = n(), deaths_passengers = sum(Deaths.P), deaths_all = sum(Deaths.T)) %>%
    arrange(desc(deaths_all))

head(accidents_per_country3, 20)
```

```
## Source: local data frame [20 x 4]
## 
##               Country accidents deaths_passengers deaths_all
##                 (chr)     (int)             (int)      (int)
## 1             Ireland         1               307        329
## 2             Ukraine         1               283        298
## 3                Iran         1               274        290
## 4                  UK         1               243        270
## 5         SovietUnion         1               246        269
## 6               Egypt         1               217        224
## 7               Niger         1               156        170
## 8              Turkey         2               125        144
## 9             Vietnam         2               124        139
## 10              Burma         1               104        115
## 11 UnitedArabEmirates         1               107        112
## 12           Abkhazia         1               100        108
## 13             Israel         1               100        108
## 14           Colombia         1               101        107
## 15           SriLanka         2                97        107
## 16        Afghanistan         2                47        105
## 17             France         1                89         95
## 18             Greece         1                79         88
## 19             Russia         1                76         84
## 20              Italy         1                77         81
```

Статистика для России по годам по авариям во время посадки, взлета или набора высоты для
коммерческих перевозчиков. Сортировку производим по общему количеству жертв.

```r
accidents_ru_by_year <- data %>%
    filter(Country == "Russia") %>%
    filter(Phase %in% c("LDG", "TOF", "ICL") & Type == "COM") %>%
    group_by(Year) %>%
    summarise(accidents = n(), deaths_passengers = sum(Deaths.P), deaths_all = sum(Deaths.T)) %>%
    arrange(desc(deaths_all))

head(accidents_ru_by_year, 20)
```

```
## Source: local data frame [10 x 4]
## 
##     Year accidents deaths_passengers deaths_all
##    (int)     (int)             (int)      (int)
## 1   1984         1               169        178
## 2   1972         2               153        170
## 3   2006         1               120        125
## 4   1973         1               100        108
## 5   1967         1                99        107
## 6   1982         1                82         90
## 7   1986         1                66         70
## 8   1976         1                56         62
## 9   2016         1                55         62
## 10  2013         1                44         50
```

Статистика для США по годам по авариям во время посадки, взлета или набора высоты для
коммерческих перевозчиков. Сортировку производим по общему количеству жертв.

```r
accidents_us_by_year <- data %>%
    filter(Country == "US") %>%
    filter(Phase %in% c("LDG", "TOF", "ICL") & Type == "COM") %>%
    group_by(Year) %>%
    summarise(accidents = n(), deaths_passengers = sum(Deaths.P), deaths_all = sum(Deaths.T)) %>%
    arrange(desc(deaths_all))

head(accidents_us_by_year, 20)
```

```
## Source: local data frame [8 x 4]
## 
##    Year accidents deaths_passengers deaths_all
##   (int)     (int)             (int)      (int)
## 1  1979         1               258        273
## 2  1982         2               208        231
## 3  1987         1               148        156
## 4  1989         1               110        111
## 5  1962         1                87         95
## 6  1985         1                64         70
## 7  1960         1                59         62
## 8  1949         1                52         53
```

Года в которых было больше всего жертв аварий по всем странам.

```r
most_deadly_years <- data %>%
    group_by(Year) %>%
    summarise(accidents = n(), deaths_passengers = sum(Deaths.P), deaths_all = sum(Deaths.T)) %>%
    arrange(desc(deaths_all))

head(most_deadly_years, 20)
```

```
## Source: local data frame [20 x 4]
## 
##     Year accidents deaths_passengers deaths_all
##    (int)     (int)             (int)      (int)
## 1   2001         7               754       3495
## 2   1985        14              1940       2110
## 3   1996        13              1592       1961
## 4   1972        15              1533       1654
## 5   1973        18              1508       1626
## 6   1974        13              1362       1453
## 7   1989        13              1234       1368
## 8   1976        16              1109       1356
## 9   1979        10              1146       1235
## 10  1992        11              1112       1199
## 11  1977         8              1117       1187
## 12  1988         8              1017       1164
## 13  1969        13               809       1084
## 14  1997        10               853       1064
## 15  1980         8               983       1053
## 16  1962        11               946       1048
## 17  1983         8               973       1047
## 18  1994         9               951       1026
## 19  1971        13               939       1013
## 20  2002         8               748        976
```

Месяцы в которых было больше всего жертв аварий по всем странам по всем полетам.

```r
most_deadly_months <- data %>%
    group_by(Month) %>%
    summarise(accidents = n(), deaths_passengers = sum(Deaths.P), deaths_all = sum(Deaths.T)) %>%
    arrange(desc(deaths_all))

head(most_deadly_months, 12)
```

```
## Source: local data frame [12 x 4]
## 
##    Month accidents deaths_passengers deaths_all
##    (int)     (int)             (int)      (int)
## 1      9        53              4716       8058
## 2      7        57              5480       6053
## 3      8        51              4703       5407
## 4     12        54              4649       5297
## 5     11        51              4478       4950
## 6      3        41              3901       4380
## 7      6        39              3467       4005
## 8      2        42              3521       3915
## 9     10        40              3367       3803
## 10     5        32              3022       3466
## 11     1        37              2827       3453
## 12     4        30              2401       2784
```
