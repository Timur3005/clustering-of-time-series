# Загрузка необходимых библиотек
library(tidyverse)
library(lubridate)
library(cluster)
library(dtwclust)

# Загрузка данных
data_yearly <- read.csv("C://Users//TimurMakhmutov//Desktop//cursovaya//Inflation by Year.csv")
data_quarterly <- read.csv("C://Users//TimurMakhmutov//Desktop//cursovaya//Inflation by Quarter.csv")
data_monthly <- read.csv("C://Users//TimurMakhmutov//Desktop//cursovaya//Inflation by Month.csv")

# Переименование колонок для удобства
colnames(data_yearly) <- c("Year", "Inflation")
colnames(data_quarterly) <- c("Year", "Quarter", "Inflation")
colnames(data_monthly) <- c("Year", "Month", "Inflation")

# Обработка данных
data_yearly <- data_yearly %>% mutate(Date = ymd(paste(Year, "01-01", sep = "-"))) %>% select(Date, Inflation)
data_quarterly <- data_quarterly %>%
  mutate(
    Month = case_when(
      Quarter == "Q1" ~ "01",
      Quarter == "Q2" ~ "04",
      Quarter == "Q3" ~ "07",
      Quarter == "Q4" ~ "10"
    ),
    Date = ymd(paste(Year, Month, "01", sep = "-"))
  ) %>% select(Date, Inflation)
data_monthly <- data_monthly %>%
  mutate(
    Month = case_when(
      Month == "JAN" ~ "01",
      Month == "FEB" ~ "02",
      Month == "MAR" ~ "03",
      Month == "APR" ~ "04",
      Month == "MAY" ~ "05",
      Month == "JUN" ~ "06",
      Month == "JUL" ~ "07",
      Month == "AUG" ~ "08",
      Month == "SEP" ~ "09",
      Month == "OCT" ~ "10",
      Month == "NOV" ~ "11",
      Month == "DEC" ~ "12"
    ),
    Date = ymd(paste(Year, Month, "01", sep = "-"))
  ) %>% select(Date, Inflation)

# Объединение всех данных
data <- bind_rows(data_yearly, data_quarterly, data_monthly) %>% arrange(Date)

# Заполнение пропущенных значений методом линейной интерполяции
data <- data %>% complete(Date = seq(min(Date), max(Date), by="month")) %>% fill(Inflation, .direction = "downup")

