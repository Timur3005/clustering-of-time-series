# Загрузка необходимых библиотек
library(tidyverse)
library(lubridate)
library(cluster)
library(dtwclust)

# Загрузка данных
data_yearly <- read.csv("/mnt/data/Inflation by Year.csv")
data_quarterly <- read.csv("/mnt/data/Inflation by Quarter.csv")
data_monthly <- read.csv("C:\Users\TimurMakhmutov\Desktop\cursovaya\Inflation by Month.csv")

# Обработка данных
data_yearly <- data_yearly %>% mutate(Date = ymd(paste(Year, "-01-01", sep = ""))) %>% select(Date, Inflation)
data_quarterly <- data_quarterly %>% mutate(Date = ymd(paste(Year, (Quarter-1)*3+1, "-01", sep = ""))) %>% select(Date, Inflation)
data_monthly <- data_monthly %>% mutate(Date = ymd(paste(Year, Month, "01", sep = "-"))) %>% select(Date, Inflation)

# Объединение всех данных
data <- bind_rows(data_yearly, data_quarterly, data_monthly) %>% arrange(Date)
data

# Заполнение пропущенных значений методом линейной интерполяции
data <- data %>% complete(Date = seq(min(Date), max(Date), by="month")) %>% fill(Inflation, .direction = "downup")
