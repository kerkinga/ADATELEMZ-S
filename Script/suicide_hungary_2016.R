library("rstudioapi")                                 # Load rstudioapi package
library(dplyr)
library(ggplot2)
library(here)

print("Adatelemzés házi feladat - Öngyilkosság és munkanélküliség kapcsolata 1990-2014-ig")
print("Öngyilkossági adatokat tartalmazó CSV fájl beolvasása.")

#TODO: File beolvasas kiszervezese fuggvenybe
data <- read.csv(here("Data", "ongyilkossag.csv"),sep=",", header = FALSE) 

print("Öngyilkossági adatokat tartalmazó CSV fájl sikeresen beolvasva.")
cat("Beolvasott öngyilkossági adatok száma:", nrow(data), "darab sor")

#Beolvasott adatok oszlopainak elnevezése
names(data) <- c('country','year','sex','age','suicides_no','population','suicides/100k pop','country-year','HDI for year',' gdp_for_year ($) ','gdp_per_capita ($)','generation')

#Szűrés a magyar 2016 adatokra.
hunData <- subset( data, country == "Hungary")
hun2016Data <- subset(hunData, year == "2016")

#Csoportosítás példa
hun2016PieChartData <- hun2016Data %>% 
  group_by(age) %>% 
  summarise(suicides_no = sum(as.integer(suicides_no)))

#Ábra képfájl elnevezése
png(file = here("Output/Chart", "SuicideHungary2016ByAges.png"))

#Ábra generálása.
pie(hun2016PieChartData$suicides_no, 
    labels = paste(hun2016PieChartData$age, sep = ":", hun2016PieChartData$suicides_no),
    col = rainbow(length(hun2016PieChartData$suicides_no)), 
    main = "2016-s Magyrországi öngyilkosságok száma korosztályonként"
    )

dev.off()
