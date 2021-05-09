library("rstudioapi")                                 # Load rstudioapi package
library(dplyr)
library(ggplot2)
library(here)
library(magrittr)



print("Adatelemzés házi feladat - Öngyilkosság és Egyfőre jutó GDP kapcsolata 1991-2016-ig")
print("Öngyilkossági adatokat tartalmazó CSV fájl beolvasása.")

#TODO: File beolvasas kiszervezese fuggvenybe
data <- read.csv(here("Data", "ongyilkossag.csv"),sep=",", header = FALSE) 

print("Öngyilkossági adatokat tartalmazó CSV fájl sikeresen beolvasva.")
cat("Beolvasott öngyilkossági adatok száma:", nrow(data), "darab sor")

#Beolvasott adatok oszlopainak elnevezése
names(data) <- c('country','year','sex','age','suicides_no','population','suicides/100k pop','country-year','HDI for year',' gdp_for_year ($) ','gdp_per_capita','generation')

#Szűrés a magyar adatokra.
hunData <- subset( data, country == "Hungary")


hunCor <-hunData %>%
  group_by(year, gdp_per_capita) %>% 
  summarise(suicides_no = sum(as.integer(suicides_no)))

# Pearson korreláció számolás
x<- cor(hunCor$suicides_no, as.numeric(hunCor$gdp_per_capita))

print(paste("Korreláció: ", x))

if(x == 0){
  print("nincs lineáris kapcsolat")
} else if (-0.2 < x & x < 0 | 0 < x & x <0.2) {
  print("gyenge, majdnem hanyagolható kapcsolat")
} else if (-0.4 < x & x < -0.2 | 0.2 < x & x < 0.4) {
  print("biztos, de gyenge kapcsolat")
} else if (-0.7 < x & x < -0.4 | 0.4 < x & x < 0.7) {
  print("közepes korreláció, jelentős kapcsolat")
} else if (-0.9 < x & x < -0.7 | 0.7 < x & x < 0.9) {
  print("magas korreláció, markáns kapcsolat") 
} else if (-1 < x & x < -0.9 | 0.9 < x & x <= 1) {
  print("nagyon magas korreláció, erős függő kapcsolat") 
  
  
} else
  print("Hibás")

#Korreláció vizuálisan:
png(file = here("Output/Chart", "CorrelationBetweenSuicideNoAndGDPPerCapita.png"))

#Scatterplot

plot(hunCor$suicides_no, hunCor$gdp_per_capita , main="Korreláció az öngyilkosságok száma és az egyfőre jutó GDP között ",
     xlab="Öngyilkosságok száma évente (fő)", ylab="Egyfőre jutó GDP ($) ", pch=19)
# Add fit lines
abline(lm(hunCor$gdp_per_capita~hunCor$suicides_no), col="red") # regression line (y~x)
lines(lowess(hunCor$suicides_no,hunCor$gdp_per_capita), col="blue") # lowess line (x,y)

dev.off()