library("rstudioapi")                                 # Load rstudioapi package
library(dplyr)
library(ggplot2)
library(here)


print("Adatelemzés házi feladat - Öngyilkosság és munkanélküliség kapcsolata 1990-2014-ig")
print("Öngyilkossági adatokat tartalmazó CSV fájl beolvasása.")

#TODO: File beolvasas kiszervezese fuggvenybe
data <- read.csv(here("~/GitHub/ADATELEMZ-S/Data", "ongyilkossag.csv"),sep=",", header = FALSE) 

print("Öngyilkossági adatokat tartalmazó CSV fájl sikeresen beolvasva.")
cat("Beolvasott öngyilkossági adatok száma:", nrow(data), "darab sor")

#Beolvasott adatok oszlopainak elnevezése
names(data) <- c('country','year','sex','age','suicides_no','population','suicides/100k pop','country-year','HDI_for_year','gdp_for_year ($) ','gdp_per_capita ($)','generation')

data <- data[-c(1),]

#Csoportosítás példa
byCountriesData <- data  %>%
  group_by(country, year)  %>%
  
  summarise( suicides_no = sum(as.integer(suicides_no)), population = sum(as.integer(population)) , gdp_per_capita = mean (as.integer(`gdp_per_capita ($)`)))  %>%
  summarise(year = year, `suicides/100k pop` = suicides_no/(population/100000), gdp_per_capita = gdp_per_capita)

countriesToDrop <- ""
countriesToDropNum <- 1

for (row in 1:nrow(byCountriesData)){
  
  if(as.integer(byCountriesData[row, "suicides/100k pop"]) == 0){
    countriesToDrop[countriesToDropNum] <- byCountriesData[row, "country"]
    countriesToDropNum <- countriesToDropNum + 1
  }
}

countriesToDrop <- as.data.frame(matrix(countriesToDrop))
countriesToDrop <- summarise(group_by(countriesToDrop, V1))
row <- 1

while (row < nrow(byCountriesData)){
  
  # print(byCountriesData[row, "country"])
  
  if(is.na(byCountriesData[row, "country"]) ){
    break
  }
  
  print(paste("Row", row, "Country:", byCountriesData[row, "country"], "year:", byCountriesData[row, "year"]))
  
  
  for (countryRowToDrop in 1:nrow(countriesToDrop)){
    
    
    if(byCountriesData[row, "country"] == countriesToDrop[countryRowToDrop, "V1"]){
      
      
      byCountriesData <- byCountriesData[-c(row),]
      row <- row -1
      break
    }
  }
  row <- row +1
}
