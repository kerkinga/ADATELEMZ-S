library("rstudioapi")                                 # Load rstudioapi package
library(dplyr)
library(ggplot2)
library(here)


print("Adatelemz√©s h√°zi feladat - √ñngyilkoss√°g √©s munkan√©lk√ºlis√©g kapcsolata 1990-2014-ig")
print("√ñngyilkoss√°gi adatokat tartalmaz√≥ CSV f√°jl beolvas√°sa.")

#TODO: File beolvasas kiszervezese fuggvenybe
data <- read.csv(here("~/GitHub/ADATELEMZ-S/Data", "ongyilkossag.csv"),sep=",", header = FALSE) 

print("√ñngyilkoss√°gi adatokat tartalmaz√≥ CSV f√°jl sikeresen beolvasva.")
cat("Beolvasott √∂ngyilkoss√°gi adatok sz√°ma:", nrow(data), "darab sor")

#Beolvasott adatok oszlopainak elnevez√©se
names(data) <- c('country','year','sex','age','suicides_no','population','suicides/100k pop','country-year','HDI_for_year','gdp_for_year ($) ','gdp_per_capita ($)','generation')

data <- data[-c(1),]

#CsoportosÌt·s
byCountriesData <- data  %>%
  group_by(country, year)  %>%
  
  summarise( suicides_no = sum(as.integer(suicides_no)), population = sum(as.integer(population)) , gdp_per_capita = mean (as.integer(`gdp_per_capita ($)`)))  %>%
  summarise(year = year, `suicides/100k pop` = suicides_no/(population/100000), gdp_per_capita = gdp_per_capita)


# innentıl kˆvetkezik az a csod·latos adattisztÌt·s, hogy ne tartalmazzon olyan orsz·got, ahol van 0-t tartalmazÛ Èv,
# mert ott vagy nem elÈg nagy a popul·ciÛ, vagy hi·nyos az adat

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
  
  if(is.na(byCountriesData[row, "country"]) ){
    break
  }
  
  # print(paste("Row", row, "Country:", byCountriesData[row, "country"], "year:", byCountriesData[row, "year"]))
  
  
  for (countryRowToDrop in 1:nrow(countriesToDrop)){
    
    
    if(byCountriesData[row, "country"] == countriesToDrop[countryRowToDrop, "V1"]){
      
      
      byCountriesData <- byCountriesData[-c(row),]
      row <- row -1
      break
    }
  }
  row <- row +1
}

byCountriesData <- byCountriesData  %>%
  group_by(country)  %>%
  summarise(`suicides/100k pop` = mean(`suicides/100k pop`), gdp_per_capita = mean(gdp_per_capita))

highSuicideCountries <- subset(byCountriesData, `suicides/100k pop` > 20)
richcountries=

ggplot(byCountriesData, aes(x = gdp_per_capita, y = `suicides/100k pop`)) + geom_point() +theme_bw() +
  geom_encircle(aes(x=area, y=poptotal), data=highSuicideCountries, color="red", size=2, expand=0.08)



