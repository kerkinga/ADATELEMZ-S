library("rstudioapi")                                 # Load rstudioapi package
library(dplyr)
library(ggplot2)
library(ggalt)
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

#Csoportos�t�s
byCountriesData <- data  %>%
  group_by(country, year)  %>%
  
  summarise( suicides_no = sum(as.integer(suicides_no)), population = sum(as.integer(population)) , gdp_per_capita = mean (as.integer(`gdp_per_capita ($)`)))  %>%
  summarise(year = year, `suicides/100k pop` = suicides_no/(population/100000), gdp_per_capita = gdp_per_capita)


# innent�l k�vetkezik az a csod�latos adattiszt�t�s, hogy ne tartalmazzon olyan orsz�got, ahol van 0-t tartalmaz� �v,
# mert ott vagy nem el�g nagy a popul�ci�, vagy hi�nyos az adat

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

# korrel�ci�t sz�molunk
x<- cor(byCountriesData$`suicides/100k pop`, as.numeric(byCountriesData$gdp_per_capita))

# kiemelj�k a kifejezetten magas �ngyilkoss�gi r�t�val, �s a kifejezten magas egy f�re jut� gdp-vel rendelkez� orszgokat
richCountries <- subset(byCountriesData, gdp_per_capita > 45000)
highSuicideCountries <- subset(byCountriesData, `suicides/100k pop` > 28)

ggplot(byCountriesData, aes(x = gdp_per_capita, y = `suicides/100k pop`)) + geom_point() +theme_bw() + geom_smooth(method="loess", se=FALSE) +
  geom_encircle(aes(x = gdp_per_capita, y = `suicides/100k pop`), data=highSuicideCountries, color="red", size=3) + 
  geom_encircle(aes(x = gdp_per_capita, y = `suicides/100k pop`), data=richCountries, color="green", size=3)

ggplot(byCountriesData, aes(x = gdp_per_capita, y = `suicides/100k pop`))+
  stat_density_2d(aes(fill = ..count..), geom = "raster", contour = FALSE)+
  scale_fill_distiller(palette= "Spectral", direction=-1) +
  geom_point() +theme_bw() + geom_smooth(method="loess", se=FALSE)

