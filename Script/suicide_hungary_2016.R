library("rstudioapi")                                 # Load rstudioapi package
library(dplyr)
library(ggplot2)
library(here)
library(magrittr)



print("Adatelemzés házi feladat - Öngyilkosság és munkanélküliség kapcsolata 1990-2014-ig")
print("Öngyilkossági adatokat tartalmazó CSV fájl beolvasása.")

#TODO: File beolvasas kiszervezese fuggvenybe
data <- read.csv(here("Data", "ongyilkossag.csv"),sep=",", header = FALSE) 

print("Öngyilkossági adatokat tartalmazó CSV fájl sikeresen beolvasva.")
cat("Beolvasott öngyilkossági adatok száma:", nrow(data), "darab sor")

#Beolvasott adatok oszlopainak elnevezése
names(data) <- c('country','year','sex','age','suicides_no','population','suicides/100k pop','country-year','HDI for year',' gdp_for_year ($) ','gdp_per_capita','generation')

#Szűrés a magyar 2016 adatokra.
hunData <- subset( data, country == "Hungary")
#hun2016Data <- subset(hunData, year <="2016" & year>="1991",)

#szűrésGDP-re
#hunGDPData <- subset( data, country == "Hungary")
#hunGDP2016 <-subset(hunGDPData,year <="2016" & year>="2010",)
#hunGDP2016group <-hunGDP2016 %>% 
 # group_by(year, gdp_per_capita) %>%

#Csoportosítás példa
hun2016ChartData <- hunData %>% 
  group_by(year, gdp_per_capita) %>% 
  summarise(suicides_no = sum(as.integer(suicides_no)))
  
  

#Ábra képfájl elnevezése
png(file = here("Output/Chart", "SuicideHungary2010-2016withGDPpercapita.png"))





#próba line chart
#ggplot(hun2016PieChartData, aes(x = year, y = gdp_per_capita)) +                            
  #geom_line(size = 1.5, color="red", group = 1)




# EZAJÓchart
ggplot(data=hun2016ChartData, aes(x=year, y=suicides_no), ) +
geom_bar(stat="identity", fill="steelblue") +
geom_text(aes(label=suicides_no), vjust=-0.3, size=3.5)+
geom_line(aes(x = year, y =0.2*as.numeric(gdp_per_capita)), size = 0.5, color="red", group = 1)+
#geom_text(aes(label=gdp_per_capita, x=year, y=0.21*as.numeric(gdp_per_capita)), colour="black")+
 scale_y_continuous(sec.axis = sec_axis(~.*5, name = "gdp per capita"))

x<- cor(hun2016ChartData$suicides_no, as.numeric(hun2016ChartData$gdp_per_capita))

if(x == 0){
  print("nincs lineáris kapcsolat")
} else if (-0.2 < x & x < 0 | 0 < x & x <0.2) {
  print("gyenge, majdnem hanyagolható kapcsolat")
} else if (-0.4 < x & x <-0.2 | 0.2<x<0.4) {
  print("biztos, de gyenge kapcsolat")
} else if (-0.7<x & x<-0.4 | 0.4<x & x<0.7) {
  print("közepes korreláció, jelentős kapcsolat")
} else if (-0.9<x & x<-0.7 | 0.7<x & x<0.9) {
  print("magas korreláció, markáns kapcsolat") 
} else if (-1<x & x<-0.9 | 0.9<x & x<1) {
  print("nagyon magas korreláció, erős függő kapcsolat") 
  
  
  } else
  print("Nem jó")



dev.off()

