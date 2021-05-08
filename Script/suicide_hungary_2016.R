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
#hun2016Data <- subset(hunData, year <="2016" & year>="1991",)


#Csoportosítás példa
hun2016ChartData <- hunData %>% 
  group_by(year, gdp_per_capita) %>% 
  summarise(suicides_no = sum(as.integer(suicides_no)))

hunCor <-hunData %>%
  group_by(year, gdp_per_capita) %>% 
  summarise(suicides_no = sum(as.integer(suicides_no)))
  
  

#Ábra képfájl elnevezése
png(file = here("Output/Chart", "SuicideHungary1991-2016withGDPpercapita.png"))


# EZAJÓchart
ggplot(data=hun2016ChartData, aes(x=year, y=suicides_no), ) +
  geom_col(width = 0.5) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  #scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
geom_bar(stat="identity", fill="steelblue") +
geom_text(aes(label=suicides_no), vjust=0.1, size=3.2, angle=45, hjust = 0)+ 
   
geom_line(aes(x = year, y =0.2*as.numeric(gdp_per_capita)), size = 0.5, color="red", group = 1)+
#geom_text(aes(label=gdp_per_capita, x=year, y=0.21*as.numeric(gdp_per_capita)), colour="black")+
 scale_y_continuous(sec.axis = sec_axis(~.*5, name = "gdp per capita"))



dev.off()


