# 1.Prezentacja i opis danych 
house_data<- read.csv("data.csv")
#statystyki min, max,mean, 3rd i 1st Qu
summary(house_data)
#typy danych
str(house_data)

#zestaw zawiera 20 zmiennych: 19 objasniajacych i 1 zmienna objasniana price. 
#zmienne (6) waterfront, date, zipcode, view, condition, grade są zmiennymi jakościowymi z podana skala (za wyjątkiem grade- tutaj wartości znajdują się w zakresie 4-13)

#Przed obliczeniem korelacji w punkcie 1 należy poradzić sobie z problemami brakujących i odstających wartości. Należy więc najpierw wykonać punkt 2, by następnie wrócić do pierwszego
#2
house_data[complete.cases(house_data),]
for(i in 1:20){
  isNA<-any(is.na(house_data[,i]))
  if(isNA){
    print(names(house_data)[i])
  }
}
# poniżej znajduje się zestawienie zmiennych z brakującymi wartościami oraz opis jak problem rozwiązano:
# a) date- zastąpienie zmienną najczęsciej powtarzającą się
# b) bathrooms- zastąpienie brakujących wartości średnią (nie znamy specyfiki rynku amerykańskiego, ale zakładamy że dom musi mieć łazienkę)
# c) sqft_lot- zastąpienie średnią
# d) grade- zastapienie srednia
# e) sqft_lot15-> gdy brak, zastąpienie wartością najbliższej nieruchomości (gdy kilka tak samo bliskcih-> średnia)
mostFrequentDate<- tail(names(sort(table(house_data$date))),1)
house_data["date"][is.na(house_data["date"])]<- mostFrequentDate
avgBathrooms<- mean(house_data["bathrooms"][!is.na(house_data["bathrooms"])])
avgSqftLot<- mean(house_data["sqft_lot"][!is.na(house_data["sqft_lot"])])
avgGrade<- mean(house_data["grade"][!is.na(house_data["grade"])])
house_data["bathrooms"][is.na(house_data["bathrooms"])]<- avgBathrooms
house_data["sqft_lot"][is.na(house_data["sqft_lot"])]<- avgSqftLot
house_data["grade"][is.na(house_data["grade"])]<- avgGrade

naSqft_lot15<- which(is.na(house_data$sqft_lot15))

library("geosphere")

for(j in naSqft_lot15){
  distance<- 10000000000000000000000000
  val<- 0
  for(i in 1:nrow(house_data)){
    if(i!=j){
      tempDist<-distm(c(house_data[j,18],house_data[j,17]),c(house_data[i,18],house_data[i,17]),fun=distHaversine)
      if(tempDist<distance){
        distance<-tempDist
        val<- house_data[i,20]
      }
    }
  }
  house_data[i,20]<-val
}
#zamiana zmiennej typu char "date" na zmienne liczbowe- kolejne miesiace
year_month <- c("2014 May", "2014 June", "2014 Jul", "2014 Aug", "2014 Sep", "2014 Oct", "2014 Nov", "2014 Dec",
                "2015 Jan", "2015 Feb", "2015 Mar", "2015 Apr", "2015 May")

for (i in 1: length(year_month))
{
  house_data$date <- replace(house_data$date, house_data$date == year_month[i], i)
}

house_data$date <- as.numeric(house_data$date)
unique(house_data$date) #sprawdzenie czy wszystko zostało poprawnie zamienione

#Zamiana roku renowacji na rok budowy w przypadku gdy rok renowacji ==0
house_data$yr_renovated <- ifelse(house_data$yr_renovated == 0, house_data$yr_built, house_data$yr_renovated)

#Po przekształceniu danych i pozbyciu się braków wracamy do punktu 1
#Wydzielenie zmiennych numerycznych
library(dplyr)
install.packages("psych")
library(psych)
numerical_variables<- house_data[,c("date","bedrooms", "bathrooms", "sqft_living", "sqft_lot", 
                                  "floors","sqft_above","sqft_basement","yr_built",  "yr_renovated", "lat", "long"  )]
describe(numerical_variables)

library(ggplot2)
# wykres pudełkowy
par(mfrow=c(2,2))
for (i in 1:length(numerical_variables)) {
  boxplot(numerical_variables[,i], main=names(numerical_variables[i]), type="l")
}

#histogram
par(mfrow=c(2,2))
for (i in 1:length(numerical_variables)) {
  hist(numerical_variables[,i], main=names(numerical_variables[i]))
}

#wydzielenie zmiennych skalarnych
scalar_variables <- house_data[, c("view", "condition", "grade")]

describe(scalar_variables)
# wykres pudełkowy
par(mfrow=c(2,2))
for (i in 1:length(scalar_variables)) {
  boxplot(scalar_variables[,i], main=names(scalar_variables[i]), type="l")
}

#histogram
par(mfrow=c(2,2))
for (i in 1:length(scalar_variables)) {
  hist(scalar_variables[,i], main=names(scalar_variables[i]))
}

#wspolczynnik zmiennosci miedzy zmiennymi numerycznymi
sapply(numerical_variables[], function(x) sd(x) / mean(x) * 100)
correlationTable<- round(cor(numerical_variables,method="pearson"),,digits=4)
#tablica korelacji zmiennych numerycznych
correlationTable

corPlot(numerical_variables, cex=0.8)

# wykresy zaleznosci ceny od zmiennych numerycznych
par(mfrow=c(2,2))
plot(numerical_variables[][,1],house_data[,"price"], xlab=names(numerical_variables[1]), ylab=names(house_data["price"]))
plot(numerical_variables[][,2],house_data[,"price"], xlab=names(numerical_variables[2]), ylab=names(house_data["price"]))
plot(numerical_variables[][,3],house_data[,"price"], xlab=names(numerical_variables[3]), ylab=names(house_data["price"]))
plot(numerical_variables[][,4],house_data[,"price"], xlab=names(numerical_variables[4]), ylab=names(house_data["price"]))
plot(numerical_variables[][,5],house_data[,"price"], xlab=names(numerical_variables[5]), ylab=names(house_data["price"]))
plot(numerical_variables[][,6],house_data[,"price"], xlab=names(numerical_variables[6]), ylab=names(house_data["price"]))
plot(numerical_variables[][,7],house_data[,"price"], xlab=names(numerical_variables[7]), ylab=names(house_data["price"]))
plot(numerical_variables[][,8],house_data[,"price"], xlab=names(numerical_variables[8]), ylab=names(house_data["price"]))
plot(numerical_variables[][,9],house_data[,"price"], xlab=names(numerical_variables[9]), ylab=names(house_data["price"]))
plot(numerical_variables[][,10],house_data[,"price"], xlab=names(numerical_variables[10]), ylab=names(house_data["price"]))
plot(numerical_variables[][,11],house_data[,"price"], xlab=names(numerical_variables[11]), ylab=names(house_data["price"]))
plot(numerical_variables[][,12],house_data[,"price"], xlab=names(numerical_variables[12]), ylab=names(house_data["price"]))

# 3. Podzial zbiorow na testowy i uczacy
smp_size <- floor(0.8 * nrow(house_data))
set.seed(306511)
train_ind <- sample(seq_len(nrow(house_data)), size = smp_size)
train  <- house_data[train_ind, ]
test   <- house_data[-train_ind, ]
summary(train)
summary(test)
# 4. Dobor zmiennych do modelu
# zdecydowalismy sie uzyc metody Hellwiga