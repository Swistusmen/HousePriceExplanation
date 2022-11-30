# 1.Prezentacja i opis danych 
house_data<- read.csv("data.csv")
#statystyki min, max,mean, 3rd i 1st Qu
summary(house_data)
#statystyki: min, maks,avg, korelacje, wykresy
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
# d) grade- zastąpienie wartością najbliżeszej nieruchomości (gdy kilka tak samo bliskich-> średnia)
# e) sqft_lot15-> gdy brak, zastąpienie wartością najbliższej nieruchomości (gdy kilka tak samo bliskcih-> średnia)
mostFrequentDate<- tail(names(sort(table(house_data$date))),1)
house_data["date"][is.na(house_data["date"])]<- mostFrequentDate
avgBathrooms<- mean(house_data["bathrooms"][!is.na(house_data["bathrooms"])])
avgSqftLot<- mean(house_data["sqft_lot"][!is.na(house_data["sqft_lot"])])
house_data["bathrooms"][is.na(house_data["bathrooms"])]<- avgBathrooms
house_data["sqft_lot"][is.na(house_data["sqft_lot"])]<- avgSqftLot

install.packages("rgeos")
library(rgeos)

#naGrade<- house_data[which(is.na(house_data$grade)),]
naGrade<- house_data[which(is.na(house_data$grade),arr.ind=TRUE)]

rowIndexesOfMissingValues<- function(colname, df){
  noRows<- nrow(df)
  foundedRows<- list(c())
  index<-1
  for(i in 1:noRows){
    if(is.na(df[i,colname])){
      foundedRows[index]<-i
      index<- index+1
    }
  }
  return (foundedRows)
}

b<- rowIndexesOfMissingValues("grade",house_data)
print(b[1])



