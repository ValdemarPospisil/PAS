library(DescTools)
load("cars.RData")

############################
## Databaze 93 nahodne vybranych vozu prodavanych v USA v roce 1993

## Promenne
# Manufacturer: vyrobce vozu
# Model: model vozu
# Type: typ vozu s kategoriemi:
#       "Small" - velmi maly, "Sporty" - sportovni, "Compact" - maly,
#       "Midsize" - stredni, "Large" - velky, "Van" - (mensi) dodavky
# Price - cena
# MPG.city - pocet mil ujetych na 1 gallon paliva ve meste
# AirBags - pocet aibagu s kategoriemi:
#       "None" - zadny, "Driver only" - ridic, "Driver & Passenger" - ridic a spolujezdec
# DriveTrain - pohon vozu s kategoriemi:
#       "Front" - predni, "Rear" - zadni, "4WD" - na 4 kola
# EngineSize - velikost motoru v litrech
# Horsepower - sila vozu
# RPM - pocet otacek za minutu v maximalni sile
# Man.trans.avail. - manualni prevodovka
# Passengers - maximalni pocet osob
# Length - delka vozu v palcich
# Width - sirka vozu v palcich
# Rear.seat.room - velikost zadniho prostoru pro cestujici
# Luggage.room - velikost zavazadloveho prostoru 
# Weight - vaha v pencich
# Origin - puvod s kategoriemi:
#       "USA" - americke auto, "non-USA" - auto puvodem z jine zeme
# Cylinders - pocet valcu

#############################
## Uprava dat
cars$Type <- factor(cars$Type, levels = c("Small", "Compact", "Midsize", "Large", "Van", "Sporty"))

#############################
## Popisne statistiky ciselnych promennych
ind.num <- c(4,5,8:10,13:17)
ciselne <- cars[,ind.num]
  # pouze ciselne promenne
vystup.num <- matrix(NA,length(ind.num),14)
for(i in 1:length(ind.num)){
  vystup.num[i,1] <- sum(is.na(ciselne[,i]) == F)
  vystup.num[i,2] <- mean(ciselne[,i], na.rm = T)
  vystup.num[i,3] <- HuberM(ciselne[,i], na.rm = T)
  vystup.num[i,4:8] <- fivenum(ciselne[,i], na.rm = T)
  vystup.num[i,9] <- sd(ciselne[,i], na.rm = T)
  vystup.num[i,10] <- IQR(ciselne[,i], na.rm = T)
  vystup.num[i,11] <- MAD(ciselne[,i], center = median(ciselne[,i], na.rm = T), na.rm = TRUE)
  vystup.num[i,12] <- CoefVar(ciselne[,i], na.rm = T)
  vystup.num[i,13] <- Skew(ciselne[,i], na.rm = T)
  vystup.num[i,14] <- Kurt(ciselne[,i], na.rm = T)
} 
rownames(vystup.num) <- names(cars)[ind.num]
colnames(vystup.num) <- c("n","Mean","Huber","Min","1st Qu","Median","3rd Qu","Max","SD","IQR","MAD","CoefVar","Skew","Kurt")
vystup.num

## Histogramy ciselnych promennych spolu s jadrovymi odhady hustoty
par(mfrow = c(1,2))
hist(cars$Price, breaks=8, col="white", xlab="Cena vozu", ylab="Hustota", 
     main="Histogram pro promennou Price", freq = F)
lines(density(cars$Price))
hist(cars$RPM, breaks=10, col="white", xlab="Otacky za min", ylab="Hustota", 
     main="Histogram pro promennou RPM", freq = F)

lines(density(cars$RPM, bw = 200))
hist(cars$Length, breaks=10, col="white", xlab="Delka vozu", ylab="Hustota", 
     main="Histogram pro promennou Length", freq = F)
lines(density(cars$Length))
hist(cars$Width, breaks=10, col="white", xlab="Sirka vozu", ylab="Hustota", 
     main="Histogram pro promennou Width", freq = F)
lines(density(cars$Width, bw = 1.1))
hist(cars$Luggage.room, breaks=10, col="white", xlab="Zavazadlovy prostor", ylab="Hustota", 
     main="Histogram pro promennou Luggage.room", freq = F)
lines(density(cars$Luggage.room, bw = 1, na.rm=T))
hist(cars$Weight, breaks=10, col="white", xlab="Vaha vozu", ylab="Hustota", 
     main="Histogram pro promennou Weight", freq = F)
lines(density(cars$Weight, bw = 180))

## Krabicove grafy ciselnych promennych
boxplot(cars$Price ~ cars$Man.trans.avail, col ="white", xlab = "Manualni prevodovka", ylab = "Cena vozu", main = "Krabicovy graf")
boxplot(cars$Horsepower ~ cars$DriveTrain, col ="white", xlab = "Pohon vozu", ylab = "Sila vozu", main = "Krabicovy graf")
boxplot(cars$Width ~ cars$Origin, col ="white", xlab = "Puvod vozu", ylab = "Sirka vozu", main = "Krabicovy graf")
boxplot(cars$EngineSize ~ cars$Man.trans.avail, col ="white", xlab = "Manualni prevodovka", ylab = "Velikost motoru", main = "Krabicovy graf")
boxplot(cars$Rear.seat.room ~ cars$DriveTrain, col ="white", xlab = "Pohon vozu", ylab = "Velikost prostoru pro cest.", main = "Krabicovy graf")
boxplot(cars$RPM ~ cars$Origin, col ="white", xlab = "Puvod vozu", ylab = "Otacky za min", main = "Krabicovy graf")
boxplot(cars$MPG.city ~ cars$Origin, col ="white", xlab = "Puvod vozu", ylab = "Mile na gallon paliva", main = "Krabicovy graf")
boxplot(cars$Weight ~ cars$Man.trans.avail, col ="white", xlab = "Manualni prevodovka", ylab = "Vaha vozu", main = "Krabicovy graf")
par(mfrow = c(1,1))

## Korelacni matice
round(cor(ciselne, use = "pairwise.complete"),4)

## Bodove grafy
par(mfrow = c(2,2))
plot(cars$Price ~ cars$MPG.city, pch=19, ylab="Cena vozu", xlab = "Mile na gallon paliva")
plot(cars$Price ~ cars$EngineSize, pch=19, ylab="Cena vozu", xlab = "Velikost motoru")
plot(cars$Price ~ cars$Horsepower, pch=19, ylab="Cena vozu", xlab = "Sila vozu")
plot(cars$Price ~ cars$RPM, pch=19, ylab="Cena vozu", xlab = "Otacky za min")
plot(cars$Price ~ cars$Length, pch=19, ylab="Cena vozu", xlab = "Delka vozu")
plot(cars$Price ~ cars$Width, pch=19, ylab="Cena vozu", xlab = "Sirka vozu")
plot(cars$Price ~ cars$Rear.seat.room, pch=19, ylab="Cena vozu", xlab = "Velikost prostoru pro cest.")
plot(cars$Price ~ cars$Luggage.room, pch=19, ylab="Cena vozu", xlab = "Zavazadlovy prostor")
plot(cars$Price ~ cars$Weight, pch=19, ylab="Cena vozu", xlab = "Vaha vozu")
plot(cars$EngineSize ~ cars$Horsepower, pch=19, ylab="Velikost motoru", xlab = "Mile na gallon paliva")
plot(cars$EngineSize ~ cars$RPM, pch=19, ylab="Velikost motoru", xlab = "Mile na gallon paliva")
plot(cars$EngineSize ~ cars$Rear.seat.room, pch=19, ylab="Velikost motoru", xlab = "Velikost prostoru pro cest.")
plot(cars$EngineSize ~ cars$Weight, pch=19, ylab="Velikost motoru", xlab = "Vaha vozu")
plot(cars$Width ~ cars$Rear.seat.room, pch=19, ylab="Sirka vozu", xlab = "Velikost prostoru pro cest.")
plot(cars$Width ~ cars$Luggage.room, pch=19, ylab="Sirka vozu", xlab = "Zavazadlovy prostor")
plot(cars$Width ~ cars$Weight, pch=19, ylab="Sirka vozu", xlab = "Vaha vozu")
par(mfrow = c(1,1))

###################################
## Popisne statistiky kategorickych promennych
cbind("bezne abs. cetnosti"=table(cars$Type),"kumulativni abs. cetnosti"=cumsum(table(cars$Type)),
      "bezne rel. cetnosti"=round(prop.table(table(cars$Type)),4),"kumulativni rel. cetnosti"=cumsum(round(prop.table(table(cars$Type)),4)))
cbind("bezne abs. cetnosti"=table(cars$AirBags),"kumulativni abs. cetnosti"=cumsum(table(cars$AirBags)),
      "bezne rel. cetnosti"=round(prop.table(table(cars$AirBags)),4),"kumulativni rel. cetnosti"=cumsum(round(prop.table(table(cars$AirBags)),4)))
cbind("bezne abs. cetnosti"=table(cars$DriveTrain),"kumulativni abs. cetnosti"=cumsum(table(cars$DriveTrain)),
      "bezne rel. cetnosti"=round(prop.table(table(cars$DriveTrain)),4),"kumulativni rel. cetnosti"=cumsum(round(prop.table(table(cars$DriveTrain)),4)))
cbind("bezne abs. cetnosti"=table(cars$Man.trans.avail),"kumulativni abs. cetnosti"=cumsum(table(cars$Man.trans.avail)),
      "bezne rel. cetnosti"=round(prop.table(table(cars$Man.trans.avail)),4),"kumulativni rel. cetnosti"=cumsum(round(prop.table(table(cars$Man.trans.avail)),4)))
cbind("bezne abs. cetnosti"=table(cars$Origin),"kumulativni abs. cetnosti"=cumsum(table(cars$Origin)),
      "bezne rel. cetnosti"=round(prop.table(table(cars$Origin)),4),"kumulativni rel. cetnosti"=cumsum(round(prop.table(table(cars$Origin)),4)))
cbind("bezne abs. cetnosti"=table(cars$Cylinders),"kumulativni abs. cetnosti"=cumsum(table(cars$Cylinders)),
      "bezne rel. cetnosti"=round(prop.table(table(cars$Cylinders)),4),"kumulativni rel. cetnosti"=cumsum(round(prop.table(table(cars$Cylinders)),4)))

## Grafy kategorickych promennych
popis<-paste(sort(unique(cars$DriveTrain)),"(",round(prop.table(table(cars$DriveTrain))*100,2),"%)")
pie(table(cars$DriveTrain),lab=popis,col="white",main="Kolacovy graf pro promennou DriveTrain")
popis<-paste(sort(unique(cars$Origin)),"(",round(prop.table(table(cars$Origin))*100,2),"%)")
pie(table(cars$Origin),lab=popis,col="white",main="Kolacovy graf pro promennou Origin")

par(mfrow = c(1,2),mar = c(5, 5, 6, 2))
barplot(table(cars$Type),col="white",main="Sloupcovy graf pro promennou Type",ylab="Pocty", ylim=c(0,25))
x <- barplot(table(cars$Type),plot=F)[,1]; text(x,table(cars$Type),labels=table(cars$Type),pos=3)
barplot(table(cars$AirBags),col="white",main="Sloupcovy graf pro promennou AirBags",ylab="Pocty", ylim = c(0,50))
x <- barplot(table(cars$AirBags),plot=F)[,1]; text(x,table(cars$AirBags),labels=table(cars$AirBags),pos=3)
barplot(table(cars$Man.trans.avail),col="white",main="Sloupcovy graf pro promennou Man.trans.avail",ylab="Pocty", ylim = c(0,70))
x <- barplot(table(cars$Man.trans.avail),plot=F)[,1]; text(x,table(cars$Man.trans.avail),labels=table(cars$Man.trans.avail),pos=3)
barplot(table(cars$Cylinders),col="white",main="Sloupcovy graf pro promennou Cylinders",ylab="Pocty", ylim = c(0,60))
x <- barplot(table(cars$Cylinders),plot=F)[,1]; text(x,table(cars$Cylinders),labels=table(cars$Cylinders),pos=3)
par(mfrow = c(1,1))

## Graf pro dvojici kategorickych promennych
plot(cars$AirBags ~ cars$Type, xlab ="Typ vozu", ylab = "Airbagy")
plot(as.factor(cars$Cylinders) ~ cars$DriveTrain, xlab="Pohon vozu", ylab = "Pocet valcu")
