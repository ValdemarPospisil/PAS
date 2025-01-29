## (5) Regresivní přímka (rovnice regresivní přímky)

# používá se k odhadu hodnot závislé proměnné Y na základě nezávislé proměnné X.
# Rovnice: Y = a + bX
#   a -> intercept (hodnota Y, X=0)
#   b -> sKlon přímky (změna Y při zvýšení X o 1 jednotku)


data <- data.frame(
  X = seq(1, 50, by = 1), # nezávislá proměnná (např. hodiny studia)
  Y = seq(1, 50, by = 1) + rnorm(50, mean = 0, sd = 2)  # závsilá proměnná s šumem
)

plot(data$X, data$Y)
library(ggplot2)
ggplot(data, aes(x= X, y = Y)) + 
  geom_point() +
  labs(title = "X a Y", x="Hodiny studia", y="Známky") +
  theme_minimal()
# pozitivní rostoucí vztah

# výpočet regresivní přímky
model <- lm(Y ~ X, data = data)
summary(model)
# Intercept (a) a sklon (b) přímky: - Intercept představuje očekávanou hodnotu Y, když X=0.
#   - Sklon ukazuje, o kolik se změní Y, když X vzroste o jednu jednotku. 
# R-squared: tento koeficient determinace ukazuje, jak dobře model vysvětuluje variabilitu Y.
#   hodnota blízká 1 znamená, že model velmi dobře vysvětluje data. Například že hodnota 0.85
#   značí, že 85 % variability Y lze vysvětlit modelem.
# P-hodnoty: - P-hodnota u sklonu testuje hypotézu, že skutečný sklon je nulový 
#   (tj. že mezi X a Y není žádný vztah). 
#   - Pokud je p-hodnota menší než 0.05, můžeme vztah považovat za statisticky významný.


# přidání regresivní přímky do grafu
abline(model, col= "red", lwd = 2)
# Červená přímka je regresní přímka, která popisuje vztah mezi hodinami studia a známkami. 
# Přímka minimalizuje součet čtverců rozdílů mezi skutečnými hodnotami a hodnotami predikovanými modelem.

intercept <- coef(model)[1]
slope <- coef(model)[2]

cat("Rovnice regresivní přímky: Y = ", round(intercept, 2), " + ", round(slope, 2), "* X\n")
# rovnice nám umožnuje odhadnout hodnotu Y (známky) pro libovolnou hodnotu X (hodiny studia)


## Predikce hodnot 

nova_data <- data.frame(X = c(21, 22, 23 ,24, 25))
predikce <- predict(model, newdata = nova_data)

data.frame(X = nova_data$X, Predikce_Y = round(predikce, 2))
# predikované hodnoty Y odpovídají očekávaným známkám  pro nové hodnoty hodin studia.

## Metoda nejmenších čtvreců (MNC)
#   slouží k nalezení regresivní přímky, která minimalizuje chyby mezi skutečnými hodnotami Y 
#   a predikovanými hodnotami Y°

# Princip metody
#   Rezidua: e = Y - Y°, kde Y  je skutečná hodnota a Y° je predikovaná hodnota.
#   MNC minimalizuje součet čtverců reziduí:
#   S = sum(e˘2) = sum(Y - (a + b * X))^2

# Proč čtverce?
#   negativní a pozitivní odchylky se nevyruší
#   velké odchylky jsou penalizovány více než malé, což zduraznuje jejich vliv.

# Co MNC ukazuje?
#   koeficient b: udává, o kolik se změní Y, když X vzroste o jednu jednotku
#   koeficient a: reprezentuje očekávanou hodnotu Y, když X = 0
#   rezidua: ukazují jak dobře model odpovídá datům. menší rezidua znamenají lepší model.

rezidua <- data$Y - predict(model)

plot(data$X, data$Y, 
     main= "Metoda nejmenších čtvreců: Rezidua",
     xlab="Hodiny studia (X)",
     ylab="Známky(Y)",
     pch = 16, col= "blue", lwd = 2)

abline(model, col= "red", lwd = 2)

for (i in 1:nrow(data)) {
  segments(data$X[i], predict(model)[i], data$X[i], data$Y[i], col="green", lwd= 2)
}
# Modrá: skutečná data (X, Y)
# Červená: regresivní přímka
# Zelená: Rezidua, která MNC minimalizuje (Y = a + bX)

