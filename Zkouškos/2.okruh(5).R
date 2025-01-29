## (5) Regresivní přímka (rovnice regresivní přímky)

# Používá se k odhadu hodnot závislé proměnné Y na základě nezávislé proměnné X.
# Rovnice: Y = a + bX
#   a -> intercept (hodnota Y, X=0)
#   b -> sklon přímky (změna Y při zvýšení X o 1 jednotku)

set.seed(123)
data <- data.frame(
  X = seq(1, 50, by = 1), # nezávislá proměnná (např. hodiny studia)
  Y = seq(1, 50, by = 1) + rnorm(50, mean = 0, sd = 2)  # závislá proměnná s šumem
)

library(ggplot2)
# Vykreslení bodového grafu
plot(data$X, data$Y, main = "Scatter plot X a Y", xlab = "Hodiny studia", ylab = "Známky", pch = 19)
ggplot(data, aes(x= X, y = Y)) + 
  geom_point() +
  labs(title = "X a Y", x="Hodiny studia", y="Známky") +
  theme_minimal()

# Výpočet regresní přímky
model <- lm(Y ~ X, data = data)
summary(model)

# Interpretace výsledků
intercept <- coef(model)[1]
slope <- coef(model)[2]
R_squared <- summary(model)$r.squared
p_value <- summary(model)$coefficients[2,4]

cat("Rovnice regresivní přímky: Y =", round(intercept, 2), "+", round(slope, 2), "* X\n")
cat("R-squared:", round(R_squared, 3), "(Vysvětluje", round(R_squared * 100, 1), "% variability Y)\n")
cat("P-hodnota sklonu:", p_value, "(Statistická významnost)\n")

# Přidání regresní přímky do grafu
abline(model, col= "red", lwd = 2)

# Predikce hodnot
nova_data <- data.frame(X = c(21, 22, 23 ,24, 25))
predikce <- predict(model, newdata = nova_data)
data.frame(X = nova_data$X, Predikce_Y = round(predikce, 2))

## Metoda nejmenších čtverců (MNC)
#   Slouží k nalezení regresní přímky, která minimalizuje chyby mezi skutečnými hodnotami Y 
#   a predikovanými hodnotami Y°

rezidua <- data$Y - predict(model)
plot(data$X, data$Y, 
     main= "Metoda nejmenších čtverců: Rezidua",
     xlab="Hodiny studia (X)",
     ylab="Známky(Y)",
     pch = 16, col= "blue", lwd = 2)

abline(model, col= "red", lwd = 2)

for (i in 1:nrow(data)) {
  segments(data$X[i], predict(model)[i], data$X[i], data$Y[i], col="green", lwd= 2)
}

# Interpretace koeficientů a významnosti
# - Intercept a: Očekávaná hodnota Y, když X=0.
# - Sklon b: Změna Y při zvýšení X o jednu jednotku.
# - R-squared: Ukazuje, jak dobře model vysvětluje variabilitu Y.
# - P-hodnota: Pokud je menší než 0.05, vztah je statisticky významný.
