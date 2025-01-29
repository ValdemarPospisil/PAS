## Hdonocení normality a tvaru rozdělení

# 1. Vizualizace dat
#     - Histogram
#     - QQPlot

data_normal <- rnorm(100, mean = 50, sd = 10)
data_non_normal <- runif(100, min= 20, max = 80)

par(mfrow = c(2, 2))

# Histogramy
hist(data_normal, breaks = 15, main= "Normální data", xlab= "Hodnoty", col= "skyblue", freq= FALSE)
lines(density(data_normal), col= "red", lwd = 2)
hist(data_non_normal, breaks = 15, main= "Nenormální data", xlab= "Hodnoty", col= "orange", freq= FALSE)
lines(density(data_non_normal), col= "red", lwd = 2)


# QQPloty
qqnorm(data_normal, main = "QQ-plot: Normální data")
qqline(data_normal, col = "red", lwd = 2)
qqnorm(data_non_normal, main="QQ-plot: Nenormální data")
qqline(data_non_normal, col = "red", lwd = 2)


# 2. Pomocí statistických testů

# Shapiro-Wilk test:

#   Ho : Data mají normální rozdělení.
#   Ha : Data nemají normální rozdělení.
#   p-hodnota > 0.05 naznačuje, že nelze zamítnout H0 (data jsou normální).

shapiro.test(data_normal)
# p = 0,7366 > aplha -> nezamítáme nulovou hypotézu
shapiro.test(data_non_normal)
# p = 0.0003663 < aplha -> zamítáme nulovou hypotézu -> platí alternativní -> nemá normální rozdělení


# 3. Pomocí statistických parametrů
library(DescTools)


Skew(data_normal)
# -0.03246 <- blízko nule
Kurt(data_normal)
# -0.2430022 <- blízko nule

Skew(data_non_normal)
# 0.14415 <- taky blízko nule -> může značit normální rozdělení i když není
Kurt(data_non_normal)
# -1.253365 <- daleko od nuly -> špičatost prozrazuje že proměnná nemá normální rozdělení



# Odlehlé hodnoty

outliers <- c(120, 130, 122, 111, 145)
data_with_outliers <- c(data_normal, outliers)

hist(data_with_outliers, breaks = 15, main= "Normální data", xlab= "Hodnoty", col= "skyblue", freq= FALSE)
lines(density(data_with_outliers), col= "red", lwd = 2)


quartiles <- quantile(data_with_outliers, probs= c(0.25, 0.75))
IQR <- IQR(data_with_outliers)

dolni_hranice <- quartiles[1] - 1.5 * IQR
horni_hranice <- quartiles[2] + 1.5 * IQR

hist(data_with_outliers[data_with_outliers < 77], breaks = 15, main= "Normální data", xlab= "Hodnoty", col= "skyblue", freq= FALSE)
lines(density(data_with_outliers[data_with_outliers < 77]), col= "red", lwd = 2)

