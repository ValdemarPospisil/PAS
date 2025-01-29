## Identifikace odlehlých hodnot

# outlier = odlehlá hodnota
# odlehlé hodnoty (stačí 1) znemožňují použití některých stat. metod (průměr, sd ..)
# často signalizují chybu v měření nebo nepřesně získaná data, která mají odlišné rozdělení než zbytek dat

# Reakce na outliery
#     vyloučení outlierů
#         odstranění odlehlých hodnot (prostě se smažou)
#     použití robustních metod
#         odolný průměr (medián místo průměru)
#         odolná sd (interkvartilové rozpětí)
#     identifikace outlierů
#         metoda kvartilového rozpětí (kreslení boxplotu)
#         pravidlo 3 nebo 4 sigma (pro sym./asym. rozdělení)

# Histogram
#   jdou občas vidět odlehlé hodnoty ale není to spohlivé

# BoxPlot
#   IQR
#     robustní metoda
#     jsou to hodnoty mezi prvním a třetím kvartilem
data_normal <- rnorm(100, mean = 50, sd = 10)
outliers <- c(120, 130, 122, 111, 145)
data_with_outliers <- c(data_normal, outliers)
boxplot(data_with_outliers, col = "yellow", border="orange3", main= "Krabicový graf", ylab="data", range=3)

# Z-Score
#   Ukazuje, jak daleko a v jakém směru je určitá hodnota od aritmetického průměru
#   vyjádřená v počtu směrodatných odchylek
#   outliery můžeme brát jako hodnoty co mají z-score 2x nebo 3x větší
#   není robustní

mean <- mean(data_with_outliers)
sd <- sd(data_with_outliers)

z_score <- (data_with_outliers - mean) / sd

sort(data_with_outliers[abs(z_score) > 2])
