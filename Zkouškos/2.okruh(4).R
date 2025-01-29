## (4) Hodnocení vzájemné souvislosti dvou číselných proměnných (tvar, směr, síla)

# Tvar: Vyjadřuje, zda souvislost mezi proměnnými má lineární nebo nelineární charakter.
# Směr: Určuje, zda existuje pozitivní nebo negativní vztah (např. pokud jedno číslo roste, druhé roste nebo klesá).
# Síla: Měří, jak silný je vztah mezi proměnnými. Silný vztah znamená, že jedna proměnná může dopředu predikovat druhou.

## Korelace

# Korelační koeficient
#   Měří sílu a směr lineárního vztahu mezi dvěma proměnnými.
#   Hodnota se pohybuje:
#           od -1 (dokonalá negativní korelace)
#           přes 0 (žádná lineární korelace)
#           do 1 (dokonalá pozitivní korelace)

x <- seq(0, 10, length.out = 100)
y <- 2 * x + 5
cor(x, y)

plot(x, y, main="Scatter plot mezi x a y", xlab="x", ylab="y", pch = 19)
# Korelace je 1, tudíž dokonalá pozitivní korelace

ys <- sin(x)

cor(x, ys)
plot(x, ys, main="Scatter plot mezi x a ys", xlab="x", ylab="ys", pch = 19)
# Korelace -0.0759 znamená slabou negativní lineární korelaci mezi x a ys.

# Nelineární závislost
(y_quad <- x**2)

(cor(x, y_quad))
(plot(x, y_quad, type='l', col='red', main="Scatter plot mezi x a y", xlab="x", ylab="y", lwd = 4))

# Korelační matice
set.seed(123)
data <- data.frame(
  x = rnorm(100),
  y = rnorm(100),
  z = rnorm(100)
)

# Výpočet korelací mezi všemi proměnnými
cor(data)

# Kontingenční tabulka (pro kategorické proměnné)
table(Kojeni$Hoch, Kojeni$Dudlik)

## Kovariance
# Měří, jak dvě proměnné mění své hodnoty společně.
# Na rozdíl od korelace není standardizována, což znamená, že její hodnota závisí na měřítkách proměnných.

y_sin <- sin(x)
cov(x, y_sin)
# Kovariance -0.1485821 znamená, že obě proměnné společně klesají nebo rostou.
# Není standardizované, tudíž nevíme, jak je silný vztah bez znalosti jednotek obou proměnných.

# Pearsonův korelační test
cor.test(x, y)
# p-hodnota je menší než alpha, tudíž korelace je statisticky významná.

# Spearmanův korelační test
cor.test(x, ys, method = "spearman")
# rho -0.0750435 naznačuje velmi slabou negativní korelaci.

# Test nezávislosti pro kategorické proměnné
chisq.test(table(Kojeni$Hoch, Kojeni$Dudlik))
# X-squared: hodnota testu, jak moc se pozorované frekvence liší od očekávaných.
# df: Počet stupňů volnosti (v tomto případě 1, protože máme pouze dvě kategorie).
# p-hodnota: Pokud je větší než 0.05, nezamítáme H0 a neexistuje statisticky významný vztah mezi proměnnými.

## Praktické využití korelace a kovariance

# Příklad z portfoliové analýzy:
(stock1 <- rnorm(100))
(stock2 <- rnorm(100))
(cov(stock1, stock2))

# Pokud máme dvě investice, kovariance nám pomůže zjistit, zda se jejich ceny pohybují společně.
# Například, pokud dvě akcie mají vysokou kladnou kovarianci, znamená to, že jejich ceny mají tendenci růst a klesat společně.
# Naopak, pokud mají zápornou kovarianci, když cena jedné akcie roste, cena druhé obvykle klesá.

# To se využívá například při diverzifikaci portfolia – investoři se snaží kombinovat akcie s nízkou nebo zápornou kovariancí,
# aby minimalizovali riziko a dosáhli stabilnějších výnosů.

# Další příklad využití korelace:
# V epidemiologii lze korelační analýzou zkoumat vztahy mezi faktory, jako je kouření a riziko srdečních chorob.
# Pokud je nalezena silná pozitivní korelace, může to naznačovat možnou příčinnou souvislost,
# což by mohlo vést k dalším studiím a preventivním opatřením.
