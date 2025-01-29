## (4) Hodnocení vzájemné souvislosti dvou číselných proměnných (tvar, směr, síla)

# Tvar: Vyjadřuje, zda souvislost mezi proměnnými má lineární nebo nelineární charakter.
# Směr: Určuje, zda existuje pozitivní nebo negativní vztah (např. pokud jedno číslo roste, druhé roste nebo klesá)
# Síla: Měří, jak silný je vztah mezi proměnnými. Silný vztah znamená, že jedna proměnná může dopředu predikovat druhou.

## Korelace

# korelační koeficient
#   měří sílu a směr lineárního vtahu mezi dvěma proměnnými.
#   hodnota se pohybuje:
#           od -1 (dokonalá negativní korelace)
#           přes 0 (žádná lineární korelace)
#           do 1 (dokonalá pozitivní korelace)

x <- seq(0, 10, length.out = 100)
y <- 2 * x + 5
cor(x,y)

plot(x, y, main="Scatter plot mezi x a y", xlab="x", ylab="y", pch = 19)
# korelace je 1, tudíž dokonalá potitivní korelace

ys <- sin(x)

cor(x,ys)
plot(x, ys, main="Scatter plot mezi x a y", xlab="x", ylab="y", pch = 19)
# korelace -0.0759 znamená silnou negativní lineární korelaci mezi x a ys. když roste x, y klesá.

# Nelineární zásvislot
y_quad <- x**2

cor(x, y_quad)
plot(x, y_quad, type='l',col='red', main="Scatter plot mezi x a y", xlab="x", ylab="y", lwd = 4)

# Korelační matice
set.seed(123)
data <- data.frame(
  x = rnorm(100),
  y = rnorm(100),
  z = rnorm(100)
  
)

data
výpočet korelací mezi všemi proměnnými
cor(data)

# Kontingenční tabulka (pro kategorické proměnné)

table(Kojeni$Hoch, Kojeni$Dudlik)

## Kovariance
# měří jak dvě proměnné mění své hodnoty společně.
# Na rozdíl od korelace není standardizována, což znamená, že její hodnota závisí na měřítkách proměnných

y_sin <- sin(x)
cov(x, y_sin)
# kovariance -0.1485821 že obě proměnné společně klesají nebo rostou.
# neni standardizované, tudíž nevíme jak je silný vzath bez znalosti jednotek obou proměnných.

# Pearson
cor.test(x,y)
# p je menší než alpha, tudíž korelace je staticky významná

# Spearman
cor.test(x,ys, method = "spearman")
# rho -0.0750435 naznačuje hodně slabou negativní korelaci.

# test nezávislosti pro kategorické proměnné

chisq.test(table(Kojeni$Hoch, Kojeni$Dudlik))
# X-squared: hodnota testu, jak moc se pozorované frekvence liší od očekávaných
# df: Počet stupnu volnosti (v tomto případě 1, protoze máme pouze dvě kategorie)
# p: je větší než 0.05 nezamítáme Ho. nexistuje statistcky významný vztah mezi pohlavím a dudlíkem.

# Příklad v porfoliové analýze:

stock1 <- rnorm(100)
stock2 <- rnorm(100)
cov(stock1, stock2)

# Pokud máte více investic, kovariance vám pomůže zjistit, jak se mění jejich ceny společně, 
# což je užitečné pro optimalizaci rizika.