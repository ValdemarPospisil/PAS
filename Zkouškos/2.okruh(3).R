## (3) Testování statických hypotéz v základních kontextech

# Nulová hypotéza (Ho)
#   tvrzení které testujeme
#   obvykle vyjadřuje, že neexistuje žádný rozdíl nebo že určité tvrzení je pravdivé.

# Alternativní hypotéza (Ha)
#   protiklad k nulové hypotéze
#   vyjadřuje, že existuje rozdíl nebo že tvrzení Ho není pravdivé

# Hladina významnosti (aplha)
#   maximální pravděpodobnost, kterou jsme ochotní přijmout pro chybné zamítnutí Ho
#   obvykle 0.05

# Chyba I. druhu
#   zachytíme efekt, který ve skutečnosti neexistuje (falešně pozitivní výsledek)

# Chyba II. druhu
#   nezachytíme efekt, který ve skutečnosti existuje (falešně negativní výsledek)

# Testová statistika
#   Hodnota vypočítaná z dat, která se používá k rozhodnutí, zda zamítnout Ho.
#   např, t-statistika, z-statistika

# P-hodnota
#   pravděpodnost, že dostaneme výsledky stejně extrémní nebo ještě extrémnější, než ty, které byly pozorovány
#   v našem vzorku, za předpokladu že platí nulová hypotéza
#   pokud p <= 0 ----> zamítáme Ho
#   pokud p > 0  ----> nezamítáme Ho (nedokazujeme její pravdivost, pouze ji nemůžeme zamítnout).

# Jak sestavit test:
#   1. stanovit hypotézy (Ho, Ha)
#   2. vybrat vhodný test na základě typu dat a předpokladů
#   3. spočítat testovou statistiku a p-hodnotu
#   4. rozhodnout o Ho podle p-hodnoty a hladiny významnosti
#   5. interpretovat výsledek v kontextu dat

## Test normality - Shapiro-Wilk
# Hypotéza: Ho: data mají normální rozdělení
#             Ha: data nemají normální rozdělení
# Předpoklad: data jsou spojitá a dostatečně velká (obvykle n > 3)
# Interpretace: Pokud p ≤ α -> zamítáme Ho → existuje statisticky významná korelace.
#               Pokud p > α -> Ho nezamítáme → korelace není statisticky významná.

seat <- cars$Rear.seat.room

shapiro.test(seat)
# p = 0.1133 -> p > a -> Ho nezamítáme


## Korelační test (Pearsonův korelačnčí test)
# Hypotéza: Ho: mezi proměnnými není lineární korelace (r = 0)
#           Ha: mezi proměnnými je linerární korelace (r != 0)
# Předpoklad: Data jsou kvantitativní, normálně rozdělená, a vztah je mezi proměnnými je lineární
# Interpretace: Pokud p <= alpha -> zamítáme Ho -> exstuje staticky významná korelace.
#               Pokud p > alpha -> nezamítáme Ho -> korelace není staticky významná.

price <- cars$Price
cor.test(price, seat, method= "pearson")
# p = 0.002651 -> zamítáme Ho

## Jednovýběrový t-test
# Hypotéza: Ho: Střední hodnota dat je rovna konkrétní hodnotě (μ = μ0).
#           Ha: Střední hodnota dat není rovna konkrétní hodnotě (μ ≠ μ0).
# Předpoklad: Data mají normální rozdělení (pokud ne.. dá se použít alternativa: Wilcoxonův test).
#             Měření je nezávislé
# Interpretace: Pokud p ≤ α, zamítáme Ho → střední hodnota se liší od dané hodnoty.
#               Pokud p > α, Ho nezamítáme → střední hodnota se neliší od dané hodnoty.

t.test(price, mu = 20)
# p = 0.6256 -> zamítáme Ho -> průměrná cena se liší od 20
