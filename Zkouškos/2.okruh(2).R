## (2) Bodové a intervalové odhady podílu a rozdílu podílů

# Podíl je statistická míra, která udává, jak velká část z nějakého celku splňuje určitý požadavek.
# Měříme ho jako poměr mezi počtem úspěšných případů a celkovým počtem pozorování (binomické úlohy).

# Příklad:
# Dvě třídy píšou stejný test
# V první třídě test dalo 85 ze 100
# V druhé třídě test dalo 70 ze 100

# Podíl v první třídě
(podil1 <- 85/100)
# Podíl v druhé třídě
(podil2 <- 70/100)

# Rozdíl podílů mezi dvěma třídami
(podil1 - podil2)

# Bodový odhad podílu
# Počet úspěchů / celkový počet pozorování ve vzorku

(otec <- Kojeni$Otec)

# Odhad podílu
(p <- prop.table(table(otec)))
(n <- length(otec))

(prop.table(table(otec))[1])

# kritická hodnota pro 95 % interval
(alpha <- 0.05)
(q.n <- qnorm(1 - alpha / 2))

# Intervalový odhad podílu
(lower_bound_p <- p - q.n * sqrt(p * (1 - p) / n))
(upper_bound_p <- p + q.n * sqrt(p * (1 - p) / n))

(BinomCI(table(otec)[1], n, method = "wald"))
(BinomCI(table(otec), n, method = "wald"))

# Bodový odhad rozdílu podílů
#   p̂1 - p̂2

(p1 <- 30 / 50)
(p2 <- 33 / 49)
(p1 - p2)

# Intervalový odhad rozdílu podílů
#   vzorec viz finale
(hoch <- Kojeni$Hoch)
(otec <- Kojeni$Otec)

(tab <- table(hoch, otec))

(BinomDiffCI(x1 = tab[1,1], n1 = tab[1,1] + tab[1+2], x2 = tab[2,1], n2 = tab[2,1] + tab[2+2], method ="wald"))

# Výhody a nevýhody bodového a intervalového odhadu
# Bodový odhad:
#  + Jednoduchý na interpretaci
#  + Rychlý výpočet
#  - Neudává míru nejistoty
#  - Může být nepřesný, pokud je vzorek malý

# Intervalový odhad:
#  + Udává míru nejistoty
#  + Přesnější interpretace dat
#  - Komplexnější výpočet
#  - Šířka intervalu závisí na velikosti vzorku

# Použití:
# Bodový odhad se používá pro rychlou analýzu, kde není potřeba odhadovat přesnost.
# Intervalový odhad se používá tam, kde chceme určit míru nejistoty, například při statistickém testování hypotéz.



