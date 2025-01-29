## (2) Bodové a intervalové odhady podílu a rozdílu podílu

# podíl je statistická míra která udává, jak velká část z nějakého celku splňuje určitý požadavek.
# měříme ho jako poměr mezi počtem úspěšných případů a celkovým počtem pozorování. (pouze binomický typ úloh)

# příklad:
# dvě třídy píšou stejný test
# v první třídě test dalo 85 ze 100
# v druhé třídě test dalo 70 ze 100

# podíl v první třídě
podil1 <- 85/100
# podíl v druhé třídě
podil2 <- 70/100

# rodíl podílu mezi dvěma třídami
podil1 - podil2


# Bodový odhad podílu
# počet úspěchů / celkový počet pozorování ve vzorku

# Intervalový odhad podílu
# interval ve kterém s určitou pravděpodobnostní leží skutečný podíl v populaci
# vzorec:
#   p̂ +- z * sqrt((p̂*(1-p̂))/n) 
# p̂ -> bodový odhad podílu
# n -> velikost vzorku
# z -> požadovaná spolehlivost


otec <- Kojeni$Otec

# odhad podílu
p <- prop.table(table(otec))

n <- length(otec)

prop.table(table(otec))[1]


alpha <- 0.05
q.n <- qnorm(1-alpha/2)

p - q.n*sqrt(p*(1-p)/n)
p + q.n*sqrt(p*(1-p)/n) 

BinomCI(table(otec)[1], n, method = "wald")

# Bodový odhad rozdílu podílu
#   p̂1 - p̂2

p1 <- 30 /50
p2 <- 33/49
p1 - p2
# Intervalový odhad rozdílu podílu
hoch <- Kojeni$Hoch
otec <- Kojeni$Otec

(tab <- table(hoch, otec))

BinomDiffCI(x1 = tab[1,1], n1 = tab[1,1] + tab[1+2], x2 = tab[2,1], n2 = tab[2,1] + tab[2+2], method ="wald")


