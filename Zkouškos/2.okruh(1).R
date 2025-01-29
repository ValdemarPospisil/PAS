## (1) Bodové a intervalové odhady střední hodnoty a rozdílu středních hodnot

# Bodový je jedna hodnota, průměr, rozptyl
# SEM = Střední chyba průměru
#     = sd(x)/sqrt(n)

price <- cars$Price
# pruměr
prumer <- mean(price)
n <- length(price)
# SEM
sd(price)/sqrt(n)

# Intervalový nám dává interval kde se může s určitou pravděpodobností (95%) nacházet průměr
# Dolní a horní hranice

sd <- sd(price)

alpha <- 0.05
q.n <- qnorm(1-alpha/2)
q.t <- qt(0.975,n-1)

# Dolni mez
prumer-q.n*(sd/sqrt(n))
# Horni mez
prumer+q.n*(sd/sqrt(n))

library(DescTools)
MeanCI(prumer,sd, conf.level = 0.95)

# Bodový odhad rozdílu středních hodnot
# jen se od sebe odečtou 2 průměry

syst1 <- Stulong$syst1
syst2 <- Stulong$syst2

prumer1 <- mean(syst1)
prumer2 <- mean(syst2)

prumer1 - prumer2

t.test(syst1, syst2, var.equal = FALSE)
# t = jak daleko jsou naše data od nulové hypotézy (že rozdíl je nula)
# df = počet stupnů volnosti
# p-value = ukazuje pravděpodobnost že rozdíl mezi průměry je pouze náhodný
#   p < 0.05 -> zamítáme nulovou hypotézu a rozdíl mezi průměry je statisticky významný
# dolní a horní mez -> pokud interval nezahrnuje nulu tak je statisticky významný
# pruměry obou skupin

