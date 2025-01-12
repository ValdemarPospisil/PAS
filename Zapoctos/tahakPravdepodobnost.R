### Distribuční funkce - pbinom P(x <= k)
### Pravděpodobnostní funkce - dbinom P(x = k)

## Diskretni rozdeleni

# Binomická funkce
# n = počet pokusů
# p = pravděpodobnost úspěchu
pbinom(k, n, p )
dbinom(k, n, p)

# Hypergeometrické rozdělení
# k = pocet 1. hodnoty mezi n 
# w = pocet 1. hodnoty
# b = pocet 2. hodnoty
# n = pocet pokusu
# stredni hodnota n*w/(w+b)
# rozptyl (n*w/(w+b))*(1-w/(w+b))*((w+b-n)/(w+b-1))
phyper(k,w,b,n)
dhyper(k,w,b,n)

# Geometrické rozdělení
# p = pocet neuspechu pred prvním úspěchem
# q = pravděpodobnost úspěchu
# stredni hodnota (1-p)/p
# rozptyl (1-p)/p^2
pgeom(q, p)
dgeom(q, p)

# Poisovo rozdělení
# q = pocet udalosti
# lamda = střední hodnota
# stredni hodnota lambda
# rozptyl lambda
ppois(q, lamda)
dpois(q, lamda)

--------------------------------------------------------------------------------
### Spojita rozdělení

# Normální rozdělení
# mean = strední hodnota
# sd = směrodatná odchylka
pnorm(q, mean = , sd = )
qnorm(p, mean = , sd = )  # - kvantilova funkce

# Logonormální rozdělení
# sigma (velicina ln(X) ~ N(mu, sigma^2))
plnorm(q, meannlog = , sdlog = )
qlnorm(x,mu,sigma)  # - kvantilova funkce

# Exponencialni rozdeleni
# rate = intenzita
# distribucni funkce P(X <= t) = 1 - exp(-int*t)
# stredni hodnota 1/int
# rozptyl 1/int^2
pexp(q, rate =)
qexp(x,int) # - kvantilova funkce