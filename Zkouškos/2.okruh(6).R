## (6) Identifikace vhodného podkladového rozdělení dat

# Postup

# 1. Histogram

trvani <- Kojeni$trvani
library(ggplot2)
hist(trvani, col="azure", border="darkblue")
ggplot(Kojeni, aes(x=trvani)) +
  geom_histogram(aes(y=..density..), binwidth = 2, color = "black", fill="blue") +
  labs(title = "Histogram průměrná známka z 3. ročníku", x="ss3", y="Hustota") +
  theme_minimal()

# 2. QQplot -- zda je normálně rozdělené

ggplot(Kojeni, aes(sample = trvani)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Q-Q Plot pro ss3", x="Teoretické kvantily", y="Vzorkové kvantily") +
  theme_minimal()

# 3. Cullen-Frey graf
#   bootstrap hodnoty
#   měl by se kombinovat s jinými metodami
library(fitdistrplus)
descdist(trvani, discrete=FALSE, boot=1000)
# vypadá to na uniform

# 4. Odhad modelů pro různá rozdělení
#   základ je porovnání teoretických parametrů rozdělení s charakteristikami mých dat 
#       výsledkem je objekt obsahující informace o tom, jak dobře dané rozdělení odpovídá naším datům
#     Parametry rozdělení
#           - pro normální - průměr dat a sd
#           - pro lognormální - logaritmický průměr a logaritmickou sd
# Log-věrohodnost
#   Vyšší hodnota znamená, že model lépe odpovídá datům.
#   Jak pravděpodobné je, že data pocházejí z daného rozdělení


(fit1 <- fitdist(trvani,"norm"))
(fit2 <- fitdist(trvani,"logis"))
(fit3 <- fitdist(trvani,"lnorm"))
(fit4 <- fitdist(trvani,"weibull"))
(fit5 <- fitdist(trvani,"gamma"))
(fit6 <- fitdist(trvani,"exp"))

# 5. Porovnání modelů
#   AIC
#       - Akaike informační kritérium
#       - měřítko kvality modelu, které bere v úvahu jak přesnost modelu, tak jeho složitost
#       - nižší AIC znamená lepší model
#   BIC
#       - Bayesovské informační kritérium
#       - podobné AIC, ale penalizuje složité modely více

data.frame(distr=c("Norm","Logis", "Gamma", "Exp"),
           AIC=c(fit1$aic,fit2$aic,fit5$aic,fit6$aic),
           BIC=c(fit1$bic,fit2$bic,fit5$aic,fit6$aic)
)

# 6. Intervaly spolehlivosti
#   ověřím si spolehlivost parametrů rozdělení

confint(fit6, level = 0.95)

# 7. QQploty pro různá rozdělení
#     specifické qq ploty pro rozdělení 

PlotQQ(trvani, qdist=function(p) qexp(p,rate=coef(fit6)[1]))
PlotQQ(trvani, qdist=function(p) qlogis(p,location=coef(fit6)[1])

       