## (6) Identifikace vhodného podkladového rozdělení dat

# Postup

# 1. Histogram

trvani <- Kojeni$vekM
library(ggplot2)
hist(trvani, col="azure", border="darkblue")
ggplot(Kojeni, aes(x=vekM)) +
  geom_histogram(aes(y=..density..), binwidth = 2, color = "black", fill="blue") +
  labs(title = "Histogram trvání kojení", x="Trvání", y="Hustota") +
  theme_minimal()

# 2. QQplot -- zda je normálně rozdělené

ggplot(Kojeni, aes(sample = vekM)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Q-Q Plot pro trvání kojení", x="Teoretické kvantily", y="Vzorkové kvantily") +
  theme_minimal()

# 3. Cullen-Frey graf
#   bootstrap hodnoty
#   měl by se kombinovat s jinými metodami
library(fitdistrplus)
descdist(trvani, discrete=FALSE, boot=1000)

# **Jak číst Cullen-Frey graf:**
# - Pokud je bod blízko normální distribuce (vlevo dole), data mají přibližně normální rozdělení.
# - Pokud je bod směrem k rozdělení gamma nebo Weibull, pravděpodobně jsou data šikmá.
# - Pokud se bod nachází poblíž exponenciálního rozdělení, data mohou být silně šikmá s velkým pravým ocasem.
# - Pokud se bod blíží uniformnímu rozdělení, pravděpodobně neexistuje žádná dominantní hustota.

# 4. Odhad modelů pro různá rozdělení
(fit1 <- fitdist(trvani,"norm"))
(fit2 <- fitdist(trvani,"logis"))
(fit3 <- fitdist(trvani,"lnorm"))
(fit4 <- fitdist(trvani,"weibull"))
(fit5 <- fitdist(trvani,"gamma"))
(fit6 <- fitdist(trvani,"exp"))
(fit7 <- fitdist(trvani,"cauchy"))
(fit8 <- fitdist(trvani,"beta"))

# 5. Porovnání modelů
#   AIC a BIC kritéria

model_comparison <- data.frame(
  distr = c("Norm", "Logis", "LNorm", "Weibull", "Gamma", "Exp", "Cauchy"),
  AIC = c(fit1$aic, fit2$aic, fit3$aic, fit4$aic, fit5$aic, fit6$aic, fit7$aic),
  BIC = c(fit1$bic, fit2$bic, fit3$bic, fit4$bic, fit5$bic, fit6$bic, fit7$bic)
)
print(model_comparison)

# **Jak vybrat nejlepší rozdělení?**
# - Model s nejnižší hodnotou AIC/BIC je pravděpodobně nejlepší.
# - Dále je dobré porovnat modely vizuálně pomocí histogramu a Q-Q grafů.
# - Lze také použít statistické testy jako Kolmogorov-Smirnov test nebo Anderson-Darling test.

# Kolmogorov-Smirnov test
# pro normální
ks.test(trvani, "pnorm", mean=mean(trvani), sd=sd(trvani))
library(MASS)
# pro logistické
fit_logis <- fitdistr(trvani, "logistic")
ks.test(trvani, "plogis", location=fit_logis$estimate[1], scale=fit_logis$estimate[2])

# Anderson-Darling test
library(nortest)
ad.test(trvani)  # Testuje proti normálnímu rozdělení

library(goftest)
ad.test(trvani, plogis, fit_logis$estimate[1], fit_logis$estimate[2])

# 6. Intervaly spolehlivosti
confint(fit3, level = 0.95)

# 7. QQploty pro všechna rozdělení
qq_plot_function <- function(data, fit, qdist, title) {
  qqplot(qdist(ppoints(length(data))), data,
         main = paste("Q-Q plot pro", title),
         xlab = "Teoretické kvantily", ylab = "Vzorkové kvantily")
  abline(0, 1, col = "red")
}

par(mfrow = c(2, 4))
qq_plot_function(trvani, fit1, qnorm, "Normální")
qq_plot_function(trvani, fit2, qlogis, "Logistické")
qq_plot_function(trvani, fit3, qlnorm, "Log-normální")
qq_plot_function(trvani, fit4, function(p) qweibull(p, shape=coef(fit4)[1], scale=coef(fit4)[2]), "Weibull")
qq_plot_function(trvani, fit5, function(p) qgamma(p, shape=coef(fit5)[1], rate=coef(fit5)[2]), "Gamma")
qq_plot_function(trvani, fit6, qexp, "Exponenciální")
qq_plot_function(trvani, fit7, qcauchy, "Cauchy")
qq_plot_function(trvani, fit8, qbeta, "Beta")

# 8. QQploty s ggplot2 pro všechna rozdělení
qq_plot_gg <- function(data, fit, qdist, title) {
  ggplot(data.frame(sample = data), aes(sample = sample)) +
    stat_qq(distribution = qdist, dparams = coef(fit)) +
    stat_qq_line() +
    labs(title = paste("Q-Q Plot pro", title), x = "Teoretické kvantily", y = "Vzorkové kvantily") +
    theme_minimal()
}

library(gridExtra)
p1 <- qq_plot_gg(trvani, fit1, qnorm, "Normální")
p2 <- qq_plot_gg(trvani, fit2, qlogis, "Logistické")
p3 <- qq_plot_gg(trvani, fit3, qlnorm, "Log-normální")
p4 <- qq_plot_gg(trvani, fit4, qweibull, "Weibull")
p5 <- qq_plot_gg(trvani, fit5, qgamma, "Gamma")
p6 <- qq_plot_gg(trvani, fit6, qexp, "Exponenciální")
p7 <- qq_plot_gg(trvani, fit7, qcauchy, "Cauchy")
p8 <- qq_plot_gg(trvani, fit8, qbeta, "Beta")

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol = 4)
