# 1. Klíčivost semen určitého druhu je 93%.
# a) Jaká je pravděpodobnost, že z 15 semen vám vyklíčí právě 12 semen?

## Binomicke rozdeleni: n - pocet pokusu, p - pst uspechu
#   pocet uspechu v n-pokusech
dbinom(12, size = 15, prob = 0.93)
# 6,532 %
# b) Jaká je pravděpodobnost, že z 10 semen vám vyklíčí 8 a více semen?

1 - pbinom(7, 10, 0.93)

# ------------------------------------------------------------------------
# 2. Výška mužů má normální rozdělení se střední hodnotou 180 cm a rozptylem 49cmˇ2.
  # a) jaká je pravděpodobnost, že náhodně vybraný muž je menší než 170 cm?
  
  p1 <- pnorm(170, 180, 7)
  
  # b) Jaká je pravděpodobnost, že náhodně vybraný muž bude vyšší než průmerný hráč NBA,
  # jehož výška je 195cm?

  p2 <- 1 - pnorm(195, 180, 7)
  
  # c) Na ulici potkáte v krátkém sledu 2 muže. Jaká je pravděpodobnost že první z mužů 
  # je vyšší než 195 cm a zárověn druhý je nižší než 170 cm?
  # Předpokládejte, že jste nepotkali dvakrát stejného člověka.
  
  p1 * p2
# -------------------------------------------------------------------------------
# 1. Ve třídě je 30 žáků, z toho 12 hochů a 18 dívek. K tabuli zavolám 7 z nich. 
# Jaká je pravděpodobnost, že mezi vyvolanými budou alespoň tři hoši?

1 - phyper(2, 12, 18, 7)

# 2. Hráč hraje člověče nezlob se a chce si nasadit figurku do hry. K tomu mu musí
# padnout šestka. Jaká je pravděpodobnost, že na nasazení panáčka bude čekat více než 10 hodů?  

1 - pgeom(9, 1/6)

# 3. Stojím na rušné křižovatce, kde za den nastane průměrně 1.6 nehody.
# Jaká je pravděpodobnost, že během dne, kdy křižovatku hlídá strážník Novák, 
# nastanou právě 3 nehody?

dpois(3, 1.6)

# 4. Hraji se synem hru, kde on má před sebou 5 neprůhledných kalíšků postavených dnem vzhůru
# a pod jeden z nich schová kamínek. Já se nedívám a pak hádám, kam kamínek ukryl. 
# Po každém hádání mi syn kamínek ukáže a schovává znovu. Jaká je pravděpodobnost, 
# že poprvé najdu kamínek až na patnáctý pokus?

dgeom(14, 1/5)

# 5. Vánoční řetěz má 20 paralelně zapojených žárovek. Každá z nich svítí s pravděpodobností 0.8. 
# Jaká je pravděpodobnost, že když řetěz rozsvítím, tak se mi nerozsvítí maximálě 3 žárovky?

1 - pbinom(16, 20, 0.8)

# 6. Výrobní linka v průměru vyrobí 3 vadné výrobky za den. 
# Jaká je pravděpodobnost, že v den kontroly vyrobí vadných výrobků více než 5 (tj. alespoň 6)? 

1 - ppois(5, 3)

# 7. Procházím vlakem a chci si sednout do prázdného kupé. 
# Kupé je prázdné s pravděpodobností 0.12. Ve vagoně je 15 kupéček. 
# Jaká je pravděpodobnost, že si sednu už v prvním vagóně?

pgeom(14, 0.12)

# 8. Hraji s dětmi na honěnou. Děti jsou rozdělené do dvou družstev: na čevené a modré.
# Červených hraje 10, modrých 12. Jaká je pravděpodobnost, 
# že mezi sedmi dětmi, které jsem chytla a vyřadila je tím ze hry, bude vetšina modrých?

1 - phyper(3, 12, 10, 7)

# 9. Dva stejně dobří hráči,Petr a Pavel, hrají tenis. Hrají celkem 10 utkání, 
# přičemž neuznávají remízy, takže utkání musí skončit výhrou jednoho z nich. 
# Jaká je pravděpodobnost, že Petr vyhraje alespoň 7 utkání?

1 - pbinom(6, 10, 0.5)

# 10. Na parkovišti je 50 aut, z toho 20 je značky Škoda. 
# Čekám u parkoviště asi 20 minut a kolem mě vyjede z parkoviště 10 aut a žádné tam nevjede. 
# Jaká je pravděpodobnost, že z těch 10 aut, co kolem mě projely, je maximálně 5 značky Škoda?

phyper(5, 20, 30, 10)

# 11. Paní Hana má 10 párů bot z toho troje jsou červené. 
# Ráno do práce vstává brzy a odchází ještě za tmy. 
# Poslední týden jí v předsíni nesvítí světlo a vybírá si tedy boty na den náhodně. 
# Jaká je pravděpodobnost, že z pěti dní, kdy odcházela do práce, odešla dvakrát v červených botech?

dbinom(2, 5, 3/10)

# 12. V menze vaří shruba jedno z dvaceti jídel s dýní. Denně mají na výdeji 2 různá jídla. 
# Jaká je pravděpodobnost, že když půjdu na oběd ve čtvrtek a v pátek, 
# tak že si alespoň jednou budu moct dát jídlo s dýní (v jeden den mohou nabízet obě jídla s dýní).

1 - (pbinom(0, 2, 1/20)^2)
# -------------------------------------------------------------------------------------------------
# 1. Výška dospělých mužů má normální rozdělení se střední hodnotou 180 a rozptylem 49. 
  # a) Jaká je pravděpodobnost, že náhodně vybraný muž bude měřit alespoň 190 cm? 
  1 - pnorm(190, 180, 7)

  # b) Jaká je pravděpodobnost, že když náhodně potkám 2 muže, budou oba menší než 170 cm?

  pnorm(170, 180, 7)^2

  # c) Jaká je pravděpodobnost, že náhodně vybraný muž projde bez sehnutí bránou, 
  # která má oblouk ve výšce 193 cm?
  
  1 - pnorm(193, 180, 7)

  # d) Jaká je pravděpodobnost, že výška náhodně vybraného muže bude v rozmezí od 165 do 180 cm.

  p1 <- pnorm(180, 180, 7)
  p2 <- pnorm(165, 180, 7)
  p1-p2
  
#  2. IQ lidí má střední hodnotu 100 a směrodatnou odchylku 15. 
#  Jaká je pravděpodobost, že ze tří lidí, které jsem potkala ve vlaku, 
# budou mít alespoň dva IQ větší než 120?
  
 p <- 1 - pnorm(120, 100, 15)
1 - pbinom(1, 3, p)
