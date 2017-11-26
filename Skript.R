library(tidyverse)
# Kasutame seda faili, sest siin on üheselt määratud delimiterid
# Soovitav oleks eemaldada tühikud failinimest st fail ümber nimetada
# asendades tühikud alakriipsuga _
# NB!!! Pane tähele, et kasutan suhtelist path-i st eeldan, 
# et fail asub projekti root dir-is
Katse <- "28.02.2016_katse_metaanitekke_andmed.csv"

# Impordime andmed
# Kuna murdarvudes on komad, siis oleks kõik tulbad tekst
# muuda localet impordi funktsioonis, et imporditaks numbritena
Tabel <- read_delim(Katse, delim = ";", locale = locale(decimal_mark = ","))
Tabel

# Parandame tulpade nimedhttps://github.com/tkorb/Metaaniteke.git
colnames(Tabel) <- make.names(colnames(Tabel))

# Plotime Katse 26 andmed
Gr_katse26 <- ggplot(data = Tabel, aes(x = Aeg.p, y = Katse.26, group = 1)) +
  geom_line() +
  labs(x = "Aeg, päeva", y = "Metaani ml/g", title = "Katse 26")
Gr_katse26

x1 <- c(1, 2, 3, 5)
y1 <- c(0.1, 0.2, 3, 0.5)

lineaarne_mudel <- nls(y1 ~ b*x1 + d)
summary(lineaarne_mudel)

ggplot (, aes(x = x1, y = y1, group = 1)) +
         geom_line() +
         labs(x = "x 1", y = "y 1", title = "Lineaarne mudel")
     lineaarne_mudel
       
ruutmudel <- nls(y1 ~ a*x1*x1 + b*x1 + d)
summary(ruutmudel)
       
ggplot (, aes(x = x1, y = y1, group = 1)) +
   geom_line() +
   labs(x = "x 1", y = "y 1", title = "Ruutmudel")
ruutmudel

# Õpime lähendatud funktsiooni plottima
x <- 0:100 
a <- 30
y <- a + x 
plot(y ~ x, 
     xlim = c(0, 100), 
     ylim = c(0, 150), 
     col = "red",
     type = "l")
abline(c(0, 1), lty = 2)

# Õpime lähendatud funktsiooni plottima
x <- 0:200 #y = kaal
b <- 3
y <- b*x # x = pikkus 
plot(y~x, 
     xlim=c(0, 100), 
     ylim=c(0, 100), 
     col="red",
     type="l")
abline(c(0,1), lty=2)

# Õpime lähendatud funktsiooni koos andmetega plottima
a <- 102
b <- 0.8
x <-  0:100 
y <-  a + b * x
plot(y ~ x, 
     type = "l", 
     xlab = "weight in kg", 
     ylab = "heigth in cm", 
     ylim = c(50, 200))

# fit a linear model and name the model object as m1
m1 <- lm(Sepal.Length ~ Petal.Length, iris)
# make a scatter plot, colored by the var called "Species"
plot(iris$Sepal.Length ~ iris$Petal.Length, col = iris$Species)
# draw the fitted regression line from m1
abline(m1)

# lähendame Katse 26 andmeid lineaarfunktsiooniga
x1 <- Tabel[, 1]
x1
y1 <- Tabel[, 2]
y1
lineaarne_mudel <- nls(y ~ b*x1 + d)
summary(lineaarne_mudel)

# Plotime lähendatud katseandmed
b <- 0.1771
d <- 0.4629
x <-  0:70 
y <- b * x + d
plot(y ~ x, 
     type = "l", 
     xlab = "Aeg, päeva", 
     ylab = "Metaani ml/g", 
     ylim = c(-5, 15))

# lähendame Katse 26 andmeid ruutfunktsiooniga
x1 <- Tabel[, 1]
x1
y1 <- Tabel[, 2]
y1
ruut_mudel <- nls(y ~ a*x1*x1 + b*x1 + d)
summary(ruut_mudel)

# Plotime lähendatud katseandmed
a <- -0.4909
b <- 3.1927
d <- -3.0436
x <-  0:70 
y <-  a * x * x + b * x + d
plot(y ~ x, 
     type = "l", 
     xlab = "Aeg, päeva", 
     ylab = "Metaani ml/g", 
     ylim = c(-2500, 5))

# lähendame Katse 26 andmeid hüperboolse siinuse funktsiooniga (see ei tööta!)
x1 <- Tabel[, 1]
x1
y1 <- Tabel[, 2]
y1
sinh_mudel <- nls(y ~ a1*sinh(b1*x1))
summary(sinh_mudel)

# Gompertzi funktsioon

e1 <- 2.7182
Gompertzi_mudel <- nls(y ~ a*exp(-exp(e1*b*(c1 - x1)/a + 1)))


# Plotime Katse 28 andmed
Gr_katse28 <- ggplot(data = Tabel, aes(x = Aeg.p, y = Katse.28, group = 1)) +
  geom_line() +
  labs(x = "Aeg, päeva", y = "Metaani ml/g", title = "Katse 28")
Gr_katse28

# Plotime Katse 29 andmed
Gr_katse29 <- ggplot(data = Tabel, aes(x = Aeg.p, y = Katse.29, group = 1)) +
  geom_line() +
  labs(x = "Aeg, päeva", y = "Metaani ml/g", title = "Katse 29")
Gr_katse29

# Plotime Katse 30 andmed
Gr_katse30 <- ggplot(data = Tabel, aes(x = Aeg.p, y = Katse.30, group = 1)) +
  geom_line() +
  labs(x = "Aeg, päeva", y = "Metaani ml/g", title = "Katse 30")
Gr_katse30

# Kõik katseandmed on nüüd sisestatud

