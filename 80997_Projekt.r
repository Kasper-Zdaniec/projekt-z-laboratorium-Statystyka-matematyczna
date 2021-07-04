##### Projekt ze statystyki #####
##### Autor: Kasper ¯daniec #####



# wyczyszczenie obszaru roboczego
rm(list = ls())

# za³adowanie pliku z danymi
install.packages("ISLR")
library(ISLR)
dane <- Auto

# wstêpny przegl¹d danych
head(dane)
summary(dane)

i = rep(1, 9)
for (i in 1:9) {
  cat(names(dane[i]), "\t", class(dane[, i]), "\n")
}
dane$origin <- as.factor(dane$origin)
dane$cylinders <- as.factor(dane$cylinders)

# badane zmienne numeryczne: mpg, horsepower, acceleration
class(dane$mpg)
class(dane$horsepower)
class(dane$acceleration)

# badane zmienne czynnikowe: cylinders, origin
class(dane$cylinders)
class(dane$origin)

# przyjêcie poziomów ufnoœci
CL1 <- 0.95
CL2 <- 0.99

# przyjêcie poziomów istotnoœci
SL1 <- 1 - CL1
SL2 <- 1 - CL2



### (A1) przedzia³y ufnoœci dla œredniej ###


# Poniewa¿ badana próba jest du¿a, na podstawie twierdzenia granicznego
# nie ma potrzeby badania za³o¿eñ przy budowie przedzia³ów ufnoœci


## (A1.1) zmienna mpg

t.test(dane$mpg, conf.level = CL1)$conf.int
# Przedzia³ od 22,67 do 24,22 (mil z galona benzyny) jest jednym
# z nieskoñczenie wielu mo¿liwych do uzyskania, które z prawdopodobieñ-
# stwem 95% pokrywaj¹ nieznan¹ wartoœæ oczekiwan¹ wydajnoœci samochodu.

t.test(dane$mpg, conf.level = CL2)$conf.int
# Przedzia³ od 22,43 do 24,47 (mil z galona benzyny) jest jednym
# z nieskoñczenie wielu mo¿liwych do uzyskania, które z prawdopodobieñ-
# stwem 99% pokrywaj¹ nieznan¹ wartoœæ oczekiwan¹ wydajnoœci samochodu.


## (A1.2) zmienna horsepower

t.test(dane$horsepower, conf.level = CL1)$conf.int
# Przedzia³ od 100,65 do 108,29 (KM) jest jednym z nieskoñczenie wielu
# mo¿liwych do uzyskania, które z prawdopodobieñstwem 95% pokrywaj¹
# nieznan¹ wartoœæ oczekiwan¹ mocy silnika samochodu.

t.test(dane$horsepower, conf.level = CL2)$conf.int
# Przedzia³ od 99,44 do 109,50 (KM) jest jednym z nieskoñczenie wielu
# mo¿liwych do uzyskania, które z prawdopodobieñstwem 99% pokrywaj¹
# nieznan¹ wartoœæ oczekiwan¹ mocy silnika samochodu.


## (A1.3) zmienna acceleration

t.test(dane$acceleration, conf.level = CL1)$conf.int
# Przedzia³ od 15,27 do 15,82 (sekund) jest jednym z nieskoñczenie wielu
# mo¿liwych do uzyskania, które z prawdopodobieñstwem 95% pokrywaj¹
# nieznan¹ wartoœæ oczekiwan¹ przyspieszenia samochodu (czasu osi¹gniêcia
# prêdkoœci od 0 do 60 mil na godzinê).

t.test(dane$acceleration, conf.level = CL2)$conf.int
# Przedzia³ od 15,18 do 15,90 (sekund) jest jednym z nieskoñczenie wielu
# mo¿liwych do uzyskania, które z prawdopodobieñstwem 99% pokrywaj¹
# nieznan¹ wartoœæ oczekiwan¹ przyspieszenia samochodu (czasu osi¹gniêcia
# prêdkoœci od 0 do 60 mil na godzinê).



### (A2) przedzia³y ufnoœci dla ró¿nicy œrednich ###


# Poniewa¿ badana próba jest du¿a, na podstawie twierdzenia granicznego
# nie ma potrzeby badania za³o¿eñ przy budowie przedzia³ów ufnoœci


## (A2.1) zmienna mpg dla samochodów cztero- i oœmiocylindrowych

tapply(dane$mpg, dane$cylinders, mean)
# Samochody czterocylindrowe s¹ w próbie przeciêtnie bardziej wydajne
# (29,28 mil z galona benzyny) ni¿ pojazdy oœmiocylindrowe (14,96).

t.test(dane$mpg[dane$cylinders == 4],
       dane$mpg[dane$cylinders == 8],
       conf.level = CL1)$conf.int
# Przedzia³ od 13,36 do 15,28 (mil z galona benzyny) jest jednym
# z nieskoñczenie wielu mo¿liwych do uzyskania, które z prawdopodo-
# bieñstwem 95% pokrywaj¹ nieznan¹ wartoœæ ró¿nicy miêdzy œredni¹
# wydajnoœci¹ pojazdów cztero- i oœmiocylindrowych.

t.test(dane$mpg[dane$cylinders == 4],
       dane$mpg[dane$cylinders == 8],
       conf.level = CL2)$conf.int
# Przedzia³ od 13,05 do 15,59 (mil z galona benzyny) jest jednym
# z nieskoñczenie wielu mo¿liwych do uzyskania, które z prawdopodo-
# bieñstwem 99% pokrywaj¹ nieznan¹ wartoœæ ró¿nicy miêdzy œredni¹
# wydajnoœci¹ pojazdów cztero- i oœmiocylindrowych.


## (A2.2) zmienna horsepower dla samochodów cztero- i oœmiocylindrowych

tapply(dane$horsepower, dane$cylinders, mean)
# Samochody oœmiocylindrowe maj¹ w próbie przeciêtnie mniejsz¹ moc
# (158,30 KM) ni¿ pojazdy czterocylindrowe (78,28).

t.test(dane$horsepower[dane$cylinders == 8],
       dane$horsepower[dane$cylinders == 4],
       conf.level = CL1)$conf.int
# Przedzia³ od 74,11 do 85,93 (KM) jest jednym z nieskoñczenie wielu
# mo¿liwych do uzyskania, które z prawdopodobieñstwem 95% pokrywaj¹
# nieznan¹ wartoœæ ró¿nicy miêdzy œredni¹ moc¹ silnika pojazdów
# oœmio- i czterocylindrowych.

t.test(dane$horsepower[dane$cylinders == 8],
       dane$horsepower[dane$cylinders == 4],
       conf.level = CL2)$conf.int
# Przedzia³ od 72,21 do 87,83 (KM) jest jednym z nieskoñczenie wielu
# mo¿liwych do uzyskania, które z prawdopodobieñstwem 99% pokrywaj¹
# nieznan¹ wartoœæ ró¿nicy miêdzy œredni¹ moc¹ silnika pojazdów
# oœmio- i czterocylindrowych.


## (A2.3) zmienna acceleration dla samochodów cztero- i oœmiocylindrowych

tapply(dane$acceleration, dane$cylinders, mean)
# Samochody czterocylindrowe maj¹ w próbie przeciêtnie wiêksze
# przyspieszenie od 0 do 60 mil na godzinê (16,58 sekund) od pojazdów
# oœmiocylindrowych (12,96).

t.test(dane$acceleration[dane$cylinders == 4],
       dane$acceleration[dane$cylinders == 8],
       conf.level = CL1)$conf.int
# Przedzia³ od 3,08 do 4,17 (sekund) jest jednym z nieskoñczenie wielu
# mo¿liwych do uzyskania, które z prawdopodobieñstwem 95% pokrywaj¹
# nieznan¹ wartoœæ ró¿nicy miêdzy œrednim przyspieszeniem od 0 do 60
# mil na godzinê samochodów cztero- i oœmiocylindrowych.

t.test(dane$acceleration[dane$cylinders == 4],
       dane$acceleration[dane$cylinders == 8],
       conf.level = CL2)$conf.int
# Przedzia³ od 2,91 do 4,35 (sekund) jest jednym z nieskoñczenie wielu
# mo¿liwych do uzyskania, które z prawdopodobieñstwem 99% pokrywaj¹
# nieznan¹ wartoœæ ró¿nicy miêdzy œrednim przyspieszeniem od 0 do 60
# mil na godzinê samochodów cztero- i oœmiocylindrowych.



### (A3) przedzia³y ufnoœci dla wariancji ###


# Przyjmujê za³o¿enie o normalnoœci rozk³adu poszczególnych zmiennych
# zale¿nych w badanej próbie losowej


# zainstalowanie i podczytanie potrzebnej bilioteki
install.packages("DescTools")
library(DescTools)


## (A3.1) zmienna mpg

VarCI(dane$mpg, conf.level = CL1)
# Przedzia³ od 53,21 do 70,45 jest jednym z nieskoñczenie wielu
# mo¿liwych do uzyskania, które z prawdopodobieñstwem 95%
# pokrywaj¹ nieznan¹ wariancjê wydajnoœci samochodu.

VarCI(dane$mpg, conf.level = CL2)
# Przedzia³ od 51,03 do 73,81 jest jednym z nieskoñczenie wielu
# mo¿liwych do uzyskania, które z prawdopodobieñstwem 99%
# pokrywaj¹ nieznan¹ wariancjê wydajnoœci samochodu.


## (A3.2) zmienna horsepower

VarCI(dane$horsepower, conf.level = CL1)
# Przedzia³ od 1294 do 1713 jest jednym z nieskoñczenie wielu
# mo¿liwych do uzyskania, które z prawdopodobieñstwem 95%
# pokrywaj¹ nieznan¹ wariancjê mocy silnika samochodu.

VarCI(dane$horsepower, conf.level = CL2)
# Przedzia³ od 1241 do 1795 jest jednym z nieskoñczenie wielu
# mo¿liwych do uzyskania, które z prawdopodobieñstwem 99%
# pokrywaj¹ nieznan¹ wariancjê mocy silnika samochodu.


## (A3.3) zmienna acceleration

VarCI(dane$acceleration, conf.level = CL1)
# Przedzia³ od 6,65 do 8,80 jest jednym z nieskoñczenie wielu
# mo¿liwych do uzyskania, które z prawdopodobieñstwem 95%
# pokrywaj¹ nieznan¹ wariancjê przyspieszenia pojazdu.

VarCI(dane$acceleration, conf.level = CL2)
# Przedzia³ od 6,38 do 9,22 jest jednym z nieskoñczenie wielu
# mo¿liwych do uzyskania, które z prawdopodobieñstwem 99%
# pokrywaj¹ nieznan¹ wariancjê przyspieszenia pojazdu.



### (A4) przedzia³y ufnoœci dla odchylenia standardowego ###


# Przyjmujê za³o¿enie o normalnoœci rozk³adu poszczególnych zmiennych
# zale¿nych w badanej próbie losowej


## (A4.1) zmienna mpg

sqrt(VarCI(dane$mpg, conf.level = CL1))
# Przedzia³ od 7,29 do 8,39 (mil na godzinê z galona benzyny)
# jest jednym z nieskoñczenie wielu mo¿liwych do uzyskania,
# które z prawdopodobieñstwem 95% pokrywaj¹ nieznane odchylenie
# standardowe wydajnoœci samochodu.

sqrt(VarCI(dane$mpg, conf.level = CL2))
# Przedzia³ od 7,14 do 8,59 (mil na godzinê z galona benzyny)
# jest jednym z nieskoñczenie wielu mo¿liwych do uzyskania,
# które z prawdopodobieñstwem 99% pokrywaj¹ nieznane odchylenie
# standardowe wydajnoœci samochodu.


## (A4.2) zmienna horsepower

sqrt(VarCI(dane$horsepower, conf.level = CL1))
# Przedzia³ od 35,97 do 41,39 (KM) jest jednym z nieskoñczenie
# wielu mo¿liwych do uzyskania, które z prawdopodobieñstwem 95%
# pokrywaj¹ nieznane odchylenie standardowe mocy silnika samochodu.

sqrt(VarCI(dane$horsepower, conf.level = CL2))
# Przedzia³ od 35,23 do 42,37 (KM) jest jednym z nieskoñczenie
# wielu mo¿liwych do uzyskania, które z prawdopodobieñstwem 99%
# pokrywaj¹ nieznane odchylenie standardowe mocy silnika samochodu.


## (A4.3) zmienna acceleration

sqrt(VarCI(dane$acceleration, conf.level = CL1))
# Przedzia³ od 2,578 do 2,967 (sekund) jest jednym z nieskoñczenie
# wielu mo¿liwych do uzyskania, które z prawdopodobieñstwem 95%
# pokrywaj¹ nieznane odchylenie standardowe przyspieszenia pojazdu
# od prêdkoœci 0 do 60 mil na godzinê.

sqrt(VarCI(dane$acceleration, conf.level = CL2))
# Przedzia³ od 2,525 do 3,037 (sekund) jest jednym z nieskoñczenie
# wielu mo¿liwych do uzyskania, które z prawdopodobieñstwem 99%
# pokrywaj¹ nieznane odchylenie standardowe przyspieszenia pojazdu
# od prêdkoœci 0 do 60 mil na godzinê.



### (B) przedzia³y ufnoœci dla frakcji elementów wyró¿nionych ###


# Za³o¿enie o du¿ej próbie (N > 100) jest spe³nione


## (B.1) udzia³ pojazdów szeœciocylindrowych

table(dane$cylinders)
round(table(dane$cylinders)/length(dane$cylinders)*100, 2)
# Samochodów szeœciocylindrowych by³o w badanej próbie 83.
# Stanowi to 21,17 procent wszystkich badanych pojazdów.

prop.test(length(dane$cylinders[dane$cylinders == 6]),
          length(dane$cylinders), conf.level = CL1)$conf.int
# Przedzia³ od 17,30 do 25,62 procent jest jednym z nieskoñczenie wielu
# mo¿liwych do uzyskania, które z prawdopodobieñstwem 95% pokrywaj¹
# nieznany odsetek samochodów szeœciocylindrowych.

prop.test(length(dane$cylinders[dane$cylinders == 6]),
          length(dane$cylinders), conf.level = CL2)$conf.int
# Przedzia³ od 16,25 do 27,08 procent jest jednym z nieskoñczenie wielu
# mo¿liwych do uzyskania, które z prawdopodobieñstwem 99% pokrywaj¹
# nieznany odsetek samochodów szeœciocylindrowych.


## (B.2) udzia³ pojazdów produkcji amerykañskiej

table(dane$origin)
round(table(dane$origin)/length(dane$origin)*100, 2)
# Samochodów amerykañskich by³o w badanej próbie 245, co stanowi
# 62,50 procent wszystkich badanych pojazdów.

prop.test(length(dane$origin[dane$origin == 1]),
          length(dane$origin), conf.level = CL1)$conf.int
# Przedzia³ od 57,48 do 67,27 procent jest jednym z nieskoñczenie wielu
# mo¿liwych do uzyskania, które z prawdopodobieñstwem 95% pokrywaj¹
# nieznany odsetek samochodów amerykañskich.

prop.test(length(dane$origin[dane$origin == 1]),
          length(dane$origin), conf.level = CL2)$conf.int
# Przedzia³ od 55,91 do 68,66 procent jest jednym z nieskoñczenie wielu
# mo¿liwych do uzyskania, które z prawdopodobieñstwem 99% pokrywaj¹
# nieznany odsetek samochodów amerykañskich.



### (C) hipotezy dotycz¹ce wartoœci parametrów i postaci funkcyjnych ###


## (C.1) zmienna mpg

summary(dane$mpg)
t.test(dane$mpg, alternative = "greater", mu = 22.5)
# Œrednia wydajnoœæ pojazdów z jednego galona benzyny
# jest istotnie wiêksza ni¿ 22,5 mil (p = 0,0084 < 0,05).

hist(dane$mpg)
shapiro.test(dane$mpg)
# Rozk³ad wydajnoœci samochodów ró¿ni siê istotnie
# od rozk³adu normalnego (p = 0,000000105 < 0,05).


## (C.2) zmienna horsepower

summary(dane$horsepower)
t.test(dane$horsepower, mu = 100)
# Œrednia moc silnika samochodów ró¿ni siê istotnie
# od 100 KM (p = 0,02203 < 0,05).

hist(dane$horsepower)
ks.test(dane$horsepower, "plnorm")
# Rozk³ad mocy silnika samochodów ró¿ni siê istotnie
# od rozk³adu logarytmiczno-normalnego (p < 0,001 < 0,05).


## (C.3) zmienna acceleration

summary(dane$acceleration)
t.test(dane$acceleration, alternative = "less", mu = 16)
# Œredni czas przyspieszenia pojazdów od prêdkoœci
# 0 do 60 mil na godzinê jest istotnie mniejszy
# ni¿ 16 sekund (p = 0,0005432 < 0,05).

var(dane$acceleration)
if (!require(EnvStats)) {install.packages("EnvStats")}
library(EnvStats)
VarTest(dane$acceleration, alternative = "less",
        sigma.squared = 9)
# Wariancja czasu przyspieszenia pojazdów od prêdkoœci
# 0 do 60 mil na godzinê jest istotnie mniejsza
# ni¿ 9 sekund (p = 0,01205 < 0,05).

hist(dane$acceleration)
if (!require(tseries)) {install.packages("tseries")}
library(tseries)
jarque.bera.test(dane$acceleration)
# Rozk³ad czasu przyspieszenia samochodów ró¿ni siê
# istotnie od rozk³adu normalnego (p = 0,0147 < 0,05).



### (D) Badanie zale¿noœci liczby cylindrów samochodów (dla pojazdów
#  cztero-, szeœcio- i oœmiocylindrowych) od miejsca ich produkcji


## Test niezale¿noœci chi-kwadrat
dane468 <- dane[dane$cylinders == 4 |
                  dane$cylinders == 6 |
                  dane$cylinders == 8, ]
(CT <- table(dane468$origin, dane468$cylinders)[, c("4", "6", "8")])
(X2 <- chisq.test(dane468$origin, dane468$cylinders))
# Wystêpuje zale¿noœæ stochastyczna miêdzy liczb¹ cylindrów
# pojazdów a krajem ich produkcji (p < 0,001 < 0,05).


## Oznaczenia pomocniczne:
(K <- dim(CT)[1])
(L <- dim(CT)[2])
(N <- sum(CT))
(S <- X2$statistic)


## Wspó³czynnik kontyngencji V Cramera
(VC <- sqrt(S/(N*(min(K, L)-1))))
# Zale¿noœæ w próbie losowej miêdzy liczb¹ cylindrów pojazdu
# a krajem jego produkcji jest umiarkowana silna.


## Wspó³czynnik Phi Yule'a
(PY <- sqrt(S/N))
# Zale¿noœæ w próbie losowej miêdzy liczb¹ cylindrów pojazdu
# a krajem jego produkcji jest umiarkowana silna.


## Wspó³czynnik kontyngencji T Czuprowa
(TC <- sqrt(S/(N*sqrt((K-1)*(L-1)))))
# Zale¿noœæ w próbie losowej miêdzy liczb¹ cylindrów pojazdu
# a krajem jego produkcji jest umiarkowanie silna.


## Wspó³czynnik kontyngencji C Pearsona
(CP <- sqrt(S/(S+N)))
# Zale¿noœæ w próbie losowej miêdzy liczb¹ cylindrów pojazdu
# a krajem jego produkcji jest umiarkowanie silna.



### (E.1) Modele regresji prostej ###


## Zale¿noœæ miêdzy przyspieszeniem a moc¹ silnika pojazdów:
summary(M1 <- lm(acceleration ~ horsepower, data = dane))
# Wzrost mocy silnika o 10 KM wi¹¿e siê ze spadkiem czasu
# przyspieszenia (od prêdkoœci 0 do 60 mil na godzinê)
# przeciêtnie o 0,5 sekundy (p < 0,001).


## Zale¿noœæ miêdzy wydajnoœci¹ a miejscem produkcji pojazdów:
tapply(dane$mpg, dane$origin, mean)
summary(M1 <- lm(mpg ~ origin, data = dane))
# Œrednia wydajnoœæ samochodów japoñskich (w milach z galona
# beznzyny) wynosi ponad 30, europejskich - nieca³e 28, zaœ
# amerykañskich - tylko 20.
# Samochody produkcji europejskiej (origin == 2) s¹ przeciêtnie
# bardziej wydajne od pojazdów wykonanych w USA (p < 0,001).
# Równie¿ samochody produkcji japoñskiej (origin == 2) s¹ œrednio
# bardziej wydajne od pojazdów amerykañskich (p < 0,001).



### (E.2) Model regresji wielorakiej ###

summary(lm(acceleration ~ horsepower + mpg + origin, data = dane))