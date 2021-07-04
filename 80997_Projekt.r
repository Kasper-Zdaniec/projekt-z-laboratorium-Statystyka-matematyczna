##### Projekt ze statystyki #####
##### Autor: Kasper �daniec #####



# wyczyszczenie obszaru roboczego
rm(list = ls())

# za�adowanie pliku z danymi
install.packages("ISLR")
library(ISLR)
dane <- Auto

# wst�pny przegl�d danych
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

# przyj�cie poziom�w ufno�ci
CL1 <- 0.95
CL2 <- 0.99

# przyj�cie poziom�w istotno�ci
SL1 <- 1 - CL1
SL2 <- 1 - CL2



### (A1) przedzia�y ufno�ci dla �redniej ###


# Poniewa� badana pr�ba jest du�a, na podstawie twierdzenia granicznego
# nie ma potrzeby badania za�o�e� przy budowie przedzia��w ufno�ci


## (A1.1) zmienna mpg

t.test(dane$mpg, conf.level = CL1)$conf.int
# Przedzia� od 22,67 do 24,22 (mil z galona benzyny) jest jednym
# z niesko�czenie wielu mo�liwych do uzyskania, kt�re z prawdopodobie�-
# stwem 95% pokrywaj� nieznan� warto�� oczekiwan� wydajno�ci samochodu.

t.test(dane$mpg, conf.level = CL2)$conf.int
# Przedzia� od 22,43 do 24,47 (mil z galona benzyny) jest jednym
# z niesko�czenie wielu mo�liwych do uzyskania, kt�re z prawdopodobie�-
# stwem 99% pokrywaj� nieznan� warto�� oczekiwan� wydajno�ci samochodu.


## (A1.2) zmienna horsepower

t.test(dane$horsepower, conf.level = CL1)$conf.int
# Przedzia� od 100,65 do 108,29 (KM) jest jednym z niesko�czenie wielu
# mo�liwych do uzyskania, kt�re z prawdopodobie�stwem 95% pokrywaj�
# nieznan� warto�� oczekiwan� mocy silnika samochodu.

t.test(dane$horsepower, conf.level = CL2)$conf.int
# Przedzia� od 99,44 do 109,50 (KM) jest jednym z niesko�czenie wielu
# mo�liwych do uzyskania, kt�re z prawdopodobie�stwem 99% pokrywaj�
# nieznan� warto�� oczekiwan� mocy silnika samochodu.


## (A1.3) zmienna acceleration

t.test(dane$acceleration, conf.level = CL1)$conf.int
# Przedzia� od 15,27 do 15,82 (sekund) jest jednym z niesko�czenie wielu
# mo�liwych do uzyskania, kt�re z prawdopodobie�stwem 95% pokrywaj�
# nieznan� warto�� oczekiwan� przyspieszenia samochodu (czasu osi�gni�cia
# pr�dko�ci od 0 do 60 mil na godzin�).

t.test(dane$acceleration, conf.level = CL2)$conf.int
# Przedzia� od 15,18 do 15,90 (sekund) jest jednym z niesko�czenie wielu
# mo�liwych do uzyskania, kt�re z prawdopodobie�stwem 99% pokrywaj�
# nieznan� warto�� oczekiwan� przyspieszenia samochodu (czasu osi�gni�cia
# pr�dko�ci od 0 do 60 mil na godzin�).



### (A2) przedzia�y ufno�ci dla r�nicy �rednich ###


# Poniewa� badana pr�ba jest du�a, na podstawie twierdzenia granicznego
# nie ma potrzeby badania za�o�e� przy budowie przedzia��w ufno�ci


## (A2.1) zmienna mpg dla samochod�w cztero- i o�miocylindrowych

tapply(dane$mpg, dane$cylinders, mean)
# Samochody czterocylindrowe s� w pr�bie przeci�tnie bardziej wydajne
# (29,28 mil z galona benzyny) ni� pojazdy o�miocylindrowe (14,96).

t.test(dane$mpg[dane$cylinders == 4],
       dane$mpg[dane$cylinders == 8],
       conf.level = CL1)$conf.int
# Przedzia� od 13,36 do 15,28 (mil z galona benzyny) jest jednym
# z niesko�czenie wielu mo�liwych do uzyskania, kt�re z prawdopodo-
# bie�stwem 95% pokrywaj� nieznan� warto�� r�nicy mi�dzy �redni�
# wydajno�ci� pojazd�w cztero- i o�miocylindrowych.

t.test(dane$mpg[dane$cylinders == 4],
       dane$mpg[dane$cylinders == 8],
       conf.level = CL2)$conf.int
# Przedzia� od 13,05 do 15,59 (mil z galona benzyny) jest jednym
# z niesko�czenie wielu mo�liwych do uzyskania, kt�re z prawdopodo-
# bie�stwem 99% pokrywaj� nieznan� warto�� r�nicy mi�dzy �redni�
# wydajno�ci� pojazd�w cztero- i o�miocylindrowych.


## (A2.2) zmienna horsepower dla samochod�w cztero- i o�miocylindrowych

tapply(dane$horsepower, dane$cylinders, mean)
# Samochody o�miocylindrowe maj� w pr�bie przeci�tnie mniejsz� moc
# (158,30 KM) ni� pojazdy czterocylindrowe (78,28).

t.test(dane$horsepower[dane$cylinders == 8],
       dane$horsepower[dane$cylinders == 4],
       conf.level = CL1)$conf.int
# Przedzia� od 74,11 do 85,93 (KM) jest jednym z niesko�czenie wielu
# mo�liwych do uzyskania, kt�re z prawdopodobie�stwem 95% pokrywaj�
# nieznan� warto�� r�nicy mi�dzy �redni� moc� silnika pojazd�w
# o�mio- i czterocylindrowych.

t.test(dane$horsepower[dane$cylinders == 8],
       dane$horsepower[dane$cylinders == 4],
       conf.level = CL2)$conf.int
# Przedzia� od 72,21 do 87,83 (KM) jest jednym z niesko�czenie wielu
# mo�liwych do uzyskania, kt�re z prawdopodobie�stwem 99% pokrywaj�
# nieznan� warto�� r�nicy mi�dzy �redni� moc� silnika pojazd�w
# o�mio- i czterocylindrowych.


## (A2.3) zmienna acceleration dla samochod�w cztero- i o�miocylindrowych

tapply(dane$acceleration, dane$cylinders, mean)
# Samochody czterocylindrowe maj� w pr�bie przeci�tnie wi�ksze
# przyspieszenie od 0 do 60 mil na godzin� (16,58 sekund) od pojazd�w
# o�miocylindrowych (12,96).

t.test(dane$acceleration[dane$cylinders == 4],
       dane$acceleration[dane$cylinders == 8],
       conf.level = CL1)$conf.int
# Przedzia� od 3,08 do 4,17 (sekund) jest jednym z niesko�czenie wielu
# mo�liwych do uzyskania, kt�re z prawdopodobie�stwem 95% pokrywaj�
# nieznan� warto�� r�nicy mi�dzy �rednim przyspieszeniem od 0 do 60
# mil na godzin� samochod�w cztero- i o�miocylindrowych.

t.test(dane$acceleration[dane$cylinders == 4],
       dane$acceleration[dane$cylinders == 8],
       conf.level = CL2)$conf.int
# Przedzia� od 2,91 do 4,35 (sekund) jest jednym z niesko�czenie wielu
# mo�liwych do uzyskania, kt�re z prawdopodobie�stwem 99% pokrywaj�
# nieznan� warto�� r�nicy mi�dzy �rednim przyspieszeniem od 0 do 60
# mil na godzin� samochod�w cztero- i o�miocylindrowych.



### (A3) przedzia�y ufno�ci dla wariancji ###


# Przyjmuj� za�o�enie o normalno�ci rozk�adu poszczeg�lnych zmiennych
# zale�nych w badanej pr�bie losowej


# zainstalowanie i podczytanie potrzebnej bilioteki
install.packages("DescTools")
library(DescTools)


## (A3.1) zmienna mpg

VarCI(dane$mpg, conf.level = CL1)
# Przedzia� od 53,21 do 70,45 jest jednym z niesko�czenie wielu
# mo�liwych do uzyskania, kt�re z prawdopodobie�stwem 95%
# pokrywaj� nieznan� wariancj� wydajno�ci samochodu.

VarCI(dane$mpg, conf.level = CL2)
# Przedzia� od 51,03 do 73,81 jest jednym z niesko�czenie wielu
# mo�liwych do uzyskania, kt�re z prawdopodobie�stwem 99%
# pokrywaj� nieznan� wariancj� wydajno�ci samochodu.


## (A3.2) zmienna horsepower

VarCI(dane$horsepower, conf.level = CL1)
# Przedzia� od 1294 do 1713 jest jednym z niesko�czenie wielu
# mo�liwych do uzyskania, kt�re z prawdopodobie�stwem 95%
# pokrywaj� nieznan� wariancj� mocy silnika samochodu.

VarCI(dane$horsepower, conf.level = CL2)
# Przedzia� od 1241 do 1795 jest jednym z niesko�czenie wielu
# mo�liwych do uzyskania, kt�re z prawdopodobie�stwem 99%
# pokrywaj� nieznan� wariancj� mocy silnika samochodu.


## (A3.3) zmienna acceleration

VarCI(dane$acceleration, conf.level = CL1)
# Przedzia� od 6,65 do 8,80 jest jednym z niesko�czenie wielu
# mo�liwych do uzyskania, kt�re z prawdopodobie�stwem 95%
# pokrywaj� nieznan� wariancj� przyspieszenia pojazdu.

VarCI(dane$acceleration, conf.level = CL2)
# Przedzia� od 6,38 do 9,22 jest jednym z niesko�czenie wielu
# mo�liwych do uzyskania, kt�re z prawdopodobie�stwem 99%
# pokrywaj� nieznan� wariancj� przyspieszenia pojazdu.



### (A4) przedzia�y ufno�ci dla odchylenia standardowego ###


# Przyjmuj� za�o�enie o normalno�ci rozk�adu poszczeg�lnych zmiennych
# zale�nych w badanej pr�bie losowej


## (A4.1) zmienna mpg

sqrt(VarCI(dane$mpg, conf.level = CL1))
# Przedzia� od 7,29 do 8,39 (mil na godzin� z galona benzyny)
# jest jednym z niesko�czenie wielu mo�liwych do uzyskania,
# kt�re z prawdopodobie�stwem 95% pokrywaj� nieznane odchylenie
# standardowe wydajno�ci samochodu.

sqrt(VarCI(dane$mpg, conf.level = CL2))
# Przedzia� od 7,14 do 8,59 (mil na godzin� z galona benzyny)
# jest jednym z niesko�czenie wielu mo�liwych do uzyskania,
# kt�re z prawdopodobie�stwem 99% pokrywaj� nieznane odchylenie
# standardowe wydajno�ci samochodu.


## (A4.2) zmienna horsepower

sqrt(VarCI(dane$horsepower, conf.level = CL1))
# Przedzia� od 35,97 do 41,39 (KM) jest jednym z niesko�czenie
# wielu mo�liwych do uzyskania, kt�re z prawdopodobie�stwem 95%
# pokrywaj� nieznane odchylenie standardowe mocy silnika samochodu.

sqrt(VarCI(dane$horsepower, conf.level = CL2))
# Przedzia� od 35,23 do 42,37 (KM) jest jednym z niesko�czenie
# wielu mo�liwych do uzyskania, kt�re z prawdopodobie�stwem 99%
# pokrywaj� nieznane odchylenie standardowe mocy silnika samochodu.


## (A4.3) zmienna acceleration

sqrt(VarCI(dane$acceleration, conf.level = CL1))
# Przedzia� od 2,578 do 2,967 (sekund) jest jednym z niesko�czenie
# wielu mo�liwych do uzyskania, kt�re z prawdopodobie�stwem 95%
# pokrywaj� nieznane odchylenie standardowe przyspieszenia pojazdu
# od pr�dko�ci 0 do 60 mil na godzin�.

sqrt(VarCI(dane$acceleration, conf.level = CL2))
# Przedzia� od 2,525 do 3,037 (sekund) jest jednym z niesko�czenie
# wielu mo�liwych do uzyskania, kt�re z prawdopodobie�stwem 99%
# pokrywaj� nieznane odchylenie standardowe przyspieszenia pojazdu
# od pr�dko�ci 0 do 60 mil na godzin�.



### (B) przedzia�y ufno�ci dla frakcji element�w wyr�nionych ###


# Za�o�enie o du�ej pr�bie (N > 100) jest spe�nione


## (B.1) udzia� pojazd�w sze�ciocylindrowych

table(dane$cylinders)
round(table(dane$cylinders)/length(dane$cylinders)*100, 2)
# Samochod�w sze�ciocylindrowych by�o w badanej pr�bie 83.
# Stanowi to 21,17 procent wszystkich badanych pojazd�w.

prop.test(length(dane$cylinders[dane$cylinders == 6]),
          length(dane$cylinders), conf.level = CL1)$conf.int
# Przedzia� od 17,30 do 25,62 procent jest jednym z niesko�czenie wielu
# mo�liwych do uzyskania, kt�re z prawdopodobie�stwem 95% pokrywaj�
# nieznany odsetek samochod�w sze�ciocylindrowych.

prop.test(length(dane$cylinders[dane$cylinders == 6]),
          length(dane$cylinders), conf.level = CL2)$conf.int
# Przedzia� od 16,25 do 27,08 procent jest jednym z niesko�czenie wielu
# mo�liwych do uzyskania, kt�re z prawdopodobie�stwem 99% pokrywaj�
# nieznany odsetek samochod�w sze�ciocylindrowych.


## (B.2) udzia� pojazd�w produkcji ameryka�skiej

table(dane$origin)
round(table(dane$origin)/length(dane$origin)*100, 2)
# Samochod�w ameryka�skich by�o w badanej pr�bie 245, co stanowi
# 62,50 procent wszystkich badanych pojazd�w.

prop.test(length(dane$origin[dane$origin == 1]),
          length(dane$origin), conf.level = CL1)$conf.int
# Przedzia� od 57,48 do 67,27 procent jest jednym z niesko�czenie wielu
# mo�liwych do uzyskania, kt�re z prawdopodobie�stwem 95% pokrywaj�
# nieznany odsetek samochod�w ameryka�skich.

prop.test(length(dane$origin[dane$origin == 1]),
          length(dane$origin), conf.level = CL2)$conf.int
# Przedzia� od 55,91 do 68,66 procent jest jednym z niesko�czenie wielu
# mo�liwych do uzyskania, kt�re z prawdopodobie�stwem 99% pokrywaj�
# nieznany odsetek samochod�w ameryka�skich.



### (C) hipotezy dotycz�ce warto�ci parametr�w i postaci funkcyjnych ###


## (C.1) zmienna mpg

summary(dane$mpg)
t.test(dane$mpg, alternative = "greater", mu = 22.5)
# �rednia wydajno�� pojazd�w z jednego galona benzyny
# jest istotnie wi�ksza ni� 22,5 mil (p = 0,0084 < 0,05).

hist(dane$mpg)
shapiro.test(dane$mpg)
# Rozk�ad wydajno�ci samochod�w r�ni si� istotnie
# od rozk�adu normalnego (p = 0,000000105 < 0,05).


## (C.2) zmienna horsepower

summary(dane$horsepower)
t.test(dane$horsepower, mu = 100)
# �rednia moc silnika samochod�w r�ni si� istotnie
# od 100 KM (p = 0,02203 < 0,05).

hist(dane$horsepower)
ks.test(dane$horsepower, "plnorm")
# Rozk�ad mocy silnika samochod�w r�ni si� istotnie
# od rozk�adu logarytmiczno-normalnego (p < 0,001 < 0,05).


## (C.3) zmienna acceleration

summary(dane$acceleration)
t.test(dane$acceleration, alternative = "less", mu = 16)
# �redni czas przyspieszenia pojazd�w od pr�dko�ci
# 0 do 60 mil na godzin� jest istotnie mniejszy
# ni� 16 sekund (p = 0,0005432 < 0,05).

var(dane$acceleration)
if (!require(EnvStats)) {install.packages("EnvStats")}
library(EnvStats)
VarTest(dane$acceleration, alternative = "less",
        sigma.squared = 9)
# Wariancja czasu przyspieszenia pojazd�w od pr�dko�ci
# 0 do 60 mil na godzin� jest istotnie mniejsza
# ni� 9 sekund (p = 0,01205 < 0,05).

hist(dane$acceleration)
if (!require(tseries)) {install.packages("tseries")}
library(tseries)
jarque.bera.test(dane$acceleration)
# Rozk�ad czasu przyspieszenia samochod�w r�ni si�
# istotnie od rozk�adu normalnego (p = 0,0147 < 0,05).



### (D) Badanie zale�no�ci liczby cylindr�w samochod�w (dla pojazd�w
#  cztero-, sze�cio- i o�miocylindrowych) od miejsca ich produkcji


## Test niezale�no�ci chi-kwadrat
dane468 <- dane[dane$cylinders == 4 |
                  dane$cylinders == 6 |
                  dane$cylinders == 8, ]
(CT <- table(dane468$origin, dane468$cylinders)[, c("4", "6", "8")])
(X2 <- chisq.test(dane468$origin, dane468$cylinders))
# Wyst�puje zale�no�� stochastyczna mi�dzy liczb� cylindr�w
# pojazd�w a krajem ich produkcji (p < 0,001 < 0,05).


## Oznaczenia pomocniczne:
(K <- dim(CT)[1])
(L <- dim(CT)[2])
(N <- sum(CT))
(S <- X2$statistic)


## Wsp�czynnik kontyngencji V Cramera
(VC <- sqrt(S/(N*(min(K, L)-1))))
# Zale�no�� w pr�bie losowej mi�dzy liczb� cylindr�w pojazdu
# a krajem jego produkcji jest umiarkowana silna.


## Wsp�czynnik Phi Yule'a
(PY <- sqrt(S/N))
# Zale�no�� w pr�bie losowej mi�dzy liczb� cylindr�w pojazdu
# a krajem jego produkcji jest umiarkowana silna.


## Wsp�czynnik kontyngencji T Czuprowa
(TC <- sqrt(S/(N*sqrt((K-1)*(L-1)))))
# Zale�no�� w pr�bie losowej mi�dzy liczb� cylindr�w pojazdu
# a krajem jego produkcji jest umiarkowanie silna.


## Wsp�czynnik kontyngencji C Pearsona
(CP <- sqrt(S/(S+N)))
# Zale�no�� w pr�bie losowej mi�dzy liczb� cylindr�w pojazdu
# a krajem jego produkcji jest umiarkowanie silna.



### (E.1) Modele regresji prostej ###


## Zale�no�� mi�dzy przyspieszeniem a moc� silnika pojazd�w:
summary(M1 <- lm(acceleration ~ horsepower, data = dane))
# Wzrost mocy silnika o 10 KM wi��e si� ze spadkiem czasu
# przyspieszenia (od pr�dko�ci 0 do 60 mil na godzin�)
# przeci�tnie o 0,5 sekundy (p < 0,001).


## Zale�no�� mi�dzy wydajno�ci� a miejscem produkcji pojazd�w:
tapply(dane$mpg, dane$origin, mean)
summary(M1 <- lm(mpg ~ origin, data = dane))
# �rednia wydajno�� samochod�w japo�skich (w milach z galona
# beznzyny) wynosi ponad 30, europejskich - nieca�e 28, za�
# ameryka�skich - tylko 20.
# Samochody produkcji europejskiej (origin == 2) s� przeci�tnie
# bardziej wydajne od pojazd�w wykonanych w USA (p < 0,001).
# R�wnie� samochody produkcji japo�skiej (origin == 2) s� �rednio
# bardziej wydajne od pojazd�w ameryka�skich (p < 0,001).



### (E.2) Model regresji wielorakiej ###

summary(lm(acceleration ~ horsepower + mpg + origin, data = dane))