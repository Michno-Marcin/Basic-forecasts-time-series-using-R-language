### Zainstalowanie, oraz wczytanie bibliotek odpowiadaj¹cych za:
#   import plików xls/csv, operacje na ³añcuchac tekstowych i szeregach czasowych
#   (W tym usuniêcia brakuj¹cych wartoœci), wykresy

if (!("readxl" %in% rownames(installed.packages()))) install.packages("readxl")
library("readxl")  # Do wczytania plików typu csv
if (!('dplyr' %in% rownames(installed.packages()))) install.packages('dplyr')
library('dplyr') # Dla fitrowania/wybierania danych z ramki
if (!('timeSeries' %in% rownames(installed.packages()))) install.packages('timeSeries')
library('timeSeries')  # Do usuniêcia brakuj¹cych wartoœci w szeregu
if (!('forecast' %in% rownames(installed.packages()))) install.packages('forecast')
library('forecast')  # Do wykresów (Acf, Pacf)
if (!('stats' %in% rownames(installed.packages()))) install.packages('stats')
library('stats')  # Do wykresów (np. monthplot)
if (!('lattice' %in% rownames(installed.packages()))) install.packages('lattice')
library('lattice')  # Do wykresów (np. xyplot)
if (!('stringi' %in% rownames(installed.packages()))) install.packages('stringi')
library('stringi')  # Do funkcji tekstowej (np. stri_sub)
# Ewentualnie tak¿e
# if (!('quantmod' %in% rownames(installed.packages()))) install.packages('quantmod')
# library('quantmod')
# if (!("tidyr" %in% rownames(installed.packages()))) install.packages("tidyr")
# library("tidyr")


### Wczytanie pliku z danymi
getwd() # Sprawdzenie aktualnego katalogu roboczego 
# setwd("TWÓJ_KATALOG_ROBOCZY_Z_PROJEKTEM") # <- Wybranie domyœlnego katalogu roboczego z projektem
getwd() # Sprawdzenie czy katalog roboczy zosta³ zmieniony 
my_data <- read.csv("country_vaccinations.csv") # Wczytanie tabeli z danymi z pliku
# my_data <- read.csv(file.choose()) # Ewentualny wybór pliku z danymi do wczytania

# Wyœwietlenie informacji o tabeli z danymi
# str(my_data)
# summary(my_data)
# head(my_data)



### Wybór kraju dla analizy jej danych (np. "Poland", "Palestine")
sort(unique(my_data$country))
ChosenCountry <- "Poland"
ChosenRows <- which(my_data[,1] == ChosenCountry,TRUE)

### "Wyci¹gniêcie" odpowiednich danych dla wybranego kraju
ExaminedData <- select(my_data, country,date,people_fully_vaccinated)
dim(ExaminedData) # Wymiary ca³ego zestawu danych
# Przypisanie danych wy³¹cznie danego kraju
NData <- ExaminedData[
  sapply(ExaminedData$country, 
         function (x) (
           if (x ==  ChosenCountry
               | is.null(x) | is.na(x)) TRUE else FALSE)), 1:dim(ExaminedData)[2]]

# Wyœwietlenie informacji o liczbie osób ca³kowicie zaszczepionych z danego kraju
summary(NData$people_fully_vaccinated) # Dla danych dziennych



### Utworzenie szeregów czasowych
TimeSerialD<-as.ts(NData$people_fully_vaccinated)
# Ewentualnie: TimeSerialD <- ts(NData$people_fully_vaccinated,start=c(2020,12,8),frequency=365.25)
# Interpolacja obserwacji brakuj¹cych
TimeSerialD<-na.interp(TimeSerialD)
ts.plot(TimeSerialD,xlab = "Dzieñ", ylab = "Liczba ca³kowitych szczepieñ",col = c("red"),main="Liczba ca³kowitych szczepieñ na COVID19 w Polsce") 


### Omówienie g³ównych cech analizowanego szeregu na podstawie ró¿nych typów wykresów, wnioski
#   Wykres z u¿yciem xyplot()
xyplot(TimeSerialD, main="Liczba ca³kowitych szczepieñ w Polsce", 
       xlab = "Dzieñ",ylab="Liczba zaszczepionych", aspect = 1/3) 

# Wykres panelowy
xyplot(TimeSerialD, main="Liczba ca³kowitych szczepieñ w Polsce", 
       xlab = "Dzieñ",ylab="Liczba zaszczepionych", aspect = 1/3,
       cut = list(number = 3, overlap = 0.1)) # overlap - jaka czêœæ danych powtórzona w kolejnym panelu) 

# Wykres pude³kowy (Na wykresach: kwartyle 1,3 rzêdu, mediany, wartoœæ minimalna/maksymalna, odstaj¹ce)
boxplot(TimeSerialD ~ cycle(TimeSerialD), main="Liczba ca³kowitych szczepieñ w Polsce", 
                            xlab = "Dzieñ",ylab="Liczba zaszczepionych", aspect = 1/3)

# Wykres rozrzutu dla wartoœci opóŸnionych
lag.plot(TimeSerialD, lags=12, do.lines = FALSE) 
lag.plot(TimeSerialD) # Dobrze widoczny trend wzrostowy
# Najsilniejsza korelacja (zale¿noœæ) dla opóŸnienia pierwszego pokazuje widoczny trend wzrostowy
plot(TimeSerialD)

# Wykresy: funkcji autokorelacji - Acf, cz¹stkowej funkcji autokorelacji - Pacf

# acf(TimeSerialD, lag.max = 30, xlab = "Odstêp dzienny", ylab="Korelacja danych",
#    main = "Korelogram dla liczby ca³kowitych szczepieñ w Polsce")
par(mfrow = c(2,1))
Acf(TimeSerialD, lag.max = 30, xlab = "Odstêp dzienny", ylab="Korelacja danych",
    main = "Korelogram dla œrednich temperatur miesiêcznych w Polsce")
# Widaæ wyraŸny trend poprzez ³agodne zmiany na wykresie

# pacf(TimeSerialD, lag.max = 30, xlab = "Odstêp dzienny", ylab="Bezpoœrednia korelacja danych",
#      main = "Korelogram dla liczby ca³kowitych szczepieñ w Polsce")
Pacf(TimeSerialD, lag.max = 30, xlab = "Odstêp dzienny", ylab="Bezpoœrednia korelacja danych",
     main = "Korelogram dla liczby ca³kowitych szczepieñ w Polsce")
# Widaæ doœæ mocny trend tak¿e dziêki bliskiej wartoœci - jeden wartoœci dla "laga" pierwszego 



### Zastosowanie œrednich ruchomych (lewostronnych) do wyg³adzania wykresu
ma3 <- filter(TimeSerialD, sides = 1, filter = rep(1/3,3)) # Dla trzech dni
ma14 <- filter(TimeSerialD, sides = 1, filter = rep(1/14,14)) # Dla 14-stu dni

# Porównanie wykresów
plot(TimeSerialD, main = "Liczba ca³kowitych szczepieñ w Polsce",
     xlab = "Rok", ylab="Odchy³ka temperatury [C]")
lines(ma3,col="red", lty = 8)
lines(ma14,col="green", lty = 8)



### Dekompozycje na podstawie modelu regresji: trend liniowy/wielomianowy, sezonowoœæ, transformacja Boxa-Coxa 
TimeSerialD_T <- tslm(TimeSerialD ~ trend) 
# TimeSerialD_S <- tslm(TimeSerialD ~ season) # Brak sezonowoœci 
TimeSerialD_TSB5 <- tslm((TimeSerialD) ~ trend, lambda = 0.5) # Pierwiastkowa
TimeSerialD_TSB1 <- tslm((TimeSerialD) ~ trend, lambda = 0) # Logarytmiczna transformacja
TimeSerialD_TSBA <- tslm((TimeSerialD) ~ trend, lambda = "auto") # Automatyczny dobór rzêdu

# Trend kwadratowy
TimeSerialD_WS<- tslm(TimeSerialD ~ poly(trend,raw=TRUE,degree=2))
# Trend stopnia trzeciego
TimeSerialD_WS3<- tslm(TimeSerialD ~ poly(trend,raw=TRUE,degree=3))
summary(TimeSerialD_TSBA) # Informacje o przyk³adowym z wybranych modeli

# Porównamy metody
par(mfrow = c(2,1))
plot(TimeSerialD, main="Liczba ca³kowitych szczepieñ w Polsce",
     xlab = "Dzieñ", ylab="Liczba zaszczepionych")
lines(fitted(TimeSerialD_T), col = "green", lty = 2) # Wizualizacja wykrytego trendu liniowego
lines(fitted(TimeSerialD_WS), col = "blue", lty = 2) # Wizualizacja wykrytego trendu kwadratowego
lines(fitted(TimeSerialD_WS3), col = "red", lty = 2) # Wizualizacja wykrytego trendu stopnia trzeciego

plot(TimeSerialD, main="Liczba ca³kowitych szczepieñ w Polsce",
     xlab = "Dzieñ", ylab="Liczba zaszczepionych")
lines(fitted(TimeSerialD_TSB5), col = "green", lty = 2) # Wizualizacja transformacji pierwiastkowej
lines(fitted(TimeSerialD_TSB1), col = "blue", lty = 2) # Wizualizacja transformacji logarytmicznej
lines(fitted(TimeSerialD_TSBA), col = "red", lty = 2) # Wizualizacja transformacji automatycznego rzêdu

# Widzimy, ¿e sama transformacja Boxa_Coxa w tym przypadku nie poprawia wystarczaj¹co rezultatu.
# Koniecznym za³o¿eniem jest tutaj trend wielomianowy, najprawdopodobniej stopnia trzeciego.


# Porównanie reszt ró¿nymi metodami dekompozycji na podstawie modelu regresji
tsdisplay(residuals(TimeSerialD_T),main="Reszty dla trendu liniowego") # Wyeliminowany trend liniowy
tsdisplay(residuals(TimeSerialD_WS),main="Reszty dla trendu kwadratowego") # Wyeliminowany trend kwadratowy
tsdisplay(residuals(TimeSerialD_WS3),main="Reszty dla trendu stopnia trzeciego") # Wyeliminowany trend stopnia trzeciego



### Uczynienie szeregu stacjonarnym - usuniêcie trendu, sezonowoœci: ró¿nicowanie, *transformacja Boxa-Coxa
### Sprawdzenie, czy s¹ realizacj¹ szumu bia³ego, którego rzêdu modele AR(p), MA(q) warto braæ pod uwagê
tsdisplay(TimeSerialD,main="Rozk³ad liczby ca³kowitych szczepieñ w Polsce",
          xlab = "Dzieñ", ylab="Liczba zaszczepionych", lag.max=60)
hist(TimeSerialD,main="Histogram reszt") 

TimeSerialDBox<-BoxCox(TimeSerialD,lambda="auto")
tsdisplay(TimeSerialDBox,main="Rozk³ad liczby ca³kowitych szczepieñ w Polsce",
          xlab = "Dzieñ", ylab="Liczba zaszczepionych", lag.max=60)
# Poni¿ej wartoœæ okreœlaj¹ca maksymaln¹ odchy³kê od przedzia³u ufnoœci dla szeregu stacjonarnego
(Val <- 1.96/sqrt(length(TimeSerialDBox))) 
ndiffs(TimeSerialDBox) # Iloœæ potrzebnych ró¿nicowañ rzêdu ró¿nicowania z opóŸnieniem wartoœci: jeden

TimeSerialDT <- diff(x=TimeSerialDBox, lag=1) # Usuniêcie trendu z opóŸnieniem: 1
hist(TimeSerialDT, main="Histogram reszt") 
# Rozk³ad wartoœci jest czêœciowo zbli¿ony do normalnego, jednak nadal nim nie jest.
tsdisplay(TimeSerialDT, main="Rozk³ad liczby ca³kowitych szczepieñ w Polsce bez trendu",
          xlab = "Dzieñ", ylab="Liczba zaszczepionych",lag.max=60)
(Val <- 1.96/sqrt(length(TimeSerialDT))) 
ndiffs(TimeSerialDT) # Iloœæ potrzebnych ró¿nicowañ rzêdu ró¿nicowania z opóŸnieniem wartoœci: jeden

TimeSerialDTS <- diff(x=TimeSerialDT, lag=1) # Usuniêcie trendu z opóŸnieniem: 1 z otrzymanych reszt
hist(TimeSerialDTS, main="Histogram reszt") 
# Rozk³ad wartoœci jest bardzo zbli¿ony do normalnego.
ndiffs(TimeSerialDTS) # Iloœæ potrzebnych ró¿nicowañ rzêdu ró¿nicowania z opóŸnieniem wartoœci: jeden
tsdisplay(TimeSerialDTS, main="Rozk³ad liczby ca³kowitych szczepieñ w Polsce bez trendu",
          xlab = "Dzieñ", ylab="Liczba zaszczepionych",lag.max=length(TimeSerialDTS))

# Widaæ jednak dalej pewn¹ sezonowoœæ reszt. W celu doboru rzêdu ró¿nicowania sprawdzimy wykresy rozrzutu.
lag.plot(TimeSerialDTS,lags=12, do.lines = FALSE) # Dobrze widoczny trend wzrostowy
TimeSerialDTS7 <- diff(x=TimeSerialDTS, lag=7) # Usuniêcie trendu z opóŸnieniem sezonowym (7) z otrzymanych reszt
tsdisplay(TimeSerialDTS7, main="Rozk³ad liczby ca³kowitych szczepieñ w Polsce bez trendu",
          xlab = "Dzieñ", ylab="Liczba zaszczepionych",lag.max=length(TimeSerialDTS7))
# Wartoœci: ACF(1,7,8,21,23,30,31), PACF(1,3,7,*14,21,22) wykraczaj¹ poza przedzia³ ufnoœci
# Warto wzi¹æ pod uwagê modele  MA(q) oraz AR(p) o wspó³czynnikach poni¿ej:
q <- c(1,7,8,21,23,30,31)
p <- c(1,3,7,14,21,22)

# Sprawdzenie stacjonarnoœci szeregu - pomocnicza linia pokazuj¹ca, czy wartoœæ skrajnych odchy³ek nie jest zbyt du¿a
MaxDifference <- 1.96/sqrt(length(TimeSerialDTS7))
AcfLimit <- qnorm((1+0.95)/2)/sqrt(sum(!is.na(TimeSerialDTS7)))
StatBD <- MaxDifference+AcfLimit
Acf(TimeSerialDTS7,lag.max=60)
curve(StatBD + 0*x, add = TRUE, col="red")
curve(-StatBD + 0*x, add = TRUE, col="red")
# Jest to wiêc szereg stacjonarny.

# Usuniêcie œredniej
TimeSerialDNM<-TimeSerialDTS7-mean(TimeSerialDTS7)



### Wyznaczenie wspó³czynników modelu autoregresji i porównanie dopasowania ró¿nymi metodami estymacji
TimeSerialMS.ar1.yule <- ar(TimeSerialDNM, aic = FALSE, order.max =1, method = c("yule-walker")) # Utworzenie modelu 
# Mo¿na ewentualnie zobaczyæ strukturê tego modelu: str(TimeSerialMS.ar1)
print(TimeSerialMS.ar1.yule) # Wyznaczenie wspó³czynników modelu

TimeSerialMS.ar1.burg <- ar(TimeSerialDNM, aic = FALSE, order.max =1, method = c("burg")) # Utworzenie modelu 
print(TimeSerialMS.ar1.burg) # Wyznaczenie wspó³czynników modelu

TimeSerialMS.ar1.ols <- ar(TimeSerialDNM, aic = FALSE, order.max =1, method = c("ols")) # Utworzenie modelu 
print(TimeSerialMS.ar1.ols) # Wyznaczenie wspó³czynników modelu

TimeSerialMS.ar1.mle <- ar(TimeSerialDNM, aic = FALSE, order.max =1, method = c("mle")) # Utworzenie modelu 
print(TimeSerialMS.ar1.mle) # Wyznaczenie wspó³czynników modelu

TimeSerialMS.ar1.yw <- ar(TimeSerialDNM, aic = FALSE, order.max =1, method = c("yw")) # Utworzenie modelu 
print(TimeSerialMS.ar1.yw) # Wyznaczenie wspó³czynników modelu

# Automatycznie dobrana wartoœæ rzêdu
TimeSerialMS.arAIC.yw <- ar(TimeSerialDNM, aic = TRUE, order.max = 100, method = c("yule-walker"))
print(TimeSerialMS.arAIC.yw)
# Wartoœæ 1 (AR) to pierwsza z piêciu i najwiêksza zaobserwowana na wykresie Pacf korelacja z propozycji.



### Wyznaczenie wspó³czynników modelu ruchomej œredniej z u¿yciem funkcji Arima()
TimeSerialDNM.m.ar0i0ma1 <- Arima(TimeSerialDNM, order =c(0,0,1))
summary(TimeSerialDNM.m.ar0i0ma1)                          
TimeSerialDNM.m.ar0i0ma7 <- Arima(TimeSerialDNM, order =c(0,0,7))
summary(TimeSerialDNM.m.ar0i0ma7)
# TimeSerialDNM.m.ar0i0ma30 <- Arima(TimeSerialDNM, order =c(0,0,30))
# summary(TimeSerialDNM.m.ar0i0ma30)

# Porównanie modelu wyznaczonego powy¿ej z odpowiadaj¹cym mu modelem AR
# Analiza dobroci dopasowania, oraz wartoœci b³êdów prognoz
TimeSerialDNM.m.ar1i0ma0 <- Arima(TimeSerialDNM, order = c(1,0,0))
summary(TimeSerialDNM.m.ar1i0ma0)
TimeSerialDNM.ar7i0ma0S <- Arima(TimeSerialDNM, order = c(7,0,0))
summary(TimeSerialDNM.ar7i0ma0S)
# TimeSerialDNM.m.ar21i0ma0 <- Arima(TimeSerialDNM, order = c(21,0,0))
# summary(TimeSerialDNM.m.ar21i0ma0)
# TimeSerialDNM.m.ar22i0ma0 <- Arima(TimeSerialDNM, order = c(22,0,0))
# summary(TimeSerialDNM.m.ar22i0ma0)



### Wyznaczenie optymalnych modeli z wykorzystaniem funkcji auto.arima() oraz wyznaczenie ich wspó³czynników
### Porównanie modeli, wybór najlepszego na podstawie kryteriów dopasowania: aic, aicc, bi (g³ównie ostatnich).
# Modele automatyczne
(TimeSerialDTSaicc.auto <- auto.arima(TimeSerialDNM, ic = "aicc"))
summary(TimeSerialDTSaicc.auto) 
# Wyznaczono: ARIMA(1,0,0) z zerow¹ œredni¹ ; AIC=1436.71   AICc=1436.79   BIC=1442.79
(TimeSerialDTSaic.auto <- auto.arima(TimeSerialDNM, ic= "aic"))
summary(TimeSerialDTSaic.auto) 
# Wyznaczono: ARIMA(1,0,0) z zerow¹ œredni¹ ; AIC=1436.71   AICc=1436.79   BIC=1442.79
(TimeSerialDTSbic.auto <- auto.arima(TimeSerialDNM, ic= "bic"))
summary(TimeSerialDTSbic.auto) 
# Wyznaczono: ARIMA(1,0,0) z zerow¹ œredni¹ ; AIC=1436.71   AICc=1436.79   BIC=1442.79
# W ka¿dym przypadku wyznaczono model ARIMA(1,0,0) z zerow¹ œredni¹ o tych wspó³czynnikach dopasowania.

# Modele w³asne
summary(TimeSerialDNM.m.ar0i0ma1) 
# Wyznaczono: ARIMA(0,0,1) z niezerow¹ œredni¹ ; AIC=1446.66   AICc=1446.82   BIC=1455.79
summary(TimeSerialDNM.m.ar1i0ma0) 
# Wyznaczono: ARIMA(1,0,0) z niezerow¹ œredni¹ ; AIC=1438.71   AICc=1438.86   BIC=1447.84

# Najlepszym modelem na podstawie wartoœci kryteriów najbli¿szych zeru jest model: ARIMA(1,0,0).



### Sprawdzenie, czy otrzymane modele mo¿na uznaæ za szum bia³y
tsdisplay(TimeSerialDTSbic.auto$residuals)
# Sprawdzenie stacjonarnoœci szeregu z pomocnicz¹ lini¹ pokazuj¹ca, czy wartoœæ skrajnych odchy³ek nie jest zbyt du¿a
MaxDifference <- 1.96/sqrt(length(TimeSerialDTSbic.auto))
AcfLimit <- qnorm((1+0.95)/2)/sqrt(sum(!is.na(TimeSerialDTSbic.auto)))
StatBD <- MaxDifference+AcfLimit
Acf(TimeSerialDTSbic.auto$residuals,lag.max=1000)
curve(StatBD + 0*x, add = TRUE, col="red")
curve(-StatBD + 0*x, add = TRUE, col="red")
# Jest to wiêc szereg stacjonarny, poniewa¿ ¿adna z wartoœci nie wystaje znacz¹co poza przedzia³ ufnoœci.
hist(TimeSerialDTSbic.auto$residuals) # Histogram jest bardzo zbli¿ony do rozk³adu normalnego.
# Sprawdzenie za³o¿enia o normalnoœci rozk³adu reszt
shapiro.test(TimeSerialDTSbic.auto$residuals) 
# tsdiag(TimeSerialDTSbic.auto, gof.lag=50)
# Po wartoœciach p mo¿na stwierdziæ, ¿e próby pochodz¹ z populacji o rozk³adzie normalnym (poniewa¿ p.value < 0,05).



### Prognozowanie z wykorzystaniem metod naiwnych
# Prognoza oparta na œredniej
TimeSerialD.auto.meanf <- meanf(TimeSerialD, h = 60) 
plot(TimeSerialD.auto.meanf) 

# Prognozowanie naiwne 
TimeSerialD.naive <-naive(TimeSerialD, h=24)
plot(TimeSerialD.naive)
TimeSerialD.snaive <-snaive(TimeSerialD, h=24)
plot(TimeSerialD.snaive)

# Prognozowanie naiwne z dryfem
TimeSerialD.rwf <- rwf(TimeSerialD, h=24, drift = TRUE)
plot(TimeSerialD.rwf)

# Najlepsz¹ metod¹ jest zastosowanie tutaj prognozowania naiwnego z dryfem.