
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
# Ewentualnie tak¿e
# if (!('quantmod' %in% rownames(installed.packages()))) install.packages('quantmod')
# library('quantmod')
# if (!("tidyr" %in% rownames(installed.packages()))) install.packages("tidyr")
# library("tidyr")



### Wczytanie pliku z danymi
getwd() # Sprawdzenie aktualnego katalogu roboczego 
# setwd("TWÓJ_KATALOG_ROBOCZY_Z_PROJEKTEM") # <- Wybranie domyœlnego katalogu roboczego z projektem
getwd() # Sprawdzenie czy katalog roboczy zosta³ zmieniony 
my_data <- read.csv("city_temperature.csv") # Wczytanie tabeli z danymi z pliku
# my_data <- read.csv(file.choose()) # Ewentualny wybór pliku z danymi do wczytania

# Wyœwietlenie informacji o tabeli z danymi
# str(my_data)
# summary(my_data)
# head(my_data)



### Wybór regionu i pañstwa dla analizy jego danych (np. "Europe", "Poland")
sort(unique(my_data$Region))
ChosenRegion <- "Europe"
ChosenRows <- which(my_data[,1] == ChosenRegion,TRUE)
sort(unique(my_data[ChosenRows,]$Country))
ChosenCountry <- "Poland"



### "Wyci¹gniêcie" odpowiednich danych dla wybranego kraju
ExaminedData <- select(my_data, Country, Year,Month,Day, AvgTemperature)
dim(ExaminedData) # Wymiary ca³ego zestawu danych
# Przypisanie danych wy³¹cznie danego kraju
NData <- ExaminedData[
  sapply(ExaminedData$Country, 
         function (x) (
           if (x ==  ChosenCountry 
               | is.null(x) | is.na(x)) TRUE else FALSE)), 1:dim(ExaminedData)[2]]
# Zast¹pienie wartoœci "-99" w przypadku braku danych pomiarowych wartoœci¹ "NaN"
Temp <- NData[1:dim(NData)[1],5]
Temp[Temp==-99] <- "NaN"
# Konwersja skali temperatury z Fahrenheita [F] do Celsjuszy [C]
Temp <- round((as.double(Temp)-32)/1.8, digits = 1)
# Przypisanie poprawnych wartoœci temperatury do ramki danych
NData[1:dim(NData)[1],5] <- Temp



### Utworzê œrednie dla miesiêcy w danych latach dla wiêkszej iloœci wykresów, ich przejrzystoœci
#   Mo¿na zrobiæ podobnie dla lat w danym okresie, powsta³aby jednak ma³a, niereprezentatywna próba 
MonthTemp <- c()
for (i in unique(NData$Year)){
  for (j in unique(NData$Month)){
    Value = mean(NData[((NData[1:dim(NData)[1],2]==i) & (NData[1:dim(NData)[1],3]==j)),5], na.rm=TRUE)
    MonthTemp <- c(MonthTemp, Value)
  }
}

# Wyœwietlenie informacji o temperaturach z danego kraju
summary(Temp) # Dla œrednich dziennych
summary(MonthTemp) # Dla œrednich miesiêcznych



### Utworzenie szeregów czasowych ze œrednimi dziennymi, miesiêcznymi w danych latach
(TimeSerialD <- ts(Temp, start = c(NData$Year[1],NData$Month[1],NData$Day[1]), frequency = 365,
                   end = c(tail(NData$Year, n=1),tail(NData$Month, n=1),tail(NData$Day, n=1))))
(TimeSerialM <- ts(MonthTemp, start = c(NData$Year[1],NData$Month[1]), frequency = 12,
                   end = c(tail(NData$Year, n=1),tail(NData$Month, n=1))))



### Ewentualne korekty kalendarzowe dla szeregu miesiêcznego
#   W naszym przypadku akurat s¹ one zbêdne, poniewa¿ przy pomiarze temperatury obliczamy œrednie :)

# Uwzglêdnienie ró¿nicy dni lutego w latach przestêpnych
(SrDniWRoku <- 365.25/12) 
(DniWMies <- monthdays(TimeSerialM)) 
TimeSerialM2 <- TimeSerialM/DniWMies*SrDniWRoku 

# Wykresy pokazuj¹ce ró¿nice po uwzglêdnieniu istnienia lat przestêpnych
ts.plot(TimeSerialM, TimeSerialM2,  col = c("red", "blue"), 
        xlab = "Rok", ylab="Temperatura [C]",
        main = "Œrednie temperatury miesiêczne w Polsce")

# Ewentualne uwzglêdnienie dni roboczych w miesi¹cach (Nie dla naszego szeregu akurat :) )
(RoboczeWMies <- bizdays(TimeSerialM)) 
(sredniaRoboczych <- sum(RoboczeWMies)/length(RoboczeWMies)) 
TimeSerialM3 <- TimeSerialM/RoboczeWMies*sredniaRoboczych 

# Wykresy pokazuj¹ce ró¿nice po uwzglêdnieniu ró¿nicy liczby dni roboczych w miesi¹cach
ts.plot(TimeSerialM, TimeSerialM3,  col = c("red", "blue"), 
        xlab = "Rok", ylab="Temperatura [C]",
        main = "Œrednie temperatury miesiêczne w Polsce")



### Utworzenie szeregu czasowego dla kwarta³ów w poszczególnych latach poprzez agregacjê danych miesiêcznych
TimeSerialQ <- aggregate(TimeSerialM, nfrequency = 4, FUN = mean)
# Porównanie wykresów dla wiêkszej i mniejszej czêstotliwoœci 
par(mfrow = c(3,1)) 
ts.plot(TimeSerialM,xlab = "Rok", ylab="Temperatura [C]", col = c("blue"),main="Œrednie temperatury miesiêczne w Polsce") 
ts.plot(TimeSerialQ,xlab = "Rok", ylab = "Temperatura [C]",col = c("green"),main="Œrednie temperatury kwartalne w Polsce") 
ts.plot(TimeSerialD,xlab = "Rok", ylab = "Temperatura [C]",col = c("red"),main="Œrednie temperatury dzienne w Polsce") 


### Omówienie g³ównych cech analizowanego szeregu na podstawie ró¿nych typów wykresów, wnioski
#   Wykresy z u¿yciem xyplot()
xyplot(TimeSerialD, 
       main="Œrednie temperatury dzienne w Polsce",
       xlab = "Rok",ylab="Temperatura [C]",
       aspect = 1/3) 
xyplot(TimeSerialM, 
       main="Œrednie temperatury miesiêczne w Polsce",
       xlab = "Rok",ylab="Temperatura [C]",
       aspect = 1/3) 
xyplot(TimeSerialQ, 
       main="Œrednie temperatury kwartalne w Polsce",
       xlab = "Rok",ylab="Temperatura [C]",
       aspect = 1/3) 

# Wykresy panelowe
xyplot(TimeSerialD, 
       main="Œrednie temperatury dzienne w Polsce",
       xlab = "Rok",ylab="Temperatura [C]",
       aspect = 1/3,
       cut = list(number = 3, overlap = 0.1)) # overlap - jaka czêœæ danych powtórzona w kolejnym panelu
xyplot(TimeSerialM, 
       main="Œrednie temperatury miesiêczne w Polsce",
       xlab = "Rok",ylab="Temperatura [C]",
       aspect = 1/3,
       cut = list(number = 3, overlap = 0.1)) # overlap - jaka czêœæ danych powtórzona w kolejnym panelu
xyplot(TimeSerialQ, 
       main="Œrednie temperatury kwartalne w Polsce",
       xlab = "Rok",ylab="Temperatura [C]",
       aspect = 1/3,
       cut = list(number = 3, overlap = 0.1)) # overlap - jaka czêœæ danych powtórzona w kolejnym panelu

# Wykresy sezonowe (monthplot,seasonplot)
monthplot(TimeSerialM,
          main="Œrednie temperatury miesiêczne w Polsce",
          xlab = "Miesi¹c",ylab="Temperatura [C]",
          lty=7)

par(mfrow = c(3,1)) 
seasonplot(TimeSerialD,
           main="Œrednie temperatury dzienne w Polsce",
           xlab = "Dzieñ",ylab="Temperatura [C]", year.labels = TRUE,col = rainbow(25))
seasonplot(TimeSerialM,
           main="Œrednie temperatury miesiêczne w Polsce",
           xlab = "Miesi¹c",ylab="Temperatura [C]", year.labels = TRUE, col = rainbow(25))
seasonplot(TimeSerialQ,
           main="Œrednie temperatury kwartalne w Polsce",
           xlab = "Kwarta³",ylab="Temperatura [C]", col = rainbow(25))

# Wykresy pude³kowe (Na wykresach: kwartyle 1,3 rzêdu, mediany, wartoœci minimalne/maksymalne, odstaj¹ce)
par(mfrow = c(3,1))
boxplot(TimeSerialD ~ cycle(TimeSerialD),
        main="Rozk³ad temperatur dziennych w Polsce",
        xlab="Dzieñ",ylab="Temperatura [C]") 
boxplot(TimeSerialM ~ cycle(TimeSerialM),
        main="Rozk³ad temperatur miesiêcznych w Polsce",
        xlab="Miesi¹c",ylab="Temperatura [C]")   
boxplot(TimeSerialQ ~ cycle(TimeSerialQ),
        main="Rozk³ad temperatur kwartalnych w Polsce",
        xlab="Kwarta³",ylab="Temperatura [C]")  

# Wykres rozrzutu dla wartoœci opóŸnionych
lag.plot(TimeSerialM) # Widaæ raczej brak trendu miesiêcznego
lag.plot(TimeSerialQ) # Widaæ dzia³anie pór roku (Du¿e ró¿nice wartoœci temperatur co pó³ roku)
lag.plot(TimeSerialM, lags=12, do.lines = FALSE) 
# Najsilniejsza korelacja (zale¿noœæ) dla "laga" dwunastego, co sugeruje sezonowoœæ roczn¹ (12-miesiêczn¹)
# Najs³absza korelacja dla "laga" szóstego, co pokazuje dzia³anie pór roku
# Kszta³t "laga" trzeciego, dziewi¹tego pokazuje p³ynne przejœcia  pór roku
lag.plot(TimeSerialQ, lags=4, do.lines = FALSE) 
# Najsilniejsza korelacja (zale¿noœæ) dla "laga" czwartego, co sugeruje sezonowoœæ roczn¹ (4-kwartaln¹)
# Najs³absza korelacja dla "laga" drugiego, co pokazuje dzia³anie pór roku
# Kszta³t "laga" pierwszego, trzeciego mo¿e pokazywaæ przejœcia pór roku


# Wykresy: funkcji autokorelacji - Acf, cz¹stkowej funkcji autokorelacji - Pacf

# acf(TimeSerialM, lag.max = 30, xlab = "Odstêp sezonowy", ylab="Korelacja danych",
#    main = "Korelogram dla œrednich temperatur miesiêcznych w Polsce")
par(mfrow = c(2,1))
Acf(TimeSerialM, lag.max = 30, xlab = "Odstêp miesiêczny", ylab="Korelacja danych",
    main = "Korelogram dla œrednich temperatur miesiêcznych w Polsce")
# Widaæ wyraŸn¹ sezonowoœæ poprzez ³agodne zmiany na wykresie dodanie i ujemne

# pacf(TimeSerialM, lag.max = 30, xlab = "Odstêp sezonowy", ylab="Bezpoœrednia korelacja danych",
#      main = "Korelogram dla œrednich temperatur miesiêcznych w Polsce")
Pacf(TimeSerialM, lag.max = 30, xlab = "Odstêp miesiêczny", ylab="Bezpoœrednia korelacja danych",
     main = "Korelogram dla œrednich temperatur miesiêcznych w Polsce")
# Widaæ doœæ mocny trend tak¿e dziêki bliskiej wartoœci - jeden wartoœci dla "laga" pierwszego 

par(mfrow = c(2,1))
Acf(TimeSerialQ, lag.max = 15) # Widaæ sezonowoœæ, jednak dla bardzo krótkich odstêpóW (2 kwarta³y)
Pacf(TimeSerialQ, lag.max = 15) # Widoczny trend co 2-kwartalny



### Zastosowanie œrednich ruchomych (lewostronnych) do wyg³adzania wykresu
ma14 <- filter(TimeSerialD, sides = 1, filter = rep(1/14,14)) # Dla 20-stu dni 
ma100 <- filter(TimeSerialD, sides = 1, filter = rep(1/100,100)) # Dla 100 dni
ma2M <- filter(TimeSerialM, sides = 1, filter = rep(1/2,2)) # Dla 20-stu dni 
ma6M <- filter(TimeSerialM, sides = 1, filter = rep(1/6,6)) # Dla 100 dni
# Porównanie wykresów
par(mfrow = c(2,1))
plot(TimeSerialD, main = "Œrednie temperatury dzienne w Polsce",
     xlab = "Rok", ylab="Odchy³ka temperatury [C]")
lines(ma14,col="red", lty = 8)
lines(ma100,col="green", lty = 8)
plot(TimeSerialM, main = "Œrednie temperatury miesiêczne w Polsce",
     xlab = "Rok", ylab="Odchy³ka temperatury [C]")
lines(ma2M,col="red", lty = 8)
lines(ma6M,col="green", lty = 8) # Tuaj widaæ zbyt du¿e zaokr¹glenie ju¿



### Dekompozycje na podstawie ruchomej œredniej 
TimeSerialM2 <- decompose(TimeSerialM, type="multiplicative")
TimeSerialM22 <- decompose(TimeSerialM, type="additive")
TimeSerialD3 <- na.interp(TimeSerialD) # Interpolacja obserwacji brakuj¹cych 
TimeSerialD2 <- decompose(TimeSerialD3, type="multiplicative")
TimeSerialD22 <- decompose(TimeSerialD3, type="additive")

# Porównanie wyników oboma metodami
plot(TimeSerialM2, col = c("blue"), xlab = "Rok") # Widaæ Ÿle dobran¹ sezonowoœæ
plot(TimeSerialM22, col = c("blue"), xlab = "Rok") # Lepiej dobrana sezonowoœæ, nadal brak stacjonarnoœci
# W przypadku wiêkszej iloœci obserwacji powy¿sze obserwacje widaæ wyraŸniej
# Lepsz¹ opcj¹ w tym przypadku jest wybranie metody addytywnej
plot(TimeSerialD2, col = c("blue"), xlab = "Rok")
plot(TimeSerialD22, col = c("blue"), xlab = "Rok")

# Porównanie reszt oboma metodami
tsdisplay(TimeSerialM2$random,main="Reszty otrzymane metod¹ multiplikatywn¹")
tsdisplay(TimeSerialM22$random,main="Reszty otrzymane metod¹ addytywn¹")
tsdisplay(TimeSerialD2$random)
tsdisplay(TimeSerialD22$random)



### Dekompozycje na podstawie modelu regresji: trend liniowy/wielomianowy, sezonowoœæ, transformacja Boxa-Coxa 
TimeSerialM_T <- tslm(TimeSerialM ~ trend) 
TimeSerialM_S <- tslm(TimeSerialM ~ season) 
TimeSerialM_TS <- tslm(TimeSerialM ~ trend + season)
TimeSerialM_TSB <- tslm((TimeSerialM) ~ trend + season, lambda = 0.5) # Pierwiastkowa, ew. "auto"
# Trend wielomianowy
TimeSerialM_WS<- tslm(TimeSerialM ~ poly(trend,raw=TRUE,degree=2)+season)
TimeSerialM_WSB<- tslm(TimeSerialM ~ poly(trend,raw=TRUE,degree=2)+season, lambd=0.5)
summary(TimeSerialM_TSB) # Informacje o przyk³adowym z wybranych modeli

# Porównamy metody
par(mfrow = c(2,1))
plot(TimeSerialM, main="Rozk³ad temperatur miesiêcznych w Polsce",
     xlab = "Rok", ylab="Odchy³ka temperatury [C]")
lines(fitted(TimeSerialM_T), col = "orange", lty = 2) # Wizualizacja wykrytego trendu
lines(fitted(TimeSerialM_S), col = "darkblue", lty = 2)  # Wizualizacja wykrytej sezonowoœci
lines(fitted(TimeSerialM_TS), col = "green", lty = 2) # Wizualizacja wykrytych: trendu, stacjonarnoœci
lines(fitted(TimeSerialM_TSB), col = "red", lty = 2) # Wizualizacja wykrytych: jw., wariancji
plot(TimeSerialM, main="Rozk³ad temperatur miesiêcznych w Polsce",
     xlab = "Rok", ylab="Odchy³ka temperatury [C]")
lines(fitted(TimeSerialM_WS), col = "darkblue", lty = 2) # Wizualizacja wykrytych: jw, trend kwadratowy
lines(fitted(TimeSerialM_WSB), col = "red", lty = 2) # Wizualizacja wykrytych: jw, trend kwadratowy
# Transformacja Boxa_Coxa w tym przypadku bardzo nie poprawi³a wyniku
# Trend kwadratowy jest b³êdnym za³o¿eniem, lepszym modelem jest trend liniowy

# Porównanie reszt ró¿nymi metodami dekompozycji na podstawie modelu regresji
TimeSerialM_D <- decompose((TimeSerialM), type = "additive") 
tsdisplay(TimeSerialM_D$random) # Wykryte reszty z funkcji decompose()
tsdisplay(residuals(TimeSerialM_T)) # Wyeliminowany trend
tsdisplay(residuals(TimeSerialM_S)) # Wyeliminowana sezonowoœæ
tsdisplay(residuals(TimeSerialM_TS)) # Wyeliminowany trend i sezonowoœæ
tsdisplay(residuals(TimeSerialM_TSB)) # Wyeliminowany trend i sezonowoœæ, stabilizacja wariancji
tsdisplay(residuals(TimeSerialM_WSB)) # Wyeliminowany trend wielomianowy, sezonowoœæ, stabilizacja wariancji
# Trend kwadratowy jest b³êdnym za³o¿eniem, lepszym modelem jest trend liniowy



### Eliminacja trendu/sezonowoœci z wykorzystaniem funkcji decompose()
TimeSerialM_Dec <- decompose(TimeSerialM, type = "additive")
TimeSerialMRandom <- TimeSerialM_Dec$random
plot(TimeSerialM,main="Wykres temperatur w Polsce",
     xlab = "Rok", ylab="Odchy³ka temperatury [C]")
lines(TimeSerialMRandom,col="green",lty=2)
lines(TimeSerialM_Dec$seasonal,col="darkblue",lty=2)
lines(TimeSerialM_Dec$trend,col="red",lty=2)



### Uczynienie szeregu stacjonarnym - usuniêcie trendu, sezonowoœci: ró¿nicowanie, *transformacja Boxa-Coxa
### Sprawdzenie, czy s¹ realizacj¹ szumu bia³ego, którego rzêdu modele AR(p), MA(q) warto braæ pod uwagê
ndiffs(TimeSerialM) # Iloœæ potrzebnych ró¿nicowañ z opóŸnieniem wartoœci jeden
nsdiffs(TimeSerialM) # Iloœæ potrzebnych ró¿nicowañ z opóŸnieniem sezonowym (wartoœci: dwanaœcie)

hist(TimeSerialM,main="Histogram reszt") 
tsdisplay(TimeSerialM,main="Rozk³ad temperatur miesiêcznych w Polsce",
          xlab = "Rok", ylab="Odchy³ka temperatury [C]")
# Poni¿ej wartoœæ okreœlaj¹ca maksymaln¹ odchy³kê od przedzia³u ufnoœci dla szeregu stacjonarnego
(Val <- 1.96/sqrt(length(TimeSerialM))) 
# Dobrze pasowa³by tutaj prawdopodobnie model ARMA(1,1).

TimeSerialMS <- diff(x=TimeSerialM, lag=12) # Usuniêcie sezonowoœci rocznej
hist(TimeSerialMS,main="Histogram reszt") 
# Rozk³ad wartoœci jest zbli¿ony do normalnego.
tsdisplay(TimeSerialMS, main="Rozk³ad temperatur w Polsce bez trendu, sezonowoœci",
          xlab = "Rok", ylab="Odchy³ka temperatury [C]",lag.max=60)
(Val <- 1.96/sqrt(length(TimeSerialMS))) 
ndiffs(TimeSerialMS) # Iloœæ potrzebnych ró¿nicowañ z opóŸnieniem wartoœci jeden
nsdiffs(TimeSerialMS) # Iloœæ potrzebnych ró¿nicowañ z opóŸnieniem sezonowym wartoœci: dwanaœcie
# Wartoœci: ACF(12,59), PACF(12,24,36,47,49) wykraczaj¹ poza przedzia³ ufnoœci
# Warto wzi¹æ pod uwagê modele  MA(q) oraz AR(p) o wspó³czynnikach poni¿ej:
q <- c(12,59)
p <- c(12,24,36,47,49)

# Sprawdzenie stacjonarnoœci szeregu - pomocnicza linia pokazuj¹ca, czy wartoœæ skrajnych odchy³ek nie jest zbyt du¿a
MaxDifference <- 1.96/sqrt(length(TimeSerialMS))
AcfLimit <- qnorm((1+0.95)/2)/sqrt(sum(!is.na(TimeSerialMS)))
StatBD <- MaxDifference+AcfLimit
Acf(TimeSerialMS,lag.max=60)
Pacf(TimeSerialMS,lag.max=60)
curve(StatBD + 0*x, add = TRUE, col="red")
curve(-StatBD + 0*x, add = TRUE, col="red")
# Nie jest to wiêc szereg stacjonarny.

# Usuniêcie œredniej
TimeSerialMS2<-TimeSerialMS-mean(TimeSerialMS)


### Wyznaczenie wspó³czynników modelu autoregresji i porównanie dopasowania ró¿nymi metodami estymacji
TimeSerialMS.ar12.yule <- ar(TimeSerialMS2, aic = FALSE, order.max =12, method = c("yule-walker")) # Utworzenie modelu 
# Mo¿na ewentualnie zobaczyæ strukturê tego modelu: str(TimeSerialMS.ar12)
print(TimeSerialMS.ar12.yule) # Wyznaczenie wspó³czynników modelu

TimeSerialMS.ar12.burg <- ar(TimeSerialMS2, aic = FALSE, order.max =12, method = c("burg")) # Utworzenie modelu 
print(TimeSerialMS.ar12.burg) # Wyznaczenie wspó³czynników modelu

TimeSerialMS.ar12.ols <- ar(TimeSerialMS2, aic = FALSE, order.max =12, method = c("ols")) # Utworzenie modelu 
print(TimeSerialMS.ar12.ols) # Wyznaczenie wspó³czynników modelu

TimeSerialMS.ar12.mle <- ar(TimeSerialMS2, aic = FALSE, order.max =12, method = c("mle")) # Utworzenie modelu 
print(TimeSerialMS.ar12.mle) # Wyznaczenie wspó³czynników modelu

TimeSerialMS.ar12.yw <- ar(TimeSerialMS2, aic = FALSE, order.max =12, method = c("yw")) # Utworzenie modelu 
print(TimeSerialMS.ar12.yw) # Wyznaczenie wspó³czynników modelu

# Automatycznie dobrana wartoœæ rzêdu
TimeSerialMS.arAIC.yw <- ar(TimeSerialMS2, aic = TRUE, order.max = 100, method = c("yule-walker"))
print(TimeSerialMS.arAIC.yw)
# Wartoœæ 49 to ostatnia istotna zaobserwowana na wykresie Pacf korelacja z propozycji



### Wyznaczenie wspó³czynników modelu ruchomej œredniej z u¿yciem funkcji Arima()
TimeSerialMS.m.ar0i0ma12 <- Arima(TimeSerialMS2, order =c(0,0,12))
summary(TimeSerialMS.m.ar0i0ma12)                          
TimeSerialMS.ar0i0ma12S <- Arima(TimeSerialMS2, order = c(0,0,12), seasonal=list(order=c(0,1,0), period = 12))
summary(TimeSerialMS.ar0i0ma12S)
# Otrzymujemy te same wspó³czynniki przy ró¿nych zapisach
# TimeSerialMS2.m.ar0i0ma59 <- Arima(TimeSerialMS2, order =c(0,0,59))
# summary(TimeSerialMS2.m.ar0i0ma59)

# Porównanie modelu wyznaczonego powy¿ej z odpowiadaj¹cym mu modelem AR
# Analiza dobroci dopasowania, oraz wartoœci b³êdów prognoz

TimeSerialMS2.m.ar12i0ma0 <- Arima(TimeSerialMS2, order = c(12,0,0))
summary(TimeSerialMS2.m.ar12i0ma0)
TimeSerialMS2.ar12i0ma0S <- Arima(TimeSerialMS2, order = c(12,0,0), seasonal=list(order=c(0,1,0), period = 12))
summary(TimeSerialMS2.ar12i0ma0S)
TimeSerialMS2.m.ar24i0ma0 <- Arima(TimeSerialMS2, order = c(24,0,0))
summary(TimeSerialMS2.m.ar24i0ma0)
# TimeSerialMS2.m.ar36i0ma0 <- Arima(TimeSerialMS2, order = c(36,0,0))
# summary(TimeSerialMS2.m.ar36i0ma0)
# TimeSerialMS2.m.ar47i0ma0 <- Arima(TimeSerialMS2, order = c(47,0,0))
# summary(TimeSerialMS2.m.ar47i0ma0)
# TimeSerialMS2.m.ar49i0ma0 <- Arima(TimeSerialMS2, order = c(49,0,0))
# summary(TimeSerialMS2.m.ar49i0ma0)



### Wyznaczenie optymalnych modeli z wykorzystaniem funkcji auto.arima() oraz wyznaczenie ich wspó³czynników
### Porównanie modeli, wybór najlepszego na podstawie kryteriów dopasowania: aic, aicc, bi (g³ównie ostatnich).
# Modele automatyczne
(TimeSerialMSaicc.auto <- auto.arima(TimeSerialMS2, ic = "aicc"))
summary(TimeSerialMSaicc.auto) 
# Wyznaczono: ARIMA(1,0,1)(2,0,2)[12] z zerow¹ œredni¹ ; AIC=1300.57   AICc=1300.97   BIC=1326.34
(TimeSerialMSaic.auto <- auto.arima(TimeSerialMS2, ic= "aic"))
summary(TimeSerialMSaic.auto) 
# Wyznaczono: ARIMA(1,0,1)(2,0,2)[12] z zerow¹ œredni¹ ; AIC=1300.57   AICc=1300.97   BIC=1326.34
(TimeSerialMbic.auto <- auto.arima(TimeSerialMS2, ic= "bic"))
summary(TimeSerialMbic.auto) 
# Wyznaczono: ARIMA(1,0,0)(2,0,2)[12] z zerow¹ œredni¹ ; AIC=1302.07   AICc=1302.37   BIC=1324.15
# Wspó³czynniki ka¿dego z trzech powy¿szych modeli s¹ bardzo zbli¿one, a pierwszych dwóch nawet identyczne. 
# W ka¿dym z przypadków wyznaczono: ARIMA(1,0,0)(2,0,2)[12] z zerow¹ œredni¹.

# Modele w³asne
summary(TimeSerialMS2.m.ar12i0ma0) 
# Wyznaczono: ARIMA(12,0,0) z niezerow¹ œredni¹ ; AIC=1405.19   AICc=1406.71   BIC=1456.72
summary(TimeSerialMS2.ar12i0ma0S) 
# Wyznaczono: ARIMA(12,0,0)(0,1,0)[12] ; AIC=1572.02   AICc=1573.38   BIC=1619.32
summary(TimeSerialMS2.m.ar24i0ma0)
# Wyznaczono: ARIMA(24,0,0) z niezerow¹ œredni¹ ; AIC=1392.32   AICc=1397.6   BIC=1488.01

# Najlepszym modelem na podstawie wartoœci kryteriów najbli¿szych zeru jest model: ARIMA(1,0,1)(2,0,2)[12].



### Sprawdzenie, czy otrzymane modele mo¿na uznaæ za szum bia³y
tsdisplay(TimeSerialMSaicc.auto$residuals)
# Sprawdzenie stacjonarnoœci szeregu z pomocnicz¹ lini¹ pokazuj¹ca, czy wartoœæ skrajnych odchy³ek nie jest zbyt du¿a
MaxDifference <- 1.96/sqrt(length(TimeSerialMSaicc.auto))
AcfLimit <- qnorm((1+0.95)/2)/sqrt(sum(!is.na(TimeSerialMSaicc.auto)))
StatBD <- MaxDifference+AcfLimit
Acf(TimeSerialMSaicc.auto$residuals,lag.max=1000)
curve(StatBD + 0*x, add = TRUE, col="red")
curve(-StatBD + 0*x, add = TRUE, col="red")
# Jest to wiêc szereg stacjonarny, poniewa¿ ¿adna z wartoœci nie wystaje znacz¹co poza przedzia³ ufnoœci.
hist(TimeSerialMSaicc.auto$residuals) # Histogram jest wizualizacj¹ rozk³adu normalnego.
# Sprawdzenie za³o¿enia o normalnoœci rozk³adu reszt
shapiro.test(TimeSerialMSaicc.auto$residuals) 
# tsdiag(TimeSerialMSaicc.auto, gof.lag=50)
# Po wartoœciach p mo¿na stwierdziæ, ¿e próby pochodz¹ z populacji o rozk³adzie normalnym (poniewa¿ p.value < 0,05).



### Prognozowanie z wykorzystaniem metod naiwnych
# Prognoza oparta na œredniej
TimeSerialMSaiccmeanf.auto <- meanf(TimeSerialM, h = 60) 
plot(TimeSerialMSaiccmeanf.auto) 

# Prognoza przy wyeliminowaniu sezonowoœci
TimeSerialMShorter <- window(TimeSerialM,start=c(2018,1),end=c(2020,1))
TimeSerialM.meanf <- meanf(diff(TimeSerialMShorter,1),12, h = 30) 
plot(TimeSerialM.meanf) 

# Prognozowanie naiwne 
TimeSerialM.naive <-naive(TimeSerialMShorter, h=24)
plot(TimeSerialM.naive)
TimeSerialMShorter.snaive <-snaive(TimeSerialMShorter, h=24)
plot(TimeSerialMShorter.snaive)

# Prognozowanie naiwne z dryfem
TimeSerialMShorter.rwf <- rwf(TimeSerialMShorter, h=24, drift = TRUE)
plot(TimeSerialMShorter.rwf)

# Najlepsz¹ metod¹ jest zastosowanie tutaj u¿ycie funkcji snaive() z powodu du¿ej sezonowoœci szeregu.