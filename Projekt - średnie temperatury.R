
### Zainstalowanie, oraz wczytanie bibliotek odpowiadaj�cych za:
#   import plik�w xls/csv, operacje na �a�cuchac tekstowych i szeregach czasowych
#   (W tym usuni�cia brakuj�cych warto�ci), wykresy

if (!("readxl" %in% rownames(installed.packages()))) install.packages("readxl")
library("readxl")  # Do wczytania plik�w typu csv
if (!('dplyr' %in% rownames(installed.packages()))) install.packages('dplyr')
library('dplyr') # Dla fitrowania/wybierania danych z ramki
if (!('timeSeries' %in% rownames(installed.packages()))) install.packages('timeSeries')
library('timeSeries')  # Do usuni�cia brakuj�cych warto�ci w szeregu
if (!('forecast' %in% rownames(installed.packages()))) install.packages('forecast')
library('forecast')  # Do wykres�w (Acf, Pacf)
if (!('stats' %in% rownames(installed.packages()))) install.packages('stats')
library('stats')  # Do wykres�w (np. monthplot)
if (!('lattice' %in% rownames(installed.packages()))) install.packages('lattice')
library('lattice')  # Do wykres�w (np. xyplot)
# Ewentualnie tak�e
# if (!('quantmod' %in% rownames(installed.packages()))) install.packages('quantmod')
# library('quantmod')
# if (!("tidyr" %in% rownames(installed.packages()))) install.packages("tidyr")
# library("tidyr")



### Wczytanie pliku z danymi
getwd() # Sprawdzenie aktualnego katalogu roboczego 
# setwd("TW�J_KATALOG_ROBOCZY_Z_PROJEKTEM") # <- Wybranie domy�lnego katalogu roboczego z projektem
getwd() # Sprawdzenie czy katalog roboczy zosta� zmieniony 
my_data <- read.csv("city_temperature.csv") # Wczytanie tabeli z danymi z pliku
# my_data <- read.csv(file.choose()) # Ewentualny wyb�r pliku z danymi do wczytania

# Wy�wietlenie informacji o tabeli z danymi
# str(my_data)
# summary(my_data)
# head(my_data)



### Wyb�r regionu i pa�stwa dla analizy jego danych (np. "Europe", "Poland")
sort(unique(my_data$Region))
ChosenRegion <- "Europe"
ChosenRows <- which(my_data[,1] == ChosenRegion,TRUE)
sort(unique(my_data[ChosenRows,]$Country))
ChosenCountry <- "Poland"



### "Wyci�gni�cie" odpowiednich danych dla wybranego kraju
ExaminedData <- select(my_data, Country, Year,Month,Day, AvgTemperature)
dim(ExaminedData) # Wymiary ca�ego zestawu danych
# Przypisanie danych wy��cznie danego kraju
NData <- ExaminedData[
  sapply(ExaminedData$Country, 
         function (x) (
           if (x ==  ChosenCountry 
               | is.null(x) | is.na(x)) TRUE else FALSE)), 1:dim(ExaminedData)[2]]
# Zast�pienie warto�ci "-99" w przypadku braku danych pomiarowych warto�ci� "NaN"
Temp <- NData[1:dim(NData)[1],5]
Temp[Temp==-99] <- "NaN"
# Konwersja skali temperatury z Fahrenheita [F] do Celsjuszy [C]
Temp <- round((as.double(Temp)-32)/1.8, digits = 1)
# Przypisanie poprawnych warto�ci temperatury do ramki danych
NData[1:dim(NData)[1],5] <- Temp



### Utworz� �rednie dla miesi�cy w danych latach dla wi�kszej ilo�ci wykres�w, ich przejrzysto�ci
#   Mo�na zrobi� podobnie dla lat w danym okresie, powsta�aby jednak ma�a, niereprezentatywna pr�ba 
MonthTemp <- c()
for (i in unique(NData$Year)){
  for (j in unique(NData$Month)){
    Value = mean(NData[((NData[1:dim(NData)[1],2]==i) & (NData[1:dim(NData)[1],3]==j)),5], na.rm=TRUE)
    MonthTemp <- c(MonthTemp, Value)
  }
}

# Wy�wietlenie informacji o temperaturach z danego kraju
summary(Temp) # Dla �rednich dziennych
summary(MonthTemp) # Dla �rednich miesi�cznych



### Utworzenie szereg�w czasowych ze �rednimi dziennymi, miesi�cznymi w danych latach
(TimeSerialD <- ts(Temp, start = c(NData$Year[1],NData$Month[1],NData$Day[1]), frequency = 365,
                   end = c(tail(NData$Year, n=1),tail(NData$Month, n=1),tail(NData$Day, n=1))))
(TimeSerialM <- ts(MonthTemp, start = c(NData$Year[1],NData$Month[1]), frequency = 12,
                   end = c(tail(NData$Year, n=1),tail(NData$Month, n=1))))



### Ewentualne korekty kalendarzowe dla szeregu miesi�cznego
#   W naszym przypadku akurat s� one zb�dne, poniewa� przy pomiarze temperatury obliczamy �rednie :)

# Uwzgl�dnienie r�nicy dni lutego w latach przest�pnych
(SrDniWRoku <- 365.25/12) 
(DniWMies <- monthdays(TimeSerialM)) 
TimeSerialM2 <- TimeSerialM/DniWMies*SrDniWRoku 

# Wykresy pokazuj�ce r�nice po uwzgl�dnieniu istnienia lat przest�pnych
ts.plot(TimeSerialM, TimeSerialM2,  col = c("red", "blue"), 
        xlab = "Rok", ylab="Temperatura [C]",
        main = "�rednie temperatury miesi�czne w Polsce")

# Ewentualne uwzgl�dnienie dni roboczych w miesi�cach (Nie dla naszego szeregu akurat :) )
(RoboczeWMies <- bizdays(TimeSerialM)) 
(sredniaRoboczych <- sum(RoboczeWMies)/length(RoboczeWMies)) 
TimeSerialM3 <- TimeSerialM/RoboczeWMies*sredniaRoboczych 

# Wykresy pokazuj�ce r�nice po uwzgl�dnieniu r�nicy liczby dni roboczych w miesi�cach
ts.plot(TimeSerialM, TimeSerialM3,  col = c("red", "blue"), 
        xlab = "Rok", ylab="Temperatura [C]",
        main = "�rednie temperatury miesi�czne w Polsce")



### Utworzenie szeregu czasowego dla kwarta��w w poszczeg�lnych latach poprzez agregacj� danych miesi�cznych
TimeSerialQ <- aggregate(TimeSerialM, nfrequency = 4, FUN = mean)
# Por�wnanie wykres�w dla wi�kszej i mniejszej cz�stotliwo�ci 
par(mfrow = c(3,1)) 
ts.plot(TimeSerialM,xlab = "Rok", ylab="Temperatura [C]", col = c("blue"),main="�rednie temperatury miesi�czne w Polsce") 
ts.plot(TimeSerialQ,xlab = "Rok", ylab = "Temperatura [C]",col = c("green"),main="�rednie temperatury kwartalne w Polsce") 
ts.plot(TimeSerialD,xlab = "Rok", ylab = "Temperatura [C]",col = c("red"),main="�rednie temperatury dzienne w Polsce") 


### Om�wienie g��wnych cech analizowanego szeregu na podstawie r�nych typ�w wykres�w, wnioski
#   Wykresy z u�yciem xyplot()
xyplot(TimeSerialD, 
       main="�rednie temperatury dzienne w Polsce",
       xlab = "Rok",ylab="Temperatura [C]",
       aspect = 1/3) 
xyplot(TimeSerialM, 
       main="�rednie temperatury miesi�czne w Polsce",
       xlab = "Rok",ylab="Temperatura [C]",
       aspect = 1/3) 
xyplot(TimeSerialQ, 
       main="�rednie temperatury kwartalne w Polsce",
       xlab = "Rok",ylab="Temperatura [C]",
       aspect = 1/3) 

# Wykresy panelowe
xyplot(TimeSerialD, 
       main="�rednie temperatury dzienne w Polsce",
       xlab = "Rok",ylab="Temperatura [C]",
       aspect = 1/3,
       cut = list(number = 3, overlap = 0.1)) # overlap - jaka cz�� danych powt�rzona w kolejnym panelu
xyplot(TimeSerialM, 
       main="�rednie temperatury miesi�czne w Polsce",
       xlab = "Rok",ylab="Temperatura [C]",
       aspect = 1/3,
       cut = list(number = 3, overlap = 0.1)) # overlap - jaka cz�� danych powt�rzona w kolejnym panelu
xyplot(TimeSerialQ, 
       main="�rednie temperatury kwartalne w Polsce",
       xlab = "Rok",ylab="Temperatura [C]",
       aspect = 1/3,
       cut = list(number = 3, overlap = 0.1)) # overlap - jaka cz�� danych powt�rzona w kolejnym panelu

# Wykresy sezonowe (monthplot,seasonplot)
monthplot(TimeSerialM,
          main="�rednie temperatury miesi�czne w Polsce",
          xlab = "Miesi�c",ylab="Temperatura [C]",
          lty=7)

par(mfrow = c(3,1)) 
seasonplot(TimeSerialD,
           main="�rednie temperatury dzienne w Polsce",
           xlab = "Dzie�",ylab="Temperatura [C]", year.labels = TRUE,col = rainbow(25))
seasonplot(TimeSerialM,
           main="�rednie temperatury miesi�czne w Polsce",
           xlab = "Miesi�c",ylab="Temperatura [C]", year.labels = TRUE, col = rainbow(25))
seasonplot(TimeSerialQ,
           main="�rednie temperatury kwartalne w Polsce",
           xlab = "Kwarta�",ylab="Temperatura [C]", col = rainbow(25))

# Wykresy pude�kowe (Na wykresach: kwartyle 1,3 rz�du, mediany, warto�ci minimalne/maksymalne, odstaj�ce)
par(mfrow = c(3,1))
boxplot(TimeSerialD ~ cycle(TimeSerialD),
        main="Rozk�ad temperatur dziennych w Polsce",
        xlab="Dzie�",ylab="Temperatura [C]") 
boxplot(TimeSerialM ~ cycle(TimeSerialM),
        main="Rozk�ad temperatur miesi�cznych w Polsce",
        xlab="Miesi�c",ylab="Temperatura [C]")   
boxplot(TimeSerialQ ~ cycle(TimeSerialQ),
        main="Rozk�ad temperatur kwartalnych w Polsce",
        xlab="Kwarta�",ylab="Temperatura [C]")  

# Wykres rozrzutu dla warto�ci op�nionych
lag.plot(TimeSerialM) # Wida� raczej brak trendu miesi�cznego
lag.plot(TimeSerialQ) # Wida� dzia�anie p�r roku (Du�e r�nice warto�ci temperatur co p� roku)
lag.plot(TimeSerialM, lags=12, do.lines = FALSE) 
# Najsilniejsza korelacja (zale�no��) dla "laga" dwunastego, co sugeruje sezonowo�� roczn� (12-miesi�czn�)
# Najs�absza korelacja dla "laga" sz�stego, co pokazuje dzia�anie p�r roku
# Kszta�t "laga" trzeciego, dziewi�tego pokazuje p�ynne przej�cia  p�r roku
lag.plot(TimeSerialQ, lags=4, do.lines = FALSE) 
# Najsilniejsza korelacja (zale�no��) dla "laga" czwartego, co sugeruje sezonowo�� roczn� (4-kwartaln�)
# Najs�absza korelacja dla "laga" drugiego, co pokazuje dzia�anie p�r roku
# Kszta�t "laga" pierwszego, trzeciego mo�e pokazywa� przej�cia p�r roku


# Wykresy: funkcji autokorelacji - Acf, cz�stkowej funkcji autokorelacji - Pacf

# acf(TimeSerialM, lag.max = 30, xlab = "Odst�p sezonowy", ylab="Korelacja danych",
#    main = "Korelogram dla �rednich temperatur miesi�cznych w Polsce")
par(mfrow = c(2,1))
Acf(TimeSerialM, lag.max = 30, xlab = "Odst�p miesi�czny", ylab="Korelacja danych",
    main = "Korelogram dla �rednich temperatur miesi�cznych w Polsce")
# Wida� wyra�n� sezonowo�� poprzez �agodne zmiany na wykresie dodanie i ujemne

# pacf(TimeSerialM, lag.max = 30, xlab = "Odst�p sezonowy", ylab="Bezpo�rednia korelacja danych",
#      main = "Korelogram dla �rednich temperatur miesi�cznych w Polsce")
Pacf(TimeSerialM, lag.max = 30, xlab = "Odst�p miesi�czny", ylab="Bezpo�rednia korelacja danych",
     main = "Korelogram dla �rednich temperatur miesi�cznych w Polsce")
# Wida� do�� mocny trend tak�e dzi�ki bliskiej warto�ci - jeden warto�ci dla "laga" pierwszego 

par(mfrow = c(2,1))
Acf(TimeSerialQ, lag.max = 15) # Wida� sezonowo��, jednak dla bardzo kr�tkich odst�p�W (2 kwarta�y)
Pacf(TimeSerialQ, lag.max = 15) # Widoczny trend co 2-kwartalny



### Zastosowanie �rednich ruchomych (lewostronnych) do wyg�adzania wykresu
ma14 <- filter(TimeSerialD, sides = 1, filter = rep(1/14,14)) # Dla 20-stu dni 
ma100 <- filter(TimeSerialD, sides = 1, filter = rep(1/100,100)) # Dla 100 dni
ma2M <- filter(TimeSerialM, sides = 1, filter = rep(1/2,2)) # Dla 20-stu dni 
ma6M <- filter(TimeSerialM, sides = 1, filter = rep(1/6,6)) # Dla 100 dni
# Por�wnanie wykres�w
par(mfrow = c(2,1))
plot(TimeSerialD, main = "�rednie temperatury dzienne w Polsce",
     xlab = "Rok", ylab="Odchy�ka temperatury [C]")
lines(ma14,col="red", lty = 8)
lines(ma100,col="green", lty = 8)
plot(TimeSerialM, main = "�rednie temperatury miesi�czne w Polsce",
     xlab = "Rok", ylab="Odchy�ka temperatury [C]")
lines(ma2M,col="red", lty = 8)
lines(ma6M,col="green", lty = 8) # Tuaj wida� zbyt du�e zaokr�glenie ju�



### Dekompozycje na podstawie ruchomej �redniej 
TimeSerialM2 <- decompose(TimeSerialM, type="multiplicative")
TimeSerialM22 <- decompose(TimeSerialM, type="additive")
TimeSerialD3 <- na.interp(TimeSerialD) # Interpolacja obserwacji brakuj�cych 
TimeSerialD2 <- decompose(TimeSerialD3, type="multiplicative")
TimeSerialD22 <- decompose(TimeSerialD3, type="additive")

# Por�wnanie wynik�w oboma metodami
plot(TimeSerialM2, col = c("blue"), xlab = "Rok") # Wida� �le dobran� sezonowo��
plot(TimeSerialM22, col = c("blue"), xlab = "Rok") # Lepiej dobrana sezonowo��, nadal brak stacjonarno�ci
# W przypadku wi�kszej ilo�ci obserwacji powy�sze obserwacje wida� wyra�niej
# Lepsz� opcj� w tym przypadku jest wybranie metody addytywnej
plot(TimeSerialD2, col = c("blue"), xlab = "Rok")
plot(TimeSerialD22, col = c("blue"), xlab = "Rok")

# Por�wnanie reszt oboma metodami
tsdisplay(TimeSerialM2$random,main="Reszty otrzymane metod� multiplikatywn�")
tsdisplay(TimeSerialM22$random,main="Reszty otrzymane metod� addytywn�")
tsdisplay(TimeSerialD2$random)
tsdisplay(TimeSerialD22$random)



### Dekompozycje na podstawie modelu regresji: trend liniowy/wielomianowy, sezonowo��, transformacja Boxa-Coxa 
TimeSerialM_T <- tslm(TimeSerialM ~ trend) 
TimeSerialM_S <- tslm(TimeSerialM ~ season) 
TimeSerialM_TS <- tslm(TimeSerialM ~ trend + season)
TimeSerialM_TSB <- tslm((TimeSerialM) ~ trend + season, lambda = 0.5) # Pierwiastkowa, ew. "auto"
# Trend wielomianowy
TimeSerialM_WS<- tslm(TimeSerialM ~ poly(trend,raw=TRUE,degree=2)+season)
TimeSerialM_WSB<- tslm(TimeSerialM ~ poly(trend,raw=TRUE,degree=2)+season, lambd=0.5)
summary(TimeSerialM_TSB) # Informacje o przyk�adowym z wybranych modeli

# Por�wnamy metody
par(mfrow = c(2,1))
plot(TimeSerialM, main="Rozk�ad temperatur miesi�cznych w Polsce",
     xlab = "Rok", ylab="Odchy�ka temperatury [C]")
lines(fitted(TimeSerialM_T), col = "orange", lty = 2) # Wizualizacja wykrytego trendu
lines(fitted(TimeSerialM_S), col = "darkblue", lty = 2)  # Wizualizacja wykrytej sezonowo�ci
lines(fitted(TimeSerialM_TS), col = "green", lty = 2) # Wizualizacja wykrytych: trendu, stacjonarno�ci
lines(fitted(TimeSerialM_TSB), col = "red", lty = 2) # Wizualizacja wykrytych: jw., wariancji
plot(TimeSerialM, main="Rozk�ad temperatur miesi�cznych w Polsce",
     xlab = "Rok", ylab="Odchy�ka temperatury [C]")
lines(fitted(TimeSerialM_WS), col = "darkblue", lty = 2) # Wizualizacja wykrytych: jw, trend kwadratowy
lines(fitted(TimeSerialM_WSB), col = "red", lty = 2) # Wizualizacja wykrytych: jw, trend kwadratowy
# Transformacja Boxa_Coxa w tym przypadku bardzo nie poprawi�a wyniku
# Trend kwadratowy jest b��dnym za�o�eniem, lepszym modelem jest trend liniowy

# Por�wnanie reszt r�nymi metodami dekompozycji na podstawie modelu regresji
TimeSerialM_D <- decompose((TimeSerialM), type = "additive") 
tsdisplay(TimeSerialM_D$random) # Wykryte reszty z funkcji decompose()
tsdisplay(residuals(TimeSerialM_T)) # Wyeliminowany trend
tsdisplay(residuals(TimeSerialM_S)) # Wyeliminowana sezonowo��
tsdisplay(residuals(TimeSerialM_TS)) # Wyeliminowany trend i sezonowo��
tsdisplay(residuals(TimeSerialM_TSB)) # Wyeliminowany trend i sezonowo��, stabilizacja wariancji
tsdisplay(residuals(TimeSerialM_WSB)) # Wyeliminowany trend wielomianowy, sezonowo��, stabilizacja wariancji
# Trend kwadratowy jest b��dnym za�o�eniem, lepszym modelem jest trend liniowy



### Eliminacja trendu/sezonowo�ci z wykorzystaniem funkcji decompose()
TimeSerialM_Dec <- decompose(TimeSerialM, type = "additive")
TimeSerialMRandom <- TimeSerialM_Dec$random
plot(TimeSerialM,main="Wykres temperatur w Polsce",
     xlab = "Rok", ylab="Odchy�ka temperatury [C]")
lines(TimeSerialMRandom,col="green",lty=2)
lines(TimeSerialM_Dec$seasonal,col="darkblue",lty=2)
lines(TimeSerialM_Dec$trend,col="red",lty=2)



### Uczynienie szeregu stacjonarnym - usuni�cie trendu, sezonowo�ci: r�nicowanie, *transformacja Boxa-Coxa
### Sprawdzenie, czy s� realizacj� szumu bia�ego, kt�rego rz�du modele AR(p), MA(q) warto bra� pod uwag�
ndiffs(TimeSerialM) # Ilo�� potrzebnych r�nicowa� z op�nieniem warto�ci jeden
nsdiffs(TimeSerialM) # Ilo�� potrzebnych r�nicowa� z op�nieniem sezonowym (warto�ci: dwana�cie)

hist(TimeSerialM,main="Histogram reszt") 
tsdisplay(TimeSerialM,main="Rozk�ad temperatur miesi�cznych w Polsce",
          xlab = "Rok", ylab="Odchy�ka temperatury [C]")
# Poni�ej warto�� okre�laj�ca maksymaln� odchy�k� od przedzia�u ufno�ci dla szeregu stacjonarnego
(Val <- 1.96/sqrt(length(TimeSerialM))) 
# Dobrze pasowa�by tutaj prawdopodobnie model ARMA(1,1).

TimeSerialMS <- diff(x=TimeSerialM, lag=12) # Usuni�cie sezonowo�ci rocznej
hist(TimeSerialMS,main="Histogram reszt") 
# Rozk�ad warto�ci jest zbli�ony do normalnego.
tsdisplay(TimeSerialMS, main="Rozk�ad temperatur w Polsce bez trendu, sezonowo�ci",
          xlab = "Rok", ylab="Odchy�ka temperatury [C]",lag.max=60)
(Val <- 1.96/sqrt(length(TimeSerialMS))) 
ndiffs(TimeSerialMS) # Ilo�� potrzebnych r�nicowa� z op�nieniem warto�ci jeden
nsdiffs(TimeSerialMS) # Ilo�� potrzebnych r�nicowa� z op�nieniem sezonowym warto�ci: dwana�cie
# Warto�ci: ACF(12,59), PACF(12,24,36,47,49) wykraczaj� poza przedzia� ufno�ci
# Warto wzi�� pod uwag� modele  MA(q) oraz AR(p) o wsp�czynnikach poni�ej:
q <- c(12,59)
p <- c(12,24,36,47,49)

# Sprawdzenie stacjonarno�ci szeregu - pomocnicza linia pokazuj�ca, czy warto�� skrajnych odchy�ek nie jest zbyt du�a
MaxDifference <- 1.96/sqrt(length(TimeSerialMS))
AcfLimit <- qnorm((1+0.95)/2)/sqrt(sum(!is.na(TimeSerialMS)))
StatBD <- MaxDifference+AcfLimit
Acf(TimeSerialMS,lag.max=60)
Pacf(TimeSerialMS,lag.max=60)
curve(StatBD + 0*x, add = TRUE, col="red")
curve(-StatBD + 0*x, add = TRUE, col="red")
# Nie jest to wi�c szereg stacjonarny.

# Usuni�cie �redniej
TimeSerialMS2<-TimeSerialMS-mean(TimeSerialMS)


### Wyznaczenie wsp�czynnik�w modelu autoregresji i por�wnanie dopasowania r�nymi metodami estymacji
TimeSerialMS.ar12.yule <- ar(TimeSerialMS2, aic = FALSE, order.max =12, method = c("yule-walker")) # Utworzenie modelu 
# Mo�na ewentualnie zobaczy� struktur� tego modelu: str(TimeSerialMS.ar12)
print(TimeSerialMS.ar12.yule) # Wyznaczenie wsp�czynnik�w modelu

TimeSerialMS.ar12.burg <- ar(TimeSerialMS2, aic = FALSE, order.max =12, method = c("burg")) # Utworzenie modelu 
print(TimeSerialMS.ar12.burg) # Wyznaczenie wsp�czynnik�w modelu

TimeSerialMS.ar12.ols <- ar(TimeSerialMS2, aic = FALSE, order.max =12, method = c("ols")) # Utworzenie modelu 
print(TimeSerialMS.ar12.ols) # Wyznaczenie wsp�czynnik�w modelu

TimeSerialMS.ar12.mle <- ar(TimeSerialMS2, aic = FALSE, order.max =12, method = c("mle")) # Utworzenie modelu 
print(TimeSerialMS.ar12.mle) # Wyznaczenie wsp�czynnik�w modelu

TimeSerialMS.ar12.yw <- ar(TimeSerialMS2, aic = FALSE, order.max =12, method = c("yw")) # Utworzenie modelu 
print(TimeSerialMS.ar12.yw) # Wyznaczenie wsp�czynnik�w modelu

# Automatycznie dobrana warto�� rz�du
TimeSerialMS.arAIC.yw <- ar(TimeSerialMS2, aic = TRUE, order.max = 100, method = c("yule-walker"))
print(TimeSerialMS.arAIC.yw)
# Warto�� 49 to ostatnia istotna zaobserwowana na wykresie Pacf korelacja z propozycji



### Wyznaczenie wsp�czynnik�w modelu ruchomej �redniej z u�yciem funkcji Arima()
TimeSerialMS.m.ar0i0ma12 <- Arima(TimeSerialMS2, order =c(0,0,12))
summary(TimeSerialMS.m.ar0i0ma12)                          
TimeSerialMS.ar0i0ma12S <- Arima(TimeSerialMS2, order = c(0,0,12), seasonal=list(order=c(0,1,0), period = 12))
summary(TimeSerialMS.ar0i0ma12S)
# Otrzymujemy te same wsp�czynniki przy r�nych zapisach
# TimeSerialMS2.m.ar0i0ma59 <- Arima(TimeSerialMS2, order =c(0,0,59))
# summary(TimeSerialMS2.m.ar0i0ma59)

# Por�wnanie modelu wyznaczonego powy�ej z odpowiadaj�cym mu modelem AR
# Analiza dobroci dopasowania, oraz warto�ci b��d�w prognoz

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



### Wyznaczenie optymalnych modeli z wykorzystaniem funkcji auto.arima() oraz wyznaczenie ich wsp�czynnik�w
### Por�wnanie modeli, wyb�r najlepszego na podstawie kryteri�w dopasowania: aic, aicc, bi (g��wnie ostatnich).
# Modele automatyczne
(TimeSerialMSaicc.auto <- auto.arima(TimeSerialMS2, ic = "aicc"))
summary(TimeSerialMSaicc.auto) 
# Wyznaczono: ARIMA(1,0,1)(2,0,2)[12] z zerow� �redni� ; AIC=1300.57   AICc=1300.97   BIC=1326.34
(TimeSerialMSaic.auto <- auto.arima(TimeSerialMS2, ic= "aic"))
summary(TimeSerialMSaic.auto) 
# Wyznaczono: ARIMA(1,0,1)(2,0,2)[12] z zerow� �redni� ; AIC=1300.57   AICc=1300.97   BIC=1326.34
(TimeSerialMbic.auto <- auto.arima(TimeSerialMS2, ic= "bic"))
summary(TimeSerialMbic.auto) 
# Wyznaczono: ARIMA(1,0,0)(2,0,2)[12] z zerow� �redni� ; AIC=1302.07   AICc=1302.37   BIC=1324.15
# Wsp�czynniki ka�dego z trzech powy�szych modeli s� bardzo zbli�one, a pierwszych dw�ch nawet identyczne. 
# W ka�dym z przypadk�w wyznaczono: ARIMA(1,0,0)(2,0,2)[12] z zerow� �redni�.

# Modele w�asne
summary(TimeSerialMS2.m.ar12i0ma0) 
# Wyznaczono: ARIMA(12,0,0) z niezerow� �redni� ; AIC=1405.19   AICc=1406.71   BIC=1456.72
summary(TimeSerialMS2.ar12i0ma0S) 
# Wyznaczono: ARIMA(12,0,0)(0,1,0)[12] ; AIC=1572.02   AICc=1573.38   BIC=1619.32
summary(TimeSerialMS2.m.ar24i0ma0)
# Wyznaczono: ARIMA(24,0,0) z niezerow� �redni� ; AIC=1392.32   AICc=1397.6   BIC=1488.01

# Najlepszym modelem na podstawie warto�ci kryteri�w najbli�szych zeru jest model: ARIMA(1,0,1)(2,0,2)[12].



### Sprawdzenie, czy otrzymane modele mo�na uzna� za szum bia�y
tsdisplay(TimeSerialMSaicc.auto$residuals)
# Sprawdzenie stacjonarno�ci szeregu z pomocnicz� lini� pokazuj�ca, czy warto�� skrajnych odchy�ek nie jest zbyt du�a
MaxDifference <- 1.96/sqrt(length(TimeSerialMSaicc.auto))
AcfLimit <- qnorm((1+0.95)/2)/sqrt(sum(!is.na(TimeSerialMSaicc.auto)))
StatBD <- MaxDifference+AcfLimit
Acf(TimeSerialMSaicc.auto$residuals,lag.max=1000)
curve(StatBD + 0*x, add = TRUE, col="red")
curve(-StatBD + 0*x, add = TRUE, col="red")
# Jest to wi�c szereg stacjonarny, poniewa� �adna z warto�ci nie wystaje znacz�co poza przedzia� ufno�ci.
hist(TimeSerialMSaicc.auto$residuals) # Histogram jest wizualizacj� rozk�adu normalnego.
# Sprawdzenie za�o�enia o normalno�ci rozk�adu reszt
shapiro.test(TimeSerialMSaicc.auto$residuals) 
# tsdiag(TimeSerialMSaicc.auto, gof.lag=50)
# Po warto�ciach p mo�na stwierdzi�, �e pr�by pochodz� z populacji o rozk�adzie normalnym (poniewa� p.value < 0,05).



### Prognozowanie z wykorzystaniem metod naiwnych
# Prognoza oparta na �redniej
TimeSerialMSaiccmeanf.auto <- meanf(TimeSerialM, h = 60) 
plot(TimeSerialMSaiccmeanf.auto) 

# Prognoza przy wyeliminowaniu sezonowo�ci
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

# Najlepsz� metod� jest zastosowanie tutaj u�ycie funkcji snaive() z powodu du�ej sezonowo�ci szeregu.