#2. Pociąg z Lublina do Warszawy przejechał połowę drogi ze średnią prędkością 120 km/h.
#Drugą połowę przejechał ze średnią prędkością 90 km/h.
#Jaka była średnia prędkość pociągu

v1 <- 120
v2 <- 90

x <- (v1+v2)/2

cat("srednia predkosc pociagu wynosi ", x, "km/h")


#3. Utwórz funkcję obliczającą współczynnik korelacji r Pearsona dla 2 wektorów o tej samej długości.

x1 <- sample.int(100, 10)
x2 <- sample.int(100, 10)

x <- c(85, 6,  100,  99,  22,  48,  73,  94,  93,  96)
y <- c(64, 11, 19, 14, 46, 25, 62, 22, 82, 17)



my_pearson <- function(x1, x2) {
  mx1 <- mean(x1)
  mx2 <- mean(x2)
  newx1 <-(x1-mx1)
  newx2 <- (x2-mx2)
  r = ( c(t(newx1) %*% newx2)/(sqrt(sum(newx1^2)) * sqrt(sum(newx2^2)))  )
  return(r)
}

my_pearson(x, y)
cor(x, y,  method = "pearson")







#3a Wczytaj dane plik dane.csv i oblicz współczynnik dla wagi i wzrostu. W komentarzu napisz co oznacza wynik.


dane <- as.data.frame(read.csv2("dane.csv"))

cor(dane$wzrost, dane$waga, method = "pearson")
f = my_pearson(c(dane$wzrost), c(dane$waga))
f = my_pearson(dane$wzrost, dane$waga)


cat("współczynnik korelacji pearsona wynosi: ", round(f,2), ", co oznacza, że waga jest silnie skorelowana ze wzrostem")

#4. Napisz funkcję zwracającą ramke danych z danych podanych przez użytkownika
#stworzDataFrame <- function(ile=1)
#W pierwszym wierszu użytkownik podaje nazwy kolumn. w kolejnych wierszach zawartość wierszy ramki danych 
#( tyle wierszy ile podaliśmy w argumencie ile. ile=1 oznacza, że gdy użytkownik nie poda żadnej wartości jako parametr, 
#domyślna wartością będzie 1)



stworzDataFrame <- function(ile=1){
  
 nazwy_kolumn <- c(unlist(strsplit(readline(prompt = paste0("Podaj nazwy kolumn oddzielone przecinkiem: ")),",")))
 df = data.frame(matrix(nrow = 0, ncol = length(nazwy_kolumn)))
 colnames(df) = nazwy_kolumn

 for(i in 1:ile){
    wiersz <- c(unlist(strsplit(readline(prompt = paste0("podaj zawartosc wiersza ", i," (wartości oddziel przecinkiem): ")),",")))
    df[nrow(df) + 1,] <- wiersz}

 return(df)
}

myDf <- stworzDataFrame(2) #w nawiasie podaj liczbę wierszy
print(myDf)

#5 Napisz funkcję , która pobiera sciezkeKatalogu, nazweKolumny, jakaFunkcje, DlaIluPlikow i liczy: 
#mean, median,min,max w zależności od podanej nazwy funkcji w argumencie, z katologu który podaliśmy i z tylu plików ilu podaliśmy dla wybranej nazwy kolumny. 
# UWAGA: w podanych plikach R pobierając komórki nazwane liczbami R wstawi przed nazwy X. Funkcję przetestuj dla katalogu smogKrakow.zip.  Wykonując obliczenia pomiń brakujące wartości.


 

 liczZplikow <- function(sciezka = "./smogKrakow2", Funkcja, nazwaKolumny, DlaIluPlikow = 1) {
   pliki <- c(list.files(sciezka))
   
  for (i in 1:DlaIluPlikow){
   sciezkaPliku <- paste(sciezka,"/",pliki[i],sep="")
   myDF <- data.frame(read.csv(paste(sciezkaPliku)))
   
   if (Funkcja=="mean"){
     out <- mean(na.omit(myDF[[nazwaKolumny]]))
   }
   if (Funkcja=="median"){
     out <- median(na.omit(myDF[[nazwaKolumny]]))
   }
   if (Funkcja=="max"){
     out <- max(na.omit(myDF[[nazwaKolumny]]))
   }
   if (Funkcja=="min"){
     out <- min(na.omit(myDF[[nazwaKolumny]]))
   }

   print(paste("Funkcja ", Funkcja, " wynosi: ", out, "dla kolumny: ", nazwaKolumny, "w pliku ", pliki[i]))
  }   
 }

 
 liczZplikow(Funkcja = "mean", DlaIluPlikow = 12, nazwaKolumny = "X140_pressure" )




