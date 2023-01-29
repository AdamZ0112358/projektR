install.packages("RSelenium")
library(RSelenium)
library(rvest)
install.packages("stringr")     
library("stringr")           
install.packages("gtools")
library(gtools)
install.packages(c("DBI","RMySQL","rstudioapi"))
library(DBI)
library(RMySQL)
library(rstudioapi)
library(RMySQL)
library(dplyr)
library(ggplot2)
library(stringr)

#usuwa obiekty (rozwiazanie problemu z zajetym portem):
rm(rd)
rm(remDr)
rd[["server"]]$stop()


url<-"https://www.otomoto.pl/osobowe/mazda/3/seg-compact--seg-coupe--seg-sedan/od-2013?search%5Bfilter_float_price%3Ato%5D=60000"
read_html(url)%>%html_node(".eoupkm71.css-190hi89.e11e36i3")

rd<- RSelenium::rsDriver(browser = "chrome",chromever = "108.0.5359.71" )
remDr<- rd[['client']]
remDr$navigate(url)



wektorLinkow<-c()

for ( i in 1:7){
  urll<- paste0("https://www.otomoto.pl/osobowe/mazda/3/seg-compact--seg-coupe--seg-sedan/od-2013?search%5Bfilter_float_price%3Ato%5D=60000&page=",i)
  remDr$navigate(urll)
  Sys.sleep(1)
  webElement<- remDr$findElement("css","body")
  webElement$sendKeysToElement(list(key="end"))
  Sys.sleep(1)
  webElement$sendKeysToElement(list(key="end"))
  Sys.sleep(1)
  pageFromSeleniumL <- remDr$getPageSource()[[1]] %>% rvest::read_html()
  linki<- (pageFromSeleniumL%>%html_elements(".ooa-9tzypk.e1p19lg715") ) %>%
    html_elements(".e1p19lg76.e1p19lg720.ooa-10p8u4x.er34gjf0")%>%html_node("a")%>%html_attr("href")
  wektorLinkow<-c(wektorLinkow,linki)
  linkiElements<- pageFromSeleniumL%>%html_nodes(".ooa-9tzypk.e1p19lg71")
  linkiElements%>%html_attr("href")
}

wektorLinkow<-unique(wektorLinkow)

w<-1
data = "29.01.2023"


zrobWiersz<- function(w,wektorLinkow,data,remDr){
  urll<- paste0(wektorLinkow[w])
  remDr$navigate(urll)
  Sys.sleep(1)
  webElement<- remDr$findElement("css","body")
  webElement$sendKeysToElement(list(key="end"))
  Sys.sleep(1)
  webElement$sendKeysToElement(list(key="end"))
  Sys.sleep(1)
  pageFromSeleniumL <- remDr$getPageSource()[[1]] %>% rvest::read_html()
  cena<-pageFromSeleniumL%>%html_element(".offer-price__number")%>%html_text()
  
 v<-pageFromSeleniumL%>%html_elements(".offer-params__label")%>%html_text()
 indexy<- seq(1,length(v))
 nazwyKolumn <- v
 w<-pageFromSeleniumL%>%html_elements(".offer-params__value")%>%html_text()%>% str_replace_all(., "\n", "") %>% str_trim(., "right") %>% str_trim(., "left")
 wartosci <-w
 
 df1<-data.frame( t(wartosci) )
 names(df1)<-nazwyKolumn
 if( !any(is.na(names(df1) )) ) {
   
   df1<- cbind(df1,data=data)
   df1<-cbind(cena=cena,df1)
 }
 df1
}


autaDF<-NULL
liczbaLinkow<-length(wektorLinkow)
data = "29.01.2023"

for( l in 1:liczbaLinkow ){
  skip<-FALSE
  tryCatch(
    temp<-zrobWiersz(l,wektorLinkow,data,remDr=remDr),
    error=function(e){
      print(l)
      skip<<-TRUE
      }
    )
  if(skip){next}
  print(names(temp))
  if ( !any(is.na(names(temp))) ){
    if( is.null(autaDF) )
      autaDF<-temp
    else{
      autaDF<-smartbind(autaDF,temp )
    }
  }
}

View(autaDF)

write.csv(auta2DF, "C:\\Users\\User\\Desktop\\PJATK\\R\\directory\\webscrapping\\webscrapping\\autaDF.csv", row.names=FALSE)




autaDF$cena <-str_remove(autaDF$cena, "PLN\n")
autaDF$cena <-gsub("[[:space:]]", "", autaDF$cena)
autaDF$cena <-as.numeric(autaDF$cena)

autaDF$Przebieg <-str_remove(autaDF$Przebieg, "km")
autaDF$Przebieg <-gsub("[[:space:]]", "", autaDF$Przebieg)
autaDF$Przebieg <-as.numeric(autaDF$Przebieg)

autaDF$`Pojemność skokowa` <-str_remove(autaDF$`Pojemność skokowa`, "cm3")
autaDF$`Pojemność skokowa` <-gsub("[[:space:]]", "", autaDF$`Pojemność skokowa`)
autaDF$`Pojemność skokowa` <-as.numeric(autaDF$`Pojemność skokowa`)


autaDF$Moc <-str_remove(autaDF$Moc, "KM")
autaDF$Moc <-gsub("[[:space:]]", "", autaDF$Moc)
autaDF$Moc <-as.numeric(autaDF$Moc)

autaDF <- autaDF[!is.na(autaDF$cena),]
autaDF <- autaDF[!is.na(autaDF$Przebieg),]




MojeAuto <- autaDF[autaDF$Bezwypadkowy == "Tak",]
MojeAuto <- MojeAuto[!is.na(MojeAuto$cena),]

MojeAuto <- MojeAuto[MojeAuto$'Typ nadwozia' == "Sedan",]
MojeAuto <- MojeAuto[!is.na(MojeAuto$cena),]

MojeAuto <- MojeAuto[MojeAuto$'Rodzaj paliwa' == "Benzyna",]
MojeAuto <- MojeAuto[!is.na(MojeAuto$cena),]

MojeAuto <- MojeAuto[MojeAuto$Przebieg <= mean(autaDF$Przebieg),]
MojeAuto <- MojeAuto[!is.na(MojeAuto$cena),]

MojeAuto <- MojeAuto[MojeAuto$'Generacja' == "III (2013-)",]
MojeAuto <- MojeAuto[!is.na(MojeAuto$cena),]

MojeAuto <- MojeAuto[order(MojeAuto$cena),]


MojeAuto <- head(MojeAuto,1)

View(MojeAuto)




View(autaDF)
con <- DBI::dbConnect(RMySQL::MySQL(),
                      encoding ="UTF-8",
                      host = "51.83.185.240",
                      user = "student",
                      dbname = "rzajecia23",
                      password ="!r23_NIEPODAWACGITpjatK_23!"#rstudioapi::askForPassword("Database password")
)

dbGetQuery(con,'SET NAMES utf8')
dbGetQuery(con,'set character set "utf8"')
install.packages("dplyr")
library(dplyr)
dbListTables(con)
zachwieja<- tbl(con,"zachwieja_miasta")
zachwieja%>%select(cena)
dbDisconnect(con)


auta2<- tbl(con,"otomoto15012023")
auta2DF <- data.frame(auta2)

View(auta2DF)

auta2DF$cena <-str_remove(auta2DF$cena, "PLN\n")
auta2DF$cena <-gsub("[[:space:]]", "", auta2DF$cena)
auta2DF$cena <-as.numeric(auta2DF$cena)

auta2DF$Przebieg <-str_remove(auta2DF$Przebieg, "km")
auta2DF$Przebieg <-gsub("[[:space:]]", "", auta2DF$Przebieg)
auta2DF$Przebieg <-as.numeric(auta2DF$Przebieg)

auta2DF$`Pojemność.skokowa` <-str_remove(auta2DF$`Pojemność.skokowa`, "cm3")
auta2DF$`Pojemność.skokowa` <-gsub("[[:space:]]", "", auta2DF$`Pojemność.skokowa`)
auta2DF$`Pojemność.skokowa` <-as.numeric(auta2DF$`Pojemność.skokowa`)


auta2DF$Moc <-str_remove(auta2DF$Moc, "KM")
auta2DF$Moc <-gsub("[[:space:]]", "", auta2DF$Moc)
auta2DF$Moc <-as.numeric(auta2DF$Moc)

auta2DF <- auta2DF[!is.na(auta2DF$cena),]
auta2DF <- auta2DF[!is.na(auta2DF$Przebieg),]


#brak mazdy, wiec sprawdze to dla opla astry, który jest bezposrednio konkurentem mazdy
MojeAuto2 <- auta2DF[auta2DF$'Marka.pojazdu' == "Opel",]
MojeAuto2 <- MojeAuto2[!is.na(auta2DF$cena),]

MojeAuto2 <- MojeAuto2[MojeAuto2$'Model.pojazdu' == "Astra",]
MojeAuto2 <- MojeAuto2[!is.na(auta2DF$cena),]


MojeAuto2 <- MojeAuto2[MojeAuto2$Bezwypadkowy == "Tak",]
MojeAuto2 <- MojeAuto2[!is.na(MojeAuto2$cena),]

MojeAuto2 <- MojeAuto2[MojeAuto2$'Typ.nadwozia' == "Sedan",]
MojeAuto2 <- MojeAuto2[!is.na(MojeAuto2$cena),]

MojeAuto2 <- MojeAuto2[MojeAuto2$'Rodzaj.paliwa' == "Benzyna",]
MojeAuto2 <- MojeAuto2[!is.na(MojeAuto2$cena),]

MojeAuto2 <- MojeAuto2[MojeAuto2$Przebieg <= mean(autaDF$Przebieg),]
MojeAuto2 <- MojeAuto2[!is.na(MojeAuto2$cena),]

MojeAuto2 <- MojeAuto2[MojeAuto2$'Rok.produkcji' >= 2013,]
MojeAuto2 <- MojeAuto2[!is.na(MojeAuto2$cena),]

MojeAuto2 <- MojeAuto2[order(MojeAuto2$cena),]


MojeAuto2 <- head(MojeAuto2,1)

View(MojeAuto2)


#Opel jest tańszy i ma mniejszy przebieg, ale jest starszy. 



