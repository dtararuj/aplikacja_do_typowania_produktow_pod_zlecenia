###otwieraj zawsze with encoding utf - 8 



server1 = function(input, output,session){
  
#podajemy nazwe folderu gdzie sa dane
nazwa_folderu <- reactive({input$folder})

#pobieramy z tego folderu raport opracowany w odzielnym skrypcie
ranking<-reactive({
  read_csv(file.path(nazwa_folderu(),"ranking.csv"))
})

#pobieram informacje czy bierzemy pod uwage sale czy nie, zrobie to na odwrot, bo potem latwiej to wykorzystac
czy_sale <- reactive({
  status = input$sale
  if(status == "WYPRZEDAZ"){
   return(0)
  }else if (status == "NIEWYPRZEDAZ"){
    return(1)
  }else{
    return(2)
  }
})


# pobieramy wszyskie tabele w ramach grup produktowych zawieracje wskazana ilosc modeli

## obuwie 
buty_meskie_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "OBUWIE"
  Dep = "1_MĘŻCZYZNA"
  grupa = "BUTY"
  ile = input$buty_meskie
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1) %>%  head(ile)
  }) 

buty_damskie_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "OBUWIE"
  Dep = "2_KOBIETA"
  grupa = "BUTY"
  ile = input$buty_damskie
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

buty_juniorskie_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "OBUWIE"
  Dep = "3_CHŁOPAK"
  grupa = "BUTY"
  ile = input$buty_juniorskie
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

klapki_meskie_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "OBUWIE"
  Dep = "1_MĘŻCZYZNA"
  grupa = "KLAPKI"
  ile = input$klapki_meskie
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

klapki_damskie_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "OBUWIE"
  Dep = "2_KOBIETA"
  grupa = "KLAPKI"
  ile = input$klapki_damskie
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

klapki_juniorskie_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "OBUWIE"
  Dep = "3_CHŁOPAK"
  grupa = "klapki"
  ile = input$klapki_juniorskie
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

sandaly_meskie_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "OBUWIE"
  Dep = "1_MĘŻCZYZNA"
  grupa = "SANDAŁY"
  ile = input$sandaly_meskie
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

sandaly_damskie_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "OBUWIE"
  Dep = "2_KOBIETA"
  grupa = "SANDAŁY"
  ile = input$sandaly_damskie
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

sandaly_juniorskie_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "OBUWIE"
  Dep = "3_CHŁOPAK"
  grupa = "SANDAŁY"
  ile = input$sandaly_juniorskie
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

## tekstylia 
bluzy_meskie_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "TEKSTYLIA"
  Dep = "1_MĘŻCZYZNA"
  grupa = "BLUZA"
  ile = input$bluzy_meskie
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

bluzy_damskie_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "TEKSTYLIA"
  Dep = "2_KOBIETA"
  grupa = "BLUZA"
  ile = input$bluzy_damskie
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

bluzy_juniorskie_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "TEKSTYLIA"
  Dep = "3_CHŁOPAK"
  grupa = "BLUZA"
  ile = input$bluzy_juniorskie
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

kurtki_meskie_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "TEKSTYLIA"
  Dep = "1_MĘŻCZYZNA"
  grupa = "KURTKA"
  ile = input$kurtki_meskie
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

kurtki_damskie_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "TEKSTYLIA"
  Dep = "2_KOBIETA"
  grupa = "KURTKA"
  ile = input$kurtki_damskie
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

kurtki_juniorskie_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "TEKSTYLIA"
  Dep = "3_CHŁOPAK"
  grupa = "KURTKA"
  ile = input$kurtki_juniorskie
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 


sleevy_meskie_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "TEKSTYLIA"
  Dep = "1_MĘŻCZYZNA"
  grupa = "SLEEVELESS"
  ile = input$sleevy_meskie
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

sleevy_damskie_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "TEKSTYLIA"
  Dep = "2_KOBIETA"
  grupa = "SLEEVELESS"
  ile = input$sleevy_damskie
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

sleevy_juniorskie_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "TEKSTYLIA"
  Dep = "3_CHŁOPAK"
  grupa = "SLEEVELESS"
  ile = input$sleevy_juniorskie
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

spodenki_meskie_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "TEKSTYLIA"
  Dep = "1_MĘŻCZYZNA"
  grupa = "SPODENKI"
  ile = input$spodenki_meskie
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

spodenki_damskie_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "TEKSTYLIA"
  Dep = "2_KOBIETA"
  grupa = "SPODENKI"
  ile = input$spodenki_damskie
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

spodenki_juniorskie_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "TEKSTYLIA"
  Dep = "3_CHŁOPAK"
  grupa = "SPODENKI"
  ile = input$spodenki_juniorskie
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

spodnie_meskie_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "TEKSTYLIA"
  Dep = "1_MĘŻCZYZNA"
  grupa = "SPODNIE"
  ile = input$spodnie_meskie
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

spodnie_damskie_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "TEKSTYLIA"
  Dep = "2_KOBIETA"
  grupa = "SPODNIE"
  ile = input$spodnie_damskie
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

spodnie_juniorskie_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "TEKSTYLIA"
  Dep = "3_CHŁOPAK"
  grupa = "SPODNIE"
  ile = input$spodnie_juniorskie
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

koszulki_meskie_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "TEKSTYLIA"
  Dep = "1_MĘŻCZYZNA"
  grupa = "T-SHIRT"
  ile = input$koszulki_meskie
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

koszulki_damskie_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "TEKSTYLIA"
  Dep = "2_KOBIETA"
  grupa = "T-SHIRT"
  ile = input$koszulki_damskie
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

koszulki_juniorskie_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "TEKSTYLIA"
  Dep = "3_CHŁOPAK"
  grupa = "T-SHIRT"
  ile = input$koszulki_juniorskie
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

tighty_damskie_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "TEKSTYLIA"
  Dep = "2_KOBIETA"
  grupa = "TIGHT"
  ile = input$tighty_damskie
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

sukienki_damskie_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "TEKSTYLIA"
  Dep = "2_KOBIETA"
  grupa = "SUKIENKA"
  ile = input$sukienki_damskie
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

jeansy_meskie_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "TEKSTYLIA"
  Dep = "1_MĘŻCZYZNA"
  grupa = "JEANS"
  ile = input$jeansy_meskie
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

## akcesoria

obuwnicze_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "AKCESORIA"
  Dep = "5_AKCESORIA"
  grupa = "AKCESORIA OBUWNICZE"
  ile = input$obuwnicze
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

rozne_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "AKCESORIA"
  Dep = "5_AKCESORIA"
  grupa = "AKCESORIA RÓŻNE"
  ile = input$rozne
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

zimowe_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "AKCESORIA"
  Dep = "5_AKCESORIA"
  grupa = "AKCESORIA ZIMOWE"
  ile = input$zimowe
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

czapki_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "AKCESORIA"
  Dep = "5_AKCESORIA"
  grupa = "CZAPKA"
  ile = input$czapki
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

okulary_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "AKCESORIA"
  Dep = "5_AKCESORIA"
  grupa = "OKULARY"
  ile = input$okulary
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

plecaki_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "AKCESORIA"
  Dep = "5_AKCESORIA"
  grupa = "PLECAK"
  ile = input$plecaki
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

torby_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "AKCESORIA"
  Dep = "5_AKCESORIA"
  grupa = "TORBA"
  ile = input$torby
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

torby_treningowe_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "AKCESORIA"
  Dep = "5_AKCESORIA"
  grupa = "TORBA TRENINGOWA"
  ile = input$torby_treningowe
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

## Bielizna

termo_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "BIELIZNA"
  Dep = "6_BIELIZNA"
  grupa = "BIELIZNA TERMOAKTYWNA"
  ile = input$termo
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

kapielowki_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "BIELIZNA"
  Dep = "6_BIELIZNA"
  grupa = "KĄPIELÓWKI"
  ile = input$kapielowki
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

majtki_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "BIELIZNA"
  Dep = "6_BIELIZNA"
  grupa = "MAJTKI"
  ile = input$majtki
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

skarpety_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "BIELIZNA"
  Dep = "6_BIELIZNA"
  grupa = "SKARPETY"
  ile = input$skarpety
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

skarpety_dlugie_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "BIELIZNA"
  Dep = "6_BIELIZNA"
  grupa = "SKARPETY DŁUGIE"
  ile = input$skarpety_dlugie
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

stroj_zrzut <- reactive({
  ranking1<- ranking()
  czy_sale1<-czy_sale()
  Kat = "BIELIZNA"
  Dep = "6_BIELIZNA"
  grupa = "STRÓJ KĄPIELOWY"
  ile = input$stroj
  ranking1 %>% as.data.frame() %>%  filter(KATEGORIA == Kat, DEPARTAMENT == Dep, grupa_towarowanie == grupa, Wyprzedaz != czy_sale1)%>%  head(ile)
}) 

# Lacze wszystkie tabele w 1 do pobrania
zrzut <- reactive({
  bind_rows(buty_meskie_zrzut(), buty_damskie_zrzut(), buty_juniorskie_zrzut(), klapki_meskie_zrzut(), klapki_damskie_zrzut(), klapki_juniorskie_zrzut(), 
            sandaly_meskie_zrzut(), sandaly_damskie_zrzut(), sandaly_juniorskie_zrzut(),
            bluzy_meskie_zrzut(), bluzy_damskie_zrzut(), bluzy_juniorskie_zrzut(), kurtki_meskie_zrzut(), kurtki_damskie_zrzut(), kurtki_juniorskie_zrzut(), 
            sleevy_meskie_zrzut(), sleevy_damskie_zrzut(), sleevy_juniorskie_zrzut(), spodenki_meskie_zrzut(), spodenki_damskie_zrzut(), spodenki_juniorskie_zrzut(),
            spodnie_meskie_zrzut(), spodnie_damskie_zrzut(), spodnie_juniorskie_zrzut(), koszulki_meskie_zrzut(), koszulki_damskie_zrzut(), koszulki_juniorskie_zrzut(), 
            tighty_damskie_zrzut(), sukienki_damskie_zrzut(), jeansy_meskie_zrzut(), 
            obuwnicze_zrzut(), rozne_zrzut(), zimowe_zrzut(), czapki_zrzut(), okulary_zrzut(), plecaki_zrzut(), torby_zrzut(), torby_treningowe_zrzut(), 
            termo_zrzut(), kapielowki_zrzut(), majtki_zrzut(), skarpety_zrzut(), skarpety_dlugie_zrzut(), stroj_zrzut())
})

# przerabiam tabele w liste indeksow

indeksy_do_sciagniecia = reactive({
  zrzut() %>% select(1) %>% ungroup()
})

output$podsumowanie <- renderTable({
  zrzut() %>% group_by(DEPARTAMENT, KATEGORIA, grupa_towarowanie) %>% summarise(ile = n())
  
  })
  
  

## Tworzenie pliku do pobrania
output$upload <- downloadHandler(filename = "lista_indeksow.csv", content = function(file) {
  write.csv(indeksy_do_sciagniecia(), file, row.names = FALSE)})

  
}






#zrobic to w tabeli i dac tez info  ile ma byc bestow



