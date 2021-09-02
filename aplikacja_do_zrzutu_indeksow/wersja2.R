library(shiny)
library(DT)
library(tidyverse)


lista_sklepow<- read_csv("../zrzut_dane/stan_sklep.csv") %>% select(1) %>%  distinct() %>% pull()


ui <- fluidPage(
  
  # Application title
  titlePanel("Wyznaczanie indeksow"),
  
  sidebarLayout(
    
    sidebarPanel(
      textInput("folder","Sciezka do folderu z danymi",value="Z:/PRODUKT/NOWE SKLEPY/algorytm zwrotow pod zatowarowanie/skrypty/wyznaczanie indeksow i bestow/zrzut_dane"),
      helpText("Wskaz czy chcesz odtowarowywac konkretny sklep, jesli nie pozostaw opcje: IGNORUJ - wtedy wskaze indeksy z sieci"),
      selectInput(inputId ="SKLEP", "Sklep do odtowarowania",
                  choices = c("IGNORUJ",lista_sklepow),
                  selected = "iIGNORUJ"),                  
      selectInput(inputId ="sale", "Status indeksu?",
                  choices = c("WYPRZEDAZ","NIE WYPRZEDAZ", "WSZYSTKIE"),
                  selected = "NIE WYPRZEDAZ"),     
      selectInput(inputId ="kolejnosc", "Sposob sortowania?",
                  choices = c("BESTY","KITY"),
                  selected = "besty"),
      helpText("Wskaz ile potrzebujesz indeksow z danego depu"),
      DTOutput("my_datatable"),
      actionButton("go",label = "odswiez"),
      downloadButton("upload","pobierz plik"),
      width = 8
    ),
    
    # Show plot
    mainPanel(
      tableOutput("podsumowanie1")
    )
  )
)

server <- function(input, output) {
  
  #podajemy nazwe folderu gdzie sa dane wsadowe
  nazwa_folderu <- reactive({input$folder})
  
  #pobieramy z tego folderu raport opracowany w odzielnym skrypcie
  ## najpierw ranking
  ranking<-reactive({
    read_csv(file.path(nazwa_folderu(),"ranking.csv"))
  })
  
  ## nastepnie stan sklepow
  stan_sklep<-reactive({
    read_csv(file.path(nazwa_folderu(),"stan_sklep.csv"))
  })
  
  #przygotowuje stan sklepu tylko dla wybranego miasta
  stan_konkretny_sklep = reactive({
    sklep = input$SKLEP
    stan_sklep() %>%  filter(Magazyn == sklep) %>% group_by(Magazyn, KodProduktu) %>%  summarise(ilosc = sum(ilosc))
  })
  
  #dostosowuje ranking tylko do indeksow, ktore sa w tym konkretnym sklepie (jezeli zostal wskazany), sortuje i usuwam niepotrzebne kolumny (te dodane z joina)
  ranking_sklep <- reactive({
    sklep = input$SKLEP
    if (sklep == "IGNORUJ"){
      ranking()
    }else{
      ranking() %>% right_join(stan_konkretny_sklep(), by = "KodProduktu") %>% select(-Magazyn)     
    }
    })
  
  # sortoje dane ze wzgledu na wskazana opcje (besty/kity)
  ranking_sklep_sort <- reactive({
    sortowanie = input$kolejnosc
    if (sortowanie == "BESTY"){
      ranking_sklep()  
    }else{
      ranking_sklep() %>% arrange(KATEGORIA, DEPARTAMENT, grupa_towarowanie, desc(rotacja))     #przyjalem dla kitow ze to rotacja z calej sieci bedzie je determinowac 
    }
  })
  
  
  #pobieram informacje czy bierzemy pod uwage sale czy nie,(przypisanie jest troche na odwrot, ale dzieki temu latwiej bedzie to liczyc)
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
  
  #podaje liste mozliwych departamentow i grup
  dep = c('1_MĘŻCZYZNA','1_MĘŻCZYZNA','1_MĘŻCZYZNA','1_MĘŻCZYZNA','1_MĘŻCZYZNA','1_MĘŻCZYZNA','1_MĘŻCZYZNA','1_MĘŻCZYZNA','1_MĘŻCZYZNA','1_MĘŻCZYZNA','2_KOBIETA','2_KOBIETA','2_KOBIETA','2_KOBIETA','2_KOBIETA','2_KOBIETA','2_KOBIETA','2_KOBIETA','2_KOBIETA','2_KOBIETA','2_KOBIETA','3_CHŁOPAK','3_CHŁOPAK','3_CHŁOPAK','3_CHŁOPAK','3_CHŁOPAK','3_CHŁOPAK','3_CHŁOPAK','5_AKCESORIA','5_AKCESORIA','5_AKCESORIA','5_AKCESORIA','5_AKCESORIA','5_AKCESORIA','5_AKCESORIA','5_AKCESORIA','6_BIELIZNA','6_BIELIZNA','6_BIELIZNA','6_BIELIZNA','6_BIELIZNA','6_BIELIZNA')
  grup = c('BLUZA','BUTY','JEANS','KLAPKI','KURTKA','SANDAŁY','SLEEVELESS','SPODENKI','SPODNIE','T-SHIRT','BLUZA','BUTY','KLAPKI','KURTKA','SANDAŁY','SLEEVELESS','SPODENKI','SPODNIE','SUKIENKA','T-SHIRT','TIGHT','BLUZA','BUTY','KLAPKI','KURTKA','SPODENKI','SPODNIE','T-SHIRT','AKCESORIA OBUWNICZE','AKCESORIA RÓŻNE','AKCESORIA ZIMOWE','CZAPKA','OKULARY','PLECAK','TORBA','TORBA TRENINGOWA','KĄPIELÓWKI','MAJTKI','SKARPETY','SKARPETY DŁUGIE','STRÓJ KĄPIELOWY', 'BIELIZNA TERMOAKTYWNA')
  
  
  
  #tworze czysta tabele 
  v <- reactiveValues(data = { 
    data.frame(Dep =  dep, grupa = grup,  ile_modeli = rep(0,length(dep)),ile_bestow = rep(0,length(dep))) 
  })
  
  #tworze ze zdefiniowanej tabeli jej edytowalna wersje
  output$my_datatable <- renderDT({
    DT::datatable(v$data, editable = TRUE)
  })
  
  #when there is any edit to a cell, write that edit to the initial dataframe
  #check to make sure it's positive, if not convert
  observeEvent(input$my_datatable_cell_edit, {
    #get values
    info = input$my_datatable_cell_edit
    i = as.numeric(info$row)
    j = as.numeric(info$col)
    k = as.numeric(info$value)
    if(k < 0){ #convert to positive if negative
      k <- k * -1
    }
    
    #write values to reactive
    v$data[i,j] <- k
  })  
  
  #przygototowuje raport z danych wsadowych i wybranych w aplikacji opcji
  wynik <-eventReactive(input$go,{
    #przerobmy ranking pod katem tego czy sale czy nie
    ranking1 <- ranking_sklep_sort() %>%  filter(Wyprzedaz != czy_sale())
    #wypiszemy teraz  wszystkie indeksy
    
    zbior = v$data %>%  filter(ile_modeli >0)
    pelen_zbior = data.frame()
    
    #przypiszemy teraz indeksy wg edytowalnej tabeli do pustej tabeli
    for (i in 1:nrow(zbior)){
      pelen_zbior =  bind_rows( ranking1 %>%  filter(DEPARTAMENT == zbior$Dep[i] & grupa_towarowanie == zbior$grupa[i]) %>%  head(zbior$ile_modeli[i]),pelen_zbior)
    }
    lista = pelen_zbior %>% select(1)
    
    #wypiszemy teraz indeksy bestow
    
    besty = v$data %>%  filter(ile_bestow >0)
    zbior_bestow = data.frame()
    
    #przypiszemy teraz indeksy wg edytowalnej tabeli do pustej tabeli
    if (nrow(besty) < 1){
      zbior_bestow = data.frame("KodProduktu" = "0")
    }else {
      for (i in 1:nrow(besty)){
        zbior_bestow =  bind_rows( ranking1 %>%  filter(DEPARTAMENT == besty$Dep[i] & grupa_towarowanie == besty$grupa[i]) %>%  head(besty$ile_bestow[i]),zbior_bestow)
      } 
    }
    
    lista_besty = zbior_bestow %>% select(1) %>% mutate(czy_best = "TAK")
    
    lista %>% left_join(lista_besty, by = "KodProduktu")
    
  })
  
  
    # prezentuje wynik akcji
  output$podsumowanie1 <- renderTable({
    sklep = input$SKLEP
    if (sklep == "IGNORUJ"){
      NULL
    }else{
      wynik() %>%  left_join(stan_konkretny_sklep(), by = "KodProduktu") %>% left_join(ranking(), by = "KodProduktu") %>% 
        group_by(KATEGORIA, DEPARTAMENT, grupa_towarowanie) %>%  summarise(ILOSC = sum(ilosc), WARTOSC = sum((Wartosc_indeks/ilosc_indeks) *ilosc))
    }
    
    #wynik()
    # i dodaj dla bestow ilosci  - tak
  })  
  
  ## Tworzenie pliku do pobrania
  output$upload <- downloadHandler(filename = "lista_indeksow.csv", content = function(file) {
    sklep = input$SKLEP
    if (sklep == "IGNORUJ"){
      write.csv(wynik(), file, row.names = FALSE)
    } else {
      pobierz =wynik() %>%  left_join(stan_sklep(), by = "KodProduktu") %>% select(1,5,4)
      write_xlsx(pobierz, file)
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)




#dodaj opcje czy besty i sortuje od gory, czy kity i od dolu
# zrob tak, zeby ranking filtowalo wg sklepu, a jak nic nie dasz to wcale 
