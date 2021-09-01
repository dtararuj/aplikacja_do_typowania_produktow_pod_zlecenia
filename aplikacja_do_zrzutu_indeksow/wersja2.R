library(shiny)
library(DT)
library(tidyverse)

ui <- fluidPage(
  
  # Application title
  titlePanel("Indeksy pod zatowarowanie"),
  
  sidebarLayout(
    
    sidebarPanel(
      textInput("folder","sciezka do folderu z danymi",value="Z:/PRODUKT/NOWE SKLEPY/algorytm zwrotow pod zatowarowanie/skrypty/wyznaczanie indeksow i bestow"),
      selectInput(inputId ="sale", "Status indeksu?",
                  choices = c("WYPRZEDAZ","NIEWYPRZEDAZ", "WSZYSTKIE"),
                  selected = "NIE"),
      helpText("Wskaz ile potrzebujesz indeksow z danego depu"),
      DTOutput("my_datatable"),
      actionButton("go",label = "Plot Data"),
      width = 6
    ),
    
    # Show plot
    mainPanel(
      tableOutput("podsumowanie1"),
      verbatimTextOutput("podsumowanie2")
    )
  )
)

server <- function(input, output) {
  
  #podajemy nazwe folderu gdzie sa dane
  nazwa_folderu <- reactive({input$folder})
  
  #pobieramy z tego folderu raport opracowany w odzielnym skrypcie
  ranking<-reactive({
    nazwa_folderu1<-nazwa_folderu()
    nazwa_pliku = list.files(file.path(nazwa_folderu1), pattern = "csv") 
    read_csv(file.path(nazwa_folderu1, nazwa_pliku))
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

  dep = c('1_MĘŻCZYZNA','1_MĘŻCZYZNA','1_MĘŻCZYZNA','1_MĘŻCZYZNA','1_MĘŻCZYZNA','1_MĘŻCZYZNA','1_MĘŻCZYZNA','1_MĘŻCZYZNA','1_MĘŻCZYZNA','1_MĘŻCZYZNA','2_KOBIETA','2_KOBIETA','2_KOBIETA','2_KOBIETA','2_KOBIETA','2_KOBIETA','2_KOBIETA','2_KOBIETA','2_KOBIETA','2_KOBIETA','2_KOBIETA','3_CHŁOPAK','3_CHŁOPAK','3_CHŁOPAK','3_CHŁOPAK','3_CHŁOPAK','3_CHŁOPAK','3_CHŁOPAK','5_AKCESORIA','5_AKCESORIA','5_AKCESORIA','5_AKCESORIA','5_AKCESORIA','5_AKCESORIA','5_AKCESORIA','5_AKCESORIA','6_BIELIZNA','6_BIELIZNA','6_BIELIZNA','6_BIELIZNA','6_BIELIZNA','6_BIELIZNA')
  grup = c('BLUZA','BUTY','JEANS','KLAPKI','KURTKA','SANDAŁY','SLEEVELESS','SPODENKI','SPODNIE','T-SHIRT','BLUZA','BUTY','KLAPKI','KURTKA','SANDAŁY','SLEEVELESS','SPODENKI','SPODNIE','SUKIENKA','T-SHIRT','TIGHT','BLUZA','BUTY','KLAPKI','KURTKA','SPODENKI','SPODNIE','T-SHIRT','AKCESORIA OBUWNICZE','AKCESORIA RÓŻNE','AKCESORIA ZIMOWE','CZAPKA','OKULARY','PLECAK','TORBA','TORBA TRENINGOWA','KĄPIELÓWKI','MAJTKI','SKARPETY','SKARPETY DŁUGIE','STRÓJ KĄPIELOWY', 'BIELIZNA TERMOAKTYWNA')
  

  
    #stworzenie czystej tabeli
  v <- reactiveValues(data = { 
    data.frame(Dep =  dep, grupa = grup,  ile_modeli = rep(0,length(Dep)),ile_bestow = rep(0,length(Dep))) 
  })
  
  #output the datatable based on the dataframe (and make it editable)
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

  wynik <- reactive({
    ranking1<- ranking()
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
  
  output$podsumowanie1 <- renderTable({
    wynik()
    
    #potem do filtra doloz jeszcze sale
    # i dodaj dla bestow ilosci  - tak
  })  
  
}

# Run the application 
shinyApp(ui = ui, server = server)




#ranking

### dobra mam dane wsadowe, a teraz musze wyciagnac to co wpisalem, odfiltrowac ranking i go zrzucic do csv
