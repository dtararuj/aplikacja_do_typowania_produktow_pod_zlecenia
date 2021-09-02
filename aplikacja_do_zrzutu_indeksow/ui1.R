library(shiny)
library(tidyr)
library(tidyverse)
library(readxl)
library(writexl)
library(DT)

ui1 = fluidPage(
  titlePanel("Indeksy pod zatowarowanie"),

  sidebarLayout(

    sidebarPanel(
    textInput("folder","sciezka do folderu z danymi",value="Z:/PRODUKT/NOWE SKLEPY/algorytm zwrotow pod zatowarowanie/skrypty/wyznaczanie indeksow i bestow/zrzut_dane"),
    selectInput(inputId ="sale", "Status indeksu?",
                        choices = c("WYPRZEDAZ","NIEWYPRZEDAZ", "WSZYSTKIE"),
                        selected = "NIE"),
    helpText("Wskaz ile potrzebujesz indeksow z danego depu"),
    numericInput('buty_meskie','Ile modeli buty_meskie', min=1, max=150, value=0,step=1),
    numericInput('buty_damskie','Ile modeli buty_damskie', min=1, max=150, value=0,step=1),
    numericInput('buty_juniorskie','Ile modeli buty_juniorskie', min=1, max=150, value=0,step=1),
    numericInput('klapki_meskie','Ile modeli klapki_meskie', min=1, max=150, value=0,step=1),
    numericInput('klapki_damskie','Ile modeli klapki_damskie', min=1, max=150, value=0,step=1),
    numericInput('klapki_juniorskie','Ile modeli klapki_juniorskie', min=1, max=150, value=0,step=1),
    numericInput('sandaly_meskie','Ile modeli sandaly_meskie', min=1, max=150, value=0,step=1),
    numericInput('sandaly_damskie','Ile modeli sandaly_damskie', min=1, max=150, value=0,step=1),
    numericInput('sandaly_juniorskie','Ile modeli sandaly_juniorskie', min=1, max=150, value=0,step=1),
    numericInput('bluzy_meskie','Ile modeli bluzy_meskie', min=1, max=150, value=0,step=1),
    numericInput('bluzy_damskie','Ile modeli bluzy_damskie', min=1, max=150, value=0,step=1),
    numericInput('bluzy_juniorskie','Ile modeli bluzy_juniorskie', min=1, max=150, value=0,step=1),
    numericInput('kurtki_meskie','Ile modeli kurtki_meskie', min=1, max=150, value=0,step=1),
    numericInput('kurtki_damskie','Ile modeli kurtki_damskie', min=1, max=150, value=0,step=1),
    numericInput('kurtki_juniorskie','Ile modeli kurtki_juniorskie', min=1, max=150, value=0,step=1),
    numericInput('sleevy_meskie','Ile modeli sleevy_meskie', min=1, max=150, value=0,step=1),
    numericInput('sleevy_damskie','Ile modeli sleevy_damskie', min=1, max=150, value=0,step=1),
    numericInput('sleevy_juniorskie','Ile modeli sleevy_juniorskie', min=1, max=150, value=0,step=1),
    numericInput('spodenki_meskie','Ile modeli spodenki_meskie', min=1, max=150, value=0,step=1),
    numericInput('spodenki_damskie','Ile modeli spodenki_damskie', min=1, max=150, value=0,step=1),
    numericInput('spodenki_juniorskie','Ile modeli spodenki_juniorskie', min=1, max=150, value=0,step=1),
    numericInput('spodnie_meskie','Ile modeli spodnie_meskie', min=1, max=150, value=0,step=1),
    numericInput('spodnie_damskie','Ile modeli spodnie_damskie', min=1, max=150, value=0,step=1),
    numericInput('spodnie_juniorskie','Ile modeli spodnie_juniorskie', min=1, max=150, value=0,step=1),
    numericInput('koszulki_meskie','Ile modeli koszulki_meskie', min=1, max=150, value=0,step=1),
    numericInput('koszulki_damskie','Ile modeli koszulki_damskie', min=1, max=150, value=0,step=1),
    numericInput('koszulki_juniorskie','Ile modeli koszulki_juniorskie', min=1, max=150, value=0,step=1),
    numericInput('tighty_damskie','Ile modeli tighty_damskie', min=1, max=150, value=0,step=1),
    numericInput('sukienki_damskie','Ile modeli sukienki_damskie', min=1, max=150, value=0,step=1),
    numericInput('jeansy_meskie','Ile modeli jeansy_meskie', min=1, max=150, value=0,step=1),
    numericInput('obuwnicze','Ile modeli obuwnicze', min=1, max=150, value=0,step=1),
    numericInput('rozne','Ile modeli rozne', min=1, max=150, value=0,step=1),
    numericInput('zimowe','Ile modeli zimowe', min=1, max=150, value=0,step=1),
    numericInput('czapki','Ile modeli czapki', min=1, max=150, value=0,step=1),
    numericInput('okulary','Ile modeli okulary', min=1, max=150, value=0,step=1),
    numericInput('plecaki','Ile modeli plecaki', min=1, max=150, value=0,step=1),
    numericInput('torby','Ile modeli torby', min=1, max=150, value=0,step=1),
    numericInput('torby_treningowe','Ile modeli torby_treningowe', min=1, max=150, value=0,step=1),
    numericInput('termo','Ile modeli termo', min=1, max=150, value=0,step=1),
    numericInput('kapielowki','Ile modeli kapielowki', min=1, max=150, value=0,step=1),
    numericInput('majtki','Ile modeli majtki', min=1, max=150, value=0,step=1),
    numericInput('skarpety','Ile modeli skarpety', min=1, max=150, value=0,step=1),
    numericInput('skarpety_dlugie','Ile modeli skarpety_dlugie', min=1, max=150, value=0,step=1),
    numericInput('stroj','Ile modeli stroj', min=1, max=150, value=0,step=1),
    downloadButton("upload","pobierz plik"),
    width = 4
    ),
    mainPanel(
    tableOutput("podsumowanie")
    )
  )
)



shinyApp(ui = ui1, server = server1)

