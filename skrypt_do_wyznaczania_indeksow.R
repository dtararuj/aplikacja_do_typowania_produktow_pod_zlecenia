# raport indeksow i bestow do alokacji

library(tidyr)
library(tidyverse)
library(readxl)
library(writexl)
library(xlsx)



folder = "Z:/PRODUKT/NOWE SKLEPY/algorytm zwrotow pod zatowarowanie"
magazyny_detalowe <-c("MAGAZYN DETAL","MAGAZYN DOMÓWIEŃ","TYMCZASOWY MAGAZYN ZATOWAROWANIA")

#1 pobieranie danych
# a)remanent sklepów
plik = list.files(file.path(folder,"remanent magazyn"),pattern = "csv")

stan_lista = read_csv2(file.path(file.path(folder,"remanent magazyn"),plik), locale(encoding = "UTF-8",decimal_mark = ",", grouping_mark = "."), col_names = TRUE, col_types = NULL) 

# podczyszczenie listy
stan = stan_lista %>%  filter(Magazyn %in% magazyny_detalowe | str_sub(stan_lista$Magazyn, 1, 5) == "SKLEP") %>%  select(2,3,4,5,6,7) %>% 
  group_by(KodProduktu, Rozmiar, Wyprzedaz) %>%  summarise(ilosc = sum(Ilosc), Wartosc = sum(Ilosc * CenaZakup))

#policzmy jeszcze ilosc i wartosc na indeksie
stan_na_indeks = stan %>%  group_by(KodProduktu,Wyprzedaz) %>%  summarise(ilosc_indeks = sum(ilosc), Wartosc_indeks = sum(Wartosc))

#b)hierarchia
hierarchia<-read_xlsx(file.path("//192.168.100.8/Thinkretail/","HierarchiaProd.xlsx"), sheet = "listaModeli")

#trochę je oczyszczamy
hierarchia_1<- hierarchia %>% select(KodProduktu=2,KATEGORIA=4,11,12)

#dajemy grupy z duzej liter
hierarchia_1$GRUPA <- hierarchia_1$GRUPA %>% toupper()

#dodam jeszcze przypisanie wg depow bardziej ogólnych
grupy_towarowanie = read_excel(file.path(folder, "skrypty/wyznaczanie indeksow i bestow/udzialy_depow.xlsx"), sheet = "na_grupy") %>%  select(1,2,3,16) %>% mutate(GRUPA = toupper(GRUPA))


#c)paragony
list.files(file.path(folder,"paragony"))->paragony_folder
paragony<-read_csv2(paste0(file.path(folder,"paragony"),"/",paragony_folder)) 

#trochę je oczyszczamy
paragony_1 = paragony %>% select(KodProduktu = 6, ILOSC = 8, Cena = 11) %>% 
  mutate(Cena = str_replace(Cena," zł",""), Cena = as.double(str_replace(Cena, ",","."))) %>% 
  group_by(KodProduktu) %>% 
  summarise(SlsU= sum(ILOSC), SlsR = sum(ILOSC * Cena)) %>%  arrange(desc(SlsU)) %>% ungroup()


#2 Laczymy dane i tworzymy ranking
rank = stan_na_indeks %>% left_join(paragony_1, by = "KodProduktu")  %>%  left_join(hierarchia_1, by = "KodProduktu" ) %>% 
  left_join(grupy_towarowanie, by = c("DEPARTAMENT", "KATEGORIA", "GRUPA"))
rank[is.na(rank)] <- 0

# odfiltrujemy resztki po ilosci calkowitej oraz art dla sklepow
rank = rank %>%  filter(ilosc_indeks >20 & KATEGORIA != "ARTYKUŁY DLA SKLEPÓW")

#dodajmy przedzialy, by moc lepiej sortowac indeksy po sprzedazy w ramach przedzialu
rank$przedzial_ilosci = rank$ilosc_indeks %>%  cut(breaks = c(20,50, 200, 400, 800, 10000))

# sortujemy wstepnie liste
ranking = rank %>% arrange(KATEGORIA, DEPARTAMENT, grupa_towarowanie, desc(przedzial_ilosci), desc(SlsR)) %>% ungroup()

#3 Zapiszmy stworzony plik do wykorzystania w aplikacji
write_csv(ranking, file.path(folder,"skrypty/wyznaczanie indeksow i bestow/ranking.csv"))

##############pytania 
#czy wyprzedaz jako filtr !!
# struktura marek jaka jest ? sprawdzic 
# daj jeszcze indeksy do bestow


# patrzyc jakie grupy dalo, np pilkarskie wyklucz
# zastanowic sie czy dodawac indeksy, ktore sa juz na tym sklepie -- czy w jakis sposob o te ilosc zmniejszac dane 
