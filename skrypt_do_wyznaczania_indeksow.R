# raport indeksow i bestow do alokacji

library(tidyr)
library(tidyverse)
library(readxl)
library(writexl)
library(xlsx)

# wprowadz dane startowe:
folder = "Z:/PRODUKT/NOWE SKLEPY/algorytm zwrotow pod zatowarowanie"
magazyny_detalowe <-c("MAGAZYN DETAL","MAGAZYN DOMÓWIEŃ","TYMCZASOWY MAGAZYN ZATOWAROWANIA")

#1 pobieranie danych
# a)remanent sklepów
plik = list.files(file.path(folder,"remanent magazyn"),pattern = "csv")

#jak nie dziala (nie ma polskich znakow to:)
#stan_lista = read_csv2(file.path(file.path(folder,"remanent magazyn"),plik), locale(encoding = "UTF-8",decimal_mark = ",", grouping_mark = "."), col_names = TRUE, col_types = NULL) 
stan_lista = read_csv2(file.path(file.path(folder,"remanent magazyn"),plik), col_names = TRUE, col_types = NULL) 

#to nam pokaze czy sa polskie znaki
stan_lista$Magazyn %>% unique() %>% as.data.frame() %>% select(sklep = 1) %>% mutate(sklepp = str_sub(sklep,1,10)) %>% filter(sklepp == "SKLEP TARN")

# podczyszczenie listy
stan = stan_lista %>%  filter(Magazyn %in% magazyny_detalowe | str_sub(stan_lista$Magazyn, 1, 5) == "SKLEP") %>%  select(2,3,4,5,6,7) %>% 
  group_by(KodProduktu, Rozmiar, Wyprzedaz) %>%  summarise(ilosc = sum(Ilosc), Wartosc = sum(Ilosc * CenaZakup))

#policzmy jeszcze ilosc i wartosc na indeksie
stan_na_indeks = stan %>%  group_by(KodProduktu,Wyprzedaz) %>%  summarise(ilosc_indeks = sum(ilosc), Wartosc_indeks = sum(Wartosc))

#b)hierarchia
hierarchia<-read_xlsx(file.path(folder,"HierarchiaProd.xlsx"), sheet = "listaModeli")

#trochę je oczyszczamy
hierarchia_1<- hierarchia %>% select(KodProduktu=2,KATEGORIA=4,11,12)

#dajemy grupy z duzej liter
hierarchia_1$GRUPA <- hierarchia_1$GRUPA %>% toupper()

#dodam jeszcze przypisanie wg depow bardziej ogólnych
grupy_towarowanie = read_excel(file.path(folder, "skrypty/wyznaczanie indeksow i bestow/udzialy_depow.xlsx"), sheet = "na_grupy") %>%  select(1,2,3,16) %>% mutate(GRUPA = toupper(GRUPA))


#c)paragony z ostatnich 4 tygodni
list.files(file.path(folder,"paragony"))->paragony_folder
paragony<-read_csv2(paste0(file.path(folder,"paragony"),"/",paragony_folder)) 

#trochę je oczyszczamy
paragony_1 = paragony %>% select(KodProduktu = 6, ILOSC = 8, Cena = 11) %>% 
  mutate(Cena = str_replace(Cena," zł",""), Cena = as.double(str_replace(Cena, ",","."))) %>% 
  group_by(KodProduktu) %>% 
  summarise(SlsU= sum(ILOSC), SlsR = sum(ILOSC * Cena)) %>%  arrange(desc(SlsU)) %>% ungroup()


#2 Laczymy dane i tworzymy ranking
rank1 = stan_na_indeks %>% left_join(paragony_1, by = "KodProduktu")  %>%  left_join(hierarchia_1, by = "KodProduktu" ) %>% 
  left_join(grupy_towarowanie, by = c("DEPARTAMENT", "KATEGORIA", "GRUPA"))

#zamienimy na dla kolumn z wartosciami:
lista_z_wartosciami = sapply(rank1, typeof) %>% as.data.frame() %>%  filter(. != "character") %>%  rownames_to_column()

rank1[lista_z_wartosciami[,1]][is.na(rank1[lista_z_wartosciami[,1]])] <- 0
#omijamy na w innych kolumnach
rank1 = rank1 %>%  na.omit()

# odfiltrujemy resztki po ilosci calkowitej oraz art dla sklepow
rank1 = rank1 %>%  filter(ilosc_indeks >20 & KATEGORIA != "ARTYKUŁY DLA SKLEPÓW")

#dodajmy przedzialy, by moc lepiej sortowac indeksy po sprzedazy w ramach przedzialu
rank1$przedzial_ilosci = rank1$ilosc_indeks %>%  cut(breaks = c(20,50, 200, 400, 800, 10000))

# sortujemy wstepnie liste
ranking = rank1 %>% mutate(rotacja = ifelse(SlsU == 0,0,ilosc_indeks/(SlsU/4))) %>% arrange(KATEGORIA, DEPARTAMENT, grupa_towarowanie, desc(przedzial_ilosci), desc(SlsR)) %>% ungroup()

#3 Stworze liste indeksow per sklep
stan_sklep = stan_lista  %>%  filter(str_sub(stan_lista$Magazyn,1, 5) == "SKLEP") %>%  select(2,3,4,5,6) %>% 
  group_by(Magazyn,KodProduktu,Rozmiar) %>%  summarise(ilosc = sum(Ilosc)) %>% select(1,2,4,3)


#4 Zapiszmy stworzony plik do wykorzystania w aplikacji
write_csv(ranking, file.path(folder,"skrypty/wyznaczanie indeksow i bestow/zrzut_dane/ranking.csv"))

write_csv(stan_sklep, file.path(folder,"skrypty/wyznaczanie indeksow i bestow/zrzut_dane/stan_sklep.csv"))

# zastanowic sie czy dodawac indeksy, ktore sa juz na tym sklepie -- czy w jakis sposob o te ilosc zmniejszac dane 
