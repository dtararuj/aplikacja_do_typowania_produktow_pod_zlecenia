# raport indeksow i bestow do alokacji

library(tidyr)
library(tidyverse)
library(readxl)
library(writexl)
library(xlsx)

# wprowadz dane startowe:
folder = "Z:/PRODUKT/NOWE SKLEPY/algorytm zwrotow pod zatowarowanie"
magazyny_detalowe <-c("MAGAZYN DETAL","MAGAZYN DOMÓWIEŃ","TYMCZASOWY MAGAZYN ZATOWAROWANIA","MAGAZYN WSPÓŁDZIELONY", "MAGAZYN DETAL ALOKACYJNY")

# marki w ktorych bierzemy skarpety kolorowe, w celu stworzenia nowej grupy
skarpety_kolorowe_marki = c("KREBO", "COMODO", "DOTS SOCKS","KAES")

# 1. pobieranie danych
# a)remanent magazynow i sklepow
plik = list.files(file.path(folder,"remanent magazyn"),pattern = "csv")

# remanent samych sklepow
plik1 = list.files(file.path(folder,"remanenty"),pattern = "csv")

#### jak nie dziala (nie ma polskich znakow to skorzystaj z kodu który jest zakomentowany)
# stan_lista = read_csv2(file.path(file.path(folder,"remanent magazyn"),plik), locale(encoding = "UTF-8",decimal_mark = ",", grouping_mark = "."), col_names = TRUE, col_types = NULL) 
stan_lista = read_csv2(file.path(file.path(folder,"remanent magazyn"),plik), col_names = TRUE, col_types = NULL) 

# to nam pokaze czy sa polskie znaki
stan_lista$Magazyn %>% unique() %>% as.data.frame() %>% select(sklep = 1) %>% mutate(sklepp = str_sub(sklep,1,10)) %>% filter(sklepp == "SKLEP TARN")

# uzyskamy tylko stan magazynow wirtualnych
stan_mag = stan_lista %>% filter(str_starts(Magazyn,"MAGAZYN "))

# uzyskamy tylko stan sklepow stacjonarnych
stan_biezacy_lista = read_csv2(file.path(file.path(folder,"remanenty"),plik1), col_names = TRUE, col_types = NULL) 

# podczyszczone stany sklepow
stan_biezacy_lista1 = stan_biezacy_lista %>% mutate(CenaZakupu = as.numeric(str_replace_all(.$'CENA ZAKUPU',c(" zł" = "","," ="."))), Wartosc = WYNIK * CenaZakupu) %>%  
  select(KodProduktu = 3, Rozmiar = 4, Wyprzedaz = 12, Ilosc = 9, Wartosc) %>%   group_by(KodProduktu, Rozmiar, Wyprzedaz) %>%  summarise(ilosc = sum(Ilosc), Wartosc = sum(Wartosc))

# podczyszczenie listy stanow magazynowych 
stan_magazyn_lista = stan_mag %>%  filter(Magazyn %in% magazyny_detalowe) %>%  select(2,3,4,5,6,7) %>% 
  group_by(KodProduktu, Rozmiar, Wyprzedaz) %>%  summarise(ilosc = sum(Ilosc), Wartosc = sum(Ilosc * CenaZakup))

# polaczenie obu list, tej ze stanami magazynowymi i stanami sklepowymi
stan = bind_rows(stan_magazyn_lista, stan_biezacy_lista1) %>% group_by(KodProduktu, Rozmiar, Wyprzedaz) %>% summarise(ilosc = sum(ilosc), Wartosc = sum(Wartosc))

# policzmy jeszcze ilosc i wartosc na indeksie
stan_na_indeks = stan %>%  group_by(KodProduktu,Wyprzedaz) %>%  summarise(ilosc_indeks = sum(ilosc), Wartosc_indeks = sum(Wartosc))

# b)hierarchia
hierarchia = read_xlsx(file.path(folder,"HierarchiaProd.xlsx"), sheet = "listaModeli")

# trochę je oczyszczamy ze zbednych kolumn
hierarchia_pelna =  hierarchia %>% select(KodProduktu=2,KATEGORIA=4,11,12,7,6)

# dajemy grupy z duzej liter
hierarchia_pelna$GRUPA =  hierarchia_pelna$GRUPA %>% toupper()

# stworzmy obok hierarchi pelnej jeszcze hierarchie uproszczona
hierarchia_1 = hierarchia_pelna %>% select(1:4) 

# przerobmy hierarchie pod nasze grupy
hierarchia_specjalna = hierarchia_pelna %>% mutate(GRUPA_1 = ifelse(GRUPA == "TORBA" & Typ == "NERKA", "NERKI",
                                                                    ifelse(str_detect(GRUPA, "SKARPETY D") & Firma %in% skarpety_kolorowe_marki, "DOTSY",
                                                                           ifelse(GRUPA == "AKCESORIA ZIMOWE" & Typ == "CZAPKA ZIMOWA", "CZAPKI ZIMOWE",
                                                                                  ifelse(str_detect(GRUPA, "BUTY"), "BUTY",
                                                                                         ifelse(Typ == "WOREK NA BUTY", "WOREK NA BUTY",
                                                                                                ifelse(Typ == "STOPKI" | Typ == "KRÓTKIE (QUARTER)","SKARPETY",GRUPA)))))))

# dodam jeszcze przypisanie wg depow bardziej ogólnych
grupy_towarowanie = hierarchia_specjalna %>% select(3,2,4)

# c)paragony z ostatnich 4 tygodni
list.files(file.path(folder,"paragony"))->paragony_folder
paragony<-read_csv2(paste0(file.path(folder,"paragony"),"/",paragony_folder)) 

# trochę je oczyszczamy
paragony_1 = paragony %>% select(KodProduktu = 6, ILOSC = 8, Cena = 11) %>% 
  mutate(Cena = str_replace(Cena," zł",""), Cena = as.double(str_replace(Cena, ",","."))) %>% 
  group_by(KodProduktu) %>% 
  summarise(SlsU= sum(ILOSC), SlsR = sum(ILOSC * Cena)) %>%  arrange(desc(SlsU)) %>% ungroup()


# 2. Laczymy dane i tworzymy ranking
rank1 = stan_na_indeks %>% left_join(paragony_1, by = "KodProduktu")  %>%  
  left_join(hierarchia_specjalna %>% select(1,2,3,grupa_towarowanie = 7 ), by = "KodProduktu" ) 

# zamienimy wartosci "NA" dla kolumn z wartosciami:
lista_z_wartosciami = sapply(rank1, typeof) %>% as.data.frame() %>%  filter(. != "character") %>%  rownames_to_column()

rank1[lista_z_wartosciami[,1]][is.na(rank1[lista_z_wartosciami[,1]])] <- 0

# omijamy wartosci "NA" w innych kolumnach
rank1 = rank1 %>%  na.omit()

# odfiltrujemy resztki (male ilosci) po ilosci calkowitej oraz artykuly dla sklepow
rank1 = rank1 %>%  filter(ilosc_indeks >20 & KATEGORIA != "ARTYKUŁY DLA SKLEPÓW")

# dodajmy przedzialy, by moc lepiej sortowac indeksy po sprzedazy w ramach przedzialu
rank1$przedzial_ilosci = rank1$ilosc_indeks %>%  cut(breaks = c(20,50, 200, 400, 800, 10000))

# sortujemy wstepnie liste
ranking = rank1 %>% mutate(rotacja = ifelse(SlsU == 0,0,ilosc_indeks/(SlsU/4)))  %>% ungroup()

# dolozmy jeszcze informacje czy dany indeks jest w ilosci wiekszej niz 60 szt na magazynie, jezeli tak to te w pierwszej kolejnosci.
ilosc_mag = stan_lista %>%  filter(Magazyn %in% magazyny_detalowe) %>% group_by(KodProduktu) %>% summarise(EopuM = sum(Ilosc)) %>% 
  mutate(duzy_zapas = ifelse(EopuM > 40, "TAK", "NIE"))

# polacze ranking i raz jeszcze posortuje, teraz dodatkowo po tym czy jest ten duzy zapas
ranking = ranking %>%  left_join(ilosc_mag %>% select(1,3), by = "KodProduktu") %>% arrange(KATEGORIA, DEPARTAMENT, grupa_towarowanie, desc(przedzial_ilosci),desc(duzy_zapas), desc(SlsR))


# 3. Stworze liste indeksow per sklep
stan_sklep = stan_biezacy_lista %>%  select(Magazyn = 2, KodProduktu = 3, Rozmiar = 4,  Ilosc = 9, Wyprzedaz = 12) %>% 
  group_by(Magazyn,KodProduktu,Rozmiar) %>%  summarise(ilosc = sum(Ilosc)) %>% select(1,2,4,3)


# 4. Zapiszmy stworzony plik do wykorzystania w aplikacji
write_csv(ranking, file.path(folder,"skrypty/wyznaczanie indeksow i bestow/zrzut_dane/ranking.csv"))

write_csv(stan_sklep, file.path(folder,"skrypty/wyznaczanie indeksow i bestow/zrzut_dane/stan_sklep.csv"))

