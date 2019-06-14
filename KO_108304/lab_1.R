#=================================================================================
#..Subject     : Statystyka
#..Module      : Laboratorium 1
#..Description : Home task
#..Author      : Kostiantyn Ostapenko
#..Album       : 108304
#..Date created: 2019/03/04
#=================================================================================

install.packages("devtools")
install.packages("dplyr")
devtools::install_github("pbiecek/PogromcyDanych")

library(dplyr)
library(PogromcyDanych)

#1. Która Marka wystepuje najczesciej w zbiorze danych auta2012?
auta2012 %>%
  group_by(Marka) %>%
  summarise(liczba = n()) %>%
  arrange(desc(liczba)) %>%
  filter(liczba == max(liczba))

#2. Sposród aut marki Toyota, który model wystepuje najczesciej.
auta2012 %>%
  filter(Marka == "Toyota") %>%
  group_by(Model) %>%
  summarise(liczba = n()) %>%
  arrange(desc(liczba)) %>%
  filter(liczba == max(liczba))

#3. Sprawdz ile jest aut z silnikiem diesla wyprodukowanych w 2007 roku?
auta2012 %>%
  select(Rok.produkcji, Rodzaj.paliwa) %>%
  filter(Rok.produkcji == 2007, 
         grepl('diesel', Rodzaj.paliwa)) %>%
  group_by(Rok.produkcji, Rodzaj.paliwa) %>%
  summarise(liczba = n())%>%
  arrange(desc(liczba))

#3.b Jakiego koloru auta maja najmniejszy medianowy przebieg?
auta2012 %>%
  select(Przebieg.w.km, Kolor) %>%
  group_by(Kolor) %>%
  summarise(mediana.przebiegu = median(Przebieg.w.km, na.rm = TRUE)) %>%
  arrange(desc(mediana.przebiegu))%>%
  filter(mediana.przebiegu == max(mediana.przebiegu))

#4. Gdy ograniczyc sie tylko do aut wyprodukowanych w 2007, 
#   która Marka wystepuje najczesciej w zbiorze danych auta2012?
auta2012 %>%
  select(Rok.produkcji, Marka) %>%
  filter(Rok.produkcji == 2007) %>%
  group_by(Rok.produkcji, Marka) %>%
  summarise(liczba = n()) %>%
  arrange(desc(liczba)) %>%
  filter(liczba == max(liczba))


#5. Sposród aut marki Toyota, który model najbardziej stracil na cenie 
#   pomiedzy rokiem produkcji 2007 a 2008.
auta2012 %>%
  select(Marka, Model, Cena, Rok.produkcji) %>%
  filter(Rok.produkcji >= 2007, Rok.produkcji <= 2008, Marka == "Toyota") %>%
  group_by(Model, Rok.produkcji) %>%
  summarise(var_price = mean(Cena)) %>%
  #na.omit() %>%
  #lag(x, y) - this function shifting the time by a given number of observation
  #in my case its shift from 2008 to 2007
  mutate(var_dif = var_price - lag(var_price, 1)) %>%
  select(Rok.produkcji, Model, var_dif) %>%
  na.omit %>%
  arrange(var_dif)

#6. Sposród aut z silnikiem diesla 
# wyprodukowanych w 2007 roku która marka jest najdrozsza?
auta2012 %>%
  select(Rok.produkcji, Rodzaj.paliwa, Cena, Marka) %>%
  filter(Rok.produkcji == 2007,
         grepl('diesel', Rodzaj.paliwa)) %>%
  arrange(desc(Cena)) %>%
  filter(Cena == max(Cena))

#7. Ile jest aut z klimatyzacja?
auta2012 %>%
  summarise(klimatyzacja <- sum(grepl("klimatyzacja", Wyposazenie.dodatkowe)))

auta2012 %>%
  select(Wyposazenie.dodatkowe) %>%
  filter(Wyposazenie.dodatkowe == "klimatyzacja") %>%
  group_by(Wyposazenie.dodatkowe) %>%
  summarise(amount = n())

#8 Gdy ograniczyc sie tylko do aut z silnikiem ponad 100 KM,
#   która Marka wystepuje najczesciej w zbiorze danych auta2012?       
auta2012 %>%
  select(Marka, KM) %>%
  filter(KM > 100) %>%
  group_by(Marka) %>%
  #arrange(desc(KM)) %>%
  summarise(amount = n()) %>%
  arrange(desc(amount))

#9 Sposród aut marki Toyota, który model ma najwieksza róznice 
#   cen gdy porównac silniki benzynowe a diesel?

auta2012 %>%
  select(Marka, Model, Cena, Rodzaj.paliwa) %>%
  filter(Marka == "Toyota", Rodzaj.paliwa == "benzyna") %>%
  group_by(Model, Rodzaj.paliwa) %>%
  summarise(price_benz = mean(Cena, na.rm = TRUE)) %>%
  mutate(price_dif = price_benz - lag(price_benz, 1)) %>%
  select(Model, price_dif) %>%
  arrange(price_dif)

#10 Sposród aut z silnikiem diesla wyprodukowanych w 2007 roku która marka jest najtansza?
auta2012 %>%
  select(Rok.produkcji, Rodzaj.paliwa, Cena, Marka, Model) %>%
  filter(Rok.produkcji == 2007,
         grepl('diesel', Rodzaj.paliwa)) %>%
  arrange(desc(Cena)) %>%
  filter(Cena == min(Cena))

#11 W jakiej marce klimatyzacja jest najczesciej obecna?
auta2012 %>%
  select(Marka, Wyposazenie.dodatkowe) %>%
  filter(Wyposazenie.dodatkowe == 'klimatyzacja') %>%
  group_by(Marka) %>%
  summarise(amount = n()) %>%
  filter(amount == max(amount))
  #arrange(desc(amount))

#12 Gdy ograniczyc sie tylko do aut o cenie ponad 50 000 PLN, 
#   która Marka wystepuje najczesciej w zbiorze danych auta2012?
auta2012 %>%
  select(Cena, Marka) %>%
  filter(Cena > 50000) %>%
  group_by(Marka) %>%
  summarise(amount = n()) %>%
  filter(amount == max(amount))
  #arrange(desc(amount))

#13 Sposród aut marki Toyota, który model ma najwiekszy medianowy przebieg?
auta2012 %>%
  select(Marka, Model, Przebieg.w.km) %>%
  filter(Marka == 'Toyota') %>%
  group_by(Model, Marka) %>%
  summarise(Med_run = median(Przebieg.w.km, na.rm = TRUE)) %>%
  #filter(Med_run == min(Med_run))
  arrange(desc(Med_run))

#14 Sposród aut z silnikiem diesla wyprodukowanych w 2007 roku który model jest najdrozszy?
auta2012 %>%
  select(Marka, Model, Cena.w.PLN, Rok.produkcji, Rodzaj.paliwa) %>%
  filter(Rok.produkcji == 2007,
         grepl('diesel', Rodzaj.paliwa)) %>%
  arrange(desc(Cena.w.PLN, na.rm = TRUE))

#15 W jakim modelu klimatyzacja jest najczesciej obecna?
auta2012 %>%
  select(Model, Wyposazenie.dodatkowe) %>%
  filter(Wyposazenie.dodatkowe == 'klimatyzacja') %>%
  group_by(Model) %>%
  summarise(num = n()) %>%
  filter(num == max(num))

#16 Gdy ograniczyc sie tylko do aut o przebiegu ponizej 50 000 km o silniku diesla, 
#   która Marka wystepuje najczesciej w zbiorze danych auta2012?
auta2012 %>%
  select(Marka, Rodzaj.paliwa, Przebieg.w.km) %>%
  filter(Przebieg.w.km < 50000,
         grepl('diesel', Rodzaj.paliwa)) %>%
  group_by(Marka) %>%
  summarise(num = n()) %>%
  filter(num == max(num))

#17 Sposród aut marki Toyota wyprodukowanych w 2007 roku, który model jest srednio najdrozszy?
auta2012 %>%
  filter(Rok.produkcji == 2007, Marka == "Toyota") %>%
  group_by(Model) %>%
  summarise(num = mean(Cena.w.PLN, na.rm = TRUE)) %>%
  arrange(desc(num))

#18 Sposród aut z silnikiem diesla wyprodukowanych w 2007 roku który model jest najtanszy?
auta2012 %>%
  select(Model, Rok.produkcji, Rodzaj.paliwa, Cena.w.PLN) %>%
  filter(Rok.produkcji == 2007,
         grepl('diesel', Rodzaj.paliwa)) %>%
  group_by(Model) %>%
  arrange(Cena.w.PLN)

#19 Jakiego koloru auta maja najwiekszy medianowy przebieg?
auta2012 %>%
  select(Kolor, Przebieg.w.km) %>%
  group_by(Kolor) %>%
  summarise(km = median(Przebieg.w.km, na.rm = TRUE)) %>%
  arrange(desc(km))

auta2012 %>% 
  select(Marka, Model, Rodzaj.paliwa, Cena) %>%
  filter(Marka == 'Toyota',
         grepl('^benzyna$|diesel', Rodzaj.paliwa), Model != 'inny') %>%
  group_by(Model, Rodzaj.paliwa) %>%
  summarise(mean_price = mean(Cena)) %>%
  mutate(price_diff = abs(mean_price - lag(mean_price, 1))) %>%
  ungroup %>%
  select(Model, price_diff) %>%
  arrange(desc(price_diff))


auta2012 %>%
  select(Marka, Model, Cena.w.PLN, Rok.produkcji, Rodzaj.paliwa) %>%
  filter(Rok.produkcji == 2007,
         grepl('diesel', Rodzaj.paliwa, Cena.w.PLN)) %>%
  group_by(Model) %>%
  arrange(desc(Cena.w.PLN, na.rm = TRUE))
