#Bauer 99029

#1. Ktora Marka wystepuje najczesciej w zbiorze danych auta2012?
auta2012 %>%
  group_by(Marka) %>%
  summarise(liczba = n()) %>%
  arrange(desc(liczba)) %>%
  filter(liczba == max(liczba))

#2. Sposrod aut marki Toyota, który model wystepuje najczesciej.
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
  summarise(liczba = n())

#4 Jakiego koloru auta maja najmniejszy medianowy przebieg?
auta2012 %>%
  select(Kolor, Przebieg.w.km)%>%
  group_by(Kolor, Przebieg.w.km)%>%
  summarise(mediana = median(Przebieg.w.km, na.rm = TRUE))%>%
  arrange(mediana)

#5. Gdy ograniczyc sie tylko do aut wyprodukowanych w 2007, ktora Marka wystepuje najczesciej w zbiorze danych auta2012?
auta2012 %>%
  select(Rok.produkcji, Marka) %>%
  filter(Rok.produkcji == 2007) %>%
  group_by(Rok.produkcji, Marka) %>%
  summarise(liczba = n()) %>%
  arrange(desc(liczba)) %>%
  filter(liczba == max(liczba))

#6. Sposrod aut marki Toyota, który model najbardziej stracil na cenie pomiedzy rokiem produkcji 2007 a 2008.
auta2012 %>%
  select(Marka, Model, Cena, Rok.produkcji) %>%
  filter(Rok.produkcji >= 2007, Rok.produkcji <= 2008, Marka == "Toyota") %>%
  group_by(Model, Rok.produkcji) %>%
  summarise(moja.cena = mean(Cena)) %>%
  mutate(roznica = moja.cena - lag(moja.cena, 1)) %>%
  select(Rok.produkcji, Model, roznica) %>%
  arrange(roznica)

#7. Sposrod aut z silnikiem diesla wyprodukowanych w 2007 roku ktora marka jest najdrozsza?
auta2012 %>%
  select(Marka, Model, Cena.w.PLN, Rodzaj.paliwa, Rok.produkcji) %>%
  filter(Rok.produkcji==2007, Rodzaj.paliwa=="olej napedowy (diesel)") %>%
  group_by(Marka, Model, Cena.w.PLN) %>%
  arrange(desc(Cena.w.PLN))

#8. Ile jest aut z klimatyzacja?
auta2012 %>%
  select(Wyposazenie.dodatkowe) %>%
  filter(Wyposazenie.dodatkowe=="klimatyzacja") %>%
  summarise(liczba = n()) %>%
  arrange(desc(liczba))

#9. Gdy ograniczyc sie tylko do aut z silnikiem ponad 100 KM, ktora Marka wystepuje najczesciej w zbiorze danych auta2012?
auta2012 %>%
  select(KM,Marka) %>%
  filter(KM>100) %>%
  group_by(Marka) %>%
  summarise(liczba = n()) %>%
  arrange(desc(liczba))

#10. Sposrod aut marki Toyota, ktory model ma najwieksza ro¿nice cen gdy porownac silniki benzynowe a diesel?
#auta2012 %>%
#  select(Marka,Model, Rodzaj.paliwa, Cena.w.PLN) %>%
#  filter(Marka =="Toyota") %>%
#  filter(Rodzaj.paliwa =="olej napedowy (diesel)" || Rodzaj.paliwa == "benzyna") %>%
  
  
#11. Sposrod aut z silnikiem diesla wyprodukowanych w 2007 roku ktora marka jest najtansza?
auta2012 %>%
  select(Marka, Model, Cena.w.PLN, Rodzaj.paliwa, Rok.produkcji) %>%
  filter(Rok.produkcji==2007, Rodzaj.paliwa=="olej napedowy (diesel)") %>%
  group_by(Marka, Model, Cena.w.PLN) %>%
  arrange(Cena.w.PLN)
  

#12. W jakiej marce klimatyzacja jest najczesciej obecna?
auta2012 %>%
  select(Marka, Wyposazenie.dodatkowe) %>%
  filter(Wyposazenie.dodatkowe=="klimatyzacja") %>%
  group_by(Marka) %>%
  summarise(liczba = n()) %>%
  filter(liczba==max(liczba))

#13. Gdy ograniczyc sie tylko do aut o cenie ponad 50 000 PLN, ktora Marka wystepuje najczesciej w zbiorze danych auta2012?
auta2012 %>%
  select(Cena.w.PLN, Marka) %>%
  group_by(Cena.w.PLN, Marka) %>%
  filter(Cena.w.PLN>50000) %>%
  group_by(Marka) %>%
  summarise(liczba = n()) %>%
  arrange(desc(liczba)) %>%
  filter(liczba==max(liczba))

#14. Sposrod aut marki Toyota, ktory model ma najwiekszy medianowy przebieg?
auta2012 %>%
  filter(Marka == "Toyota") %>%
  group_by(Model, Rodzaj.paliwa) %>%
  summarise(Mediana_przebiegu = median(Przebieg.w.km, na.rm=TRUE)) %>%
  arrange(desc(Mediana_przebiegu))

#15. Sposrod aut z silnikiem diesla wyprodukowanych w 2007 roku ktory model jest najdrozszy?
auta2012 %>%
  filter(Rok.produkcji==2007, Rodzaj.paliwa=="olej napedowy (diesel)") %>%
  group_by(Marka,Model) %>%
  summarise(liczba = max(Cena.w.PLN)) %>%
  arrange(desc(liczba))

#16. W jakim modelu klimatyzacja jest najczesciej obecna?
auta2012 %>%
  select(Model, Wyposazenie.dodatkowe) %>%
  filter(Wyposazenie.dodatkowe=="klimatyzacja") %>%
  group_by(Model) %>%
  summarise(liczba = n()) %>%
  filter(liczba==max(liczba))

#17. Gdy ograniczyc sie tylko do aut o przebiegu ponizej 50 000 km o silniku diesla, która Marka wystepuje najczesciej w zbiorze danych auta2012?
auta2012 %>%
  select(Przebieg.w.km, Marka,Rodzaj.paliwa) %>%
  filter(Przebieg.w.km<50000,Rodzaj.paliwa=="olej napedowy (diesel)") %>%
  group_by(Marka)%>%
  summarise(liczba = n()) %>%
  filter(liczba==max(liczba))

#18. Sposrod aut marki Toyota wyprodukowanych w 2007 roku, ktory model jest srednio najdrozszy?
auta2012 %>%
  filter(Rok.produkcji==2007, Marka=="Toyota") %>%
  group_by(Model) %>%
  summarise(Srednia_cena = mean(Cena.w.PLN, na.rm=TRUE)) %>%
  arrange(desc(Srednia_cena))

#19. Sposród aut z silnikiem diesla wyprodukowanych w 2007 roku ktory model jest najtanszy?
auta2012 %>%
  select(Model, Cena.w.PLN, Rodzaj.paliwa, Rok.produkcji) %>%
  filter(Rok.produkcji==2007, Rodzaj.paliwa=="olej napedowy (diesel)") %>%
  group_by(Model) %>%
  arrange(Cena.w.PLN)

#20. Jakiego koloru auta maj¹ najwiêkszy medianowy przebieg?
auta2012 %>%
  select(Kolor,Przebieg.w.km) %>%
  group_by(Kolor) %>%
  summarise(Przebiegg = median(Przebieg.w.km, na.rm=TRUE)) %>%
  arrange(desc(Przebiegg))
