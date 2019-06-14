#Emil Nejbauer
#Indeks: 99283

#LAB 1
#Zadanie 1
auta <- auta2012
auta2012 %>%
  group_by(Marka) %>%
  summarise(liczba = n()) %>%
  arrange(desc(liczba)) ->
  najczestszaMarka

head(najczestszaMarka, 1)

#Zadanie 2

auta2012 %>%
  filter(Marka == "Toyota") %>%
  group_by(Model) %>%
  summarise(liczba = n()) %>%
  arrange(desc(liczba)) ->
  najczestszaToyota

head(najczestszaToyota, 1)

#Zadanie 3

auta2012 %>%
  filter(Rodzaj.paliwa == "olej napedowy (diesel)", Rok.produkcji == 2007) ->
  samochodyDiesel

nrow(samochodyDiesel)

#Zadanie 4

auta2012 %>%
  group_by(Kolor) %>%
  summarise(medianaPrzebiegu = median(Przebieg.w.km, na.rm = TRUE)) %>%
  arrange(medianaPrzebiegu) ->
  kolorZNajmniejszymPrzebiegiem

head(kolorZNajmniejszymPrzebiegiem, 1)

#Zadanie 4

auta2012 %>%
  filter(Rok.produkcji == 2007) %>%
  group_by(Marka) %>%
  summarise(liczba = n()) %>%
  arrange(desc(liczba)) ->
  najczestszaMarka2007

head(najczestszaMarka2007, 1)

#Zadanie 5

auta2012 %>%
  filter(Marka == "Toyota", Rok.produkcji == 2007) %>%
  group_by(Model) %>%
  summarise(sredniaCena = mean(Cena.w.PLN)) ->
  toyota2007

#Zadanie 6

#Zadanie 7

auta2012 %>%
  summarise(liczba.aut.z.klimatyzacja = sum(grepl("klimatyzacja", Wyposazenie.dodatkowe))) ->
              autaZKlimatyzacja
            
head(autaZKlimatyzacja)

#Zadanie 8

auta2012 %>%
  filter(KM > 100) %>%
  group_by(Marka) %>%
  summarise(liczba = n()) %>%
  arrange(desc(liczba)) ->
  najczestszaMarkaPonad100KM

head(najczestszaMarkaPonad100KM, 1)

#Zadanie 9

#Zadanie 10

auta2012 %>%
  filter(Rodzaj.paliwa == "olej napedowy (diesel)", Rok.produkcji == 2007) %>%
  group_by(Marka) %>%
  summarise(sredniaCena = mean(Cena.w.PLN)) %>%
  arrange(sredniaCena) ->
  najtanszaMarka2007Diesel

head(najtanszaMarka2007Diesel, 1)

#Zadanie 11

auta2012 %>%
  filter(grepl("klimatyzacja", Wyposazenie.dodatkowe)) %>%
  group_by(Marka) %>%
  summarise(liczba = n()) %>%
  arrange(desc(liczba)) ->
  najczestszaMarkaZKlimatyzacja

head(najczestszaMarkaZKlimatyzacja, 1)

#Zadanie 12

auta2012 %>%
  filter(Cena.w.PLN > 50000) %>%
  group_by(Marka) %>%
  summarise(liczba = n()) %>%
  arrange(desc(liczba)) ->
  najczestszaMarkaPonad50K

head(najczestszaMarkaPonad50K, 1)

#Zadanie 13

auta2012 %>%
  filter(Marka == "Toyota") %>%
  group_by(Model) %>%
  summarise(medianowyPrzebieg = median(Przebieg.w.km)) %>%
  arrange(desc(medianowyPrzebieg)) ->
  toyotaMedianowyPrzebieg

head(toyotaMedianowyPrzebieg, 1)

#Zadanie 14

auta2012 %>%
  filter(Rodzaj.paliwa == "olej napedowy (diesel)", Rok.produkcji == 2007) %>%
  group_by(Model) %>%
  summarise(sredniaCena = mean(Cena.w.PLN)) %>%
  arrange(desc(sredniaCena)) ->
  najdrozszyDiesel2007

head(najdrozszyDiesel2007, 1)

#Zadanie 15

auta2012 %>%
  filter(grepl("klimatyzacja", Wyposazenie.dodatkowe)) %>%
  group_by(Model) %>%
  summarise(liczba = n()) %>%
  arrange(desc(liczba)) ->
  najczestszyModelZKlimatyzacja

head(najczestszyModelZKlimatyzacja, 1)

#Zadanie 16

auta2012 %>%
  filter(Przebieg.w.km < 50000, Rodzaj.paliwa == "olej napedowy (diesel)") %>%
  group_by(Marka) %>%
  summarise(liczba = n()) %>%
  arrange(desc(liczba)) ->
  markaPonizej50kmdiesel

head(markaPonizej50kmdiesel, 1)

#Zadanie 17

auta2012 %>%
  filter(Marka == "Toyota", Rok.produkcji == 2007) %>%
  group_by(Model) %>%
  summarise(sredniaCena = mean(Cena.w.PLN)) %>%
  arrange(desc(sredniaCena)) ->
  najdrozszaToyota2007

head(najdrozszaToyota2007, 1)

#Zadanie 18

auta2012 %>%
  filter(Rodzaj.paliwa == "olej napedowy (diesel)", Rok.produkcji == 2007) %>%
  group_by(Model) %>%
  summarise(sredniaCena = mean(Cena.w.PLN)) %>%
  arrange(sredniaCena) ->
  najtanszyModelDiesel2007

head(najtanszyModelDiesel2007, 1)

#Zadanie 19

auta2012 %>%
  group_by(Kolor) %>%
  summarise(medianaPrzebiegu = median(Przebieg.w.km, na.rm=TRUE)) %>%
  arrange(desc(medianaPrzebiegu)) ->
  kolorZNajwiekszymPrzebiegiem
            
head(kolorZNajwiekszymPrzebiegiem, 1)

#LAB 3

#Zadanie 1

#Dla określenia rozkładu normalnego wymagane jest Odchylenie standardowe

#Zadanie 2

#Test Lilliefors (Kolmogorov-Smirnov), wymagane są więcej niż 4 dane
install.packages("nortest")
dane1 <- c(96.19,98.06, 103.45, 99.81, 101.60, 104.33)
lillie.test(dane1)

#Zadanie 3
x1 <- runif(n = 300, min = -1000, max = 30000)
sf.test(x1)

#Zadanie 4
grupa <- c(190, 180, 175, 169, 187, 192, 185, 195, 173, 175)
t.test(grupa, mu=180)
#Sredni wzrost w grupie jest wiekszy niz 180cm

#Zadanie 5

grupaFacet <- c(190, 180, 187, 192, 195, 175)
grupaKobieta <-c(169, 185, 173, 175)
t.test(grupaFacet, grupaKobieta)
#Wzrost mezczyzn jest wiekszy w grupie

#Zadanie 6
dob <- c(40, 80, 60)
zla <- c(10, 60, 20)
chisq.test(cbind(dob, zla))

#Zalezy od metody