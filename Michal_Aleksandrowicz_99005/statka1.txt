install.packages("dplyr")
install.packages("devtools")
devtools::install_github("pbiecek/PogromcyDanych")
library(dplyr)
library(PogromcyDanych)

#1
auta2012%>%
group_by(Marka)%>%
summarise(liczba = n())%>%
arrange(desc(liczba))%>%
filter(liczba==max(liczba))

#2
auta2012%>%
filter(Marka=="Toyota")%>%
group_by(Model)%>%
summarise(liczba = n())%>%
arrange(desc(liczba))%>%
filter(liczba==max(liczba))

#3
auta2012%>%
select(Rodzaj.paliwa, Rok.produkcji)%>%
filter(Rodzaj.paliwa=="olej napedowy (diesel)", Rok.produkcji == "2007")%>%
summarise(liczba = n())%>%
arrange(desc(liczba)) 

#3b
auta2012%>%
select(Kolor, Przebieg.w.km)%>%
group_by(Kolor, Przebieg.w.km)%>%
summarise(mediana = median(Przebieg.w.km, na.rm = TRUE))%>%
arrange(mediana)

#4
auta2012%>%
  select(Rok.produkcji, Marka)%>%
  group_by(Rok.produkcji, Marka)%>%
  filter(Rok.produkcji==2007)%>%
  summarise(liczba = n())%>%
  arrange(desc(liczba))%>%
filter(liczba==max(liczba))

#5
auta2012%>%
  select(Marka, Rok.produkcji, Model, Cena.w.PLN)%>%
  filter(Marka=="Toyota",Rok.produkcji>=2007, Rok.produkcji <=2008)%>%
  group_by(Model, Rok.produkcji)%>%
  summarise(Cena.w.PLN=mean(Cena.w.PLN), na.rm = TRUE)%>%
  mutate(roznica=Cena.w.PLN-lag(Cena.w.PLN,1))%>%
  select(Rok.produkcji, Model, roznica)%>%
  na.omit%>%
  arrange(desc(roznica))%>%
  filter(roznica==max(roznica))

#6
auta2012%>%
  select(Marka, Model, Cena.w.PLN, Rodzaj.paliwa, Rok.produkcji)%>%
  filter(Rok.produkcji==2007, Rodzaj.paliwa=="olej napedowy (diesel)")%>%
  group_by(Marka, Model, Cena.w.PLN)%>%
  arrange(desc(Cena.w.PLN))

#7
auta2012%>%
  select(Wyposazenie.dodatkowe)%>%
  filter(Wyposazenie.dodatkowe=="klimatyzacja")%>%
  summarise(liczba = n())%>%
  arrange(desc(liczba))

#8
auta2012%>%
  select(KM,Marka)%>%
  filter(KM>100)%>%
  group_by(Marka)%>%
  summarise(liczba = n())%>%
  arrange(desc(liczba))
         
#10
auta2012%>%
  select(Marka, Model, Cena.w.PLN, Rodzaj.paliwa, Rok.produkcji)%>%
  filter(Rok.produkcji==2007, Rodzaj.paliwa=="olej napedowy (diesel)")%>%
  group_by(Marka, Model, Cena.w.PLN)%>%
  arrange(Cena.w.PLN)

#11
auta2012%>%
  select(Marka, Wyposazenie.dodatkowe)%>%
  filter(Wyposazenie.dodatkowe=="klimatyzacja")%>%
  group_by(Marka)%>%
  summarise(liczba = n())%>%
  filter(liczba==max(liczba))

#12
auta2012%>%
  select(Cena.w.PLN, Marka)%>%
  group_by(Cena.w.PLN, Marka)%>%
  filter(Cena.w.PLN>50000)%>%
  group_by(Marka)%>%
  summarise(liczba = n())%>%
  arrange(desc(liczba))%>%
  filter(liczba==max(liczba))

#13
auta2012 %>%
  filter(Marka == "Toyota") %>%
  group_by(Model, Rodzaj.paliwa) %>%
  summarise(Mediana_przebiegu = median(Przebieg.w.km, na.rm=TRUE))%>%
  arrange(desc(Mediana_przebiegu))

#14    
auta2012%>%
  filter(Rok.produkcji==2007, Rodzaj.paliwa=="olej napedowy (diesel)")%>%
  group_by(Marka,Model)%>%
  summarise(liczba = max(Cena.w.PLN))%>%
  arrange(desc(liczba))

#15
auta2012%>%
  select(Model, Wyposazenie.dodatkowe)%>%
  filter(Wyposazenie.dodatkowe=="klimatyzacja")%>%
  group_by(Model)%>%
  summarise(liczba = n())%>%
  filter(liczba==max(liczba))

#16
auta2012%>%
  select(Przebieg.w.km, Marka,Rodzaj.paliwa)%>%
  filter(Przebieg.w.km<50000,Rodzaj.paliwa=="olej napedowy (diesel)")%>%
  group_by(Marka)%>%
  summarise(liczba = n())%>%
  filter(liczba==max(liczba))

#17
auta2012%>%
  filter(Rok.produkcji==2007, Marka=="Toyota")%>%
  group_by(Model)%>%
  summarise(Srednia_cena = mean(Cena.w.PLN, na.rm=TRUE))%>%
  arrange(desc(Srednia_cena))

#18
auta2012%>%
  select(Model, Cena.w.PLN, Rodzaj.paliwa, Rok.produkcji)%>%
  filter(Rok.produkcji==2007, Rodzaj.paliwa=="olej napedowy (diesel)")%>%
  group_by(Model)%>%
  arrange(Cena.w.PLN)

#19
auta2012%>%
  select(Kolor,Przebieg.w.km)%>%
  group_by(Kolor)%>%
  summarise(Przebiegg = median(Przebieg.w.km, na.rm=TRUE))%>%
  arrange(desc(Przebiegg))
