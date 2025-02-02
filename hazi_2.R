# 0. Feladat: Working Directory be�ll�t�sa
setwd('C:/Users/B�lint/OneDrive/Dokumentumok/BCE 2023-24/�konometria/H�zi2')

# A kommentek ISO-8859-2 k�dol�ssal olvashat�ak!!!

# 1. Feladat: Adatok beolvas�sa R-be, hi�nyz� adatok (NA-k) kisz�r�se �s v�ltoz�k �talak�t�sa:
library(readxl)
ESS2020 <- read_excel("2. h�zi feladat adatb�zisok.xlsx", sheet = 'ESS2020')
str(ESS2020)
View(ESS2020)

# Az utols� 3 oszlop nem kell, mert a magyar�zatot tartalmazza + Data Frame-� alak�t�s:
ESS2020 <- as.data.frame(ESS2020[,1:12]) # Adatb�zis fel�l�r�sa.
View(ESS2020)
str(ESS2020)

# Hi�nyz� (NA) adatok kisz�r�se:
sapply(ESS2020, function(x) sum(is.na(x))) # Hi�nyz� adatok kimutat�sa v�ltoz�kk�nt.
sum(is.na(ESS2020)) # �sszesen 26 hi�nyz� (NA) adat.
ESS2020 <- ESS2020[complete.cases(ESS2020),] # A 26 NA-t tartalmaz� sor/megfigyel�s kisz�r�se.
sum(is.na(ESS2020)) # Ellen�rz�s.
table(ESS2020$TrustInParlament)
summary(ESS2020)
# Min�s�gi v�ltoz�k �talak�t�sa (Logikai �s Faktor):
# Logikai:
ESS2020$TrustInParlament <- ifelse(ESS2020$TrustInParlament == 'Yes', 1, 0)
ESS2020$TrustInParlament <- as.logical(ESS2020$TrustInParlament)

ESS2020$SecretGroupInfluenceWorldPol <- ifelse(ESS2020$SecretGroupInfluenceWorldPol == 'Yes', 1, 0)
ESS2020$SecretGroupInfluenceWorldPol <- as.logical(ESS2020$SecretGroupInfluenceWorldPol)

ESS2020$ScientistsDecievePublic <- ifelse(ESS2020$ScientistsDecievePublic == 'Yes', 1, 0)
ESS2020$ScientistsDecievePublic <- as.logical(ESS2020$ScientistsDecievePublic)

ESS2020$COVID19 <- ifelse(ESS2020$COVID19 == 'Yes', 1, 0)
ESS2020$COVID19 <- as.logical(ESS2020$COVID19)

ESS2020$ContactCOVID19 <- ifelse(ESS2020$ContactCOVID19 == 'Yes', 1, 0)
ESS2020$ContactCOVID19 <- as.logical(ESS2020$ContactCOVID19)

ESS2020$GetVaccince <- ifelse(ESS2020$GetVaccince == 'Yes', 1, 0)
ESS2020$GetVaccince <- as.logical(ESS2020$GetVaccince)


# Faktor
ESS2020$PoliticalPartyPref <- as.factor(ESS2020$PoliticalPartyPref)
ESS2020$Region <- as.factor(ESS2020$Region)

str(ESS2020) # Ellen�rz�s.
summary(ESS2020)

# 2. Feladat: Outliersz�r�s
1073*0.01 # Ha 1% adat kisz�rhet�, akkor maximum 10-11 megfigyel�st sz�rhetek ki
1073*0.03 # Ha a 2-3%-os maxim�lisan elfogadhat� adatot sz�rn�m ki, akkor 32-33 adat sz�rhet� ki. Ez m�r a legmaxim�lisabb t�r�shat�r.
# Enn�l t�bb adat kisz�r�se nagyon nyom�s indok eset�n csak.

summary(ESS2020)
# R�n�z�sre vannak el�g �rdekes mindh�rom mennyis�gi v�ltoz�n�l. Ezeket megvizsg�lom egyenk�nt is.

# PoliticalRadioTVPerDay_Minutes
boxplot(ESS2020$PoliticalRadioTVPerDay_Minutes)
head(sort(ESS2020$PoliticalRadioTVPerDay_Minutes), 30)
tail(sort(ESS2020$PoliticalRadioTVPerDay_Minutes), 90)
# Els� gondolkod�sra ezek az adatok relev�nsnak tekinthet�ek, mert benne vannak a 24 �r�s napi limitben(24*60=1440 perc). Az �sszes adat ezen korl�t alatt van.
# Viszont, ha jobban megvizsg�lom, akkor vannak el�g �rdekes eredm�nyek is. Vannak olyan emberek, akik 15+ �r�t hallgatnak politikai r�di�t.
# Ez egy kicsit irrelev�nsan tud hangzani. Felt�telezz�k, hogy egy �tlagos embernek 8-10 �ra sz�ks�ges alv�sra. Tegy�k fel, hogy a marad�k id�t maxim�lisan kihaszn�lja, am�g �bren van �s eg�sz nap a politikai r�di�t hallgatja.
# Ez m�r hihet�bb �s relev�nsnak mondhat� szerintem. A boxploton l�tszik, hogy 800-n�l van egy kis t�r�s, ahol nincsenek adatok. Ezekn�l az adatokn�l h�zom meg a hat�rt.
# Viszont 800/600= ~13,3 �ra, ez�rt a kerek�t�s �s a sz�p eg�sz sz�m miatt 14 �r�s hat�rt h�zok meg (�gy belef�r a 8-10 �ra alv�s is. Szuper.) 
# Ez a 14 �r�s hat�r 840 percet jelent, sz�val itt a 840 perc f�l�tti adatokat kisz�r�m.
# A 0 �rt�kekb�l 111 van. Ezt k�t okb�l nem sz�r�m ki: ez rengeteg adat, de a legfontosabb, hogy ezek relev�nsak �s �rtelmezhet�ek, mert rengetegen nem hallgatnak politikai r�di�t.
sum(ESS2020$PoliticalRadioTVPerDay_Minutes == 0) # 111-en nem hallgatnak semmilyen politikai r�di�t egy percig sem 
sum(ESS2020$PoliticalRadioTVPerDay_Minutes > 840) # 27 adat van 840 perc f�l�tt. Ezeket fogom kidobni az im�nt eml�tett okok miatt.
# �sszegz�s: A boxploton l�tottak alapj�n felfel� sok a kil�g� �rt�k, itt a feljebb eml�tett okokn�l fogva csak a 840-n�l kisebb �rt�keket tartom meg. Lefel� nincsen kil�g� �rt�k �s a 0 is teljesen racion�lisan �rtelmezhet� �s nem kell kisz�rni.
ESS2020 <- ESS2020[ESS2020$PoliticalRadioTVPerDay_Minutes < 840,] 
# Ezzel 1046-ra cs�kkent az adatok sz�ma.

# NetUsePerDay_Minutes
boxplot(ESS2020$NetUsePerDay_Minutes)
head(sort(ESS2020$NetUsePerDay_Minutes), 30)
tail(sort(ESS2020$NetUsePerDay_Minutes), 30)
# Ism�telten az mondhat� el, mint az el�z� magyar�z�v�ltoz�n�l.
# Els� gondolkod�sra az �rt�kek relev�nsak, mert benne vannak a 24 �r�s/1440 perces napi korl�tban.
# Viszont itt is, ha jobban megvizsg�lom, akkor itt is vannak �rdekes, irracion�lis �rt�kek. Teh�t itt is neh�z elk�pzelni a 15 �ra(900 perc) feletti �rt�keket.
# Itt is tegy�k fel, hogy egy �tlagos embernek 8-10 �ra sz�ks�ges alv�sra �s a marad�k id�t maxim�lisan kihaszn�lja �s az internetet haszn�lja addig.
# A boxploton itt is l�tszik egy t�r�s 800 percn�l, ahol nincsenek adatok. Ez�rt ezen okok ment�n itt ism�telten a 14 �r�s (840 perces) hat�rt h�zn�m meg. 
# A 840 perc feletti �rt�keket kisz�r�m. Ez 7 darab adatot jelent.
# Lefel� nincsenek kil�g� �rt�kek, viszont vannak alacsony �rt�kek: pl.: 0,2,10. Ezek az �rt�kek teljesen racion�lisak, mert vannak olyan emberek, akik val�ban keveset haszn�lj�k naponta az internetet.
# Egy l�p�st viszont most tenn�k meg, amely a h�zi feladatom k�s�bbi szakasz�ban lesz relev�ns. Kicsi spoiler. :)
# A 0 �rt�ket kisz�rn�m, mert az eredm�nyv�ltoz�mat logaritmiz�lni fogom �s a 0 �rt�k nem tenn� ezt nekem lehet�v� �gy, hogy dolgozni tudjak a modellel.
# Ezt a probl�m�t �gy is tudn�m kezelni, hogy hozz�adok egy konstanst a NetUsePerDay_Minutes �rt�keihez, viszont mivel csak 1 darab 0 �rt�k van, ezt az egyszer�s�g kedv��rt ink�bb kisz�r�m.
# DE ha a konstans hozz�ad�s m�dszert v�lasztottam volna, akkor �gy n�zett volna ki az R-k�d: log(NetUsePerDay_Minutes+1).
sum(ESS2020$NetUsePerDay_Minutes > 840) # 7 darab megfigyel�s van 840 perc f�l�tt.
# �sszegz�s: A boxploton l�tottak alapj�n itt is felfel� vannak kil�g� �rt�kek. Az im�nt eml�tett okok miatt a fels� hat�rt a 840 percn�l h�ztam meg, az als� hat�rt pedig 0 percn�l.
# Ez �sszesen 7+1= 8 darab adat kisz�r�s�t jelenti.
ESS2020 <- ESS2020[ESS2020$NetUsePerDay_Minutes < 840 & ESS2020$NetUsePerDay_Minutes > 0,] 
# Ezzel 1038-ra cs�kkent a megfigyel�sek sz�ma.

# Education_Years
boxplot(ESS2020$Education_Years)
head(sort(ESS2020$Education_Years), 30)
tail(sort(ESS2020$Education_Years), 30)
# Itt is ellehet mondani, hogy a boxplotra �s a adatokra r�n�zve el�g �rdekes eredm�nyek sz�lettek.
# Van n�h�ny alacony �rt�k, ami azt jelenti, hogy valaki m�g a 8 �ltal�nos �vet sem v�gezte el. Ez a gondolatmenet egy darabig lehet racion�lis, viszont valahol �n �gy gondolom kell h�zni egy hat�rt.
# A 4 oktat�si �v azt jelenti, hogy valaki elkezdi 6 �vesen az iskol�t �s 10 �vesen abbahagyja. Ezt neh�z elk�pzelni.
# A boxploton ez lefel� kil�g� �rt�kk�nt l�tszik. Ezen indokok miatt ezt a 2 megfigyel�st �n itt lesz�rn�m.
# Tov�bb gondolkodtam �s vizsg�ltam a 8 �v alatti �rt�keket. A 6 �s 7 �veket �n racion�lisabban fel tudom dolgozni.
# P�ld�ul azt m�r k�nnyebben el tudom k�pzelni, hogy valaki 6 �v oktat�s ut�n hagyja ott az iskol�t 12 �vesen ilyen vagy olyan egy�b indokok miatt. Ez is egy nagyon alacsony �rt�k, de ez m�r sz�momra elfogadhat�bb.
# Itt ezen indokok miatt teh�t az als� hat�rt a 6 �vn�l h�zom meg. Teh�t Education_Years >= 6
sum(ESS2020$Education_Years < 6) # 2 megfigyel�s van 6 oktat�si �v alatt.
# �gy eddig jelenleg 2 adatot sz�r�k ki.
# Felfel� is vannak �rdekesebb adatok, de ezek sz�momra relev�nsak. A 20-30 oktat�si �vet �n azzal hihet�nek tudom magyar�zni, hogy valaki elv�gzi a 12 oszt�lyz ut�na pedig ak�r 2 BSc diplom�t �s 1 MSc diplom�t tesz le.
# Vagy ak�r orvosnak jog�sznak tanul �s mellette m�s tanulm�nyokat is elv�gez. Pl.: doktori c�m megszerz�se stb.
# Viszont ezek ellen�re a boxploton l�tottak alapj�n is van egy kil�g� �rt�k a 40 �v. �gy gondolom ez m�r nagyon torz�tan� a modellt �s kicsit nehezebb is tudom ezt m�r feldolgozni.
# Az m�g hihet� �s racion�lis volt sz�momra, hogy az indokok alapj�n valaki a 26-36 �let�vei k�z�tt fejezi be a tanulm�nyait (Tegy�k fel, hogy 6 �vesen kezdte el az 1. oszt�lyt �s nem bukott.).
# Az viszont m�r sz�momra irracion�lis, hogy valaki 40 �ven kereszt�l tanul �s 46 �ves kor�ra fejezi be a tanulm�nyait (Itt is felt�ve, hogy 6 �ves kor�t�l oktat�sban r�szes�l.)
# Sz�val ezen gondolkod�smenet ment�n �n itt a 30 �vn�l h�zn�m meg a fels� hat�rt, sz�val: Education_Years <= 30
sum(ESS2020$Education_Years >30) # 5 megfigyel�s van 30 oktat�si �v felett.
# �gy ezzel m�r 2+5=7 adatot sz�r�k ki.
# �sszegz�s: A boxplot �s a saj�t gondolatmenetem �s indokok alapj�n �n �sszesen az als� �s fels� korl�tokkal 7 darab adatot sz�rn�k ki ezen v�ltoz� ment�n.
ESS2020 <- ESS2020[ESS2020$Education_Years >= 6 & ESS2020$Education_Years <=30,]
# �gy v�glegesen 1031 adatom maradt.

# Outliersz�r�s �sszegz�se:
# Az outliervizsg�latot a 3 mennyis�gi v�ltoz� ment�n v�geztem.
# Ezek k�z�tt 27+8+7=42 adatot sz�rtem ki. Ez sajnos meghaladja mind az 1%, mind a 2-3%-os maxim�lisan lesz�rhet� megfigyel�sek sz�m�t.
# Viszont �n �gy gondolom, hogy itt l�tfontoss�g� volt ezen adatok lesz�r�se, hogy ne kapjak torz becsl�seket �s eredm�nyeket.
# �n �gy gondolom, hogy a gondolatmenetem �s az indokl�saim �sszer�ek �s relev�nsak voltak �s hi�ba l�ptem �t a maxim�lisan toler�lhat� limitet az outliersz�r�sn�l, �gy is jogosan tettem ezt.
# A boxplotokra ut�l�g r�n�zve l�that�, hogy j�l hat�roztam meg a hat�rokat, ahol kisz�rtem az adatokat.
# �sszesen a kezdeti adatb�zisom 1073 megfigyel�ssel rendelkezett.(Ebb�l 26 megfigyel�st hi�nyz� �rt�kek miatt m�r kisz�rtem). 42 megfigyel�st ebb�l outliersz�r�s miatt t�vol�tottam el.
# Ezen ment�n 1031-re cs�kkentettem az adatb�zisom, amellyel dolgozni fogok. Ami azt jelenti, hogy az adatb�zisom 96,1%-�t megtartottam �s 3,9%-�t hagytam el outlierek miatt.
1031/1073 # 96,1%

# Le�ro statisztika a mennyis�gi v�ltoz�kra:
library(psych)
leirostat_1 <- describe(ESS2020[c(2,3,6)])
leirostat_2 <- as.data.frame(summary(ESS2020[c(2,3,6)]))

library(writexl)
write_xlsx(leirostat_1, 'Leirostat_1.xlsx')
write_xlsx(leirostat_2, 'Leirostat_2.xlsx')


# Korrel�ci�s m�trix: 
library(corrplot)
KorrelMatrix <- cor(ESS2020[,c(2,3,6)])
corrplot(KorrelMatrix, method = 'color')
# Nagyon gyenge kapcsolatok mindh�rom |r| < 0,1
# Napi internethaszn�lat ~ Politikai r�di�: nagyon gyenge pozit�v/egyir�ny� kapcsolat
# Napi internethaszn�lat ~ Oktat�si �vek sz�ma: nagyon gyenge pozit�v/egyir�ny� kapcsolat
# Oktat�si �vek sz�ma ~ Politikai r�di�: nagyon gyenge negat�v/ellent�tes kapcsolat

# 3. T�bbv�ltoz�s line�ris regresszi� �s modellspecifik�ci� tesztel�se
# A faktorokn�l a viszony�t�si alap �t�ll�t�sa, ha sz�ks�ges:
levels(ESS2020$PoliticalPartyPref) # Itt az egy�b a viszony�t�si alap. Ezt �t�ll�tom Fidesz-KDNP-re, mivel ez a p�rt van jelenleg hatalmon �s ez�rt nekem �sszer� ehhez hasonl�tani. Mellesleg ezt k�nnyeben lehet �rtelmezni, mint az Egy�b kateg�ri�t.
ESS2020$PoliticalPartyPref <- relevel(ESS2020$PoliticalPartyPref, ref = 'Fidesz-KDNP')
levels(ESS2020$Region) # Itt a Budapest/Pest kateg�ria a viszony�t�si alap. Ez nekem megfelel, mivel ez Magyarorsz�g k�zpontja �s �ltal�ban ehhez szokt�k viszony�tani a telep�l�seket/v�rosokat/megy�ket.

# Line�ris regresszi�:
modell_1 <- lm(NetUsePerDay_Minutes ~ .-id, data = ESS2020)
summary(modell_1)
# H�ha. Ez egy el�g gyenge magyar�z�erej� modell. Az eredm�nyv�ltoz�ban l�v� inform�ci�nak csak a 9,51%-�t magyar�zza a modell.
# Gyenge magyar�z�erej� modell.
# Az R^2 az 9,51%. A korrig�lt R^2 az pedig 8,54%. 
# A Glob�lis F-pr�ba p-�rt�ke nagyon alacsony, ez azt jelenti, hogy minden szok�sos szignifikanciaszinten elutas�tjuk a H0-t.
# Ez azt jelenti, hogy nem csak a val� �letben, hanem a mint�ban is gyeng�n magya�zz�k a v�ltoz�k a napi internethaszn�latot. / A modell kiterjeszthet� a sokas�gra.
# Amikor r�n�ztem az excel adatb�zisra, akkor �szint�n elcsod�lkoztam az adatokon �s azon, hogy tartalmilag mennyire elt�r� v�ltoz�kat tartalmaz.
# Nem sz�m�tottam nagy magyar�z�er�re, maximum 10-15%-os R^2-et v�rtam, azt sejtettem, hogy tal�n �ppen meg�ti a k�zepes magyar�z�er� hat�r�t(,ami 10%-t�l kezd�dik).
# Az eredm�nyek majdnem el�rt�k a hat�rt, de v�g�l nem tudt�k megl�pni ezt. Els�re nagyon alacsonynak t�nhet az eredm�ny, de egy�ltal�n nem meglep�.
# Ha v�gign�zz�k a magyar�z�v�ltoz�kat, �gy gondolom, hogy egy�ltal�n nem meglep�ek a kapott eredm�nyek.

# Modellspecifik�ci� tesztel�se:
library(lmtest)
resettest(modell_1)
# Hipot�zisvizsg�lat: H0: modell j�l specifik�lt, H1: modell nem j�l specifik�lt
# A p-�rt�k  5,38%, sz�val H0-t nem utas�tjuk el minden szok�sos szignifikanciaszinten. 10%-on m�g elutas�tjuk, de Alfa= 5% �s 1%-os szignifikanciaszinten m�g elfogadjuk.
# Ez azt jelenti, hogy a modell nem minden szok�sos szignifikanciaszinten nmondhat� el, hogy j�l specifik�lt.

# �sszegz�s:
# A modell gyenge magyar�z�erej� a mint�ban �s a sokas�gban is. 
# A modellre nem minden szok�sos szignifikanciaszinten mondhat� el, hogy j�l specifik�lt.
modell_1_szukitett <-lm(NetUsePerDay_Minutes ~ PoliticalRadioTVPerDay_Minutes + PoliticalPartyPref + Education_Years + Region + SecretGroupInfluenceWorldPol,data = ESS2020)
AIC(modell_1, modell_1_szukitett)
BIC(modell_1, modell_1_szukitett)
anova(modell_1, modell_1_szukitett)
# 4. �j tagok keres�se a modellbe:
# A mennyis�gi v�ltoz�k eloszl�s�nak a vizsg�lata. Hisztogrammok vizsg�lata.
# Az eredm�nyv�ltoz�m: NetUsePerDay_Minutes
hist(ESS2020$NetUsePerDay_Minutes) # Kicsit balra ferde jobbra elny�l�.
hist(log(ESS2020$NetUsePerDay_Minutes)) # Kicsit jobbra ferde, tal�n jobb mint az eredeti.
# Nagy minta, de jobb, ha az eredm�nyv�ltoz� norm�lis eloszl�st k�vet. Ezt ennek �rdek�ben ink�bb logaritmiz�ln�m.

# Az egyik mennyis�gi magyar�z�v�ltoz�m: 
hist(ESS2020$PoliticalRadioTVPerDay_Minutes) # Er�sen balra ferde, jobbra elny�l�. 
hist(log(ESS2020$PoliticalRadioTVPerDay_Minutes)) # Ha logaritmiz�lom ez megjavul.
# A modellben vehetem ennek a logaritm�z�ltj�t.

# A m�sik mennyis�gi magyar�z�v�ltoz�m: Education_Years
hist(ESS2020$Education_Years) # Nem er�sen balra ferde. �vekn�l �ltal�ban a kvadratikus tag a j�. De megn�zem a logaritmust is az�rt itt is.
hist(log(ESS2020$Education_Years)) # A logaritmus egy norm�lishoz k�zelebb eloszl�st ad.
# Ezt lehet logaritmiz�lni, de szerintem a n�gyzetes tag az jobb lesz ebb�l, mert �vsz�m. Ezt majd megvizsg�lom.

# Tov�bbi v�ltoz�p�rok k�z�tti kapcsolatok vizsg�lata (Interakci�k keres�se):
library(ggplot2)
#
ggplot(data = ESS2020, aes (x = PoliticalRadioTVPerDay_Minutes, y = log(NetUsePerDay_Minutes), color = Region)) +
  geom_point() +
  stat_smooth(method=lm) # Itt csak a tenelymetszet k�l�nb�zik, a meredeks�gek nem, szinte p�rhuzamosak. Itt nem kell interakci�.

ggplot(data = ESS2020, aes(x = PoliticalRadioTVPerDay_Minutes, y = log(NetUsePerDay_Minutes), color = TrustInParlament)) +
  geom_point() +
  stat_smooth(method=lm) ### Itt nincs nagy k�l�nbs�g a meredeks�gben. Itt nem kellene interakci�.

ggplot(data = ESS2020, aes(x = PoliticalRadioTVPerDay_Minutes, y = log(NetUsePerDay_Minutes), color = PoliticalPartyPref)) +
  geom_point() +
  stat_smooth(method=lm) # Vannak kicsit meredekebb egyenesek. Itt lehet, de nem musz�j interakci�.

ggplot(data = ESS2020, aes(x = PoliticalRadioTVPerDay_Minutes, y = log(NetUsePerDay_Minutes), color = SecretGroupInfluenceWorldPol)) +
  geom_point() +
  stat_smooth(method=lm) # M�s a tengelymetszet, de p�rhuzamosak az egyenesek. Nem kell interakci�.

ggplot(data = ESS2020, aes(x = PoliticalRadioTVPerDay_Minutes, y = log(NetUsePerDay_Minutes), color = ScientistsDecievePublic )) +
  geom_point() +
  stat_smooth(method=lm) # Nagyon minim�lis a meredeks�gk�l�nbs�g az elej�n, azt�n egym�sba torkollik a 2 egyenes. Nem kell interakci�.

ggplot(data = ESS2020, aes(x = PoliticalRadioTVPerDay_Minutes, y = log(NetUsePerDay_Minutes), color = COVID19)) +
  geom_point() +
  stat_smooth(method=lm) # Val�ban l�tv�nyos a meredek�gk�l�nbs�g, de ez csak a kil�g� �rt�kek miatt van. Lehet interakci�.

ggplot(data = ESS2020, aes(x = PoliticalRadioTVPerDay_Minutes, y = log(NetUsePerDay_Minutes), color = ContactCOVID19)) +
  geom_point() +
  stat_smooth(method=lm) # Val�ban l�tv�nyos a meredek�gk�l�nbs�g, de ez csak a kil�g� �rt�kek miatt van. Lehet interakci�.

ggplot(data = ESS2020, aes(x = PoliticalRadioTVPerDay_Minutes, y = log(NetUsePerDay_Minutes), color = GetVaccince)) +
  geom_point() +
  stat_smooth(method=lm) # Itt sincs k�l�bs�g a meredeks�gben. Nem kell interakci�.

#
ggplot(data = ESS2020, aes(x = Education_Years, y = log(NetUsePerDay_Minutes), color = Region)) +
  geom_point() +
  stat_smooth(method=lm) # Nincs nagy k�l�nbs�g a meredeks�gben. Meglep�. Itt nem kell interakci�.

ggplot(data = ESS2020, aes(x = Education_Years, y = log(NetUsePerDay_Minutes), color = TrustInParlament)) +
  geom_point() +
  stat_smooth(method=lm) # A meredeks�g nem k�l�nb�zik, szinte p�rhuzamosak az egyenesek. Nem lehet interakci�.

ggplot(data = ESS2020, aes(x = Education_Years, y = log(NetUsePerDay_Minutes), color = PoliticalPartyPref)) +
  geom_point() +
  stat_smooth(method=lm) # Az egyenesek nem p�rhuzamosak ugyan, de nincsen drasztikus k�l�nbs�g a meredeks�gek k�z�tt. # Itt nem venn�k be interakci�t, de ez preferencia k�rd�se. 

ggplot(data = ESS2020, aes(x = Education_Years, y = log(NetUsePerDay_Minutes), color = SecretGroupInfluenceWorldPol)) +
  geom_point() +
  stat_smooth(method=lm) # Nincs nagy k�l�nbs�g a meredeks�gben. Nem kell interakci�.

ggplot(data = ESS2020, aes(x = Education_Years, y = log(NetUsePerDay_Minutes), color = ScientistsDecievePublic )) +
  geom_point() +
  stat_smooth(method=lm) # Ism�t minim�lis a meredeks�gk�l�nbs�g. Nem kell interakci�.

ggplot(data = ESS2020, aes(x = Education_Years, y = log(NetUsePerDay_Minutes), color = COVID19)) +
  geom_point() +
  stat_smooth(method=lm) # Nem �rzem drasztikusnak a 2 meredeks�g k�l�nbs�g�t. Nem kell interakci�.

ggplot(data = ESS2020, aes(x = Education_Years, y = log(NetUsePerDay_Minutes), color = ContactCOVID19)) +
  geom_point() +
  stat_smooth(method=lm) # Szinte p�rhuzamos a 2 egyenes. Nem kell interakci�.

ggplot(data = ESS2020, aes(x = Education_Years, y = log(NetUsePerDay_Minutes), color = GetVaccince)) +
  geom_point() +
  stat_smooth(method=lm) # Itt sincs k�l�bs�g a meredeks�gben, szinte egym�st fedi le a 2 egyenes. Nem kell interakci�.

str(ESS2020)
# Tov�bbi v�ltoz�p�rok k�z�tti kapcsolatok vizsg�lata (Nem-line�ris transzform�ltak keres�se):

# Logaritmiz�l�s kell-e vagy nem (Ink�bb hisztogramm alapj�n) : 
# Education_Years
ggplot(data = ESS2020, aes(x = Education_Years, y = log(NetUsePerDay_Minutes))) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") # �gy n�z ki a kapcsolat jelenleg.

ggplot(data = ESS2020, aes(x = log(Education_Years), y = log(NetUsePerDay_Minutes))) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") # Val�ban nem rossz a logaritmiz�l�s.

ggplot(data = ESS2020, aes(x = I(Education_Years^2), y = log(NetUsePerDay_Minutes))) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") # A 30-as kil�g� �rt�k miatt lehet jobbnak l�tszik a logaritmiz�l�s, de a n�gyzetre emel�s az elej�n szerintem jobban illeszkedik r�n�z�sre. 

# PoliticalRadioTVPerDay_Minutes
ggplot(data = ESS2020, aes(x = PoliticalRadioTVPerDay_Minutes, y = log(NetUsePerDay_Minutes))) +
  geom_point() +
  stat_smooth() +
  stat_smooth(color="red") # Ez a jelenlegi kapcsolat.

ggplot(data = ESS2020, aes(x = log(PoliticalRadioTVPerDay_Minutes+1), y = log(NetUsePerDay_Minutes))) +
  geom_point() +
  stat_smooth() +
  stat_smooth(color="red") # Logaritmiz�l�s 1-gyel. 

ggplot(data = ESS2020, aes(x = log(PoliticalRadioTVPerDay_Minutes+0.01), y = log(NetUsePerDay_Minutes))) +
  geom_point() +
  stat_smooth() +
  stat_smooth(color="red") # Logaritmiz�l�s egy 1-n�l kisebb konstanssal

ggplot(data = ESS2020, aes(x = log(PoliticalRadioTVPerDay_Minutes+53.12), y = log(NetUsePerDay_Minutes))) +
  geom_point() +
  stat_smooth() +
  stat_smooth(color="red") # Logaritmiz�l�s 53.12-vel, ami az �tlaga a v�ltoz�nak. 
# K�vetkeztet�s: az �br�n is l�tszik, hogy nagyon sok m�lik azon, hogy mi a konstans. Inkozisztens eredm�nyek. Konstans �rt�k�t�l f�gg�en m�s eredm�ny. 
# Mi alapj�n d�ntsem el, hogy mi a j� konstans �rt�ke? Nem tudom ezt meghat�rozni. Ez�rt tan�csos lenne kihagyni.

# Az eredm�nyek tesztel�se (megn�zni, hogy melyik kombin�ci�kkal kapom a sz�momra legjobb modellt, ami a modell_2 lesz.):

modell_proba_1 <-lm(log(NetUsePerDay_Minutes) ~ PoliticalRadioTVPerDay_Minutes + PoliticalPartyPref + log(Education_Years) + Region + SecretGroupInfluenceWorldPol,data = ESS2020)
summary(modell_proba_1) # R^2: 9,22%, korrig�lt R^2: 8,6%
resettest(modell_proba_1) # p-�rt�k: 17,2%

modell_proba_2 <-lm(log(NetUsePerDay_Minutes) ~ PoliticalRadioTVPerDay_Minutes + PoliticalPartyPref + I(Education_Years^2) + Region + SecretGroupInfluenceWorldPol,data = ESS2020)
summary(modell_proba_2) # R^2: 9,32%, korrig�lt R^2: 8,7%, Kvadratikus tagk�nt javul az Education_Years p-�rt�ke, m�g jobban szignifik�nsabb lesz.
resettest(modell_proba_2) # p-�rt�k: 25,57% -j�l specifik�lt

modell_proba_3 <-lm(log(NetUsePerDay_Minutes) ~ log(PoliticalRadioTVPerDay_Minutes+1) + PoliticalPartyPref + I(Education_Years^2) + Region + SecretGroupInfluenceWorldPol,data = ESS2020)
summary(modell_proba_3) # R^2: 9,26%, korrig�lt R^2: 8,64%, Romlik a p-�rt�ke, kb 2-szer nagyobb lesz.
resettest(modell_proba_3) # p-�rt�k: 14,77% - j�l specifik�lt

modell_proba_4 <-lm(log(NetUsePerDay_Minutes) ~ log(PoliticalRadioTVPerDay_Minutes+0.01) + PoliticalPartyPref + I(Education_Years^2) + Region + SecretGroupInfluenceWorldPol,data = ESS2020)
summary(modell_proba_4) # R^2: 9,69%, korrig�lt R^2: 9,1%. Javul a PoliticalRadioTVPerDay_Minutes p-�rt�ke, m�r 5%-on szignifik�ns lesz.
resettest(modell_proba_4) # p-�rt�k: 4,14% DE ez m�r nem j�l specifik�lt minden szok�sos szignifikanciaszinten. 

modell_proba_5 <-lm(log(NetUsePerDay_Minutes) ~ log(PoliticalRadioTVPerDay_Minutes+53.12) + PoliticalPartyPref + I(Education_Years^2) + Region + SecretGroupInfluenceWorldPol,data = ESS2020)
summary(modell_proba_5) # R^2: 9,46%, korrig�lt R^2: 8,84%. Az el�z�h�z k�pest rosszabb lesz a p-�rt�ke, de 10%-on m�g szignifik�ns v�ltoz�.
resettest(modell_proba_5) # p-�rt�k: 40,4% Az el�z�h�z k�pest ez meg javult. - j�l specifik�lt

# L�that�, hogy az eredm�ny att�l f�gg, hogy milyen konstans �rt�ket adok hozz�. Ha a konstans cs�kken, akkor javul a p-�rt�k, de a modellspec. k�r�ra. Ha a konstans n� a v�ltoz� p-�rt�ke rosszabbodik(n�), de a modellspec. az javul.
# Emiatt, hogy a konsans �rt�k�t�l f�ggnek az eredm�nyek, ez�rt sim�n csak a v�ltoz�t veszem bele, logaritmiz�l�s n�lk�l.
# Jelenleg a modell_proba_2 a legjobb sz�momra.
# Most a 2 interakci� vizsg�lata:

modell_proba_6 <-lm(log(NetUsePerDay_Minutes) ~ PoliticalRadioTVPerDay_Minutes + PoliticalPartyPref + I(Education_Years^2) + Region + SecretGroupInfluenceWorldPol + PoliticalRadioTVPerDay_Minutes*COVID19-COVID19,data = ESS2020)
summary(modell_proba_6) # R^2: 9,52%, korrig�lt R^2: 8,8%. Romlik a PoliticalRadioTVPerDay_Minutes p-�rt�ke �s m�g az interakci� sem szignifik�ns v�ltoz�.
resettest(modell_proba_6) # p-�rt�k: 53,4% - j�l specifik�lt

modell_proba_7 <-lm(log(NetUsePerDay_Minutes) ~ PoliticalRadioTVPerDay_Minutes + PoliticalPartyPref + I(Education_Years^2) + Region + SecretGroupInfluenceWorldPol + PoliticalRadioTVPerDay_Minutes*ContactCOVID19-ContactCOVID19,data = ESS2020)
summary(modell_proba_7) # R^2: 9,73%, korrig�lt R^2: 9,1%. M�g jobban romlik a PoliticalRadioTVPerDay_Minutes p-�rt�ke, de az interakci� legal�bb szignifik�ns lesz.
resettest(modell_proba_7) # p-�rt�k: 16.29% - j�l specifik�lt

# �sszegz�s:
# �n a modell_proba_2-�t v�lasztom. Itt az eredm�nyv�ltoz� �s az oktat�sban t�lt�tt �vek n�gyzetre emel�se nem j�r semmilyen negat�v hat�ssal, nincsen k�ra.
# Minden m�shol valaminek a k�r�ra tudtam volna m�g interakci�val/nem-line�ris transzform�lttal b�v�teni.
# A modellem j�l specifik�lt lett �s a v�ltoz�im szignifikanci�ja javult.

# V�gs� modellem:
summary(ESS2020)
modell_2 <- modell_proba_2
summary(modell_2)
resettest(modell_2) # p-�rt�k: 25,57% - a modellem j�l specifik�lt lett. 
# A modell j�l specifik�lt minden szok�sos szignifikanciaszinten. A magyar�z�v�ltoz�k szignifikanci�ja javult vagy nem v�ltozott negat�v ir�nyba.
# �gy gondolom j�l dolgoztam �s d�nt�ttem.

# 6. F�komponens elemz�s:
# �j adatb�zis l�trehoz�sa a f�komponens elemz�shez:
ESS2020_szuk_spec <- ESS2020[,c(2:3,5:8)] 
str(ESS2020_szuk_spec)
ESS2020_szuk_spec$Education_Years <- ESS2020_szuk_spec$Education_Years^2 
# Ez m�r �gy a modell 2-es adatb�zis

library(car)
vif(modell_2)
# L�that�, hogy minden VIF-mutat� 1 k�zeli �rt�ket vesz fel. Mindegyik b�ven kisebb, mint 1,1.
# K�ros multikollinearit�s hat�r�t ahogyan tanultuk, sokan sokf�lek�ppen �llap�tj�k meg.
# �n a HTML f�jlokban is eml�tett VIF > 5 vagy VIF > 10 �rt�kekn�l tekintek valamit k�ros multikollinearit�snak.
# L�that� az �n esetemben, hogy mindegy melyik hat�rral vizsg�ln�m (szigor�an/kev�sb� szigor�an), egyik esetben sem lenne sz� k�ros multikollinearit�sr�l.

# Nincsen �rtelme a f�komponens elemz�snek. Ez l�that� a VIF mutat�kb�l is. De megcsin�lom bemutat�s p�ld�jak�nt, hogy nincsen �rtelme a f�komponens elemz�snek.
str(ESS2020)
teljes_fokomponens_elemzes <- prcomp(ESS2020_szuk_spec[,c(1,4)], center = TRUE, scale=TRUE)
str(teljes_fokomponens_elemzes)
summary(teljes_fokomponens_elemzes)

ESS2020_szuk_spec <- cbind(ESS2020_szuk_spec, teljes_fokomponens_elemzes$x[,1:2])
str(ESS2020_szuk_spec)
KorrelMatrix <- cor(ESS2020_szuk_spec[,-c(2,3,5,6)])
library(corrplot)
corrplot(KorrelMatrix, method="color")
# Nem kell f�komponenseket alkalmazni.
# Nincsenek f�komponenseim, mert nem kell bevennem �ket a modellbe.
# Nincsen multikollinearit�s, ez�rt nincs sz�ks�g mit kezelni.

# 7. Heteroszkedaszticit�s: 
ESS2020$HibatagNegyzet <- modell_2$residuals^2
ggplot(ESS2020, aes(x=NetUsePerDay_Minutes, y=HibatagNegyzet)) + geom_point()
# Itt fenn�ll a heteroszkedaszticit�s jelens�ge.

# White-teszt: 
# H0: a modell�nkben nincsen heteroszkedaszticit�s, azaz a hibatagok homoszkedasztikusak
# H1: a modell�nkben van heteroszkedaszticit�s, azaz a hibatagok heteroszkedasztikusak.
# install.packages('skedastic')
library(skedastic)
white_teszt <- white(modell_2, interactions = FALSE)
white_teszt$p.value
# A White-teszt alapj�n a modell_2 az nem heteroszkedasztikus. Ez �rdekes. Ez ellentmond az �br�nak.
white_teszt <- white(modell_2, interactions = TRUE)
white_teszt$p.value # Ez alapj�n is a modell_2 az nem heteroszkedasztikus.

# Breusch-Pagan teszt:
library(lmtest)
bptest(modell_2, studentize = TRUE)
# Ez alapj�n a teszt alapj�n a p-�rt�k 4,26%.
# Sz�val H0-t nem tudom elutas�tani minden szok�sos szignifikanciaszinten. Alfa=1%-os szignifikanciaszintn�l a modell heteroszkedasztikus.

# Kolmogorov-Smirnov teszt: a Hibatag eloszl�s�nak vizsg�lat�ra
# H0: a hibatag norm�lis eloszl�s�
# H1: a hibatag nem norm�lis eloszl�s�
ks.test(modell_2$residuals, "pnorm")
# A p-�rt�k nagyon alacsony, sz�val a H0-t elutas�tjuk �s a hibatag nem k�vet norm�lis eloszl�st.
hist(modell_2$residuals)
# Hisztogrammon is l�that� a hibatagok nem k�vetnek norm�lis eloszl�st.. 
# A Breusch-Pagan Koenker korrekci�s verzi�ja fogja visszaadni a j� megold�st.

exp(0.0002312)

