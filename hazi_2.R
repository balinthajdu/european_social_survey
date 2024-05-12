# 0. Feladat: Working Directory beállítása
setwd('C:/Users/Bálint/OneDrive/Dokumentumok/BCE 2023-24/Ökonometria/Házi2')

# A kommentek ISO-8859-2 kódolással olvashatóak!!!

# 1. Feladat: Adatok beolvasása R-be, hiányzó adatok (NA-k) kiszûrése és változók átalakítása:
library(readxl)
ESS2020 <- read_excel("2. házi feladat adatbázisok.xlsx", sheet = 'ESS2020')
str(ESS2020)
View(ESS2020)

# Az utolsó 3 oszlop nem kell, mert a magyarázatot tartalmazza + Data Frame-é alakítás:
ESS2020 <- as.data.frame(ESS2020[,1:12]) # Adatbázis felülírása.
View(ESS2020)
str(ESS2020)

# Hiányzó (NA) adatok kiszûrése:
sapply(ESS2020, function(x) sum(is.na(x))) # Hiányzó adatok kimutatása változókként.
sum(is.na(ESS2020)) # Összesen 26 hiányzó (NA) adat.
ESS2020 <- ESS2020[complete.cases(ESS2020),] # A 26 NA-t tartalmazó sor/megfigyelés kiszûrése.
sum(is.na(ESS2020)) # Ellenõrzés.
table(ESS2020$TrustInParlament)
summary(ESS2020)
# Minõségi változók átalakítása (Logikai és Faktor):
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

str(ESS2020) # Ellenõrzés.
summary(ESS2020)

# 2. Feladat: Outlierszûrés
1073*0.01 # Ha 1% adat kiszûrhetõ, akkor maximum 10-11 megfigyelést szûrhetek ki
1073*0.03 # Ha a 2-3%-os maximálisan elfogadható adatot szûrném ki, akkor 32-33 adat szûrhetõ ki. Ez már a legmaximálisabb tûréshatár.
# Ennél több adat kiszûrése nagyon nyomós indok esetén csak.

summary(ESS2020)
# Ránézésre vannak elég érdekes mindhárom mennyiségi változónál. Ezeket megvizsgálom egyenként is.

# PoliticalRadioTVPerDay_Minutes
boxplot(ESS2020$PoliticalRadioTVPerDay_Minutes)
head(sort(ESS2020$PoliticalRadioTVPerDay_Minutes), 30)
tail(sort(ESS2020$PoliticalRadioTVPerDay_Minutes), 90)
# Elsõ gondolkodásra ezek az adatok relevánsnak tekinthetõek, mert benne vannak a 24 órás napi limitben(24*60=1440 perc). Az összes adat ezen korlát alatt van.
# Viszont, ha jobban megvizsgálom, akkor vannak elég érdekes eredmények is. Vannak olyan emberek, akik 15+ órát hallgatnak politikai rádiót.
# Ez egy kicsit irrelevánsan tud hangzani. Feltételezzük, hogy egy átlagos embernek 8-10 óra szükséges alvásra. Tegyük fel, hogy a maradék idõt maximálisan kihasználja, amíg ébren van és egész nap a politikai rádiót hallgatja.
# Ez már hihetõbb és relevánsnak mondható szerintem. A boxploton látszik, hogy 800-nál van egy kis törés, ahol nincsenek adatok. Ezeknél az adatoknál húzom meg a határt.
# Viszont 800/600= ~13,3 óra, ezért a kerekítés és a szép egész szám miatt 14 órás határt húzok meg (Így belefér a 8-10 óra alvás is. Szuper.) 
# Ez a 14 órás határ 840 percet jelent, szóval itt a 840 perc fölötti adatokat kiszûröm.
# A 0 értékekbõl 111 van. Ezt két okból nem szûröm ki: ez rengeteg adat, de a legfontosabb, hogy ezek relevánsak és értelmezhetõek, mert rengetegen nem hallgatnak politikai rádiót.
sum(ESS2020$PoliticalRadioTVPerDay_Minutes == 0) # 111-en nem hallgatnak semmilyen politikai rádiót egy percig sem 
sum(ESS2020$PoliticalRadioTVPerDay_Minutes > 840) # 27 adat van 840 perc fölött. Ezeket fogom kidobni az imént említett okok miatt.
# Összegzés: A boxploton látottak alapján felfelé sok a kilógó érték, itt a feljebb említett okoknál fogva csak a 840-nél kisebb értékeket tartom meg. Lefelé nincsen kilógó érték és a 0 is teljesen racionálisan értelmezhetõ és nem kell kiszûrni.
ESS2020 <- ESS2020[ESS2020$PoliticalRadioTVPerDay_Minutes < 840,] 
# Ezzel 1046-ra csökkent az adatok száma.

# NetUsePerDay_Minutes
boxplot(ESS2020$NetUsePerDay_Minutes)
head(sort(ESS2020$NetUsePerDay_Minutes), 30)
tail(sort(ESS2020$NetUsePerDay_Minutes), 30)
# Ismételten az mondható el, mint az elõzõ magyarázóváltozónál.
# Elsõ gondolkodásra az értékek relevánsak, mert benne vannak a 24 órás/1440 perces napi korlátban.
# Viszont itt is, ha jobban megvizsgálom, akkor itt is vannak érdekes, irracionális értékek. Tehát itt is nehéz elképzelni a 15 óra(900 perc) feletti értékeket.
# Itt is tegyük fel, hogy egy átlagos embernek 8-10 óra szükséges alvásra és a maradék idõt maximálisan kihasználja és az internetet használja addig.
# A boxploton itt is látszik egy törés 800 percnél, ahol nincsenek adatok. Ezért ezen okok mentén itt ismételten a 14 órás (840 perces) határt húznám meg. 
# A 840 perc feletti értékeket kiszûröm. Ez 7 darab adatot jelent.
# Lefelé nincsenek kilógó értékek, viszont vannak alacsony értékek: pl.: 0,2,10. Ezek az értékek teljesen racionálisak, mert vannak olyan emberek, akik valóban keveset használják naponta az internetet.
# Egy lépést viszont most tennék meg, amely a házi feladatom késõbbi szakaszában lesz releváns. Kicsi spoiler. :)
# A 0 értéket kiszûrném, mert az eredményváltozómat logaritmizálni fogom és a 0 érték nem tenné ezt nekem lehetõvé úgy, hogy dolgozni tudjak a modellel.
# Ezt a problémát úgy is tudnám kezelni, hogy hozzáadok egy konstanst a NetUsePerDay_Minutes értékeihez, viszont mivel csak 1 darab 0 érték van, ezt az egyszerûség kedvéért inkább kiszûröm.
# DE ha a konstans hozzáadás módszert választottam volna, akkor így nézett volna ki az R-kód: log(NetUsePerDay_Minutes+1).
sum(ESS2020$NetUsePerDay_Minutes > 840) # 7 darab megfigyelés van 840 perc fölött.
# Összegzés: A boxploton látottak alapján itt is felfelé vannak kilógó értékek. Az imént említett okok miatt a felsõ határt a 840 percnél húztam meg, az alsó határt pedig 0 percnél.
# Ez összesen 7+1= 8 darab adat kiszûrését jelenti.
ESS2020 <- ESS2020[ESS2020$NetUsePerDay_Minutes < 840 & ESS2020$NetUsePerDay_Minutes > 0,] 
# Ezzel 1038-ra csökkent a megfigyelések száma.

# Education_Years
boxplot(ESS2020$Education_Years)
head(sort(ESS2020$Education_Years), 30)
tail(sort(ESS2020$Education_Years), 30)
# Itt is ellehet mondani, hogy a boxplotra és a adatokra ránézve elég érdekes eredmények születtek.
# Van néhány alacony érték, ami azt jelenti, hogy valaki még a 8 általános évet sem végezte el. Ez a gondolatmenet egy darabig lehet racionális, viszont valahol én úgy gondolom kell húzni egy határt.
# A 4 oktatási év azt jelenti, hogy valaki elkezdi 6 évesen az iskolát és 10 évesen abbahagyja. Ezt nehéz elképzelni.
# A boxploton ez lefelé kilógó értékként látszik. Ezen indokok miatt ezt a 2 megfigyelést én itt leszûrném.
# Tovább gondolkodtam és vizsgáltam a 8 év alatti értékeket. A 6 és 7 éveket én racionálisabban fel tudom dolgozni.
# Például azt már könnyebben el tudom képzelni, hogy valaki 6 év oktatás után hagyja ott az iskolát 12 évesen ilyen vagy olyan egyéb indokok miatt. Ez is egy nagyon alacsony érték, de ez már számomra elfogadhatóbb.
# Itt ezen indokok miatt tehát az alsó határt a 6 évnél húzom meg. Tehát Education_Years >= 6
sum(ESS2020$Education_Years < 6) # 2 megfigyelés van 6 oktatási év alatt.
# Így eddig jelenleg 2 adatot szûrök ki.
# Felfelé is vannak érdekesebb adatok, de ezek számomra relevánsak. A 20-30 oktatási évet én azzal hihetõnek tudom magyarázni, hogy valaki elvégzi a 12 osztályz utána pedig akár 2 BSc diplomát és 1 MSc diplomát tesz le.
# Vagy akár orvosnak jogásznak tanul és mellette más tanulmányokat is elvégez. Pl.: doktori cím megszerzése stb.
# Viszont ezek ellenére a boxploton látottak alapján is van egy kilógó érték a 40 év. Úgy gondolom ez már nagyon torzítaná a modellt és kicsit nehezebb is tudom ezt már feldolgozni.
# Az még hihetõ és racionális volt számomra, hogy az indokok alapján valaki a 26-36 életévei között fejezi be a tanulmányait (Tegyük fel, hogy 6 évesen kezdte el az 1. osztályt és nem bukott.).
# Az viszont már számomra irracionális, hogy valaki 40 éven keresztül tanul és 46 éves korára fejezi be a tanulmányait (Itt is feltéve, hogy 6 éves korától oktatásban részesül.)
# Szóval ezen gondolkodásmenet mentén én itt a 30 évnél húznám meg a felsõ határt, szóval: Education_Years <= 30
sum(ESS2020$Education_Years >30) # 5 megfigyelés van 30 oktatási év felett.
# Így ezzel már 2+5=7 adatot szûrök ki.
# Összegzés: A boxplot és a saját gondolatmenetem és indokok alapján én összesen az alsó és felsõ korlátokkal 7 darab adatot szûrnék ki ezen változó mentén.
ESS2020 <- ESS2020[ESS2020$Education_Years >= 6 & ESS2020$Education_Years <=30,]
# Így véglegesen 1031 adatom maradt.

# Outlierszûrés összegzése:
# Az outliervizsgálatot a 3 mennyiségi változó mentén végeztem.
# Ezek között 27+8+7=42 adatot szûrtem ki. Ez sajnos meghaladja mind az 1%, mind a 2-3%-os maximálisan leszûrhetõ megfigyelések számát.
# Viszont én úgy gondolom, hogy itt létfontosságú volt ezen adatok leszûrése, hogy ne kapjak torz becsléseket és eredményeket.
# Én úgy gondolom, hogy a gondolatmenetem és az indoklásaim ésszerûek és relevánsak voltak és hiába léptem át a maximálisan tolerálható limitet az outlierszûrésnél, így is jogosan tettem ezt.
# A boxplotokra utólág ránézve látható, hogy jól határoztam meg a határokat, ahol kiszûrtem az adatokat.
# Összesen a kezdeti adatbázisom 1073 megfigyeléssel rendelkezett.(Ebbõl 26 megfigyelést hiányzó értékek miatt már kiszûrtem). 42 megfigyelést ebbõl outlierszûrés miatt távolítottam el.
# Ezen mentén 1031-re csökkentettem az adatbázisom, amellyel dolgozni fogok. Ami azt jelenti, hogy az adatbázisom 96,1%-át megtartottam és 3,9%-át hagytam el outlierek miatt.
1031/1073 # 96,1%

# Leíro statisztika a mennyiségi változókra:
library(psych)
leirostat_1 <- describe(ESS2020[c(2,3,6)])
leirostat_2 <- as.data.frame(summary(ESS2020[c(2,3,6)]))

library(writexl)
write_xlsx(leirostat_1, 'Leirostat_1.xlsx')
write_xlsx(leirostat_2, 'Leirostat_2.xlsx')


# Korrelációs mátrix: 
library(corrplot)
KorrelMatrix <- cor(ESS2020[,c(2,3,6)])
corrplot(KorrelMatrix, method = 'color')
# Nagyon gyenge kapcsolatok mindhárom |r| < 0,1
# Napi internethasználat ~ Politikai rádió: nagyon gyenge pozitív/egyirányú kapcsolat
# Napi internethasználat ~ Oktatási évek száma: nagyon gyenge pozitív/egyirányú kapcsolat
# Oktatási évek száma ~ Politikai rádió: nagyon gyenge negatív/ellentétes kapcsolat

# 3. Többváltozós lineáris regresszió és modellspecifikáció tesztelése
# A faktoroknál a viszonyítási alap átállítása, ha szükséges:
levels(ESS2020$PoliticalPartyPref) # Itt az egyéb a viszonyítási alap. Ezt átállítom Fidesz-KDNP-re, mivel ez a párt van jelenleg hatalmon és ezért nekem ésszerû ehhez hasonlítani. Mellesleg ezt könnyeben lehet értelmezni, mint az Egyéb kategóriát.
ESS2020$PoliticalPartyPref <- relevel(ESS2020$PoliticalPartyPref, ref = 'Fidesz-KDNP')
levels(ESS2020$Region) # Itt a Budapest/Pest kategória a viszonyítási alap. Ez nekem megfelel, mivel ez Magyarország központja és általában ehhez szokták viszonyítani a településeket/városokat/megyéket.

# Lineáris regresszió:
modell_1 <- lm(NetUsePerDay_Minutes ~ .-id, data = ESS2020)
summary(modell_1)
# Húha. Ez egy elég gyenge magyarázóerejû modell. Az eredményváltozóban lévõ információnak csak a 9,51%-át magyarázza a modell.
# Gyenge magyarázóerejû modell.
# Az R^2 az 9,51%. A korrigált R^2 az pedig 8,54%. 
# A Globális F-próba p-értéke nagyon alacsony, ez azt jelenti, hogy minden szokásos szignifikanciaszinten elutasítjuk a H0-t.
# Ez azt jelenti, hogy nem csak a való életben, hanem a mintában is gyengén magyaázzák a változók a napi internethasználatot. / A modell kiterjeszthetõ a sokaságra.
# Amikor ránéztem az excel adatbázisra, akkor õszintén elcsodálkoztam az adatokon és azon, hogy tartalmilag mennyire eltérõ változókat tartalmaz.
# Nem számítottam nagy magyarázóerõre, maximum 10-15%-os R^2-et vártam, azt sejtettem, hogy talán éppen megüti a közepes magyarázóerõ határát(,ami 10%-tól kezdõdik).
# Az eredmények majdnem elérték a határt, de végül nem tudták meglépni ezt. Elsõre nagyon alacsonynak tûnhet az eredmény, de egyáltalán nem meglepõ.
# Ha végignézzük a magyarázóváltozókat, úgy gondolom, hogy egyáltalán nem meglepõek a kapott eredmények.

# Modellspecifikáció tesztelése:
library(lmtest)
resettest(modell_1)
# Hipotézisvizsgálat: H0: modell jól specifikált, H1: modell nem jól specifikált
# A p-érték  5,38%, szóval H0-t nem utasítjuk el minden szokásos szignifikanciaszinten. 10%-on még elutasítjuk, de Alfa= 5% és 1%-os szignifikanciaszinten még elfogadjuk.
# Ez azt jelenti, hogy a modell nem minden szokásos szignifikanciaszinten nmondható el, hogy jól specifikált.

# Összegzés:
# A modell gyenge magyarázóerejû a mintában és a sokaságban is. 
# A modellre nem minden szokásos szignifikanciaszinten mondható el, hogy jól specifikált.
modell_1_szukitett <-lm(NetUsePerDay_Minutes ~ PoliticalRadioTVPerDay_Minutes + PoliticalPartyPref + Education_Years + Region + SecretGroupInfluenceWorldPol,data = ESS2020)
AIC(modell_1, modell_1_szukitett)
BIC(modell_1, modell_1_szukitett)
anova(modell_1, modell_1_szukitett)
# 4. Új tagok keresése a modellbe:
# A mennyiségi változók eloszlásának a vizsgálata. Hisztogrammok vizsgálata.
# Az eredményváltozóm: NetUsePerDay_Minutes
hist(ESS2020$NetUsePerDay_Minutes) # Kicsit balra ferde jobbra elnyúló.
hist(log(ESS2020$NetUsePerDay_Minutes)) # Kicsit jobbra ferde, talán jobb mint az eredeti.
# Nagy minta, de jobb, ha az eredményváltozó normális eloszlást követ. Ezt ennek érdekében inkább logaritmizálnám.

# Az egyik mennyiségi magyarázóváltozóm: 
hist(ESS2020$PoliticalRadioTVPerDay_Minutes) # Erõsen balra ferde, jobbra elnyúló. 
hist(log(ESS2020$PoliticalRadioTVPerDay_Minutes)) # Ha logaritmizálom ez megjavul.
# A modellben vehetem ennek a logaritmázáltját.

# A másik mennyiségi magyarázóváltozóm: Education_Years
hist(ESS2020$Education_Years) # Nem erõsen balra ferde. Éveknél általában a kvadratikus tag a jó. De megnézem a logaritmust is azért itt is.
hist(log(ESS2020$Education_Years)) # A logaritmus egy normálishoz közelebb eloszlást ad.
# Ezt lehet logaritmizálni, de szerintem a négyzetes tag az jobb lesz ebbõl, mert évszám. Ezt majd megvizsgálom.

# További változópárok közötti kapcsolatok vizsgálata (Interakciók keresése):
library(ggplot2)
#
ggplot(data = ESS2020, aes (x = PoliticalRadioTVPerDay_Minutes, y = log(NetUsePerDay_Minutes), color = Region)) +
  geom_point() +
  stat_smooth(method=lm) # Itt csak a tenelymetszet különbözik, a meredekségek nem, szinte párhuzamosak. Itt nem kell interakció.

ggplot(data = ESS2020, aes(x = PoliticalRadioTVPerDay_Minutes, y = log(NetUsePerDay_Minutes), color = TrustInParlament)) +
  geom_point() +
  stat_smooth(method=lm) ### Itt nincs nagy különbség a meredekségben. Itt nem kellene interakció.

ggplot(data = ESS2020, aes(x = PoliticalRadioTVPerDay_Minutes, y = log(NetUsePerDay_Minutes), color = PoliticalPartyPref)) +
  geom_point() +
  stat_smooth(method=lm) # Vannak kicsit meredekebb egyenesek. Itt lehet, de nem muszáj interakció.

ggplot(data = ESS2020, aes(x = PoliticalRadioTVPerDay_Minutes, y = log(NetUsePerDay_Minutes), color = SecretGroupInfluenceWorldPol)) +
  geom_point() +
  stat_smooth(method=lm) # Más a tengelymetszet, de párhuzamosak az egyenesek. Nem kell interakció.

ggplot(data = ESS2020, aes(x = PoliticalRadioTVPerDay_Minutes, y = log(NetUsePerDay_Minutes), color = ScientistsDecievePublic )) +
  geom_point() +
  stat_smooth(method=lm) # Nagyon minimális a meredekségkülönbség az elején, aztán egymásba torkollik a 2 egyenes. Nem kell interakció.

ggplot(data = ESS2020, aes(x = PoliticalRadioTVPerDay_Minutes, y = log(NetUsePerDay_Minutes), color = COVID19)) +
  geom_point() +
  stat_smooth(method=lm) # Valóban látványos a meredekégkülönbség, de ez csak a kilógó értékek miatt van. Lehet interakció.

ggplot(data = ESS2020, aes(x = PoliticalRadioTVPerDay_Minutes, y = log(NetUsePerDay_Minutes), color = ContactCOVID19)) +
  geom_point() +
  stat_smooth(method=lm) # Valóban látványos a meredekégkülönbség, de ez csak a kilógó értékek miatt van. Lehet interakció.

ggplot(data = ESS2020, aes(x = PoliticalRadioTVPerDay_Minutes, y = log(NetUsePerDay_Minutes), color = GetVaccince)) +
  geom_point() +
  stat_smooth(method=lm) # Itt sincs külöbség a meredekségben. Nem kell interakció.

#
ggplot(data = ESS2020, aes(x = Education_Years, y = log(NetUsePerDay_Minutes), color = Region)) +
  geom_point() +
  stat_smooth(method=lm) # Nincs nagy különbség a meredekségben. Meglepõ. Itt nem kell interakció.

ggplot(data = ESS2020, aes(x = Education_Years, y = log(NetUsePerDay_Minutes), color = TrustInParlament)) +
  geom_point() +
  stat_smooth(method=lm) # A meredekség nem különbözik, szinte párhuzamosak az egyenesek. Nem lehet interakció.

ggplot(data = ESS2020, aes(x = Education_Years, y = log(NetUsePerDay_Minutes), color = PoliticalPartyPref)) +
  geom_point() +
  stat_smooth(method=lm) # Az egyenesek nem párhuzamosak ugyan, de nincsen drasztikus különbség a meredekségek között. # Itt nem vennék be interakciót, de ez preferencia kérdése. 

ggplot(data = ESS2020, aes(x = Education_Years, y = log(NetUsePerDay_Minutes), color = SecretGroupInfluenceWorldPol)) +
  geom_point() +
  stat_smooth(method=lm) # Nincs nagy különbség a meredekségben. Nem kell interakció.

ggplot(data = ESS2020, aes(x = Education_Years, y = log(NetUsePerDay_Minutes), color = ScientistsDecievePublic )) +
  geom_point() +
  stat_smooth(method=lm) # Ismét minimális a meredekségkülönbség. Nem kell interakció.

ggplot(data = ESS2020, aes(x = Education_Years, y = log(NetUsePerDay_Minutes), color = COVID19)) +
  geom_point() +
  stat_smooth(method=lm) # Nem érzem drasztikusnak a 2 meredekség különbségét. Nem kell interakció.

ggplot(data = ESS2020, aes(x = Education_Years, y = log(NetUsePerDay_Minutes), color = ContactCOVID19)) +
  geom_point() +
  stat_smooth(method=lm) # Szinte párhuzamos a 2 egyenes. Nem kell interakció.

ggplot(data = ESS2020, aes(x = Education_Years, y = log(NetUsePerDay_Minutes), color = GetVaccince)) +
  geom_point() +
  stat_smooth(method=lm) # Itt sincs külöbség a meredekségben, szinte egymást fedi le a 2 egyenes. Nem kell interakció.

str(ESS2020)
# További változópárok közötti kapcsolatok vizsgálata (Nem-lineáris transzformáltak keresése):

# Logaritmizálás kell-e vagy nem (Inkább hisztogramm alapján) : 
# Education_Years
ggplot(data = ESS2020, aes(x = Education_Years, y = log(NetUsePerDay_Minutes))) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") # Így néz ki a kapcsolat jelenleg.

ggplot(data = ESS2020, aes(x = log(Education_Years), y = log(NetUsePerDay_Minutes))) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") # Valóban nem rossz a logaritmizálás.

ggplot(data = ESS2020, aes(x = I(Education_Years^2), y = log(NetUsePerDay_Minutes))) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red") # A 30-as kilógó érték miatt lehet jobbnak látszik a logaritmizálás, de a négyzetre emelés az elején szerintem jobban illeszkedik ránézésre. 

# PoliticalRadioTVPerDay_Minutes
ggplot(data = ESS2020, aes(x = PoliticalRadioTVPerDay_Minutes, y = log(NetUsePerDay_Minutes))) +
  geom_point() +
  stat_smooth() +
  stat_smooth(color="red") # Ez a jelenlegi kapcsolat.

ggplot(data = ESS2020, aes(x = log(PoliticalRadioTVPerDay_Minutes+1), y = log(NetUsePerDay_Minutes))) +
  geom_point() +
  stat_smooth() +
  stat_smooth(color="red") # Logaritmizálás 1-gyel. 

ggplot(data = ESS2020, aes(x = log(PoliticalRadioTVPerDay_Minutes+0.01), y = log(NetUsePerDay_Minutes))) +
  geom_point() +
  stat_smooth() +
  stat_smooth(color="red") # Logaritmizálás egy 1-nél kisebb konstanssal

ggplot(data = ESS2020, aes(x = log(PoliticalRadioTVPerDay_Minutes+53.12), y = log(NetUsePerDay_Minutes))) +
  geom_point() +
  stat_smooth() +
  stat_smooth(color="red") # Logaritmizálás 53.12-vel, ami az átlaga a változónak. 
# Következtetés: az ábrán is látszik, hogy nagyon sok múlik azon, hogy mi a konstans. Inkozisztens eredmények. Konstans értékétõl függõen más eredmény. 
# Mi alapján döntsem el, hogy mi a jó konstans értéke? Nem tudom ezt meghatározni. Ezért tanácsos lenne kihagyni.

# Az eredmények tesztelése (megnézni, hogy melyik kombinációkkal kapom a számomra legjobb modellt, ami a modell_2 lesz.):

modell_proba_1 <-lm(log(NetUsePerDay_Minutes) ~ PoliticalRadioTVPerDay_Minutes + PoliticalPartyPref + log(Education_Years) + Region + SecretGroupInfluenceWorldPol,data = ESS2020)
summary(modell_proba_1) # R^2: 9,22%, korrigált R^2: 8,6%
resettest(modell_proba_1) # p-érték: 17,2%

modell_proba_2 <-lm(log(NetUsePerDay_Minutes) ~ PoliticalRadioTVPerDay_Minutes + PoliticalPartyPref + I(Education_Years^2) + Region + SecretGroupInfluenceWorldPol,data = ESS2020)
summary(modell_proba_2) # R^2: 9,32%, korrigált R^2: 8,7%, Kvadratikus tagként javul az Education_Years p-értéke, még jobban szignifikánsabb lesz.
resettest(modell_proba_2) # p-érték: 25,57% -jól specifikált

modell_proba_3 <-lm(log(NetUsePerDay_Minutes) ~ log(PoliticalRadioTVPerDay_Minutes+1) + PoliticalPartyPref + I(Education_Years^2) + Region + SecretGroupInfluenceWorldPol,data = ESS2020)
summary(modell_proba_3) # R^2: 9,26%, korrigált R^2: 8,64%, Romlik a p-értéke, kb 2-szer nagyobb lesz.
resettest(modell_proba_3) # p-érték: 14,77% - jól specifikált

modell_proba_4 <-lm(log(NetUsePerDay_Minutes) ~ log(PoliticalRadioTVPerDay_Minutes+0.01) + PoliticalPartyPref + I(Education_Years^2) + Region + SecretGroupInfluenceWorldPol,data = ESS2020)
summary(modell_proba_4) # R^2: 9,69%, korrigált R^2: 9,1%. Javul a PoliticalRadioTVPerDay_Minutes p-értéke, már 5%-on szignifikáns lesz.
resettest(modell_proba_4) # p-érték: 4,14% DE ez már nem jól specifikált minden szokásos szignifikanciaszinten. 

modell_proba_5 <-lm(log(NetUsePerDay_Minutes) ~ log(PoliticalRadioTVPerDay_Minutes+53.12) + PoliticalPartyPref + I(Education_Years^2) + Region + SecretGroupInfluenceWorldPol,data = ESS2020)
summary(modell_proba_5) # R^2: 9,46%, korrigált R^2: 8,84%. Az elõzõhöz képest rosszabb lesz a p-értéke, de 10%-on még szignifikáns változó.
resettest(modell_proba_5) # p-érték: 40,4% Az elõzõhöz képest ez meg javult. - jól specifikált

# Látható, hogy az eredmény attól függ, hogy milyen konstans értéket adok hozzá. Ha a konstans csökken, akkor javul a p-érték, de a modellspec. kárára. Ha a konstans nõ a változó p-értéke rosszabbodik(nõ), de a modellspec. az javul.
# Emiatt, hogy a konsans értékátõl függnek az eredmények, ezért simán csak a változót veszem bele, logaritmizálás nélkül.
# Jelenleg a modell_proba_2 a legjobb számomra.
# Most a 2 interakció vizsgálata:

modell_proba_6 <-lm(log(NetUsePerDay_Minutes) ~ PoliticalRadioTVPerDay_Minutes + PoliticalPartyPref + I(Education_Years^2) + Region + SecretGroupInfluenceWorldPol + PoliticalRadioTVPerDay_Minutes*COVID19-COVID19,data = ESS2020)
summary(modell_proba_6) # R^2: 9,52%, korrigált R^2: 8,8%. Romlik a PoliticalRadioTVPerDay_Minutes p-értéke és még az interakció sem szignifikáns változó.
resettest(modell_proba_6) # p-érték: 53,4% - jól specifikált

modell_proba_7 <-lm(log(NetUsePerDay_Minutes) ~ PoliticalRadioTVPerDay_Minutes + PoliticalPartyPref + I(Education_Years^2) + Region + SecretGroupInfluenceWorldPol + PoliticalRadioTVPerDay_Minutes*ContactCOVID19-ContactCOVID19,data = ESS2020)
summary(modell_proba_7) # R^2: 9,73%, korrigált R^2: 9,1%. Még jobban romlik a PoliticalRadioTVPerDay_Minutes p-értéke, de az interakció legalább szignifikáns lesz.
resettest(modell_proba_7) # p-érték: 16.29% - jól specifikált

# Összegzés:
# Én a modell_proba_2-õt választom. Itt az eredményváltozó és az oktatásban töltött évek négyzetre emelése nem jár semmilyen negatív hatással, nincsen kára.
# Minden máshol valaminek a kárára tudtam volna még interakcióval/nem-lineáris transzformálttal bõvíteni.
# A modellem jól specifikált lett és a változóim szignifikanciája javult.

# Végsõ modellem:
summary(ESS2020)
modell_2 <- modell_proba_2
summary(modell_2)
resettest(modell_2) # p-érték: 25,57% - a modellem jól specifikált lett. 
# A modell jól specifikált minden szokásos szignifikanciaszinten. A magyarázóváltozók szignifikanciája javult vagy nem változott negatív irányba.
# Úgy gondolom jól dolgoztam és döntöttem.

# 6. Fõkomponens elemzés:
# Új adatbázis létrehozása a fõkomponens elemzéshez:
ESS2020_szuk_spec <- ESS2020[,c(2:3,5:8)] 
str(ESS2020_szuk_spec)
ESS2020_szuk_spec$Education_Years <- ESS2020_szuk_spec$Education_Years^2 
# Ez már így a modell 2-es adatbázis

library(car)
vif(modell_2)
# Látható, hogy minden VIF-mutató 1 közeli értéket vesz fel. Mindegyik bõven kisebb, mint 1,1.
# Káros multikollinearitás határát ahogyan tanultuk, sokan sokféleképpen állapítják meg.
# Én a HTML fájlokban is említett VIF > 5 vagy VIF > 10 értékeknél tekintek valamit káros multikollinearitásnak.
# Látható az én esetemben, hogy mindegy melyik határral vizsgálnám (szigorúan/kevésbé szigorúan), egyik esetben sem lenne szó káros multikollinearitásról.

# Nincsen értelme a fõkomponens elemzésnek. Ez látható a VIF mutatókból is. De megcsinálom bemutatás példájaként, hogy nincsen értelme a fõkomponens elemzésnek.
str(ESS2020)
teljes_fokomponens_elemzes <- prcomp(ESS2020_szuk_spec[,c(1,4)], center = TRUE, scale=TRUE)
str(teljes_fokomponens_elemzes)
summary(teljes_fokomponens_elemzes)

ESS2020_szuk_spec <- cbind(ESS2020_szuk_spec, teljes_fokomponens_elemzes$x[,1:2])
str(ESS2020_szuk_spec)
KorrelMatrix <- cor(ESS2020_szuk_spec[,-c(2,3,5,6)])
library(corrplot)
corrplot(KorrelMatrix, method="color")
# Nem kell fõkomponenseket alkalmazni.
# Nincsenek fõkomponenseim, mert nem kell bevennem õket a modellbe.
# Nincsen multikollinearitás, ezért nincs szükség mit kezelni.

# 7. Heteroszkedaszticitás: 
ESS2020$HibatagNegyzet <- modell_2$residuals^2
ggplot(ESS2020, aes(x=NetUsePerDay_Minutes, y=HibatagNegyzet)) + geom_point()
# Itt fennáll a heteroszkedaszticitás jelensége.

# White-teszt: 
# H0: a modellünkben nincsen heteroszkedaszticitás, azaz a hibatagok homoszkedasztikusak
# H1: a modellünkben van heteroszkedaszticitás, azaz a hibatagok heteroszkedasztikusak.
# install.packages('skedastic')
library(skedastic)
white_teszt <- white(modell_2, interactions = FALSE)
white_teszt$p.value
# A White-teszt alapján a modell_2 az nem heteroszkedasztikus. Ez érdekes. Ez ellentmond az ábrának.
white_teszt <- white(modell_2, interactions = TRUE)
white_teszt$p.value # Ez alapján is a modell_2 az nem heteroszkedasztikus.

# Breusch-Pagan teszt:
library(lmtest)
bptest(modell_2, studentize = TRUE)
# Ez alapján a teszt alapján a p-érték 4,26%.
# Szóval H0-t nem tudom elutasítani minden szokásos szignifikanciaszinten. Alfa=1%-os szignifikanciaszintnél a modell heteroszkedasztikus.

# Kolmogorov-Smirnov teszt: a Hibatag eloszlásának vizsgálatára
# H0: a hibatag normális eloszlású
# H1: a hibatag nem normális eloszlású
ks.test(modell_2$residuals, "pnorm")
# A p-érték nagyon alacsony, szóval a H0-t elutasítjuk és a hibatag nem követ normális eloszlást.
hist(modell_2$residuals)
# Hisztogrammon is látható a hibatagok nem követnek normális eloszlást.. 
# A Breusch-Pagan Koenker korrekciós verziója fogja visszaadni a jó megoldást.

exp(0.0002312)

