#################################################
# Zailtasuna non?                               #
# EUDIA 7 (2018/06/08) Workshopean              #
# aurkeztutako komunikazioko datu eta scriptak  #
#               Juan Abasolo                    #
#       mailto:juan.abasolo@ehu.eus             #                    
#################################################

# Kargatu datuak
library(readr)
datuakE7 <- read_csv("datuakE7.csv", na = "NA")


library(vcd)

######## Aditz nagusia baiezko esaldietan #######

# Sortu azpi data.frame bat baiezko esaldietarako
x <- subset(datuakE7, EZEZKOESALDIA==0)

# Sortu taula bat aditz nagusi eta fonazio luzatua (0)
# aldagaiekin
tx1 <- table(x$AD.NG, x$ZA.LUZ0)

dimnames(tx1) <- list(FonazioLuzatua = c("ez", "bai"),
                      AditzNagusia = c("ez", "bai"))
# Mosaic plot chi karratuaren ondarrak nabarmenduta
mosaic(tx1,
       gp = shading_hsv, 
       main = "Baiezko esaldietan")

####### Aditz nagusia ezezko esaldietan #########

# Sortu azpi data.frame bat ezetzko esaldietarako
y <- subset(datuakE7, EZEZKOESALDIA==1)

# Sortu taula bat ezezko esaldietako aditz nagusi eta
# fonazio luzatua (0)
ty1 <- table(y$AD.NG, y$ZA.LUZ0)

dimnames(ty1) <- list(FonazioLuzatua = c("ez", "bai"),
                      AditzNagusia = c("ez", "bai"))

# Mosaic plot chi karratuaren ondarrak nabarmenduta
mosaic(ty1,
       gp = shading_hsv, 
       main = "Ezezko esaldietan")


########Alderatu adizki trinkoak eta laguntzaileak###########

# Sortu taula bat adizki trinkoak eta fonazio
# luzatua aurreko berban
ttrinluz1 <- table(datuakE7$AD.TRINKOA, 
                   datuakE7$ZA.LUZ1)
dimnames(ttrinluz1) <- list(AurrekoaLuzatua = c("ez", "bai"),
                            AditzTrinkoa = c("ez", "bai"))

# Mosaic plot sortu chi karratuaren ondarrak nabarmenduta
mosaic(ttrinluz1, 
       gp = shading_hsv)

# Sortu taula bat adizki laguntzaileak eta fonazio
# luzatua aurreko berban
tlagluz1 <- table(datuakE7$AD.LAGUNTZAILEA, datuakE7$ZA.LUZ1)
dimnames(tlagluz1) <- list(AurrekoaLuzatua = c("ez", "bai"),
                           AditzLaguntzailea = c("ez", "bai"))
# Mosaic plot sortu chi karratuaren ondarrak nabarmenduta
mosaic(tlagluz1,
       gp = shading_hsv)


######## Alderatu nagusietako aspektu markak ###########

# Sortu taula bat adizki trinkoak eta fonazio
# luzatua aurreko berban
tadnagaspluz0 <- table(datuakE7$AD.NG.Q.ASP,datuakE7$ZA.LUZ0)[-3,]
dimnames(tadnagaspluz0) <- list(Aspektua = c("asp.gabekoa", "burutua", 
                                             "ez burutua", "geroa"),
                                FonazioLuzatua = c("ez", "bai"))

# Chi karratua testa
chisq.test(tadnagaspluz0)

# Mosaic plot sortu, chi karratuko hondarrak nabarmenduta, baleude.
mosaic(tadnagaspluz0, gp = shading_hcl, split_vertical = T, 
       main = "Fonazio luzatua eta aspektua")