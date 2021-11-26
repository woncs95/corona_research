install.packages('jsonlite', repos="http://cran.r-project.org/")
install.packages("readxl", repos="http://cran.r-project.org/")
install.packages('tinytex', repos="http://cran.r-project.org/")

tinytex::install_tinytex(force = TRUE)
library(tinytex)
library(readxl)

#Excel-Daten importieren 
data_xls = read_excel("Sterbefaellenvergleich 2016-2020 nach Tagen.xlsx", na="Na")
data2_xls = read_excel("D-ueberblick.xlsx", na="Na")
d_2020=as.numeric(data_xls[[2]][73:355])
d_2019=as.numeric(data_xls[[3]][73:355])
d_2018=as.numeric(data_xls[[4]][73:355])
d_2017=as.numeric(data_xls[[5]][73:355])


#Daten Sterbefaelle zwischen 2017-2020
farbe=c("red","black","darkgrey","grey")
plot(d_2020,type="l",xlab="Tage vom 13.03 bis 22.12",ylab="Sterbefaelle",col=farbe[1],ylim=c(1500,4000),main="Sterbefaellevergleich in 2017-2020 und deren Durchschnitte")
lines(d_2019,col=farbe[2])
lines(d_2018,col=farbe[3])
lines(d_2017,col=farbe[4])
legend("topright",c(expression(2020),expression(2019),expression(2018),expression(2017)),lty=c(rep(1,4),2),col=farbe,cex=0.8)

#Graphische Dartstellung (Ziel: Gibt es einen sichtbaren Unterschied zur durchschnittlichen Sterbezahl?)

m_2020=rep(mean(d_2020),length(d_2020))
m_2019=rep(mean(d_2019),length(d_2019))
m_2018=rep(mean(d_2018),length(d_2018))
m_2017=rep(mean(d_2017),length(d_2017))
lines(m_2020,col="red")
lines(m_2019,col="black")
lines(m_2018,col="darkgrey")
lines(m_2017,col="grey")

#Ist der wahre Durchschnitt auch unterschiedlich zu den letzten Jahren?

  #Normalverteilung?

  shapiro.test(d_2020)
  shapiro.test(d_2019)
  shapiro.test(d_2018)
  shapiro.test(d_2017)

  #Da es sich generell nicht um eine Normalverteilung handelt, nutze wilcox test

#Beahuptung: Mittelwerte von Sterbefaellte in 2019,2018,2017 sind groesser als 2020
wilcox.test(d_2020,d_2019,"greater",conf.level = 0.999)
wilcox.test(d_2020,d_2018,"greater",conf.level = 0.999)
wilcox.test(d_2020,d_2017,"greater",conf.level = 0.999)

#Mittelwerte der letzten drei Jahren
d_mean=rep(0,length(d_2019))
for(i in 1:length(d_2019)){
  d_mean[i]=(d_2019[i]+d_2018[i]+d_2017[i])/3
}

wilcox.test(d_2020,d_mean,"greater",conf.level=0.999)

##mit p kleiner 0.001 sind die jeweiligen Mittelwerte der Sterbefaelle von 
##2019,2018,2017 und der Mittelwert der drei Jahren signifikant kleiner als von 2020


#Gesamte kumulative Sterbefaelle in Deutschland mit Fehlerkorrektur
c_2020=data2_xls[[4]][2:284]
c_2020[4]=13
c_2020[5]=13

#kumulative Daten zu distinkiven Daten umstellen
for(i in 0:281){
  c_2020[283-i]=as.numeric(c_2020[283-i])-as.numeric(c_2020[282-i])
}

#Plot erstellen (Verhaeltnis zwischen cornabedingten Sterbefaellen und Gesamtfaellen)
plot(as.numeric(c_2020)/as.numeric(d_2020)*100,xlab="Tage 13.03-20.12",ylab="Prozent(%)",type="l",col="red",main="Verhältnis Coronabedingte Sterbefälle/Gesamtfälle")


