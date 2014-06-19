# POLITY AND ITS DERIVATIVES
# Jay Ulfelder
# 2014-05-14

# Source: Center for Systemic Peace

rm(list=ls(all=TRUE))
require(XLConnect)
polity <- readWorksheetFromFile("p4v2013.xls", sheet=1)
require(reshape)
polity <- rename(polity, c(scode = "sftgcode"))
polity <- subset(polity, select=c("sftgcode", "year", "polity", "durable", "exrec", "parcomp", "xconst"))

# Change a few country codes to match PITF standard
polity$sftgcode[polity$sftgcode=="UKG"] <- "UK "
polity$sftgcode[polity$sftgcode=="SER"] <- "SRB"
polity$sftgcode[polity$sftgcode=="MNT"] <- "MNE"
polity$sftgcode[polity$sftgcode=="GMY"] <- "GER"
polity$sftgcode[polity$sftgcode=="SSU"] <- "SSD"
polity$sftgcode[polity$sftgcode=="SDN"] <- "SUD"
polity$sftgcode[polity$sftgcode=="USR"] <- "USS"

source("countryyear.rack.maker.r")
rack <- subset(rack, select=c(country, sftgcode, year))
rack <- merge(rack, polity, all.x = TRUE)
rack <- rack[order(rack$country, rack$year),]

# TRANSFORMATIONS FOR MODELING
# Fearon & Laitin regime type (autocracy, anocracy, democracy)
rack$polcat[rack$polity >= -10 & rack$polity < -5] <- 1 
rack$polcat[rack$polity >= -5 & rack$polity <= 5] <- 2
rack$polcat[rack$polity > 5] <- 3
rack$polcat[rack$polity == -66 | rack$polity == -77 | rack$polity == -88 ] <- 7

# PITF AJPS regime types
rack$pitfcat[rack$polity==-66 | rack$polity==-77 | rack$polity==-88] <- "other"
rack$pitfcat[(rack$exrec >= 1 & rack$exrec <= 6) & (rack$parcomp == 1 | rack$parcomp == 2)] <- "A/F"
rack$pitfcat[(rack$exrec >= 1 & rack$exrec <= 6) &
  (rack$parcomp == 0 | rack$parcomp == 3 | rack$parcomp == 4 | rack$parcomp == 5)] <- "A/P"
rack$pitfcat[(rack$exrec == 7 | rack$exrec == 8) & (rack$parcomp == 1 | rack$parcomp == 2)] <- "A/P"
rack$pitfcat[rack$parcomp == 3 & (rack$exrec == 7 | rack$exrec==8)] <- "D/fact"
rack$pitfcat[rack$exrec == 8 & (rack$parcomp == 0 | rack$parcomp == 4 )] <- "D/P"
rack$pitfcat[rack$exrec == 7 & (rack$parcomp == 0 | rack$parcomp == 4 | rack$parcomp == 5)] <- "D/P"
rack$pitfcat[rack$exrec == 8 & rack$parcomp == 5] <- "D/F"

# Harff autocracy indicator
rack$autocracy <- ifelse(rack$polity <= 0 & rack$polity >= -10, 1,
  ifelse(is.na(rack$polity)==FALSE, 0, NA) )

# Dummied version of polcat for RF
rack$polcat1 <- ifelse(rack$polcat==1, 1, ifelse(is.na(rack$polcat)==TRUE, NA, 0) )
rack$polcat2 <- ifelse(rack$polcat==2, 1, ifelse(is.na(rack$polcat)==TRUE, NA, 0) )
rack$polcat3 <- ifelse(rack$polcat==3, 1, ifelse(is.na(rack$polcat)==TRUE, NA, 0) )
rack$polcat7 <- ifelse(rack$polcat==7, 1, ifelse(is.na(rack$polcat)==TRUE, NA, 0) )

# Dummied version of PITF regime type for RF
rack$pitfcat1 <- ifelse(rack$pitfcat=="A/F", 1, ifelse(is.na(rack$pitfcat)==TRUE, NA, 0) )
rack$pitfcat2 <- ifelse(rack$pitfcat=="A/P", 1, ifelse(is.na(rack$pitfcat)==TRUE, NA, 0) )
rack$pitfcat3 <- ifelse(rack$pitfcat=="D/fact", 1, ifelse(is.na(rack$pitfcat)==TRUE, NA, 0) )
rack$pitfcat4 <- ifelse(rack$pitfcat=="D/P", 1, ifelse(is.na(rack$pitfcat)==TRUE, NA, 0) )
rack$pitfcat5 <- ifelse(rack$pitfcat=="D/F", 1, ifelse(is.na(rack$pitfcat)==TRUE, NA, 0) )
rack$pitfcat6 <- ifelse(rack$pitfcat=="other", 1, ifelse(is.na(rack$pitfcat)==TRUE, NA, 0) )

# Regime stability, logged
rack$durableln <- log1p(rack$durable)

# WRITE IT OUT
write.csv(rack, "polity.csv", row.names = FALSE)

# Plots of time trends with special codes (-66, -77, -88) omitted

polsub <- subset(rack, polity > -66)

polmeans <- tapply(polsub$polity, polsub$year, mean, na.rm = TRUE)
png(file = "polity.mean.over.time.png", width=10, height=6, units='cm', bg='white', res = 150)
par(mar=c(2,2,3,1))
par(cex.axis = 0.6)
par(cex.main = 0.75)
plot(polmeans, type = "l", ylim = c(-5,5), axes = FALSE,
  lwd = 3, col = "red2", main = "World Average Polity Score, 1945-2013")
axis(1, at = c(6,16,26,36,46,56,66), labels = c("1950", "1960", "1970", "1980", "1990", "2000", "2010"),
  tick = FALSE, line = -1)
axis(2, at = c(-5,-2.5,0,2.5,5), tick = FALSE, las = 2, line = -1)
abline(h = -5, lwd = 0.5, col = "gray")
abline(h = -2.5, lwd = 0.5, col = "gray")
abline(h = 0, lwd = 0.5, col = "gray")
abline(h = 2.5, lwd = 0.5, col = "gray")
abline(h = 5, lwd = 0.5, col = "gray")
dev.off()

polmedians <- tapply(rack$polity, rack$year, median, na.rm = TRUE)
png(file = "polity.median.over.time.png", width=10, height=6, units='cm', bg='white', res = 150)
par(mar=c(2,2,3,1))
par(cex.axis = 0.6)
par(cex.main = 0.75)
plot(polmedians, type = "l", ylim = c(-10,10), axes = FALSE,
  lwd = 3, col = "blue2", main = "World Median Polity Score, 1945-2013")
axis(1, at = c(6,16,26,36,46,56,66), labels = c("1950", "1960", "1970", "1980", "1990", "2000", "2010"),
  tick = FALSE, line = -1)
axis(2, at = c(-10,-5,0,5,10), tick = FALSE, las = 2, line = -1)
abline(h = -10, lwd = 0.5, col = "gray")
abline(h = -5, lwd = 0.5, col = "gray")
abline(h = 0, lwd = 0.5, col = "gray")
abline(h = 5, lwd = 0.5, col = "gray")
abline(h = 10, lwd = 0.5, col = "gray")
dev.off()
