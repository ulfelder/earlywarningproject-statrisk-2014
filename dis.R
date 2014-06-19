# DISCRIMINATION
# Jay Ulfelder
# 2014-05-14

# Source: Center for Systemic Peace via PITF

rm(list=ls(all=TRUE))

require(XLConnect)
dis <- readWorksheetFromFile("diss2013.xls", sheet=1)

# Create variable summing population proportions of groups subjected to level 4 (state-led) discrimination
dis$d4p1 <- ifelse(dis$pdis1==4 | dis$edis1==4, dis$gprop1, 0)
dis$d4p2 <- ifelse(dis$pdis2==4 | dis$edis2==4, dis$gprop2, 0)
dis$d4p3 <- ifelse(dis$pdis3==4 | dis$edis3==4, dis$gprop3, 0)
dis$d4p4 <- ifelse(dis$pdis4==4 | dis$edis4==4, dis$gprop4, 0)
dis$d4p5 <- ifelse(dis$pdis5==4 | dis$edis5==4, dis$gprop5, 0)
dis$d4p6 <- ifelse(dis$pdis6==4 | dis$edis6==4, dis$gprop6, 0)
dis$d4p7 <- ifelse(dis$pdis7==4 | dis$edis7==4, dis$gprop7, 0)
dis$d4p8 <- ifelse(dis$pdis8==4 | dis$edis8==4, dis$gprop8, 0)
dis$d4p9 <- ifelse(dis$pdis9==4 | dis$edis9==4, dis$gprop9, 0)
dis$d4p10 <- ifelse(dis$pdis10==4 | dis$edis10==4, dis$gprop10, 0)
dis$d4p11 <- ifelse(dis$pdis11==4 | dis$edis11==4, dis$gprop11, 0)
dis$d4p12 <- ifelse(dis$pdis12==4 | dis$edis12==4, dis$gprop12, 0)
dis$d4p13 <- ifelse(dis$pdis13==4 | dis$edis13==4, dis$gprop13, 0)
dis$d4p14 <- ifelse(dis$pdis14==4 | dis$edis14==4, dis$gprop14, 0)
dis$d4p15 <- ifelse(dis$pdis15==4 | dis$edis15==4, dis$gprop15, 0)
dis$d4p16 <- ifelse(dis$pdis16==4 | dis$edis16==4, dis$gprop16, 0)
dis$d4p17 <- ifelse(dis$pdis17==4 | dis$edis17==4, dis$gprop17, 0)
dis$d4p18 <- ifelse(dis$pdis18==4 | dis$edis18==4, dis$gprop18, 0)
dis$d4p19 <- ifelse(dis$pdis19==4 | dis$edis19==4, dis$gprop19, 0)
dis$d4p20 <- ifelse(dis$pdis20==4 | dis$edis20==4, dis$gprop20, 0)
for (i in 1:dim(dis)[1]) dis$dispota4[i] <- sum(dis[i,83:102], na.rm = TRUE)

dis <- subset(dis, select=c(scode, year, dispota4))
names(dis) <- c("sftgcode", "year", "dispota4")
dis$sftgcode <- as.character(dis$sftgcode)
dis$sftgcode[dis$sftgcode=="UK"] <- "UK "

source("countryyear.rack.maker.r")
rack <- subset(rack, select=c(country, sftgcode, year))
rack <- merge(rack, dis, all.x = TRUE)
rack <- rack[order(rack$country, rack$year),]

# Fill in uncoded series as zeroes
rack$dispota4[is.na(rack$dispota4)==TRUE & rack$year >= min(dis$year) & rack$year <= max(dis$year)] <- 0

# Log of pop share subject to state-led discrimination
rack$dispota4ln <- log1p(100*rack$dispota4)

write.csv(rack, "dis.csv", row.names=FALSE)
