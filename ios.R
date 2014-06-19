# INTERNATIONAL ORGANIZATIONS AND TREATY REGIMES
# Jay Ulfelder
# 2014-04-23

rm(list=ls(all=TRUE))

ios <- read.csv("ulfelder io data 2010.csv")
ios$sftgcode <- as.character(ios$pitfcode)
ios <- subset(ios, select = c(sftgcode, year, iccpr1, gattwto))
names(ios) <- c("sftgcode", "year", "io.iccpr1", "io.wto")
ios$sftgcode[ios$sftgcode=="UK"] <- "UK "

source("countryyear.rack.maker.r")
rack <- subset(rack, select=c(country, sftgcode, year))
rack <- merge(rack, ios, all.x = TRUE)
rack <- rack[order(rack$country, rack$year),]

# Updates source: http://treaties.un.org/Pages/ViewDetails.aspx?src=TREATY&mtdsg_no=IV-5&chapter=4&lang=en
for (i in 1:dim(rack)[1]) rack$io.iccpr1[i] <- ifelse(rack$year[i] > 2010, rack$io.iccpr1[i-1], rack$io.iccpr1[i])
rack$io.iccpr1[rack$sftgcode=="TUN" & rack$year>=2011] <- 1 # acceded in 2011
rack$io.iccpr1[rack$sftgcode=="USA"] <- 0 
rack$io.iccpr1[rack$sftgcode=="SSD"] <- 0 
rack$io.iccpr1[rack$sftgcode=="MNG"] <- 0 
rack$io.iccpr1[rack$sftgcode=="SWA"] <- 0 
rack$io.iccpr1[rack$sftgcode=="GNB" & rack$year>=2013] <- 1  # acceded in 2013

# Source: http://www.wto.org/english/thewto_e/whatis_e/tif_e/org6_e.htm
for (i in 1:dim(rack)[1]) rack$io.wto[i] <- ifelse(rack$year[i] > 2010, rack$io.wto[i-1], rack$io.wto[i])
rack$io.wto[rack$sftgcode=="MNG" & rack$year>=2012] <- 1  # acceded in 2012
rack$io.wto[rack$sftgcode=="RUS" & rack$year>=2012] <- 1  # acceded in 2012
rack$io.wto[rack$sftgcode=="SRB"] <- 0  # fill in missing
rack$io.wto[rack$sftgcode=="SSD"] <- 0  # fill in missing
rack$io.wto[rack$sftgcode=="USA"] <- 1  # fill in missing
rack$io.wto[rack$sftgcode=="LAO" & rack$year>=2013] <- 1  # acceded in 2013
rack$io.wto[rack$sftgcode=="TAJ" & rack$year>=2013] <- 1  # acceded in 2013

write.csv(rack, "ios.csv", row.names = FALSE)
