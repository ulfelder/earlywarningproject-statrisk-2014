# CENSUS BUREAU INFANT MORTALITY ESTIMATES
# Jay Ulfelder
# 2014-05-15

# Source: U.S. Census Bureau International Division, via PITF

rm(list=ls(all=TRUE))

imrates <- read.csv("pitf.csv")
imrates <- imrates[,1:4]
names(imrates) <- c("sftgcode", "year", "cnsimr", "xxxcimr")
imrates$sftgcode <- substr(as.character(imrates$sftgcode),1,3)
imrates$cnsimr <- as.numeric(as.character(imrates$cnsimr))
imrates$xxxcimr <- as.numeric(substr(as.character(imrates$xxxcimr),1,5))

source("countryyear.rack.maker.r")
rack <- subset(rack, select=c(country, sftgcode, year))
rack <- merge(rack, imrates, all.x = TRUE)
rack <- rack[order(rack$country, rack$year),]

# Hard code values for USA, which source doesn't cover, to values for Canada
rack$cnsimr[rack$sftgcode=="USA"] <- rack$cnsimr[rack$sftgcode=="CAN"]
rack$xxxcimr[rack$sftgcode=="USA"] <- rack$xxxcimr[rack$sftgcode=="CAN"]

# Log of rate rel to annual global median
rack$xxxcimrln <- log(rack$xxxcimr)

write.csv(rack, "imrate.csv", row.names = FALSE)
