# ELITE CHARACTERISTICS
# Jay Ulfelder
# 2014-05-15

# Source: Center for Systemic Peace via PITF

rm(list=ls(all=TRUE))

require(XLConnect)
elc <- readWorksheetFromFile("elc2013.xls", sheet=1)
names(elc) <- c("sftgcode", "year", "elceleth", "elceliti")
elc$sftgcode <- as.character(elc$sftgcode)

# Change some code for merging
elc$sftgcode[elc$sftgcode=="UKG"] <- "UK "
elc$sftgcode[elc$sftgcode=="SER"] <- "SRB"
elc$sftgcode[elc$sftgcode=="MNT"] <- "MNE"
elc$sftgcode[elc$sftgcode=="SSU"] <- "SSD"
elc$sftgcode[elc$sftgcode=="SDN"] <- "SUD"
elc$sftgcode[elc$sftgcode=="GMY"] <- "GER"

source("countryyear.rack.maker.r")
rack <- subset(rack, select=c(country, sftgcode, year))
rack <- merge(rack, elc, all.x = TRUE)
rack <- rack[order(rack$country, rack$year),]

# Hard code values for United States, which source doesn't cover
rack$elceliti[rack$sftgcode=="USA" & rack$year >= min(elc$year) & rack$year <= max(elc$year)] <- 0
rack$elceleth[rack$sftgcode=="USA" & rack$year >= min(elc$year) & rack$year <= max(elc$year)] <- 0

# Create indicator for politically salient elite ethnicity: either, minority, majority
rack$elcelethc <- ifelse(rack$elceleth==1 | rack$elceleth==2, 1, ifelse(is.na(rack$elceleth)==FALSE, 0, NA) )
rack$elceleth2 <- ifelse(rack$elceleth==2, 1, ifelse(is.na(rack$elceleth)==FALSE, 0, NA) )
rack$elceleth1 <- ifelse(rack$elceleth==1, 1, ifelse(is.na(rack$elceleth)==FALSE, 0, NA) )

write.csv(rack, "elc.csv", row.names=FALSE)
