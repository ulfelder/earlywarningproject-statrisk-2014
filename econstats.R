# ECONOMIC GROWTH AND INFLATION MELDED FROM WORLD BANK AND IMF
# Jay Ulfelder
# 2014-05-15

# Measure used in models is categorical: annual % change in GDP per capita < 2%
# WDI is used for historical data, IMF World Economic Outlook for latest year (forecast)

rm(list=ls(all=TRUE))

# Create scaffolding
source("countryyear.rack.maker.r")
rack <- subset(rack, select=c(country, sftgcode, year, yrborn))

# WDI annual percent change in GDP per capita
require(WDI)
wdi <- WDI(country="all", indicator = "NY.GDP.MKTP.KD.ZG", extra = FALSE,
  start = 1960, end = as.numeric(substr(Sys.Date(),1,4)))
names(wdi) <- c("iso2c", "name", "wdi.gdppcgrow", "year")
data <- wdi
source("pitf_code_maker.R")
wdi <- data
require(reshape)
wdi <- rename(wdi, c(code="sftgcode"))
wdi <- subset(wdi, is.na(sftgcode)==FALSE, select=c("sftgcode", "year", "wdi.gdppcgrow"))
rack <- merge(rack, wdi, all.x = TRUE)

# 2013 updates from IMF
# Source: http://www.imf.org/external/pubs/ft/weo/2014/01/weodata/index.aspx
# Download "By countries" > All countries > Continue > 
# [check] Gross domestic product per capita, constant prices (national currency) > 
# [start year -> 1980] [end year -> 2013] [uncheck] Append country/series-specific notes > Prepare report
weo2013 <- read.delim("imfgdppc.aspx")
weo2013$g2013 <- as.numeric(gsub(",", "", as.character(weo2013$X2013)))
weo2013$g2012 <- as.numeric(gsub(",", "", as.character(weo2013$X2012)))
weo2013$g2011 <- as.numeric(gsub(",", "", as.character(weo2013$X2011)))
weo2013$g2010 <- as.numeric(gsub(",", "", as.character(weo2013$X2010)))
weo2013$g2009 <- as.numeric(gsub(",", "", as.character(weo2013$X2009)))
weo2013$g2008 <- as.numeric(gsub(",", "", as.character(weo2013$X2008)))
weo2013$g2007 <- as.numeric(gsub(",", "", as.character(weo2013$X2007)))
weo2013$g2006 <- as.numeric(gsub(",", "", as.character(weo2013$X2006)))
weo2013$g2005 <- as.numeric(gsub(",", "", as.character(weo2013$X2005)))
weo2013$g2004 <- as.numeric(gsub(",", "", as.character(weo2013$X2004)))
weo2013$g2003 <- as.numeric(gsub(",", "", as.character(weo2013$X2003)))
weo2013$g2002 <- as.numeric(gsub(",", "", as.character(weo2013$X2002)))
weo2013$g2001 <- as.numeric(gsub(",", "", as.character(weo2013$X2001)))
weo2013$g2000 <- as.numeric(gsub(",", "", as.character(weo2013$X2000)))
weo2013$g1999 <- as.numeric(gsub(",", "", as.character(weo2013$X1999)))
weo2013$g1998 <- as.numeric(gsub(",", "", as.character(weo2013$X1998)))
weo2013$g1997 <- as.numeric(gsub(",", "", as.character(weo2013$X1997)))
weo2013$g1996 <- as.numeric(gsub(",", "", as.character(weo2013$X1996)))
weo2013$g1995 <- as.numeric(gsub(",", "", as.character(weo2013$X1995)))
weo2013$g1994 <- as.numeric(gsub(",", "", as.character(weo2013$X1994)))
weo2013$g1993 <- as.numeric(gsub(",", "", as.character(weo2013$X1993)))
weo2013$g1992 <- as.numeric(gsub(",", "", as.character(weo2013$X1992)))
weo2013$g1991 <- as.numeric(gsub(",", "", as.character(weo2013$X1991)))
weo2013$g1990 <- as.numeric(gsub(",", "", as.character(weo2013$X1990)))
weo2013$g1989 <- as.numeric(gsub(",", "", as.character(weo2013$X1989)))
weo2013$g1988 <- as.numeric(gsub(",", "", as.character(weo2013$X1988)))
weo2013$g1987 <- as.numeric(gsub(",", "", as.character(weo2013$X1987)))
weo2013$g1986 <- as.numeric(gsub(",", "", as.character(weo2013$X1986)))
weo2013$g1985 <- as.numeric(gsub(",", "", as.character(weo2013$X1985)))
weo2013$g1984 <- as.numeric(gsub(",", "", as.character(weo2013$X1984)))
weo2013$g1983 <- as.numeric(gsub(",", "", as.character(weo2013$X1983)))
weo2013$g1982 <- as.numeric(gsub(",", "", as.character(weo2013$X1982)))
weo2013$g1981 <- as.numeric(gsub(",", "", as.character(weo2013$X1981)))
weo2013$g1980 <- as.numeric(gsub(",", "", as.character(weo2013$X1980)))
weo2013 <- subset(weo2013, select=c(1,40:73))
require(reshape)
weomelt <- melt(weo2013)
names(weomelt) <- c("country", "gyear", "imf.gdppc")
weomelt$year <- as.numeric(substr( as.character(weomelt$gyear),2,5  ) )
weomelt$gyear <- NULL
weomelt$name <- as.character(weomelt$country)
data <- weomelt
source("pitf_code_maker.r")
weomelt <- data
weomelt <- rename(weomelt, c(code="sftgcode"))
rack <- merge(rack, subset(weomelt, select=c(2,3,5)), all.x = TRUE)
rack <- rack[order(rack$country,rack$year),]
for (i in 1:dim(rack)[1]) rack$imf.gdppcgrow[i] <- ifelse(rack$year[i] > rack$yrborn[i] & is.na(rack$imf.gdppc[i])==FALSE, 
  100 * ((rack$imf.gdppc[i] - rack$imf.gdppc[i-1])/rack$imf.gdppc[i-1]), NA )

# Check to make sure two series are similar (cor ~ 0.88)
cor(subset(rack, select=c(5,7)), use="complete.obs")

# Create meld of two growth series
rack$mix.gdppcgrow <- NA
rack$mix.gdppcgrow <- replace(rack$mix.gdppcgrow, which(is.na(rack$imf.gdppcgrow)==FALSE),
  rack$imf.gdppcgrow[is.na(rack$imf.gdppcgrow)==FALSE] )
rack$mix.gdppcgrow <- replace(rack$mix.gdppcgrow,
  which(is.na(rack$imf.gdppcgrow)==TRUE & is.na(rack$wdi.gdppcgrow)==FALSE),
  rack$wdi.gdppcgrow[is.na(rack$imf.gdppcgrow)==TRUE & is.na(rack$wdi.gdppcgrow)==FALSE] )

rack$mix.gdppcgrowsr <- ifelse( rack$mix.gdppcgrow <= 0, -1 * sqrt(abs(rack$mix.gdppcgrow)),
  sqrt(abs(rack$mix.gdppcgrow)) )

# Create slow growth dummy using wdi first, imf if not available
rack$slowgrowth <- ifelse(rack$mix.gdppcgrow < 2, 1, ifelse(is.na(rack$mix.gdppcgrow)==FALSE, 0, NA) )
rack$slowgrowth[is.na(rack$wdi.gdppcgrow)==FALSE] <- ifelse(rack$wdi.gdppcgrow[is.na(rack$wdi.gdppcgrow)==FALSE] < 2,
  1, 0)
rack$slowgrowth[is.na(rack$wdi.gdppcgrow)==TRUE & is.na(rack$imf.gdppcgrow)==FALSE] <-
  ifelse(rack$imf.gdppcgrow[is.na(rack$wdi.gdppcgrow)==TRUE & is.na(rack$imf.gdppcgrow)==FALSE] < 2, 1, 0)

write.csv(rack, "econstats.csv", row.names = FALSE)
