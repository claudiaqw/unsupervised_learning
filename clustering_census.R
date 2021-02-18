census <- read.csv(file = 'data/census2000_conLongitudLatitud.csv')
summary(census)

census$MedHHInc <- as.numeric(gsub('[$,]', '', census$MedHHInc))
census$MeanHHSz <- as.numeric(gsub('[$,]', '', census$MeanHHSz))
census$RegPop <- as.numeric(gsub('[,]', '', census$RegPop))

census_data <- census[c(-1, -2, -3)]
census_data$RegDens <- log(census_data$RegDens-min(census_data$RegDens)+1)
census_data$RegPop <- log(census_data$RegPop-min(census_data$RegPop)+1)
census_data$MedHHInc <- log(census_data$MedHHInc-min(census_data$MedHHInc)+1)
census_data$MeanHHSz <- log(census_data$MeanHHSz-min(census_data$MeanHHSz)+1)
