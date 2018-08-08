# sublime regex search terms
# for preparation of data
#
# 1) replace line beginnings with "
# 		what: ^ with: "
# 2) replace separators with ";"
#		what: ; with: ";"
# 3) replace line ending with "
#		what: \n with: "\n
#
# remember to 1) delete the last line of the file and 2) save the file in UTF-8 format
#

version <- "limited_wo_outlier"
setwd("C:/Users/bence.kiss-dobronyi/Documents/Private/MAthesis_data/controlGroup/processedData/modif")

# olvassuk be a kontroll csoport fajlait
long2 <- read.csv(file="mod_long2.txt", sep=";", quote="\"", stringsAsFactors=FALSE, header=TRUE, encoding="UTF-8")
short <- read.csv(file="mod_short.txt", sep=";", quote="\"", stringsAsFactors=FALSE, header=TRUE, encoding="UTF-8")
short2 <- read.csv(file="mod_short2.txt", sep=";", quote="\"", stringsAsFactors=FALSE, header=TRUE, encoding="UTF-8")
short3 <- read.csv(file="mod_short3.txt", sep=";", quote="\"", stringsAsFactors=FALSE, header=TRUE, encoding="UTF-8")

# szeretnenk osszekapcsolni a long2 + short + short2 data.frame-eket

library(plyr)
data <- rbind.fill(long2, short, short2, short3)
data <- data[with(data, order(id)),]

# write out a backup version
write.csv(data, file="processedData_sample.txt")
data_backup <- data

# keressunk masolatokat es ha talalunk, akkor valasszuk azokat amik kesobbiek 
# DE NEM NA-k
# eloszor teljes duplikacio, azaz megegyezo sor
data <- data[!duplicated(data),]
# masodszorra nem teljes duplikacio, azaz megegyezo azonosito + ev, de nem azonos adattartalom
ev_arr <- c(2010, 2011, 2012, 2013, 2014)
id_arr <- unique(data$id)
new_df <- NULL
new_df <- data.frame()
dim(new_df) <- c(dim(data[1,])[1], 0)
for(i in id_arr){
	for(k in ev_arr){
		if (dim(data[data$id==i&data$ev==k,])[1] > 1){
			if(is.na(data$e1[data$id==i&data$ev==k][dim(data[data$id==i&data$ev==k,])[1]])){
				new_df <- rbind(new_df, data[data$id==i&data$ev==k,][1,])
			}else{
				new_df <- rbind(new_df, data[data$id==i&data$ev==k,][dim(data[data$id==i&data$ev==k,])[1],])
			}
		} else {
			new_df <- rbind(new_df, data[data$id==i&data$ev==k,])
		}
	}
}

## csereljuk az NA-kat 0-ra

for(n in names(new_df)){
	new_df[,n][is.na(new_df[,n])] <- 0
}

# számoljuk ki a mérlegállományok évközi átlagait
# new working DF -> new_df

append_avg <- NULL
append_avg <- data.frame()
dim(append_avg) <- c(0,14)

for (i in 1:dim(new_df)[1]){
	if(dim(new_df[new_df$ev==(new_df[i,]$ev-1)&new_df$id==new_df[i,]$id,])[1] != 0){
		tmp <- c(new_df[i,]$id, new_df[i,]$ev, (new_df[i,4:15] + new_df[new_df$ev==(new_df[i,]$ev-1)&new_df$id==new_df[i,]$id,4:15])/2)
		names(tmp) <- c("id","ev",names(new_df[,4:15]))
		append_avg <- rbind(append_avg, tmp)
	}
}
names(append_avg) <- gsub("m", "mavg_", names(append_avg))
append_avg <- append_avg[,-c(7,10,11,12,13)]


# # adjuk hozzá az eredeti adatokhoz a mérlegállományok átlagait
new_df_w_avg <- merge(new_df, append_avg, all.x=TRUE)  
data <- new_df_w_avg

# számoljuk ki a teljesítményt leíró mutatókat (stock adatokkal, mérlegátlagot nem figyelembe véve)

data$c_eladosodottsag <- data$mf / data$mse
data$c_likvidrata <- data$mb / data$mf3

## ROE with ee ef helyett
data$c_tokearanyosjovROE <- data$ee / data$md 
data$c_ROA <- data$ee / data$mse

# számoljuk ki a mérlegállományok átlagaival számoló mutatókat

data$cavg_eladosodottsag <- data$mavg_f / data$mavg_se
data$cavg_likvidrata <- data$mavg_b / data$mavg_f3
data$cavg_tokearanyosjovROE <- data$ee / data$mavg_d
data$cavg_ROA <- data$ee / data$mavg_se


setwd("C:/Users/bence.kiss-dobronyi/Documents/Private/MAthesis_data/controlGroup/")
helyek <- read.csv(file="names_places_.txt", stringsAsFactors=FALSE, encoding="UTF-8", sep=";")
names(helyek)[2] <- "id"
names(helyek)[6] <- "Megye"
names(helyek)[8] <- "Település"
data <- merge(data,helyek,x.all=TRUE)

# check if there is na
helyek[is.na(helyek$alapitas),]

# szamoljuk ki a ceg korat 
# az alapitas es a megfigyeles evenek kulonbsegebol

data$kor <- data$ev - data$alapitas

setwd("C:/Users/bence.kiss-dobronyi/Documents/Private/MAthesis_data/MAthesis/processeddata")

treatedData <- read.csv(file="processedData_w_CALC_hely_na.txt", stringsAsFactors=FALSE, sep=",", encoding="UTF-8")

# a treated csoport id-jet toljuk el, hogy ne legyen konflikt
treatedData$id <- treatedData$id + 1000

# merge everything
fullData <- rbind.fill(data, treatedData)

# igen - treated
fullData$igen[!is.na(fullData$igen)] <- 1
# nem - non-treated
fullData$igen[is.na(fullData$igen)] <- 0

setwd("C:/Users/bence.kiss-dobronyi/Documents/Private/MAthesis_data/controlGroup/")
write.csv(fullData,file="fullData_w_CALC_hely.txt")

names(fullData)[175] <- "palyazat"
unique(fullData$palyazat)

# csak a GOP-2.1.1-11/M palyazat kedvezmenyezettjeit tartjuk meg
# nezzuk meg hogy mibol mennyi van

dim(fullData) 
dim(fullData[fullData$palyazat=="GOP-2.1.1-11/M" & !is.na(fullData$palyazat),]) 
dim(fullData[is.na(fullData$palyazat),])

# 2989 sor
# 1002 sor
# 1439 sor

# switch on/off for filtering palyazat
fullDataBackup <- fullData
fullData <- fullData[(fullData$palyazat=="GOP-2.1.1-11/M" & !is.na(fullData$palyazat)) | is.na(fullData$palyazat),]
dim(fullData)

# 2441 sor

# szurjuk ki a nem Borsodi vallalatokat

dim(fullData)
dim(fullData[((fullData$Megye=="Borsod-Abaúj-Zemplén") & !is.na(fullData$Megye)) | is.na(fullData$Megye), ])

fullDataBackup <- fullData
fullData <- fullData[(fullData$Megye=="Borsod-Abaúj-Zemplén" & !is.na(fullData$palyazat)) | is.na(fullData$palyazat),]
dim(fullData)

# 2441 sor
# 2078 sor

fullData <- fullData[,1:44]

length(unique(fullDataYears$id))
length(unique(fullDataYears$id[(fullDataYears$ev==2010)&(fullDataYears$igen_long==0)]))
length(unique(fullDataYears$id[(fullDataYears$ev==2010)&(fullDataYears$igen_long==1)]))
length(unique(fullDataYears$id[(fullDataYears$ev==2011)&(fullDataYears$igen_long==0)]))
length(unique(fullDataYears$id[(fullDataYears$ev==2011)&(fullDataYears$igen_long==1)]))
length(unique(fullDataYears$id[(fullDataYears$ev==2012)&(fullDataYears$igen_long==0)]))
length(unique(fullDataYears$id[(fullDataYears$ev==2012)&(fullDataYears$igen_long==1)]))
length(unique(fullDataYears$id[(fullDataYears$ev==2013)&(fullDataYears$igen_long==0)]))
length(unique(fullDataYears$id[(fullDataYears$ev==2013)&(fullDataYears$igen_long==1)]))
length(unique(fullDataYears$id[(fullDataYears$ev==2014)&(fullDataYears$igen_long==0)]))
length(unique(fullDataYears$id[(fullDataYears$ev==2014)&(fullDataYears$igen_long==1)]))

# alulrol korlatossag beallitas

for (i in names(fullData)){
	fullData[is.infinite(fullData[,i]),i] <- 0
}

# correct TEAOR numbers
fullData$teaor[fullData$teaor < 100] <- fullData$teaor[fullData$teaor < 100] * 100
# correct kor
fullData$kor <- fullData$ev - fullData$alapitas

# filter out possibly faulty data (kor < 0)
fullData <- fullData[!fullData$kor<0,]

# assign agazat numbers
fullData$agazat <- 1

assignAgazat <- function(code, from, to){
	if(to == 0){
		fullData$agazat[(fullData$teaor > (from * 100)) & (fullData$teaor < ((to + 1) * 100))] <- code
	} else {
		fullData$agazat[(fullData$teaor > (from * 100)) & (fullData$teaor < (to * 100))] <- code
	}
	return(fullData)
}

fullData <- assignAgazat("A",0,4)
fullData <- assignAgazat("B",5,10)
fullData <- assignAgazat("C",10,35)
fullData <- assignAgazat("D",35,36)
fullData <- assignAgazat("E",36,40)
fullData <- assignAgazat("F",41,44)
fullData <- assignAgazat("G",45,48)
fullData <- assignAgazat("H",49,54)
fullData <- assignAgazat("I",55,57)
fullData <- assignAgazat("J",58,64)
fullData <- assignAgazat("K",64,68)
fullData <- assignAgazat("L",68,69)
fullData <- assignAgazat("M",69,76)
fullData <- assignAgazat("N",77,83)
fullData <- assignAgazat("O",84,85)
fullData <- assignAgazat("P",85,86)
fullData <- assignAgazat("Q",86,89)
fullData <- assignAgazat("R",90,94)
fullData <- assignAgazat("S",94,97)
fullData <- assignAgazat("T",97,99)
fullData <- assignAgazat("U",99,100)

setwd("C:/Users/bence.kiss-dobronyi/Documents/Private/MAthesis_data/controlGroup/")


####################
### convert used financial measures to log
####################

finIndic <- c("e1", "ee", "ea", "mse", "ma", "mb", "md", "mf", "msf")
for (ind in finIndic){
	tmpData <- log(fullData[,which(names(fullData)==ind)])
	tmpData[is.infinite(tmpData)] <- 0
	names(fullData)[which(names(fullData)==ind)] <- paste0("n_", ind)
	fullData <- cbind(fullData, tmpData)
	names(fullData)[length(names(fullData))] <- ind
}

# log korrekcio (NA = 0)

for (i in names(fullData)){
	fullData[is.na(fullData[,i]),i] <- 0
}

###################
### filter out serious outliers
###################

# fullData <- fullData[-which(fullData$id==464),]
# fullData <- fullData[-which(fullData$id==39),}


### FILTERING for threshold values

fullDataB <- fullData

fullData <- fullData[(abs(fullData$c_likvidrata)<20)&(abs(fullData$cavg_ROA)<3)&(abs(fullData$cavg_tokearanyosjovROE)<20),]

dim(fullData)
dim(fullDataB)
dim(fullData[is.na(fullData$ee),])
dim(fullDataB[is.na(fullDataB$ee),])

# create subset of data with only companies which are present through the sample years

fullDataYears <- fullData

fullYearsId <- NULL
for (i in unique(fullDataYears$id)){
	if (dim(fullDataYears[fullDataYears$id==i,])[1] == 5){
		fullYearsId <- c(fullYearsId, i)
	}
}

fullDataYears <- fullDataYears[fullDataYears$id %in% fullYearsId,]
dim(fullDataYears)
dim(fullDataYears[fullDataYears$igen == 0,])
dim(fullDataYears[fullDataYears$igen == 1,])

length(unique(fullDataYears$id))
length(unique(fullDataYears$id[fullDataYears$igen == 0]))
length(unique(fullDataYears$id[fullDataYears$igen == 1]))

library(psych)

setwd("R_Output")
setwd(version)

sink("R_summary.txt", append=FALSE)

cat("FOR FULL SAMPLE", "\n")
describe(fullData)
describeBy(fullData, fullData$igen)
cat("\n", "FOR LIMITED SAMPLE", "\n")
describe(fullDataYears)
describeBy(fullDataYears, fullDataYears$igen)

sink()

setwd("..")


####################
### 1st STEP
####################

full_indicators <- c("e1", "ee", "mse", "ma", "mb", "md", "mf", "cavg_ROA", "cavg_tokearanyosjovROE", "c_likvidrata", "c_eladosodottsag")
limited_indicators <- c("e1", "ee", "mse", "ma", "mb", "md", "mf")

# t-test function 
ttestFunc <- function(sample, indicators){
	text <- cat("No. of obs: ", length(unique(sample$id)))
	text <- cat(text, "var;t-value;p-value; \n",sep="")
	for (i in indicators){
		if(length(sample[sample$igen==1,i]) * length(sample[sample$igen==0,i]) > 0){
			result <- t.test(sample[sample$igen==1,i], sample[sample$igen==0,i])
			text <- cat(text, i, ";", sep="")
			text <- cat(text, result$statistic, ";", result$p.value, ";", sep="")
			text <- cat(text, "\n", sep="")
		} else {
			print(paste0("No data for: ", i))
		}
	}
}

write.csv(fullData,file="fullData_w_CALC_hely.txt")
write.csv(fullDataYears,file="fullDataYears_w_CALC_hely.txt")

# clear the output file
setwd(version)
sink("R_output_ttest.txt", append=FALSE)
cat("T-test, R output for statistic analysis of MA thesis, Bence Kiss-Dobronyi", "\n")
cat(Sys.time(), "\n")
sink()

sink("R_output_ttest.txt", append=TRUE)
# FOR FULL SAMPLE
cat("\n", "FOR FULL SAMPLE", "\n")
# t-test for full sample
cat("\n\n", "t-test for full sample", "\n")
ttestFunc(fullData, full_indicators)
# t-test for period before treatment
cat("\n\n", "t-test for period before treatment", "\n")
ttestFunc(fullData[fullData$ev < 2012,], limited_indicators)
# t-test for period after treatment
cat("\n\n", "t-test for period after treatment", "\n")
ttestFunc(fullData[fullData$ev > 2011,], limited_indicators)

# FOR LIMITED SAMPLE
cat("\n\n\n", "FOR LIMITED SAMPLE", "\n")
# t-test for full sample
cat("\n\n", "t-test for full sample", "\n")
ttestFunc(fullDataYears, full_indicators)
# t-test for period before treatment
cat("\n\n", "t-test for period before treatment", "\n")
ttestFunc(fullDataYears[fullDataYears$ev < 2012,], limited_indicators)
# t-test for period after treatment
cat("\n\n", "t-test for period after treatment", "\n")
ttestFunc(fullDataYears[fullDataYears$ev > 2011,], limited_indicators)

sink()
setwd("..")

library(plm)

####################
### 2nd STEP
####################

# OLS regressions function
OLSFunc <- function(sample, indicators){
	text_name <- "name;"
	text_igen <- "igen (coeff);"
	text_se <- "stderr;"
	text_tval <- "t-value;"
	text_pval <- "p-value;"
	text_dummy <- "no of vars included;"
	text_r2 <- "R^2;"
	text_obs <- "Obs;"
	text_obsyears <- "Obs years;"
	models <- list()
	for (i in indicators){
		ols_model <- lm(paste(i, " ~ igen"), data = sample)
		if(substr(i, 1, 1) == "e"){
			ols_model_dummies <- lm(paste(i, " ~ igen + kor + factor(agazat) + factor(ev) + mse"), data = sample)
		} else if(substr(i, 1, 1) == "m"){
			ols_model_dummies <- lm(paste(i, " ~ igen + kor + factor(agazat) + factor(ev) + e1"), data = sample)
		} else {
			ols_model_dummies <- lm(paste(i, " ~ igen + kor + factor(agazat) + factor(ev)"), data = sample)
		}
		models[[1]] <- ols_model
		models[[2]] <- ols_model_dummies
		for (model in models){
			text_name <- paste(text_name, i, ";", sep="")
			text_igen <- paste(text_igen, summary(model)$coefficients[2], ";", sep="")
			text_se <- paste(text_se, coef(summary(model))[2,2], ";", sep="")
			text_tval <- paste(text_tval, coef(summary(model))[2,3], ";", sep="")
			text_pval <- paste(text_pval, coef(summary(model))[2,4], ";", sep="")
			text_dummy <- paste(text_dummy, length(model$coefficients), ";", sep="")
			text_r2 <- paste(text_r2, summary(model)$r.squared, ";", sep="")
			text_obs <- paste(text_obs, nobs(model), ";", sep="")
			text_obsyears <- paste(text_obsyears, length(unique(sample$ev)), ";", sep="")
		}
	}
	text <- cat(text_name, "\n", text_igen, "\n", text_se, "\n", text_tval, "\n", text_pval, "\n", text_dummy, "\n", text_r2, "\n", text_obs, "\n", text_obsyears, sep="")
	# return(models)
}

setwd(version)
sink("R_output_ols_pooled_wo_dummy.txt", append=FALSE)
cat("OLS results, R output for statistic analysis of MA thesis, Bence Kiss-Dobronyi", "\n")
cat("pooled OLS, with all-years dummy", "\n")
cat(Sys.time(), "\n")
sink()

sink("R_output_ols_pooled_wo_dummy.txt", append=TRUE)
# FOR FULL SAMPLE
cat("\n", "FOR FULL SAMPLE", "\n")
# OLS for full sample
cat("\n\n", "OLS for full sample", "\n")
OLSFunc(fullData, full_indicators)
# OLS for period before treatment
cat("\n\n", "OLS for period before treatment", "\n")
OLSFunc(fullData[fullData$ev < 2012,], limited_indicators)
# OLS for period after treatment
cat("\n\n", "OLS for period after treatment", "\n")
OLSFunc(fullData[fullData$ev > 2011,], limited_indicators)

# FOR LIMITED SAMPLE
cat("\n\n\n", "FOR LIMITED SAMPLE", "\n")
# OLS for full sample
cat("\n\n", "OLS for full sample", "\n")
OLSFunc(fullDataYears, full_indicators)
# OLS for period before treatment
cat("\n\n", "OLS for period before treatment", "\n")
OLSFunc(fullDataYears[fullDataYears$ev < 2012,], limited_indicators)
# OLS for period after treatment
cat("\n\n", "OLS for period after treatment", "\n")
OLSFunc(fullDataYears[fullDataYears$ev > 2011,], limited_indicators)

sink()
setwd("..")

fullDataBackup <- fullData
fullDataYearsBackup <- fullDataYears

length(unique(fullData$id))
length(unique(fullData$id[(fullData$ev==2010)&(fullData$igen==0)]))
length(unique(fullData$id[(fullData$ev==2010)&(fullData$igen==1)]))
length(unique(fullData$id[(fullData$ev==2011)&(fullData$igen==0)]))
length(unique(fullData$id[(fullData$ev==2011)&(fullData$igen==1)]))
length(unique(fullData$id[(fullData$ev==2012)&(fullData$igen==0)]))
length(unique(fullData$id[(fullData$ev==2012)&(fullData$igen==1)]))
length(unique(fullData$id[(fullData$ev==2013)&(fullData$igen==0)]))
length(unique(fullData$id[(fullData$ev==2013)&(fullData$igen==1)]))
length(unique(fullData$id[(fullData$ev==2014)&(fullData$igen==0)]))
length(unique(fullData$id[(fullData$ev==2014)&(fullData$igen==1)]))

setwd("C:/Users/bence.kiss-dobronyi/Documents/Private/MAthesis_data/controlGroup/")
write.csv(fullData,file="fullData_w_CALC_hely.txt")
write.csv(fullDataYears,file="fullDataYears_w_CALC_hely.txt")
setwd("R_Output")

fullData$igen_long <- fullData$igen
fullDataYears$igen_long <- fullDataYears$igen

fullDataYears$igen[fullDataYears$ev < 2012] <- 0 
fullData$igen[fullData$ev < 2012] <- 0 

setwd("C:/Users/bence.kiss-dobronyi/Documents/Private/MAthesis_data/controlGroup/")
write.csv(fullData,file="fullData_w_CALC_hely_TREATMENT.txt")
write.csv(fullDataYears,file="fullDataYears_w_CALC_hely_TREATMENT.txt")
setwd("R_Output")

setwd(version)
sink("R_output_ols_pooled_w_treatement.txt", append=FALSE)
cat("OLS results, R output for statistic analysis of MA thesis, Bence Kiss-Dobronyi", "\n")
cat("pooled OLS, with treated dummy", "\n")
cat(Sys.time(), "\n")
sink()

sink("R_output_ols_pooled_w_treatement.txt", append=TRUE)
# FOR FULL SAMPLE
cat("\n", "FOR FULL SAMPLE", "\n")
# OLS for full sample
cat("\n\n", "OLS for full sample", "\n")
OLSFunc(fullData, full_indicators)

# FOR LIMITED SAMPLE
cat("\n\n\n", "FOR LIMITED SAMPLE", "\n")
# OLS for full sample
cat("\n\n", "OLS for full sample", "\n")
OLSFunc(fullDataYears, full_indicators)

dim(fullDataYears)
dim(fullDataYears[(abs(fullDataYears$c_likvidrata)<20)&(abs(fullDataYears$cavg_ROA)<3),])

# # OUTLIERS in likvidrata
# dim(fullDataYears)
# dim(fullDataYears[abs(fullDataYears$c_likvidrata)<20,])
# OLSFunc(fullDataYears[abs(fullDataYears$c_likvidrata)<20,], c("c_likvidrata"));

# dim(fullDataYears)
# dim(fullDataYears[abs(fullDataYears$c_eladosodottsag)<1,])
# OLSFunc(fullDataYears[abs(fullDataYears$c_eladosodottsag)<1,], c("c_eladosodottsag"));

# dim(fullDataYears)
# dim(fullDataYears[abs(fullDataYears$cavg_ROA)<1,])
# OLSFunc(fullDataYears[abs(fullDataYears$cavg_ROA)<1,], c("cavg_ROA"));

# dim(fullDataYears)
# dim(fullDataYears[abs(fullDataYears$cavg_tokearanyosjovROE)<1,])
# OLSFunc(fullDataYears[abs(fullDataYears$cavg_tokearanyosjovROE)<1,], c("cavg_tokearanyosjovROE"));

sink()
setwd("..")

####################
### 3rd STEP
####################

fullData <- fullData[,-35]
fullDataYears <- fullDataYears[,-35]

# FE-RE model

# Hausmann-test function
HausmannFunc <- function(sample, indicators){
	text_name <- "name;"
	text_pval <- "p-value;"
	text_dummy <- "no of vars included;"
	tests <- list()
	for (i in indicators){
		formula <- as.formula(paste(i, " ~ igen"))
		if(substr(i, 1, 1) == "e"){
			formula_dummies <- as.formula(paste(i, " ~ igen + kor + mse"))
		} else if(substr(i, 1, 1) == "m"){
			formula_dummies <- as.formula(paste(i, " ~ igen + kor + e1"))
		} else {
			formula_dummies <- as.formula(paste(i, " ~ igen + kor"))
		}
		fe_model <- plm(formula, data=sample, index=c("id", "ev"), model="within")
		fe_model_dummies <- plm(formula_dummies, data=sample, index=c("id", "ev"), model="within")
		re_model <- plm(formula, data=sample, index=c("id", "ev"), model="random")
		re_model_dummies <- plm(formula_dummies, data=sample, index=c("id", "ev"), model="random")
		hausmann <- phtest(fe_model, re_model)
		hausmann_dummies <- phtest(fe_model_dummies, re_model_dummies)
		tests[[1]] <- hausmann
		tests[[2]] <- hausmann_dummies
		for (test in tests){
			text_name <- paste(text_name, i, ";", sep="")
			text_pval <- paste(text_pval, test$p.value, ";", sep="")
			text_dummy <- paste(text_dummy, test$data.name, ";", sep="")
		}
	}
	text <- cat(text_name, "\n", text_pval, "\n", text_dummy, sep="")
	# return(models)
}

setwd(version)
sink("R_output_hausmann.txt", append=FALSE)
cat("Hausmann-test results, R output for statistic analysis of MA thesis, Bence Kiss-Dobronyi", "\n")
cat(Sys.time(), "\n")
sink()

sink("R_output_hausmann.txt", append=TRUE)
# FOR FULL SAMPLE
cat("\n", "FOR FULL SAMPLE", "\n")
HausmannFunc(fullData, full_indicators)

# FOR LIMITED SAMPLE
cat("\n\n\n", "FOR LIMITED SAMPLE", "\n")
HausmannFunc(fullDataYears, full_indicators)

sink()
setwd("..")

# FE model function
FEFunc <- function(sample, indicators){
	text_name <- "name;"
	text_igen <- "igen (coeff);"
	text_se <- "stderr;"
	text_tval <- "t-value;"
	text_pval <- "p-value;"
	text_dummy <- "no of vars included;"
	text_r2 <- "R^2;"
	text_obs <- "Obs;"
	text_obsyears <- "Obs years;"
	models <- list()
	for (i in indicators){
		formula <- as.formula(paste(i, " ~ igen"))
		if(substr(i, 1, 1) == "e"){
			formula_dummies <- as.formula(paste(i, " ~ igen + kor + mse"))
		} else if(substr(i, 1, 1) == "m"){
			formula_dummies <- as.formula(paste(i, " ~ igen + kor + e1"))
		} else {
			formula_dummies <- as.formula(paste(i, " ~ igen + kor"))
		}
		fe_model <- plm(formula, data=sample, index=c("id", "ev"), model="within")
		fe_model_dummies <- plm(formula_dummies, data=sample, index=c("id", "ev"), model="within")
		models[[1]] <- fe_model
		models[[2]] <- fe_model_dummies
		for (model in models){
			text_name <- paste(text_name, i, ";", sep="")
			text_igen <- paste(text_igen, coef(summary(model))[1,1], ";", sep="")
			text_se <- paste(text_se, coef(summary(model))[1,2], ";", sep="")
			text_tval <- paste(text_tval, coef(summary(model))[1,3], ";", sep="")
			text_pval <- paste(text_pval, coef(summary(model))[1,4], ";", sep="")
			text_dummy <- paste(text_dummy, length(model$coefficients), ";", sep="")
			text_r2 <- paste(text_r2, summary(model)$r.squared[1], ";", sep="")
			text_obs <- paste(text_obs, nobs(model), ";", sep="")
			text_obsyears <- paste(text_obsyears, length(unique(sample$ev)), ";", sep="")
		}
	}
	text <- cat(text_name, "\n", text_igen, "\n", text_se, "\n", text_tval, "\n", text_pval, "\n", text_dummy, "\n", text_r2, "\n", text_obs, "\n", text_obsyears, sep="")
	# return(models)
}

setwd(version)
sink("R_output_fe.txt", append=FALSE)
cat("FE results, R output for statistic analysis of MA thesis, Bence Kiss-Dobronyi", "\n")
cat("FE results, with treated dummy", "\n")
cat(Sys.time(), "\n")
sink()

sink("R_output_fe.txt", append=TRUE)
# FOR FULL SAMPLE
cat("\n", "FOR FULL SAMPLE", "\n")
# OLS for full sample
cat("\n\n", "FE for full sample", "\n")
FEFunc(fullData, full_indicators)

# FOR LIMITED SAMPLE
cat("\n\n\n", "FOR LIMITED SAMPLE", "\n")
# OLS for full sample
cat("\n\n", "FE for full sample", "\n")
FEFunc(fullDataYears, full_indicators)

sink()
setwd("..")

# RE model function
REFunc <- function(sample, indicators){
	text_name <- "name;"
	text_igen <- "igen (coeff);"
	text_se <- "stderr;"
	text_tval <- "t-value;"
	text_pval <- "p-value;"
	text_dummy <- "no of vars included;"
	text_r2 <- "R^2;"
	text_obs <- "Obs;"
	text_obsyears <- "Obs years;"
	models <- list()
	for (i in indicators){
		formula <- as.formula(paste(i, " ~ igen"))
		if(substr(i, 1, 1) == "e"){
			formula_dummies <- as.formula(paste(i, " ~ igen + kor + factor(agazat) + mse"))
		} else if(substr(i, 1, 1) == "m"){
			formula_dummies <- as.formula(paste(i, " ~ igen + kor + factor(agazat) + e1"))
		} else {
			formula_dummies <- as.formula(paste(i, " ~ igen + kor + factor(agazat)"))
		}
		fe_model <- plm(formula, data=sample, index=c("id", "ev"), model="random")
		fe_model_dummies <- plm(formula_dummies, data=sample, index=c("id", "ev"), model="random")
		models[[1]] <- fe_model
		models[[2]] <- fe_model_dummies
		for (model in models){
			text_name <- paste(text_name, i, ";", sep="")
			text_igen <- paste(text_igen, coef(summary(model))[1,1], ";", sep="")
			text_se <- paste(text_se, coef(summary(model))[1,2], ";", sep="")
			text_tval <- paste(text_tval, coef(summary(model))[1,3], ";", sep="")
			text_pval <- paste(text_pval, coef(summary(model))[1,4], ";", sep="")
			text_dummy <- paste(text_dummy, length(model$coefficients), ";", sep="")
			text_r2 <- paste(text_r2, summary(model)$r.squared[1], ";", sep="")
			text_obs <- paste(text_obs, nobs(model), ";", sep="")
			text_obsyears <- paste(text_obsyears, length(unique(sample$ev)), ";", sep="")
		}
	}
	text <- cat(text_name, "\n", text_igen, "\n", text_se, "\n", text_tval, "\n", text_pval, "\n", text_dummy, "\n", text_r2, "\n", text_obs, "\n", text_obsyears, sep="")
	# return(models)
}

setwd(version)
sink("R_output_re.txt", append=FALSE)
cat("RE results, R output for statistic analysis of MA thesis, Bence Kiss-Dobronyi", "\n")
cat("RE results, with treated dummy", "\n")
cat(Sys.time(), "\n")
sink()

sink("R_output_re.txt", append=TRUE)
# FOR FULL SAMPLE
cat("\n", "FOR FULL SAMPLE", "\n")
# OLS for full sample
cat("\n\n", "RE for full sample", "\n")
REFunc(fullData, full_indicators)

# FOR LIMITED SAMPLE
cat("\n\n\n", "FOR LIMITED SAMPLE", "\n")
# OLS for full sample
cat("\n\n", "RE for full sample", "\n")
REFunc(fullDataYears, full_indicators)

sink()
setwd("..")

# fullData$ttime <- ifelse(fullData$ev > 2011, 1, 0)
# fullDataYears$ttime <- ifelse(fullDataYears$ev > 2011, 1, 0)

# fullData$did <- fullData$ttime * fullData$igen_long
# fullDataYears$did <- fullDataYears$ttime * fullDataYears$igen_long


# setwd(version)
# sink("R_output_re.txt", append=FALSE)
# cat("RE results, R output for statistic analysis of MA thesis, Bence Kiss-Dobronyi", "\n")
# cat("RE results, with treated dummy", "\n")
# cat(Sys.time(), "\n")
# sink()

# sink("R_output_re.txt", append=TRUE)
# # FOR FULL SAMPLE
# cat("\n", "FOR FULL SAMPLE", "\n")
# # OLS for full sample
# cat("\n\n", "RE for full sample", "\n")
# REFunc(fullData, full_indicators)

# # FOR LIMITED SAMPLE
# cat("\n\n\n", "FOR LIMITED SAMPLE", "\n")
# # OLS for full sample
# cat("\n\n", "RE for full sample", "\n")
# REFunc(fullDataYears, full_indicators)

# sink()
# setwd("..")

setwd("pred")
predYears <- fullData
# ols_model_pred <- lm(e1 ~ igen + kor + factor(ev) + factor(agazat) + mse, data=fullDataYears)
# predYears$e1_pred <- predict(ols_model_pred, predYears)


pred <- fullData
for(i in full_indicators){
	if(substr(i, 1, 1) == "e"){
		formula_dummies <- as.formula(paste(i, " ~ igen + kor + mse"))
	} else if(substr(i, 1, 1) == "m"){
		formula_dummies <- as.formula(paste(i, " ~ igen + kor + e1"))
	} else {
		formula_dummies <- as.formula(paste(i, " ~ igen + kor"))
	}
	femod <- plm(formula_dummies, data=fullData, index=c("id", "ev"), model="within")
	pred[,paste0("pred",i)] <- fixef(femod, type="dmean")
}
setwd("C:/Users/bence.kiss-dobronyi/Documents/Private/MAthesis_data/controlGroup/R_Output/pred")
write.csv(pred,file=paste0("predictions", ".txt"))


length(fullDataYears$id[(!is.na(fullDataYears$c_likvidrata))&(abs(fullDataYears$c_likvidrata)>20)])