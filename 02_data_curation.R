source("C:/Users/Roethke/SynologyDrive/Statistik Session/R_Code/01_import_data.R")
#identify IV
Row_Cert_14 = which(data_snake$ZF01_01 == '2'|data_snake$ZF01_01 == '4')
Row_AH_loss = which(data_snake$ZF01_01 == '3'|data_snake$ZF01_01 == '4')

#create dummy variable
data_snake$Cert = numeric(length(data_snake[1]))
data_snake$Cert[Row_Cert_14] = 1
data_snake$PLE = numeric(length(data_snake[1]))
data_snake$PLE[Row_AH_loss] = 1
#clean
rm(Row_Cert_14, Row_AH_loss)

#MC-Check AH
Row_MC_AH_pass = which(data_snake[,74] == 1 & data_snake[,30] == '3'| data_snake[,74] == 1 & data_snake[,30] == '4')
Row_add_AH = which(data_snake[,30] == '1'|data_snake[,30] == '2')
Row_AH_pass = c(Row_MC_AH_pass,Row_add_AH)
data_snake = data_snake[Row_AH_pass,]
#clean
rm(Row_MC_AH_pass,Row_add_AH,Row_AH_pass)

#MC-Check Cert
Row_MC_Cert_pass = which(data_snake[,73] == 1 & data_snake[,30] == '2'| data_snake[,73] == 1 & data_snake[,30] == '4')
Row_add_Cert = which(data_snake[,30] == '1'|data_snake[,30] == '3')
Row_Cert_pass = c(Row_MC_Cert_pass,Row_add_Cert)
data_snake = data_snake[Row_Cert_pass,]
#clean
rm(Row_MC_Cert_pass,Row_add_Cert,Row_Cert_pass)

#create DV
data_snake$Select = data_snake$SZ01
data_snake$Select[data_snake$Select == 1] = 0
data_snake$Select[data_snake$Select == 2] = 1


#covariates
data_snake$RA1 = data_snake$MM05_01
data_snake$RA2 = data_snake$MM05_02
data_snake$RA3 = data_snake$MM05_03

# colnames(data_snake)[134] = "RA"

#reverse coding
RA2_temp = data_snake$RA2
data_snake$RA2 = numeric(length(RA2_temp))

data_snake$RA2[which(RA2_temp == 1)] = 7
data_snake$RA2[which(RA2_temp == 2)] = 6
data_snake$RA2[which(RA2_temp == 3)] = 5
data_snake$RA2[which(RA2_temp == 4)] = 3
data_snake$RA2[which(RA2_temp == 5)] = 2
data_snake$RA2[which(RA2_temp == 7)] = 1

#clean
rm(RA2_temp)

data_snake$RA = (data_snake$RA1 + data_snake$RA2 + data_snake$RA3)/3

data_snake$PMV1 = data_snake$MM03_01
data_snake$PMV2 = data_snake$MM03_02
data_snake$PMV3 = data_snake$MM03_03

data_snake$PMV = (data_snake$PMV1 + data_snake$PMV2 + data_snake$PMV3)/3

data_snake$Play1 = data_snake$MM06_01
data_snake$Play2 = data_snake$MM06_02
data_snake$Play3 = data_snake$MM06_03
data_snake$Play4 = data_snake$MM06_04

data_snake$Play = (data_snake$Play1 + data_snake$Play2 + data_snake$Play3 + data_snake$Play4)/4

data_snake$Gend = data_snake$SD01
data_snake$Gend[data_snake$Gend == 1] = 0
data_snake$Gend[data_snake$Gend == 3] = 0
data_snake$Gend[data_snake$Gend == 2] = 1

data_snake$GamEx = as.numeric(data_snake$EG11_02)
data_snake$GamEx[is.na(data_snake$GamEx)] = median(data_snake$GamEx, na.rm = TRUE)

data_snake$GameType1 = data_snake$EG08_01
data_snake$GameType2 = data_snake$EG08_02
data_snake$GameType3 = data_snake$EG08_03
data_snake$GameType4 = data_snake$EG08_04
data_snake$GameType5 = data_snake$EG08_05

data_snake$GameType = (data_snake$GameType1 + data_snake$GameType2 + 
                       data_snake$GameType3 + data_snake$GameType4 + data_snake$GameType5)/5

data_snake$ProdInv = data_snake$LB05_03

data_snake$LBSpend = data_snake$LB02

data_snake$PBC1 = data_snake$MM01_02
data_snake$PBC2 = data_snake$MM01_03
data_snake$PBC = (data_snake$PBC1 + data_snake$PBC2)/2

data_snake$CL1 = data_snake$MM02_01
data_snake$CL2 = data_snake$MM02_02
data_snake$CL3 = data_snake$MM02_03

data_snake$CL = (data_snake$CL1 + data_snake$CL2 + data_snake$CL3)/3

data_snake$PK1 = data_snake$LB04_01
data_snake$PK2 = data_snake$LB04_02
data_snake$PK3 = data_snake$LB04_03
data_snake$PK4 = data_snake$LB04_04

#reverse coding
PK3_temp = data_snake$PK3
data_snake$PK3 = numeric(length(PK3_temp))

data_snake$PK3[which(PK3_temp == 1)] = 7
data_snake$PK3[which(PK3_temp == 2)] = 6
data_snake$PK3[which(PK3_temp == 3)] = 5
data_snake$PK3[which(PK3_temp == 4)] = 3
data_snake$PK3[which(PK3_temp == 5)] = 2
data_snake$PK3[which(PK3_temp == 7)] = 1

#clean
rm(PK3_temp)

data_snake$ProdKnow = (data_snake$PK1 + data_snake$PK2 + data_snake$PK3 + data_snake$PK4)/4

data_snake$LBFreq = data_snake$SD10

data_snake$Age = data_snake$SD02_01
data_snake$Age[is.na(data_snake$Age)] = median(data_snake$Age, na.rm = TRUE)

data_snake$Age[data_snake$Age>69] = 6
data_snake$Age[data_snake$Age>59] = 5
data_snake$Age[data_snake$Age>49] = 4
data_snake$Age[data_snake$Age>39] = 3
data_snake$Age[data_snake$Age>29] = 2
data_snake$Age[data_snake$Age>19] = 1

