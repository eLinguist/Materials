##################################################################################
#                                                                                #
#    eLinguistics.net -> Estimating age of the nodes                             #
#                        Processing a table with distance betwenn families       #
#                                                                                #
#    Vincent Beaufils, 14.12.2017                                                #
#                                                                                #
##################################################################################


library(sqldf)
setwd("c:/Forschung/")
data <- read.csv2("PairwiseComparisons.csv",header=T,stringsAsFactors=FALSE, sep=";")

#########################################################################################################################
# CALL ALL MACRO FAMILIES IN A LOOP -> OUTPUT IN CSV #
######################################################

#Clades <- c('00_IE_PIE','01_IE_Tocharian','02_IE_Indo_Aryan','03_IE_Iranian','04_IE_Baltic','05_IE_Slavic','06_IE_Germanic','07_IE_Celtic','08_IE_Romance','09_IE_Greek','10_IE_Albanian','11_IE_Armenian','12_IE_Anatolian','20_AA_Semitic','21_AA_Berber','22_AA_Egyptian','23_AA_Chadic','24_AA_Cushitic','25_AA_Omotic','30_AL_Mongolic','31_AL_Turkic','32_TU_Tungusic','33_UR_Uralic','51_KA_Kartvelian','60_DR_Dravidian')
#Clades <- c('00_IE_PIE','01_IE_Tocharian','04_IE_Baltic','05_IE_Slavic','06_IE_Germanic')
#Clades <- c('02_IE_Indo_Aryan','03_IE_Iranian','04_IE_Baltic','05_IE_Slavic','06_IE_Germanic','07_IE_Celtic','08_IE_Romance','08_IE_Romance_Sard','08_IE_Romance_East','09_IE_Greek','10_IE_Albanian','11_IE_Armenian','20_AA_Semitic','21_AA_Berber','23_AA_Chadic','24_AA_Cushitic','25_AA_Omotic','30_AL_Mongolic','31_AL_Turkic','33_UR_Uralic','51_KA_Kartvelian','60_DR_Dravidian','50_CA_NE_Caucasian','50_CA_NW_Caucasian','40_MP_Malay','41_MP_Polynesian','52_ST_Sino_Tibetan','55_AU_Austroasiatic','53_TK_Tai_Kadai','Dummy')
#Clades <- c('01_IE_Tocharian','02_IE_Indo_Aryan','03_IE_Iranian','04_IE_Baltic','05_IE_Slavic','06_IE_Germanic','07_IE_Celtic','08_IE_Romance','08_IE_Sard_Romance','08_IE_East_Romance','09_IE_Greek','10_IE_Albanian','11_IE_Armenian','12_IE_Anatolian','Dummy')
Clades <- c('00_IE_PIE','01_IE_Tocharian','02_IE_Indo_Aryan','03_IE_Iranian','04_IE_Baltic','05_IE_Slavic','06_IE_Germanic','07_IE_Celtic','08_IE_Romance','08_IE_Sard_Romance','08_IE_East_Romance','09_IE_Greek','10_IE_Albanian','11_IE_Armenian','12_IE_Anatolian','20_AA_Semitic','21_AA_Berber','23_AA_Chadic','24_AA_Cushitic','25_AA_Omotic','30_AL_Mongolic','31_AL_Turkic','32_TU_Tungusic','33_UR_Uralic','50_CA_NE_Caucasian','50_CA_NW_Caucasian','51_KA_Kartvelian','60_DR_Dravidian','40_MP_Malay','41_MP_Polynesian','52_ST_Sino_Tibetan','55_AU_Austroasiatic','53_TK_Tai_Kadai','Dummy')

# Data Frame to store output
MonitorClades <- data.frame(Clade_1=character(),Clade_2=character(),Sample_Size=double(),Mean=double(),Sigma=double()) 

# Progress bar:
pb = txtProgressBar(min = 0, max = length(Clades), style=3) 
i <-0
# Loop to produce results
for(clade1 in Clades) {
  i <- i+1
  setTxtProgressBar(pb,i)
  for(clade2 in Clades) {
  sampleSize <- sqldf(paste0("SELECT * FROM data WHERE Group1 LIKE '%", clade1, "%' AND Group2 LIKE '%", clade2, "%' AND NbrWords > 14 AND AgeOldestLG = 2000"))
  Clade1toClade2 <- sqldf(paste0("SELECT * FROM data WHERE Group1 LIKE '%", clade1, "%' AND Group2 LIKE '%", clade2, "%' AND NbrWords >14 AND AgeOldestLG = 2000"))
  MeanClade1toClade2 <- mean(Clade1toClade2$Dist)
  sdClade1toClade2 <- sd(Clade1toClade2$Dist)  
  newrow <- data.frame(clade1,clade2,nrow(Clade1toClade2),MeanClade1toClade2,sdClade1toClade2)
  MonitorClades  <- rbind(MonitorClades,newrow)
  }
}
write.csv2(file="MonitorClades.csv", MonitorClades)

#########################################################################################################################
# CALL A SINGLE COMPARISON #
############################


Clades <- c("('00_IE_PIE','01_IE_Tocharian','02_IE_Indo_Aryan','03_IE_Iranian','04_IE_Baltic','05_IE_Slavic','06_IE_Germanic','07_IE_Celtic','08_IE_Romance','09_IE_Greek','10_IE_Albanian','11_IE_Armenian','12_IE_Anatolian','20_AA_Semitic','21_AA_Berber','22_AA_Egyptian','23_AA_Chadic','24_AA_Cushitic','25_AA_Omotic','30_AL_Mongolic','31_AL_Turkic','32_TU_Tungusic','33_UR_Uralic','51_KA_Kartvelian','60_DR_Dravidian')")
Clade1 <- '05_IE_Slavic'
Clade2 <- 'Dummy'

sampleSize <- sqldf(paste0("SELECT * FROM data WHERE Group1 LIKE '%", Clade1, "%' AND Group2 LIKE '%", Clade2, "%' AND NbrWords > 14 AND AgeOldestLG = 2000"))
Clade1toClade2 <- sqldf(paste0("SELECT * FROM data WHERE Group1 LIKE '%", Clade1, "%' AND Group2 LIKE '%", Clade2, "%' AND NbrWords >14 AND AgeOldestLG = 2000"))
MeanClade1toClade2 <- mean(Clade1toClade2$Dist)
sdClade1toClade2 <- sd(Clade1toClade2$Dist)
print(paste("Clade 1: ",Clade1))
print(paste("Clade 2: ",Clade2))
print(paste("Sample size: ",nrow(sampleSize)))
print(paste("Mean: ",MeanClade1toClade2)) 
print(paste("Sigma: ",sdClade1toClade2))

