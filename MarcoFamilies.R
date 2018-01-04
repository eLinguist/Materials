##################################################################################
#                                                                                #
#    eLinguistics.net -> Long range relationship analysis - Focus on NOSTRATIC   #
#                                                                                #
#    Vincent Beaufils, 14.12.2017                                                #
#                                                                                #
##################################################################################


library(sqldf)
setwd("c:/Forschung/")
data <- read.csv("PairwiseComparisons.csv",header=T,stringsAsFactors=FALSE, sep=";")

#########################################################################################################################
# CALL ALL MACRO FAMILIES IN A LOOP -> OUTPUT IN CSV #
######################################################

MacroFamilies <- c("_IE_","_AA_","Uralic","Turkic","Mongolic","Tungusic","Dravidian","Kartvelian","Caucasian","Niger_Congo","Bantoo","Sino_Tibetan","Austroasiatic","_MP_","Tai_Kadai","Eskimo","Korean","Japan","Ainu","Burushaski","Basque","Summerian","Elamite")
Nostratic <- c("('00_IE_PIE','01_IE_Tocharian','02_IE_Indo_Aryan','03_IE_Iranian','04_IE_Baltic','05_IE_Slavic','06_IE_Germanic','07_IE_Celtic','08_IE_Romance','09_IE_Greek','10_IE_Albanian','11_IE_Armenian','12_IE_Anatolian','20_AA_Semitic','21_AA_Berber','22_AA_Egyptian','23_AA_Chadic','24_AA_Cushitic','25_AA_Omotic','30_AL_Mongolic','31_AL_Turkic','32_TU_Tungusic','33_UR_Uralic','51_KA_Kartvelian','60_DR_Dravidian','IE_Creole')")
DistanceLimit <- 76 # Pairwise Distance limit 

# Data Frame to store output
MonitorClusters <- data.frame(Macro_Family=character(),Sample_Size=double(),SelfSmallerThan78=double(),NostraticSmallerThan78=double(),NotinNostraticSmallerThan78=double(),DummiesSmallerThan78=double()) 

# Loop to produce results
for(MacroFamily in MacroFamilies) {
  sampleSize <- sqldf(paste0("SELECT * FROM data WHERE G1 LIKE '%", MacroFamily, "%' AND G2 NOT LIKE '%", MacroFamily, "%' AND NbrWords >14"))
  CompareSELF_ALL <- sqldf(paste0("SELECT * FROM data WHERE G1 LIKE '%", MacroFamily, "%' AND G2 LIKE '%", MacroFamily, "%' AND NbrWords >14"))
  CompareSELF_BELOW_LIMIT <- sqldf(paste0("SELECT * FROM data WHERE G1 LIKE '%", MacroFamily, "%' AND G2 LIKE '%", MacroFamily, "%' AND Dist < ",DistanceLimit," AND NbrWords >14"))
  CompareDUMMIES_ALL <- sqldf(paste0("SELECT * FROM data WHERE G1 LIKE '%", MacroFamily, "%' AND G2 LIKE '%", "Dummy", "%' AND NbrWords >14"))
  CompareDUMMIES_BELOW_LIMIT <- sqldf(paste0("SELECT * FROM data WHERE G1 LIKE '%", MacroFamily, "%' AND G2 LIKE '%", "Dummy", "%' AND Dist < ",DistanceLimit," AND NbrWords >14"))
  CompareIN_NOSTRATIC_ALL <- sqldf(paste0("SELECT * FROM data WHERE G1 LIKE '%", MacroFamily, "%' AND G2 IN ",Nostratic," AND G2 NOT LIKE '%",MacroFamily,"%' AND G2 NOT LIKE 'IE_Creole' AND G2 NOT LIKE '%", "Dummy", "%' AND NbrWords > 14"))
  CompareIN_NOSTRATIC_BELOW_LIMIT <- sqldf(paste0("SELECT * FROM data WHERE G1 LIKE '%", MacroFamily, "%' AND G2 IN ",Nostratic," AND G2 NOT LIKE '%",MacroFamily,"%' AND G2 NOT LIKE 'IE_Creole' AND G2 NOT LIKE '%", "Dummy", "%' AND Dist < ",DistanceLimit," AND NbrWords > 14"))
  CompareNOT_IN_NOSTRATIC_ALL <- sqldf(paste0("SELECT * FROM data WHERE G1 LIKE '%", MacroFamily, "%' AND G2 NOT IN ",Nostratic," AND G2 NOT LIKE '%",MacroFamily,"%' AND G2 NOT LIKE '%", "Dummy", "%' AND NbrWords > 14"))
  CompareNOT_IN_NOSTRATIC_BELOW_LIMIT <- sqldf(paste0("SELECT * FROM data WHERE G1 LIKE '%", MacroFamily, "%' AND G2 NOT IN ",Nostratic," AND G2 NOT LIKE '%",MacroFamily,"%' AND Dist < ",DistanceLimit," AND G2 NOT LIKE '%", "Dummy", "%' AND NbrWords > 14"))
  newrow <- data.frame(MacroFamily,nrow(sampleSize),nrow(CompareSELF_BELOW_LIMIT)/nrow(CompareSELF_ALL),nrow(CompareIN_NOSTRATIC_BELOW_LIMIT)/nrow(CompareIN_NOSTRATIC_ALL),nrow(CompareNOT_IN_NOSTRATIC_BELOW_LIMIT)/nrow(CompareNOT_IN_NOSTRATIC_ALL),nrow(CompareDUMMIES_BELOW_LIMIT)/nrow(CompareDUMMIES_ALL))
  MonitorClusters  <- rbind(MonitorClusters,newrow)
}
write.csv2(file="Output.csv", MonitorClusters)

#########################################################################################################################
# CALL A SINGLE COMPARISON #
############################

MacroFamily <- "Dummy"
Nostratic <- c("('00_IE_PIE','01_IE_Tocharian','02_IE_Indo_Aryan','03_IE_Iranian','04_IE_Baltic','05_IE_Slavic','06_IE_Germanic','07_IE_Celtic','08_IE_Romance','09_IE_Greek','10_IE_Albanian','11_IE_Armenian','12_IE_Anatolian','20_AA_Semitic','21_AA_Berber','22_AA_Egyptian','23_AA_Chadic','24_AA_Cushitic','25_AA_Omotic','30_AL_Mongolic','31_AL_Turkic','32_TU_Tungusic','33_UR_Uralic','51_KA_Kartvelian','60_DR_Dravidian','IE_Creole')")
DistanceLimit <- 76 # Pairwise Distance limit 

sampleSize <- sqldf(paste0("SELECT * FROM data WHERE G1 LIKE '%", MacroFamily, "%' AND G2 NOT LIKE '%", MacroFamily, "%' AND NbrWords >14"))
CompareSELF_ALL <- sqldf(paste0("SELECT * FROM data WHERE G1 LIKE '%", MacroFamily, "%' AND G2 LIKE '%", MacroFamily, "%' AND NbrWords >14"))
CompareSELF_BELOW_LIMIT <- sqldf(paste0("SELECT * FROM data WHERE G1 LIKE '%", MacroFamily, "%' AND G2 LIKE '%", MacroFamily, "%' AND Dist < ",DistanceLimit," AND NbrWords >14"))
CompareDUMMIES_ALL <- sqldf(paste0("SELECT * FROM data WHERE G1 LIKE '%", MacroFamily, "%' AND G2 LIKE '%", "Dummy", "%' AND NbrWords >14"))
CompareDUMMIES_BELOW_LIMIT <- sqldf(paste0("SELECT * FROM data WHERE G1 LIKE '%", MacroFamily, "%' AND G2 LIKE '%", "Dummy", "%' AND Dist < ",DistanceLimit," AND NbrWords >14"))
CompareIN_NOSTRATIC_ALL <- sqldf(paste0("SELECT * FROM data WHERE G1 LIKE '%", MacroFamily, "%' AND G2 IN ",Nostratic," AND G2 NOT LIKE '%",MacroFamily,"%' AND G2 NOT LIKE 'IE_Creole' AND G2 NOT LIKE '%", "Dummy", "%' AND NbrWords > 14"))
CompareIN_NOSTRATIC_BELOW_LIMIT <- sqldf(paste0("SELECT * FROM data WHERE G1 LIKE '%", MacroFamily, "%' AND G2 IN ",Nostratic," AND G2 NOT LIKE '%",MacroFamily,"%' AND G2 NOT LIKE 'IE_Creole' AND G2 NOT LIKE '%", "Dummy", "%' AND Dist < ",DistanceLimit," AND NbrWords > 14"))
CompareNOT_IN_NOSTRATIC_ALL <- sqldf(paste0("SELECT * FROM data WHERE G1 LIKE '%", MacroFamily, "%' AND G2 NOT IN ",Nostratic," AND G2 NOT LIKE '%",MacroFamily,"%' AND G2 NOT LIKE '%", "Dummy", "%' AND NbrWords > 14"))
CompareNOT_IN_NOSTRATIC_BELOW_LIMIT <- sqldf(paste0("SELECT * FROM data WHERE G1 LIKE '%", MacroFamily, "%' AND G2 NOT IN ",Nostratic," AND G2 NOT LIKE '%",MacroFamily,"%' AND Dist < ",DistanceLimit," AND G2 NOT LIKE '%", "Dummy", "%' AND NbrWords > 14"))

print(paste("Sample size: ",nrow(sampleSize)))
print(paste("Self < limit: ",nrow(CompareSELF_BELOW_LIMIT)/nrow(CompareSELF_ALL)))
print(paste("Dummies < limit: ",nrow(CompareDUMMIES_BELOW_LIMIT)/nrow(CompareDUMMIES_ALL)))
print(paste("Nostratic < limit: ",nrow(CompareIN_NOSTRATIC_BELOW_LIMIT)/nrow(CompareIN_NOSTRATIC_ALL)))
print(paste("Not in Nostratic < limit: ",nrow(CompareNOT_IN_NOSTRATIC_BELOW_LIMIT)/nrow(CompareNOT_IN_NOSTRATIC_ALL)))
print(paste(CompareIN_NOSTRATIC_BELOW_LIMIT$L1,"-",CompareIN_NOSTRATIC_BELOW_LIMIT$L2,"-",CompareIN_NOSTRATIC_BELOW_LIMIT$Dist))
