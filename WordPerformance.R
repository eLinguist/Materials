##################################################################################
#                                                                                #
#    eLinguistics.net -> Measuring how the 18 words perform in the context       #
#                        of comparisons within/across families & subfamilies     #
#                                                                                #
#    Vincent Beaufils, 06.01.2017                                                #
#                                                                                #
##################################################################################

library(sqldf)
setwd("c:/Forschung/")
data <- read.csv2("PairwiseComparisons.csv",header=T,stringsAsFactors=FALSE, sep=";")

Clades1 <- c('01_IE_Tocharian','02_IE_Indo_Aryan','03_IE_Iranian','04_IE_Baltic','05_IE_Slavic','06_IE_Germanic','07_IE_Celtic','08_IE_Romance','08_IE_Sard_Romance','08_IE_East_Romance','09_IE_Greek','10_IE_Albanian','11_IE_Armenian','12_IE_Anatolian')
Clades2 <- c('01_IE_Tocharian','02_IE_Indo_Aryan','03_IE_Iranian','04_IE_Baltic','05_IE_Slavic','06_IE_Germanic','07_IE_Celtic','08_IE_Romance','08_IE_Sard_Romance','08_IE_East_Romance','09_IE_Greek','10_IE_Albanian','11_IE_Armenian','12_IE_Anatolian')

WordList <- c("Death","Ear","Eye","Four","Hand","I","Name","Night","Nose","Sun","Three","Tongue","Tooth","Two","Water","Who","Wind","You (thou)")

# Data Frames to store output
MonitorIE <- data.frame(Word=character(),BetweenFamilies=double(),WithinFamilies=double())
MonitorURALIC <- data.frame(Word=character(),BetweenFamilies=double())
MonitorDUMMIES <- data.frame(Word=character(),BetweenFamilies=double())
MonitorWordPerformance <- data.frame(IE_in_Families=character(),IE_between_Families=character(),IE_Uralic=character(),IE_Dummies=character()) 

# Process IE to IE across subfamilies and within two families (Columns 2 & 3, Column 1 is the word list)
# Progress bar:
pb = txtProgressBar(min=0, max=length(WordList), style=3)
i <- 0
# Loop to produce results
for(word in WordList) {
   PotentialBetween <- 0
   PotentialWithin <- 0
   OccurencesBetween <- 0
   OccurencesWithin <- 0
   i <- i+1
   setTxtProgressBar(pb,i)
   for(clade1 in Clades1) {
      for(clade2 in Clades2) {
         if (clade2 != clade1) {
            PotentialBetween <- PotentialBetween + nrow(sqldf(paste0("SELECT * FROM data WHERE Group1 LIKE '%", clade1, "%' AND Group2 LIKE '%", clade2, "%' AND LG1 != LG2")))
            OccurencesBetween <- OccurencesBetween + nrow(sqldf(paste0("SELECT * FROM data WHERE Group1 LIKE '%", clade1, "%' AND Group2 LIKE '%", clade2, "%' AND WordWithMatches LIKE '%", word, ",%' AND LG1 != LG2")))
         }
         if (clade2 == clade1) {
            PotentialWithin <- PotentialWithin + nrow(sqldf(paste0("SELECT * FROM data WHERE Group1 LIKE '%", clade1, "%' AND Group2 LIKE '%", clade2, "%' AND LG1 != LG2")))
            OccurencesWithin <- OccurencesWithin + nrow(sqldf(paste0("SELECT * FROM data WHERE Group1 LIKE '%", clade1, "%' AND Group2 LIKE '%", clade2, "%' AND WordWithMatches LIKE '%", word, ",%' AND LG1 != LG2")))
         }
      }
   }
   newrow <- data.frame(word,OccurencesBetween/PotentialBetween,OccurencesWithin/PotentialWithin)
   MonitorIE <- rbind(MonitorIE,newrow)
}

# Process IE to Uralic (Columns 4)
clade2 <- c('33_UR_Uralic')
# Progress bar:
pb = txtProgressBar(min=0, max=length(WordList), style=3)
i <- 0
# Loop to produce results
for(word in WordList) {
  Potential <- 0
  Occurences <- 0
  i <- i+1
  setTxtProgressBar(pb,i)
  for(clade1 in Clades1) {
        Potential <- Potential + nrow(sqldf(paste0("SELECT * FROM data WHERE Group1 LIKE '%", clade1, "%' AND Group2 LIKE '%", clade2, "%'")))
        Occurences <- Occurences + nrow(sqldf(paste0("SELECT * FROM data WHERE Group1 LIKE '%", clade1, "%' AND Group2 LIKE '%", clade2, "%' AND WordWithMatches LIKE '%", word, ",%'")))
  }
  newrow <- data.frame(word,Occurences/Potential)
  MonitorURALIC <- rbind(MonitorURALIC,newrow)
}

# Process IE to Dummies (Columns 5)
clade2 <- c('Dummy')
# Progress bar:
pb = txtProgressBar(min=0, max=length(WordList), style=3)
i <- 0
# Loop to produce results
for(word in WordList) {
  Potential <- 0
  Occurences <- 0
  i <- i+1
  setTxtProgressBar(pb,i)
  for(clade1 in Clades1) {
    Potential <- Potential + nrow(sqldf(paste0("SELECT * FROM data WHERE Group1 LIKE '%", clade2, "%' AND Group2 LIKE '%", clade1, "%'")))
    Occurences <- Occurences + nrow(sqldf(paste0("SELECT * FROM data WHERE Group1 LIKE '%", clade2, "%' AND Group2 LIKE '%", clade1, "%' AND WordWithMatches LIKE '%", word, ",%'")))
  }
  newrow <- data.frame(word,Occurences/Potential)
  MonitorDUMMIES <- rbind(MonitorDUMMIES,newrow)
}
MonitorIEAll <- cbind(MonitorIE, MonitorURALIC[,2])
MonitorIEAll <- cbind(MonitorIEAll, MonitorDUMMIES[,2])
colnames(MonitorIEAll) <- c("Word","IE between Families","IE within Families", "IE to Uralic", "IE to Dummies")
write.csv2(file="MonitorClades.csv", MonitorIEAll)