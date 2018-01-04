###################################################
#                                                 #
# Generate random language sets for bootstrapping #
#                                                 #
###################################################

# Getting repartition of the consonants and word length in the dataset will all words in all languages
# Output are 3 vectors: consonant repartition, word length repartition for "I", "you", "Three", "Two" and for the rest:
# ConsonantRepartionVector, ShortWordLengthRepartionVector, LongWordLengthRepartionVector
#setwd("C:/Sprachgenetik/R")
setwd("C:/Forschung")
library("stringr")

# CSV with the words
df2 <- read.csv2("AllWords.csv")

# Occurence of the phonems in all languages

dataset <- df2[,5]
PhonemeOccurence <- data.frame(Consonant=character(),Occurences=double(),stringsAsFactors=FALSE) 
Phonemes <- c("#-","7-", "B-", "C-", "CH-","D-","F-","G-","H-","J-","K-","KH-","L-","M-","N-","P-","R-","S-","SH-","T-","TH-","V-","W-","Z-","ZH-")

#Counting the total number of occurences 
for (strPhoneme in Phonemes) {
  nbrOccurences <- sum(str_count(dataset, strPhoneme))
  newrow <- data.frame(strPhoneme,nbrOccurences)
  PhonemeOccurence <- rbind(PhonemeOccurence, newrow)
}

NumberSyllAll <- sum(PhonemeOccurence$nbrOccurences)

#Counting the number of occurences relative to the total and setting Value in vector

ConsonantRepartionVector <- c()
for (strPhoneme in Phonemes) {
  nbrOccurences <- sum(str_count(dataset, strPhoneme))
  ConsonantRepartionVector <- c(ConsonantRepartionVector, nbrOccurences/NumberSyllAll)
}


# Number of Syllables in words

require(sqldf)


GenerateWord <- function(Word=""){
        NbrWords_4 <- nrow(sqldf(paste0("select * from df2 where (WordEnglish = '",Word,"') AND WordTranscript LIKE '-%-%-%-%-'")))
        NbrWords_3 <- nrow(sqldf(paste0("select * from df2 where (WordEnglish = '",Word,"') AND WordTranscript LIKE '-%-%-%-'"))) - NbrWords_4
        NbrWords_2 <- nrow(sqldf(paste0("select * from df2 where (WordEnglish = '",Word,"') AND WordTranscript LIKE '-%-%-'"))) - NbrWords_3 - NbrWords_4
        NbrWords_1 <- nrow(sqldf(paste0("select * from df2 where (WordEnglish = '",Word,"') AND WordTranscript LIKE '-%-'"))) - NbrWords_2 - NbrWords_3 - NbrWords_4
        Summe <- NbrWords_1+NbrWords_2+NbrWords_3+NbrWords_4
        WordLengthRepartionVector <- c(NbrWords_1/Summe,NbrWords_2/Summe,NbrWords_3/Summe,NbrWords_4/Summe)
        WordLength = sample(c(1,2,3,4),1,replace = TRUE, prob=WordLengthRepartionVector)
        WordOutput <- "-"
        i <- 0
        for (i in 0:(WordLength-1)) {
                i <- i+1;
                WordOutput <- paste0(WordOutput,sample( c("#", "7", "B", "C", "CH","D","F","G","H","J","K","KH","L","M","N","P","R","S","SH","T","TH","V","W","Z","ZH"), 1, replace=TRUE, 
                                                        ConsonantRepartionVector ),"-")
        }
        WordOutput
}


#NewRandomLanguage <- data.frame(Family=character(),Language=character(),WordEnglish=character(),WordLg=character(),WordTranscript=character(),stringsAsFactors=FALSE)
NewRandomLanguage <- data.frame(matrix(ncol = 5, nrow = 0))
x <- c("Family", "Language", "WordEnglish", "WordLg", "WordTranscript")
colnames(NewRandomLanguage) <- x


j=0
GroupToAdd <- "Dummy"

while (j < 200) {
j <- j+1
LanguageToAdd <- paste0( "Dummy_",j)
WordToAdd <- "Death"
WordToAddLG <- GenerateWord(WordToAdd)
WordToAddLGTranscript <- WordToAddLG
newrow <- data.frame(Family=GroupToAdd,Language=LanguageToAdd,WordEnglish=WordToAdd,WordLg=WordToAddLG,WordTranscript=WordToAddLGTranscript)
NewRandomLanguage <- rbind(NewRandomLanguage,newrow)
WordToAdd <- "Ear"
WordToAddLG <- GenerateWord(WordToAdd)
WordToAddLGTranscript <- WordToAddLG
newrow <- data.frame(Family=GroupToAdd,Language=LanguageToAdd,WordEnglish=WordToAdd,WordLg=WordToAddLG,WordTranscript=WordToAddLGTranscript)
NewRandomLanguage <- rbind(NewRandomLanguage,newrow)
WordToAdd <- "Eye"
WordToAddLG <- GenerateWord(WordToAdd)
WordToAddLGTranscript <- WordToAddLG
newrow <- data.frame(Family=GroupToAdd,Language=LanguageToAdd,WordEnglish=WordToAdd,WordLg=WordToAddLG,WordTranscript=WordToAddLGTranscript)
NewRandomLanguage <- rbind(NewRandomLanguage,newrow)
WordToAdd <- "Four"
WordToAddLG <- GenerateWord(WordToAdd)
WordToAddLGTranscript <- WordToAddLG
newrow <- data.frame(Family=GroupToAdd,Language=LanguageToAdd,WordEnglish=WordToAdd,WordLg=WordToAddLG,WordTranscript=WordToAddLGTranscript)
NewRandomLanguage <- rbind(NewRandomLanguage,newrow)
WordToAdd <- "Hand"
WordToAddLG <- GenerateWord(WordToAdd)
WordToAddLGTranscript <- WordToAddLG
newrow <- data.frame(Family=GroupToAdd,Language=LanguageToAdd,WordEnglish=WordToAdd,WordLg=WordToAddLG,WordTranscript=WordToAddLGTranscript)
NewRandomLanguage <- rbind(NewRandomLanguage,newrow)
WordToAdd <- "I"
WordToAddLG <- GenerateWord(WordToAdd)
WordToAddLGTranscript <- WordToAddLG
newrow <- data.frame(Family=GroupToAdd,Language=LanguageToAdd,WordEnglish=WordToAdd,WordLg=WordToAddLG,WordTranscript=WordToAddLGTranscript)
NewRandomLanguage <- rbind(NewRandomLanguage,newrow)
WordToAdd <- "Name"
WordToAddLG <- GenerateWord(WordToAdd)
WordToAddLGTranscript <- WordToAddLG
newrow <- data.frame(Family=GroupToAdd,Language=LanguageToAdd,WordEnglish=WordToAdd,WordLg=WordToAddLG,WordTranscript=WordToAddLGTranscript)
NewRandomLanguage <- rbind(NewRandomLanguage,newrow)
WordToAdd <- "Night"
WordToAddLG <- GenerateWord(WordToAdd)
WordToAddLGTranscript <- WordToAddLG
newrow <- data.frame(Family=GroupToAdd,Language=LanguageToAdd,WordEnglish=WordToAdd,WordLg=WordToAddLG,WordTranscript=WordToAddLGTranscript)
NewRandomLanguage <- rbind(NewRandomLanguage,newrow)
WordToAdd <- "Nose"
WordToAddLG <- GenerateWord(WordToAdd)
WordToAddLGTranscript <- WordToAddLG
newrow <- data.frame(Family=GroupToAdd,Language=LanguageToAdd,WordEnglish=WordToAdd,WordLg=WordToAddLG,WordTranscript=WordToAddLGTranscript)
NewRandomLanguage <- rbind(NewRandomLanguage,newrow)
WordToAdd <- "Sun"
WordToAddLG <- GenerateWord(WordToAdd)
WordToAddLGTranscript <- WordToAddLG
newrow <- data.frame(Family=GroupToAdd,Language=LanguageToAdd,WordEnglish=WordToAdd,WordLg=WordToAddLG,WordTranscript=WordToAddLGTranscript)
NewRandomLanguage <- rbind(NewRandomLanguage,newrow)
WordToAdd <- "Three"
WordToAddLG <- GenerateWord(WordToAdd)
WordToAddLGTranscript <- WordToAddLG
newrow <- data.frame(Family=GroupToAdd,Language=LanguageToAdd,WordEnglish=WordToAdd,WordLg=WordToAddLG,WordTranscript=WordToAddLGTranscript)
NewRandomLanguage <- rbind(NewRandomLanguage,newrow)
WordToAdd <- "Tongue"
WordToAddLG <- GenerateWord(WordToAdd)
WordToAddLGTranscript <- WordToAddLG
newrow <- data.frame(Family=GroupToAdd,Language=LanguageToAdd,WordEnglish=WordToAdd,WordLg=WordToAddLG,WordTranscript=WordToAddLGTranscript)
NewRandomLanguage <- rbind(NewRandomLanguage,newrow)
WordToAdd <- "Tooth"
WordToAddLG <- GenerateWord(WordToAdd)
WordToAddLGTranscript <- WordToAddLG
newrow <- data.frame(Family=GroupToAdd,Language=LanguageToAdd,WordEnglish=WordToAdd,WordLg=WordToAddLG,WordTranscript=WordToAddLGTranscript)
NewRandomLanguage <- rbind(NewRandomLanguage,newrow)
WordToAdd <- "Two"
WordToAddLG <- GenerateWord(WordToAdd)
WordToAddLGTranscript <- WordToAddLG
newrow <- data.frame(Family=GroupToAdd,Language=LanguageToAdd,WordEnglish=WordToAdd,WordLg=WordToAddLG,WordTranscript=WordToAddLGTranscript)
NewRandomLanguage <- rbind(NewRandomLanguage,newrow)
WordToAdd <- "Water"
WordToAddLG <- GenerateWord(WordToAdd)
WordToAddLGTranscript <- WordToAddLG
newrow <- data.frame(Family=GroupToAdd,Language=LanguageToAdd,WordEnglish=WordToAdd,WordLg=WordToAddLG,WordTranscript=WordToAddLGTranscript)
NewRandomLanguage <- rbind(NewRandomLanguage,newrow)
WordToAdd <- "Who"
WordToAddLG <- GenerateWord(WordToAdd)
WordToAddLGTranscript <- WordToAddLG
newrow <- data.frame(Family=GroupToAdd,Language=LanguageToAdd,WordEnglish=WordToAdd,WordLg=WordToAddLG,WordTranscript=WordToAddLGTranscript)
NewRandomLanguage <- rbind(NewRandomLanguage,newrow)
WordToAdd <- "Wind"
WordToAddLG <- GenerateWord(WordToAdd)
WordToAddLGTranscript <- WordToAddLG
newrow <- data.frame(Family=GroupToAdd,Language=LanguageToAdd,WordEnglish=WordToAdd,WordLg=WordToAddLG,WordTranscript=WordToAddLGTranscript)
NewRandomLanguage <- rbind(NewRandomLanguage,newrow)
WordToAdd <- "You (thou)"
WordToAddLG <- GenerateWord(WordToAdd)
WordToAddLGTranscript <- WordToAddLG
newrow <- data.frame(Family=GroupToAdd,Language=LanguageToAdd,WordEnglish=WordToAdd,WordLg=WordToAddLG,WordTranscript=WordToAddLGTranscript)
NewRandomLanguage <- rbind(NewRandomLanguage,newrow)
}
write.csv2(NewRandomLanguage, file = "newRandomLanguages.csv", row.names=FALSE)
