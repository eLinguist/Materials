###################################################
#                                                 #
# Generate random language sets for bootstrapping #
#                                                 #
###################################################

# Getting repartition of the consonants and word length in the dataset with all words in all languages
# Output are 2 vectors: consonant repartition, word length repartition for each word
# ConsonantRepartionVector, ShortWordLengthRepartionVector, LongWordLengthRepartionVector

#setwd("C:/Sprachgenetik/R")
setwd("C:/Forschung")
library("stringr")
require(sqldf)

# CSV with the all the available words
df2 <- read.csv2("AllWords.csv")

# Preparing the calculation of occurence of the phonems ("consonant clusters") in all languages
dataset <- df2[,2] # Data
PhonemeOccurence <- data.frame(Consonant=character(),Occurences=double(),stringsAsFactors=FALSE) # New dataframe
Phonemes <- c("#-","7-", "B-", "C-", "CH-","D-","F-","G-","H-","J-","K-","KH-","L-","M-","N-","P-","R-","S-","SH-","T-","TH-","V-","W-","Z-","ZH-")

#Counting the total number of occurences of each phoneme
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

# Function to generate words -> random according to the distribution of number of syllables the word has in all languages and according to the phoneme occurence in all languages.
GenerateWord <- function(Word=""){
        # Calculates the number of cases where a word has 1, 2, 3 and 4 syllable(s) for each of the 18 words.
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

# Preparing a dataframe to store randomly generated word in language structure (we generate random languages with all 18 word=.
NewRandomLanguage <- data.frame(matrix(ncol = 5, nrow = 0))
x <- c("Family", "Language", "WordEnglish", "WordLg", "WordTranscript")
colnames(NewRandomLanguage) <- x

# Languages are named "Dummy" + number of the itteration in the loop.
GroupToAdd <- "Dummy"
WordList <- c("Death","Ear","Eye","Four","Hand","I","Name","Night","Nose","Sun","Three","Tongue","Tooth","Two","Water","Who","Wind","You (thou)")
j =0
while (j < 5) {
   j <- j+1
   LanguageToAdd <- paste0( "Dummy_",j)
   for (strWord in WordList) {
      WordToAdd <- strWord
      WordToAddLG <- GenerateWord(WordToAdd)  # Calls the function that generates words according to the #syllable distribution of the input word
      newrow <- data.frame(Family=GroupToAdd,Language=LanguageToAdd,WordEnglish=WordToAdd,WordLg=WordToAddLG,WordTranscript=WordToAddLG)
      NewRandomLanguage <- rbind(NewRandomLanguage,newrow)
   }
}
write.csv2(NewRandomLanguage, file = "newRandomLanguages.csv", row.names=FALSE)