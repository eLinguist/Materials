# Matrix for import should have no space and no row names in first col and should include the 0 for diagonal value
# First line is 0 and last line is deleted???

# Tree annotation and manipulation in R (classical):
# http://www.jcsantosresearch.org/Class_2014_Spring_Comparative/pdf/week_6/Feb_12_2015_Chronograms.pdf
# Tree annotation (ggtree):
# https://bioconductor.org/packages/devel/bioc/vignettes/ggtree/inst/doc/treeAnnotation.html
# Other options: 
# https://www.r-phylo.org/wiki/HowTo/DataTreeManipulation#How_can_I_resolve_polytomies_in_my_phylogeny.3F
#
# https://www.r-statistics.com/wp-content/uploads/2014/07/2014-07-01-useR2014-03.pdf


setwd("C:/Sprachgenetik/R")
setwd("C:/Forschung")
library(ape)  
library(phangorn) # For UPGMA
#library(seqinr) # For Bootstraping
library(pvclust) # For Bootstraping
library("dendextend") # For tree comparisons & manipulation


#ColomnNames=c('Abkhaz','Adyghe','Afar','Afrikaans','Albanian_(Gheg)','Albanian_(Tosk)','Aleut','Altai','Amharic','Arabic','Aramaic_(Syriac)','Arbanaski','Armenian','Armenian_(Classical)','Assamese','Assyro-Babylonian','Avar','Avestan','Azeri','Balochi','Basaa','Bashkir','Belarusian','Bemba','Bengali','Bihari','Bole','Brahui','Brazilian','Breton','Bulgarian','Burmese','Buryat','Buyang','Catalan','Cebuano','Chechen','Chinese_(Cantonese)','Chinese_(Mandarin)','Chuvash','Comorian','Coptic','Cornish','Croatian','Czech','Dalmatian','Danish','Dargwa','Dhivehi_(Maldivian)','Dizi','Douala','Dutch','English','Estonian','Even','Faroese','Finnish','Flemish','French','Frisian','Friulian','Fula','Galician','Gaulish','Geez','Gelao','German','Gothic','Greek_(ancient)','Greek_(modern)','Greenlandic_(Eastern)','Greenlandic_(Western)','Gujarati','Gypsy_Romani','Hausa','Hebrew','Hindi','Hittite','Hungarian','Icelandic','Igbo','Indonesian','Irish','Ishkashimi','Istro-Romanian','Italian','Javanese','Kabardino_Cherkess','Kabylian','Kalasha','Kalmyk','Kannada','Karelian','Kashmiri','Kazakh','Khanti','Khmer','Khowar','Kivalliq','Komi','Kurdish','Kurukh','Kyrgyz','Labrador_Inuttut','Lahnda','Lak','Lao','Latin','Latvian','Lezgian','Lithuanian','Luwian','Magahi','Malagasy','Malayalam','Maltese','Manchu','Mansi','Maori','Marathi','Mari','Marwari','Middle_Persian','Mon','Mongolian','Nayi','Nenets','Nepali','Norwegian(bokmal)','Norwegian(nynorsk)','Old_Church_Slavonic','Old_Egyptian','Old_English','Old_High_German','Old_Irish','Old_Norse','Old_Persian','Oriya','Oromo','Oroqen','Ossetic_(Digor)','Ossetic_(Iron)','Pashto','Persian','Phoenician','Polish','Portuguese','Provencal','Prussian','Punjabi','Romanian','Russian','Saka','Sami','Samoan','Sanskrit','Sanskrit_(Vedic)','Sardinian_L','Sardinian_N','Sariqoli','Schwyzerduetsch','Scots_(Scottish_English)','Scottish_Gaelic','Serbian','Shan','Sheko','Sindhi','Sinhalese','Slovak','Slovene','Sogdian','Somali','Spanish','Swahili','Swedish','Tagalog','Tahitian','Tajik','Tamasheq','Tamil','Tashelhit','Tatar','Tausug','Telugu','Thai','Tibetan','Tigrigna','Tmazight_(Riffian_Berber)','Tocharian_A','Tocharian_B','Tsakonian','Turkish','Turkmen','Tuvan','Ukrainian','Urdu','Uyghur','Uzbek','Veps','Vietnamese','Vlach','Wakhi','Waziri','Welsh','Wolof','Yakut','Yiddish','Yoruba','Yupik','Zazaki','Zulu')
ColomnNames=c('Abkhaz','Adyghe','Afar','Afrikaans','Albanian_(Gheg)','Albanian_(Tosk)','Aleut','Altai','Amharic','Arabic','Aramaic_(Syriac)','Arbanaski','Armenian','Armenian_(Classical)','Assamese','Assyro-Babylonian','Avar','Avestan','Azeri','Balochi','Basaa','Bashkir','Belarusian','Bemba','Bengali','Bihari','Bole','Brahui','Brazilian','Breton','Bulgarian','Burmese','Buryat','Buyang','Catalan','Cebuano','Chechen','Chinese_(Cantonese)','Chinese_(Mandarin)','Chuvash','Comorian','Coptic','Cornish','Croatian','Czech','Dalmatian','Danish','Dargwa','Dhivehi_(Maldivian)','Dizi','Douala','Dutch','English','Estonian','Even','Faroese','Finnish','Flemish','French','Frisian','Friulian','Fula','Galician','Gaulish','Geez','Gelao','German','Gothic','Greek_(ancient)','Greek_(modern)','Greenlandic_(Eastern)','Greenlandic_(Western)','Gujarati','Gypsy_Romani','Hausa','Hebrew','Hindi','Hittite','Hungarian','Icelandic','Igbo','Indonesian','Irish','Ishkashimi','Istro-Romanian','Italian','Javanese','Kabardino_Cherkess','Kabylian','Kalasha','Kalmyk','Kannada','Karelian','Kashmiri','Kazakh','Khanti','Khmer','Khowar','Kivalliq','Komi','Kurdish','Kurukh','Kyrgyz','Labrador_Inuttut','Lahnda','Lak','Lao','Latin','Latvian','Lezgian','Lithuanian','Luwian','Magahi','Malagasy','Malayalam','Maltese','Manchu','Mansi','Maori','Marathi','Mari','Marwari','Middle_Persian','Mon','Mongolian','Nayi','Nenets','Nepali','Norwegian(bokmal)','Norwegian(nynorsk)','Old_Church_Slavonic','Old_Egyptian','Old_English','Old_High_German','Old_Irish','Old_Norse','Old_Persian','Oriya','Oromo','Oroqen','Ossetic_(Digor)','Ossetic_(Iron)','Pashto','Persian','Phoenician','Polish','Portuguese','Provencal','Prussian','Punjabi','Romanian','Russian','Saka','Sami','Samoan','Sanskrit','Sanskrit_(Vedic)','Sardinian_L','Sardinian_N','Sariqoli','Schwyzerduetsch','Scots_(Scottish_English)','Scottish_Gaelic','Serbian','Shan','Sheko','Sindhi','Sinhalese','Slovak','Slovene','Sogdian','Somali','Spanish','Swahili','Swedish','Tagalog','Tahitian','Tajik','Tamasheq','Tamil','Tashelhit','Tatar','Tausug','Telugu','Thai','Tibetan','Tigrigna','Tmazight_(Riffian_Berber)','Tocharian_A','Tocharian_B','Tsakonian','Turkish','Turkmen','Tuvan','Ukrainian','Urdu','Uyghur','Uzbek','Veps','Vietnamese','Vlach','Wakhi','Waziri','Welsh','Wolof','Yakut','Yiddish','Yoruba','Yupik','Zazaki','Zulu')


# With Dummies:
# ColomnNames=c('Abkhaz','Adyghe','Afar','Afrikaans','Ainu','Albanian_(Gheg)','Albanian_(Tosk)','Aleut','Altai','Amharic','Arabic','Aramaic_(Syriac)','Arbanaski','Armenian','Armenian_(Classical)','Armenian_(Eastern)','Assamese','Assyro-Babylonian','Avar','Avestan','Azeri','Balochi','Basaa','Bashkir','Basque','Bavarian','Belarusian','Bemba','Bengali','Bihari','Bole','Brahui','Brazilian','Breton','Bulgarian','Burmese','Burushaski','Buryat','Buyang','Catalan','Cebuano','Chechen','Chinese_(Cantonese)','Chinese_(Mandarin)','Chuvash','Comorian','Coptic','Cornish','Croatian','Czech','Dalmatian','Danish','Dargwa','Dhivehi_(Maldivian)','Dizi','Douala','Dutch','Elamite','English','Estonian','Etruscan','Even','Faroese','Finnish','Flemish','French','French_Creole_C','French_Creole_D','Frisian','Friulian','Fula','Galician','Gaulish','Geez','Gelao','Georgian','German','Gothic','Greek_(ancient)','Greek_(modern)','Greenlandic_(Eastern)','Greenlandic_(Western)','Gujarati','Gypsy_Romani','Hausa','Hebrew','Hindi','Hittite','Hungarian','Hurrian','Icelandic','Igbo','Indonesian','Irish','Ishkashimi','Istro-Romanian','Italian','Japanese','Javanese','Kabardino_Cherkess','Kabylian','Kalasha','Kalmyk','Kannada','Karelian','Kashmiri','Kazakh','Ket','Khanti','Khmer','Khowar','Kivalliq','Komi','Korean','Kurdish','Kurukh','Kyrgyz','Labrador_Inuttut','Ladin','Lahnda','Lak','Lao','Latin','Latvian','Letzebuergesch','Lezgian','Lithuanian','Luwian','Lycian','Macedonian','Magahi','Malagasy','Malayalam','Maltese','Manchu','Mansi','Maori','Marathi','Mari','Marwari','Middle_Persian','Mon','Mongolian','Mycenaean','Nayi','Nenets','Nepali','Norwegian(bokmal)','Norwegian(nynorsk)','Old_Church_Slavonic','Old_Egyptian','Old_English','Old_High_German','Old_Irish','Old_Norse','Old_Persian','Oriya','Oromo','Oroqen','Oscan','Ossetic_(Digor)','Ossetic_(Iron)','Pashto','Pennsylvania_Dutch','Persian','Phoenician','Phrygian','Polish','Portuguese','Proto_Indo_European','Provencal','Prussian','Punjabi','Romanian','Romansch','Russian','Safaitic','Saka','Sami','Samoan','Sanskrit','Sanskrit_(Vedic)','Sardinian_L','Sardinian_N','Sariqoli','Schwyzerduetsch','Scots_(Scottish_English)','Scottish_Gaelic','Serbian','Shan','Sheko','Sindhi','Sinhalese','Slovak','Slovene','Sogdian','Somali','Spanish','Sranan','Sumerian','Swahili','Swedish','Tagalog','Tahitian','Tajik','Tamasheq','Tamil','Tashelhit','Tatar','Tausug','Telugu','Thai','Tibetan','Tigrigna','Tmazight_(Riffian_Berber)','Tocharian_A','Tocharian_B','Tsakonian','Turkish','Turkmen','Tuvan','Ukrainian','Umbrian','Urartian','Urdu','Uyghur','Uzbek','Veps','Vietnamese','Vlach','Wakhi','Walloon','Warji','Waziri','Welsh','Wolof','Yakut','Yiddish','Yoruba','Yupik','Zazaki','Zulu')
# ColomnNames = c(ColomnNames,'Dummy_1','Dummy_2','Dummy_3','Dummy_4','Dummy_5','Dummy_6','Dummy_7','Dummy_8','Dummy_9','Dummy_10','Dummy_11','Dummy_12','Dummy_13','Dummy_14','Dummy_15','Dummy_16','Dummy_17','Dummy_18','Dummy_19','Dummy_20','Dummy_21','Dummy_22','Dummy_23','Dummy_24','Dummy_25','Dummy_26','Dummy_27','Dummy_28','Dummy_29','Dummy_30','Dummy_31','Dummy_32','Dummy_33','Dummy_34','Dummy_35','Dummy_36','Dummy_37','Dummy_38','Dummy_39','Dummy_40','Dummy_41','Dummy_42','Dummy_43','Dummy_44','Dummy_45','Dummy_46','Dummy_47','Dummy_48','Dummy_49','Dummy_50','Dummy_51','Dummy_52','Dummy_53','Dummy_54','Dummy_55','Dummy_56','Dummy_57','Dummy_58','Dummy_59','Dummy_60','Dummy_61','Dummy_62','Dummy_63','Dummy_64','Dummy_65','Dummy_66','Dummy_67','Dummy_68','Dummy_69','Dummy_70','Dummy_71','Dummy_72','Dummy_73','Dummy_74','Dummy_75','Dummy_76','Dummy_77','Dummy_78','Dummy_79','Dummy_80','Dummy_81','Dummy_82','Dummy_83','Dummy_84','Dummy_85','Dummy_86','Dummy_87','Dummy_88','Dummy_89','Dummy_90','Dummy_91','Dummy_92','Dummy_93','Dummy_94','Dummy_95','Dummy_96','Dummy_97','Dummy_98','Dummy_99','Dummy_100','Dummy_101','Dummy_102','Dummy_103','Dummy_104','Dummy_105','Dummy_106','Dummy_107','Dummy_108','Dummy_109','Dummy_110','Dummy_111','Dummy_112','Dummy_113','Dummy_114','Dummy_115','Dummy_116','Dummy_117','Dummy_118','Dummy_119','Dummy_120','Dummy_121','Dummy_122','Dummy_123','Dummy_124','Dummy_125','Dummy_126','Dummy_127','Dummy_128','Dummy_129','Dummy_130','Dummy_131','Dummy_132','Dummy_133','Dummy_134','Dummy_135','Dummy_136','Dummy_137','Dummy_138','Dummy_139','Dummy_140','Dummy_141','Dummy_142','Dummy_143','Dummy_144','Dummy_145','Dummy_146','Dummy_147','Dummy_148','Dummy_149','Dummy_150','Dummy_151','Dummy_152','Dummy_153','Dummy_154','Dummy_155','Dummy_156','Dummy_157','Dummy_158','Dummy_159','Dummy_160','Dummy_161','Dummy_162','Dummy_163','Dummy_164','Dummy_165','Dummy_166','Dummy_167','Dummy_168','Dummy_169','Dummy_170','Dummy_171','Dummy_172','Dummy_173','Dummy_174','Dummy_175','Dummy_176','Dummy_177','Dummy_178','Dummy_179','Dummy_180','Dummy_181','Dummy_182','Dummy_183','Dummy_184','Dummy_185','Dummy_186','Dummy_187','Dummy_188','Dummy_189','Dummy_190','Dummy_191','Dummy_192','Dummy_193','Dummy_194','Dummy_195','Dummy_196','Dummy_197','Dummy_198','Dummy_199','Dummy_200')


mat <- data.matrix(read.table("CleanNeuAPE.csv", fill=TRUE, sep=" ", col.names=ColomnNames))
#matComplete <- data.matrix(read.table("LgList2_R_APE_C_All.csv", fill=TRUE, sep=" ", col.names=ColomnNames))
diag(mat) <- 0
mat[upper.tri(mat)] <- t(mat)[upper.tri(mat)]
#diag(matComplete) <- 0
#matComplete[upper.tri(matComplete)] <- t(matComplete)[upper.tri(matComplete)]
arbol1 <- nj(as.dist(mat))
arbol2 <- fastme.ols(as.dist(mat))
arbol3 <- upgma(as.dist(mat))
plot(arbol2)



# Compare ultrametric trees:
dend <- as.dendrogram(arbol1)
dend2 <- as.dendrogram(arbol2)
dl <- dendlist(dend, dend2)
tanglegram(dl)

# Bootstraping
# https://rpubs.com/Sammi/132438
# http://www.sthda.com/english/articles/29-cluster-validation-essentials/99-computing-p-value-for-hierarchical-clustering/
# Interpretation https://stackoverflow.com/questions/43576210/p-values-in-pvclust-results-in-hclust

result2 <- pvclust(mat,method.dist="euclidean", method.hclust="average", nboot=10) # UPGMA
result3 <- pvclust(mat,method.dist="euclidean", method.hclust="ward.D", nboot=100) # NJ???
result <- pvclust(mat,method.dist="euclidean", method.hclust="complete", nboot=10) # Interessant!
result4 <- pvclust(mat,method.dist="cor", method.hclust="compl", nboot=100)

pvrect(res.pv) # Rectangles where AU > 95%

# Bootstrapping with boot.phylo
FUNC <- function(xx) (phy = upgma(as.dist(xx)))
TREE <- upgma(as.dist(mat))
BootstrappedLabels <- boot.phylo(phy = TREE, x = mat, FUN = FUNC, B=100, block = 10)
plot(TREE)
nodelabels(BootstrappedLabels)

# Bootstrap? https://web.stanford.edu/class/stats366/exs/bootphy.html


# Query common node of two taxa (yougest common ancestor)
# Various manipulations of trees
# Use in loops to compare stability of chosen notes among sevel trees?
SlavicGermanic <- mrca(arbol2)["German", "Russian"]
RomanceGermanic <- mrca(arbol2)["German", "French"]
RomanceSlavic <- mrca(arbol2)["French", "Russian"]

SubTreeRomanceSlavic <- extract.clade(arbol2,RomanceSlavic)
plot(SubTreeRomanceSlavic)
SubToRotate <- mrca(SubTreeRomanceSlavic)["Italian", "Gothic"]
SubTreeRomanceSlavicBis <- rotate(SubTreeRomanceSlavic,SubToRotate)
plot(SubTreeRomanceSlavicBis)
 

x <- cladePar(tree, 75) 
cladePar(SubTreeRomanceSlavicBis, 75,  edge.color = "blue", tip.color = "grey",  edge.width = 2, edge.lty = 1, plot=TRUE)
nodelabels("Romance",75, adj = c(1.2, -0.2), col="blue", frame = "n", cex = 0.8,bg = "lightblue",font = 3 ,width = 30, height = 10)  # Give name to node

# BATCH TREE COLOR & COMMENT
Slavic <- mrca(SubTreeRomanceSlavicBis)["Bulgarian", "Belarusian"]
Germanic <- mrca(SubTreeRomanceSlavicBis)["Faroese", "English"]
X2 <- cladePar(SubTreeRomanceSlavicBis, Slavic,  edge.color = "blue", tip.color = "grey",  edge.width = 2, edge.lty = 1, x = NULL)
nodelabels("Slavic",Slavic, adj = c(1.2, -0.2), col="blue", frame = "n", cex = 0.8,bg = "lightblue",font = 3 ,width = 30, height = 10)  # Give name to node
cladePar(SubTreeRomanceSlavicBis, Germanic,  edge.color = "red", tip.color = "grey",  edge.width = 2, edge.lty = 1, x = X2, plot=TRUE)
nodelabels("Germanic",Germanic, adj = c(1.2, -0.2), col="blue", frame = "n", cex = 0.8,bg = "lightblue",font = 3 ,width = 30, height = 10)  # Give name to node


# Remove Taxa from Tree see at the end!

# Branches shorter than the tolerance value will be collapsed into polytomies. If unspecified, the tolerance value defaults to 10E-8. For example:
arbol2 <- di2multi(arbol2, 1)  # if 2 is the value from which to collapse



# Remove Taxa from Tree 
arbol <- drop.tip(arbol,c("Etruscan","French_Creole_D","French_Creole_C","Nenets","Phrygian","Sranan","Lycian","Mycenaean","Safaitic","Umbrian","Urartian"))
arbol <- drop.tip(arbol,c("Armenian_(Eastern)","Bavarian","Ladin","Macedonian","Proto_Indo_European","Romansch","Walloon"))
arbol <- drop.tip(arbol,c("Ainu","Basque","Burushaski","Elamite","Georgian","Hurrian","Japanese","Ket","Korean","Sumerian","Warji"))
arbol <- drop.tip(arbol,c("Armenian_(Eastern)","Oscan"))

# Short version for Matrix8.csv
ColomnNames=c("Abkhaz","Adyghe","Afar","Afrikaans","Ainu","Albanian_(Gheg)","Albanian_(Tosk)","Aleut","Altai","Amharic","Arabic","Aramaic_(Syriac)","Arbanaski",
              "Armenian","Armenian_(Classical)","Armenian_(Eastern)","Assamese","Assyro-Babylonian","Avar","Avestan","Azeri","Balochi","Basaa","Bashkir","Basque")



# Bootstrap Matrix: Chose random item to resample:

sample(c('Abkhaz','Adyghe','Afar','Afrikaans','Albanian_(Gheg)','Albanian_(Tosk)','Aleut','Altai','Amharic','Arabic','Aramaic_(Syriac)','Arbanaski','Armenian','Armenian_(Classical)','Assamese','Assyro-Babylonian','Avar','Avestan','Azeri','Balochi','Basaa','Bashkir','Belarusian','Bemba','Bengali','Bihari','Bole','Brahui','Brazilian','Breton','Bulgarian','Burmese','Buryat','Buyang','Catalan','Cebuano','Chechen','Chinese_(Cantonese)','Chinese_(Mandarin)','Chuvash','Comorian','Coptic','Cornish','Croatian','Czech','Dalmatian','Danish','Dargwa','Dhivehi_(Maldivian)','Dizi','Douala','Dutch','English','Estonian','Even','Faroese','Finnish','Flemish','French','Frisian','Friulian','Fula','Galician','Gaulish','Geez','Gelao','German','Gothic','Greek_(ancient)','Greek_(modern)','Greenlandic_(Eastern)','Greenlandic_(Western)','Gujarati','Gypsy_Romani','Hausa','Hebrew','Hindi','Hittite','Hungarian','Icelandic','Igbo','Indonesian','Irish','Ishkashimi','Istro-Romanian','Italian','Javanese','Kabardino_Cherkess','Kabylian','Kalasha','Kalmyk','Kannada','Karelian','Kashmiri','Kazakh','Khanti','Khmer','Khowar','Kivalliq','Komi','Kurdish','Kurukh','Kyrgyz','Labrador_Inuttut','Lahnda','Lak','Lao','Latin','Latvian','Lezgian','Lithuanian','Luwian','Magahi','Malagasy','Malayalam','Maltese','Manchu','Mansi','Maori','Marathi','Mari','Marwari','Middle_Persian','Mon','Mongolian','Nayi','Nenets','Nepali','Norwegian(bokmal)','Norwegian(nynorsk)','Old_Church_Slavonic','Old_Egyptian','Old_English','Old_High_German','Old_Irish','Old_Norse','Old_Persian','Oriya','Oromo','Oroqen','Ossetic_(Digor)','Ossetic_(Iron)','Pashto','Persian','Phoenician','Polish','Portuguese','Provencal','Prussian','Punjabi','Romanian','Russian','Saka','Sami','Samoan','Sanskrit','Sanskrit_(Vedic)','Sardinian_L','Sardinian_N','Sariqoli','Schwyzerduetsch','Scots_(Scottish_English)','Scottish_Gaelic','Serbian','Shan','Sheko','Sindhi','Sinhalese','Slovak','Slovene','Sogdian','Somali','Spanish','Swahili','Swedish','Tagalog','Tahitian','Tajik','Tamasheq','Tamil','Tashelhit','Tatar','Tausug','Telugu','Thai','Tibetan','Tigrigna','Tmazight_(Riffian_Berber)','Tocharian_A','Tocharian_B','Tsakonian','Turkish','Turkmen','Tuvan','Ukrainian','Urdu','Uyghur','Uzbek','Veps','Vietnamese','Vlach','Wakhi','Waziri','Welsh','Wolof','Yakut','Yiddish','Yoruba','Yupik','Zazaki','Zulu'),1)

