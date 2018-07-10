
## Data must be stored in data/genotyping folder
path <- "../data/genotyping/"

## getting client genotyping folder and files Names
fileNames <- list.files(path)
filePath <- paste(path,fileNames[1],sep = "")
pdfNames <- list.files(filePath)

## getting the path to the fitness_premium file
fitnessName <- pdfNames[grepl("fitness_prem",pdfNames)]
fitnessPath <- paste(filePath,fitnessName,sep="/")

## Convert pdf to text
## pdftotext <pdfFile> <txtOutput>
command <- paste("pdftotext '",fitnessPath,"' '../data/genotypingMiner.txt'",sep="")
system(command)

## data mining
genotyping.txt <- readLines("../data/genotypingMiner.txt")

## extracting only page 13 of the fitness_premium.pdf report
genotyping.tx <- genotyping.txt[699:922]

## list to store results
genotyping <- list()

##################################################################################
## data extraction

## list of genes to extract
genes <- c("rs4646994","rs1042713","rs1042714","rs699","rs1815739","rs1799722",
           "rs1800629","rs1800012","rs143383")

## client name
genotyping <- c(genotyping,"clientName"=genotyping.txt[6])

## client id
clientId <- genotyping.txt[grepl("mero de muestra:",genotyping.txt)]
clientId <- gsub(".*mero de muestra: ","",clientId)
genotyping <- c(genotyping,"clientId"=clientId)

##############################################################################
## snps stored in genes have the following pattern
##rs699
##
##CC
##
##Pequeña asociación con la fuerza
##
## hence rsid can be match and the look for the next + 2, and + 4 lines 

## getting alleles 
alleles <- genotyping.txt[sapply(genes, function(x) grep(x,genotyping.txt))+2]

## getting recommendation
recomendation <- genotyping.txt[sapply(genes, function(x) grep(x,genotyping.txt))+4]