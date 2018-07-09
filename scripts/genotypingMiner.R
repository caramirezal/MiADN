
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
genes <- c("rs4646994","rs1042713","rs1042714","rs699","rs1815739")

## client name
genotyping <- c(genotyping,"clientName"=genotyping.txt[6])

## client id
clientId <- genotyping.txt[grepl("mero de muestra:",genotyping.txt)]
clientId <- gsub(".*mero de muestra: ","",clientId)
genotyping <- c(genotyping,"clientId"=clientId)

## ACE variant and effect
genotyping <- c(genotyping,"rs4646994"=genotyping.txt[718],
                "rs4646994effect"=genotyping.txt[720])

## ADRB2 variant and effect
genotyping <- c(genotyping,"rs1042713"=genotyping.txt[724],
                "rs1042713effect"=genotyping.txt[728])

## AGT variant and effect
genotyping <- c(genotyping,"rs699"=)
