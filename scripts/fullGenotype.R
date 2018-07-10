## data mining of page 12 of fitness_premium.pdf report

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
command <- paste("pdftotext '",fitnessPath,
                 "' '../data/genotypingMiner.txt' -raw -f 13 -l 13",
                 sep="")
system(command)

## data mining
genotyping.txt <- readLines("../data/genotypingMiner.txt")

## list to store results
genotyping <- list()

powerEndur <- genotyping.txt[8:26]

snps <- c("rs4646994","rs1042713","rs1042714","rs699","rs1815739",
          "rs1799722","rs12722","rs1205","rs1800795","rs7181866",
          "rs4253778","rs8192678","rs16892496","rs2010963","rs731236")

## extract alleles
snpLine <- powerEndur[grepl(snps[i],powerEndur)]
alleles <- sub(paste(".* ",snps[i]," ",sep=""),"",snpLine)
alleles <- sub(" .*","",alleles)
alleles

## needs to be modified
recommendation <- gsub(paste(".*",alleles," ",sep=""),"",snpLine)
nextLine <- powerEndur[grepl(snps[i+1],powerEndur)]
counter <- 1
grepl(snps[i+counter],nextLine)
counter <- counter + 1
