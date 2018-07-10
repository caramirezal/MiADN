## data mining of page 12 of fitness_premium.pdf report

initialTime <- Sys.time()

## Data must be stored in data/genotyping folder
path <- "../data/genotyping/"

## getting client genotyping folder and files Names
fileNames <- list.files(path)
filePath <- paste(path,fileNames[1],sep = "")
pdfNames <- list.files(filePath)

## getting the path to the fitness_premium file
fitnessName <- pdfNames[grepl("fitness_prem",pdfNames)]
fitnessPath <- paste(filePath,fitnessName,sep="/")

## convert page 1 to text for mining
command <- paste("pdftotext '",fitnessPath,
                 "' '../data/genotypingMiner.txt' -f 1 -l 1",
                 sep="")
system(command)

## Extract client id
firstPage <- readLines("../data/genotypingMiner.txt")
clientLine <- firstPage[grep("mero de muestra: ",firstPage)]
clientId <- sub(".*mero de muestra: ","",clientLine)

## list to store results
genotyping <- list("clientId"=clientId)

########################################################################
## Extract genotype

## Convert pdf to text
## pdftotext <pdfFile> <txtOutput>
command <- paste("pdftotext '",fitnessPath,
                 "' '../data/genotypingMiner.txt' -layout -f 13 -l 13",
                 sep="")
system(command)

## data mining
genotyping.txt <- readLines("../data/genotypingMiner.txt")
## select first and last table line
firstLine <-grep("Resultado de alelos",genotyping.txt)
finalLine <-grep("Recuperación posterior al ejercicio",genotyping.txt)

## select lines containing the table
powerEndur <- genotyping.txt[firstLine:finalLine]

## snps to extract
snps <- c("rs4646994","rs1042713","rs1042714","rs699","rs1815739",
          "rs1799722","rs12722","rs1205","rs1800795","rs7181866",
          "rs4253778","rs8192678","rs16892496","rs2010963","rs731236")


for (i in 1:length(snps)) {
        ## find the line where the snp is
        snpline.index <- grep(snps[i],powerEndur)
        snpLine <- powerEndur[snpline.index]
        
        ## get the alleles field
        alleles <- substr(snpLine,43,72)
        alleles <- gsub(" ","",alleles)
        
        ## extract effect field
        if ( nchar(snpLine) > 50 ) {
                
                effect <- substr(snpLine,73,nchar(snpLine))
        } else {
                ## When effect string is large is splitted in two
                ## this case acount for that.
                snpLine.s1 <- powerEndur[snpline.index-1]
                snpLine.s2 <- powerEndur[snpline.index+2]
                effect.s1 <- substr(snpLine.s1,73,nchar(snpLine.s1))
                effect.s2 <- substr(snpLine.s2,73,nchar(snpLine.s2))
                effect <- paste(effect.s1, effect.s2, sep=" ")
        }
        
        result <- list(alleles,effect)
        names(result) <- c(snps[i],paste(snps[i],"effect",sep=""))
        genotyping <- c(genotyping,result)
}


totalTime <- Sys.time() - initialTime 








