## data mining of page 12 of fitness_premium.pdf report
## Data must be stored in data/genotyping folder
path <- "/home/carlos/data/miadn/genotyping/"




###############################################################################
## Extract power and Endurance table

powerEndurance <- function(filePath,output="default"){
        
        ## set output path
        if (output=="default") {
                oPath <- "../data/genotypingMiner.txt" 
        } else {
                oPath <- output
        }
        
        ## convert page 1 to text for mining
        command <- paste("pdftotext '",filePath,
                         "' '",oPath,"' -f 1 -l 1",
                         sep="")
        system(command)
        
        
        
        ## Extract client id
        firstPage <- readLines(oPath,warn = FALSE)
        clientLine <- firstPage[grep("MMX",firstPage)]
        clientId <- sub(".*MMX","",clientLine)
        clientId <- paste("MMX",clientId,sep="")
        
        ## list to store results
        genotyping <- list("clientId"=clientId)
        
        ########################################################################
        ## Extract genotype
        
        ## Convert pdf to text
        ## pdftotext <pdfFile> <txtOutput>
        command <- paste("pdftotext '",filePath,
                         "' '",oPath,"' -layout -f 13 -l 13",
                         sep="")
        system(command)
        
        ## data mining
        genotyping.txt <- readLines(oPath,warn = FALSE)
        ## select first and last table line
        firstLine <-grep("Resultado de alelos | Allele Result",genotyping.txt)
        finalLine <-grep("Recuperación posterior al ejercicio | Post Exercise Recovery",genotyping.txt)
        
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
                
                ## storing results
                result <- list(alleles,effect)
                names(result) <- c(snps[i],paste(snps[i],"effect",sep=""))
                genotyping <- c(genotyping,result)
        }
        
        return(genotyping)
}

##################################################################################



getPowerEndurance <- function(fPath){
        
        fileNames <- list.files(fPath,recursive = TRUE)
        filePaths <- fileNames[grep("ness_premium",fileNames)]
        filePaths <- paste(path,filePaths,sep="")
        oPath <- "out.txt"
        
        ## store data
        data <- list()
        
        ## iterate over files
        for (i in 1:length(filePaths)) {
                command <- paste("pdftotext '",filePaths[i],
                                 "' '",oPath,"' -f 1 -l 1",
                                 sep="")
                system(command)
                
                ## Extract client id
                firstPage <- readLines(oPath,warn = FALSE)
                if ( ! sum(grepl("Birth",firstPage)) > 0 ) {
                        res <- powerEndurance(filePaths[i])
                        res <- as.data.frame(res)
                        data <- rbind(data,res)
                }
                
                
        }
        
        ## massage results
        data <- data.frame(data,row.names = NULL)
        return(data)
} 


##################################################################################

## getting client genotyping folder and files Names
fileNames <- list.files(path,recursive = TRUE)
filePaths <- fileNames[grep("ness_premium",fileNames)]
filePaths <- paste(path,filePaths,sep="")
filePaths

## convert page 1 to text for mining
command <- paste("pdftotext '",filePath,
                 "' '",oPath,"' -f 1 -l 1",
                 sep="")
system(command)

## Extract client id
firstPage <- readLines(oPath,warn = FALSE)
clientLine <- firstPage[grep("MMX",firstPage)]
clientId <- sub(".*MMX","",clientLine)
clientId <- paste("MMX",clientId,sep="")

## store data
data <- list()

## iterate over files
for (i in 1:6) {
        res <- powerEndurance(filePaths[i])
        data <- rbind(data,res)
}

## massage results
data <- data.frame(data,row.names = NULL)

write.csv(data,"/home/carlos/scripts/MiADN/data/powerEndurance.csv",row.names = FALSE)
