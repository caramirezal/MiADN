## data mining of endurance data of page 6

filePath <- "/home/carlos/data/miadn/genotyping/Abigail Canada Torres/fitness_premium.pdf"

getEndurance <- function(filePath){
        ## output file to store text converted from pdf
        oPath <- "page1.txt"
        
        ## convert page 1 to text
        command <- paste("pdftotext '",filePath,
                         "' '",oPath,"' -f 1 -l 1",
                         sep="")
        system(command)
        
        ## Extract first page
        firstPage <- readLines(oPath,warn = FALSE)
        
        ## extract client id
        clientLine <- firstPage[grep("MMX",firstPage)]
        clientId <- sub(".*MMX","",clientLine)
        clientId <- paste("MMX",clientId,sep="")
        
        ## list to store results
        genotyping <- list("clientId"=clientId)
        
        oPath <- "page6.txt"
        ## Convert pdf to text
        ## pdftotext <pdfFile> <txtOutput>
        command <- paste("pdftotext '",filePath,
                         "' '",oPath,"' -layout -f 6 -l 6",
                         sep="")
        system(command)
        
        ## data mining
        genotyping.txt <- readLines(oPath,warn = FALSE)
        genotyping.txt <- genotyping.txt[nchar(genotyping.txt)>69]
        genotyping.txt <- sapply(genotyping.txt,function(x) substr(x,60,nchar(x)))
        genotyping.txt <- unname(genotyping.txt)
        
        snps <- c("ACE","ADRB2","ACTN3","BDKRB2","COL5A1","NRF","PPARGC1A",
                  "PPARA","CRP","VEGF")
        
        
        for (i in 1:length(snps)){
                ## find the line where the snp is
                snpline.index <- grep(snps[i],genotyping.txt)
                if ( length(snpline.index) > 1) {
                        
                }
                snpLine <- genotyping.txt[snpline.index]
                
                ## get the alleles field
                alleles <- substr(snpLine,40,60)
                alleles <- gsub(" ","",alleles)
                
                ## extract effect
                effect <- substr(snpLine,nchar(snpLine)-5,nchar(snpLine))
                effect <- gsub(" ","",effect)
                effect <- gsub("•","+",effect)
                
                ## storing results
                result <- list(alleles[1],effect[1])
                names(result) <- c(snps[i],paste(snps[i],"effect",sep=""))
                genotyping <- c(genotyping,result)
                
                if ( length(snpLine) > 1 ) {
                        result <- list(alleles[2],effect[2])
                        snp2 <- paste(snps[i],"snp2",sep="")
                        names(result) <- c(snp2,paste(snp2,"effect",sep=""))
                        genotyping <- c(genotyping,result)
                }
        }
        
        return(genotyping)
} 

##################################################################################

enduranceMiner <- function(fPath){
        ## getting pdf paths from the fPath root dir
        fileNames <- list.files(fPath,recursive = TRUE)
        filePaths <- fileNames[grep("ness_premium",fileNames)]
        filePaths <- paste(fPath,"/",filePaths,sep="")
        
        ## store data
        data <- list()
        
        ## iterate over files
        for (i in 1:length(filePaths)) {
                print(filePaths[i])
                res <- getEndurance(filePaths[i])
                res <- as.data.frame(res)
                data <- rbind(data,res)
        }
        
        ## massage results
        data <- data.frame(data,row.names = NULL)
        return(data)
} 



