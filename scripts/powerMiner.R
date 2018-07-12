
filePath <- "/home/carlos/data/miadn/genotyping/Abraham Vargas Najera/fitness_premium.pdf"


getPower <- function(filePath){
        ## output file to store text converted from pdf
        oPath <- "page1.txt"
        
        ## convert page 1 to text
        command <- paste("pdftotext '",filePath,
                         "' '",oPath,"' -f 1 -l 1",
                         sep="")
        system(command)
        
        ## Extract first page
        firstPage <- readLines(oPath,warn = FALSE)
        
        ## check report language
        language <- ifelse(sum(grepl("irth",firstPage))>0,"English","Spanish")
        
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
        genotyping.txt <- substr(genotyping.txt,1,70)
        
        snps <- c("ACE","AGT","ACTN3","TRHR","PPARA","VEGF","VDR","IL6")
        
        i <- 1
        for (i in 1:length(snps)){
                ## find the line where the snp is
                snpline.index <- grep(snps[i],genotyping.txt)
                snpLine <- genotyping.txt[snpline.index]
                
                ## get the alleles field
                alleles <- substr(snpLine,20,40)
                alleles <- gsub(" ","",alleles)
                
                ## extract effect
                effect <- substr(snpLine,46,70)
                effect <- gsub(" ","",effect)
                effect <- gsub("•","+",effect)
                
                ## storing results
                result <- list(alleles,effect)
                names(result) <- c(snps[i],paste(snps[i],"effect",sep=""))
                genotyping <- c(genotyping,result)
        }
        genotyping
        
}

#######################################################################################

powerMiner <- function(fPath){
        ## getting pdf paths from the fPath root dir
        fileNames <- list.files(fPath,recursive = TRUE)
        filePaths <- fileNames[grep("ness_premium",fileNames)]
        filePaths <- paste(fPath,"/",filePaths,sep="")
        
        ## store data
        data <- list()
        
        ## iterate over files
        for (i in 1:length(filePaths)) {
                print(filePaths[i])
                res <- getPower(filePaths[i])
                res <- as.data.frame(res)
                data <- rbind(data,res)
                
        }
        
        ## massage results
        data <- data.frame(data,row.names = NULL)
        return(data)
} 


