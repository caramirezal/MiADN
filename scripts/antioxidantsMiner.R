
filePath <- "/home/carlos/data/miadn/genotyping/Abigail Canada Torres//diet_professional.pdf"


getAntioxidants <- function(filePath){
        ## path to store text
        oPath <- "page1.txt"
        
        ## convert page 7 to text
        command <- paste("pdftotext '",filePath,
                         "' '",oPath,"' -f 1 -l 1 -layout",
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
        
        ###########################################################
        oPath <- "page10.txt"
        
        ## convert page 7 to text
        command <- paste("pdftotext '",filePath,
                         "' '",oPath,"' -f 14 -l 14 -layout",
                         sep="")
        system(command)
        
        ## Extract first page
        page <- readLines(oPath,warn = FALSE)
        
        ## getting carbohydrate sensitivity level
        level <- grep("Sus resultados de pruebas|Your genetic test results",
                      page,value = TRUE)
        level <- sub(".*the possibility of a|.*una capacidad","",level)
        level <- sub("capacity to neutralise.*|para neutralizar.*",
                     "",level)
        level <- gsub(" ","",level)
        genotyping <- c(genotyping,"level"=level)
        
        ## getting alleles
        snps <- c("SOD2","CAT","GPX1")
        
        
        for (i in 1:length(snps)){
                ## find the line where the snp is
                snpline.index <- grep(snps[i],page)
                snpLine <- page[snpline.index]
                
                ## get the alleles field
                alleles <- substr(snpLine,60,80)
                alleles <- gsub(" ","",alleles)
                
                ## extract effect
                effect <- substr(snpLine,81,nchar(snpLine))
                effect <- gsub(" ","",effect)
                effect <- gsub("•","*",effect)
                
                ## storing results
                result <- list(alleles,effect)
                names(result) <- c(snps[i],paste(snps[i],"effect",sep=""))
                genotyping <- c(genotyping,result)
        }
        
        genotyping
}




#######################################################################################

antioxidantsMiner <- function(fPath){
        ## getting pdf paths from the fPath root dir
        fileNames <- list.files(fPath,recursive = TRUE)
        filePaths <- fileNames[grep("diet_pro",fileNames)]
        filePaths <- paste(fPath,"/",filePaths,sep="")
        
        ## store data
        data <- list()
        
        ## iterate over files
        for (path in filePaths) {
                print(path)
                res <- getAntioxidants(path)
                res <- as.data.frame(res)
                data <- rbind(data,res)
                
        }
        
        ## massage results
        data <- data.frame(data,row.names = NULL)
        return(data)
} 
