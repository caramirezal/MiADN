
## data mining of page 12 of fitness_premium.pdf report
## Data must be stored in data/genotyping folder

filePath <- "/home/carlos/data/miadn/genotyping/Adam Grant/fitness_premium.pdf"



########################################################################
## Extract genotype
getRecovery <- function(filePath){
        oPath <- "page1.txt"
        
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
        
        ## convertinf pdf to text
        oPath <- "page10.txt"
        command <- paste("pdftotext '",filePath,
                         "' '",oPath,"' -layout -f 10 -l 10",
                         sep="")
        system(command)
        
        text <- readLines(oPath,warn = FALSE)
        
        ## snps to extract
        snps <- c("GSTM1","GSTT1","SOD2","IL6","IL6R","CRP","TNF")
        
        
        for (i in 1:length(snps)){
                snpline.index <- grep(snps[i],text)[1]
                snpLine <- text[snpline.index]
                
                alleles <- substr(snpLine,60,80)
                alleles <- gsub(" ","",alleles)
                
                effect <- substr(snpLine,80,nchar(snpLine))
                effect <- gsub(" ","",effect)
                effect <- gsub("•","+",effect)
                
                result <- list(alleles,effect)
                effect_snp <- paste(snps[i],"effect",sep="")
                names(result) <- c(snps[i],effect_snp)
                
                genotyping <- c(genotyping,result)
        }
        
        ## extract recomendations
        oPath <- "page11.txt"
        
        ## pdf to text
        command <- paste("pdftotext '",filePath,
                         "' '",oPath,"' -f 11 -l 11 -layout",
                         sep="")
        system(command)
        
        ## extract data from text file
        text <- readLines(oPath,warn = FALSE)
        
        intakeNames <- c("Vitamina A","Betacaroteno","Vitamina C",
                         "Vitamina E","Omega 3","Verduras crucíferas",
                         "Alfa Lipoico")
        
        if (language == "Spanish") {
                recommendations1 <- c("Vitamina A","Betacaroteno","Vitamina C",
                                      "Vitamina E","Omega 3","Verduras crucíferas",
                                      "Alfa Lipoico")
                
                
                for (i in 1:length(recommendations1)){
                        snpline.index <- grep(recommendations1[i],text)
                        snpLine <- text[snpline.index+1]
                        intake <- gsub("  ","",snpLine)
                        if ( substr(intake,1,1) == " " ) {
                                intake <- substr(intake,2,nchar(intake))
                        }
                        if ( substr(intake,nchar(intake),nchar(intake)) == " " ) {
                                intake <- substr(intake,1,nchar(intake)-1)
                        }
                        
                        ## storing results
                        result <- list(intake)
                        names(result) <- recommendations1[i]
                        genotyping <- c(genotyping,result)
                }
        }
        
        
        
        if ( language == "English" ){
                
                recommendations1 <- c("Vitamin A","Beta carotene",
                                      "Vitamin E","Alpha Lipoic Acid")
                
                result <- list()
                for (i in 1:length(recommendations1)){
                        snpline.index <- grep(recommendations1[i],text)
                        snpLine <- text[snpline.index]
                        intake  <- substr(snpLine,65,nchar(snpLine))
                        intake <- gsub(" ","",intake)
                        
                        ## storing results
                        result <- c(result,intake)
                }
                names(result) <- c("Vitamina A","Betacaroteno",
                                   "Vitamina E","Alfa Lipoico")
                genotyping <- c(genotyping,result)
                
                ## this fields ands up in a broken line 
                recommendations2 <- c("Vitamin C","Omega-3","Cruciferous vegetables")
                
                result <- list()
                for (i in 1:length(recommendations2)){
                        snpline.index <- grep(recommendations2[i],text)
                        snpLine <- text[snpline.index+1]
                        intake <- gsub("  ","",snpLine)
                        if ( substr(intake,1,1) == " " ) {
                                intake <- substr(intake,2,nchar(intake))
                        }
                        if ( substr(intake,nchar(intake),nchar(intake)) == " " ) {
                                intake <- substr(intake,1,nchar(intake)-1)
                        }
                        
                        ## storing results
                        result <- c(result,intake)
                        
                }
                names(result) <- c("Vitamina C","Omega 3","Verduras crucíferas")
                genotyping <- c(genotyping,result)
        } 
        genotyping
}



####################################################################################
## extracting table of power and endurance from page 12
## iteration over all files
recoveryMiner <- function(fPath){
        ## getting pdf paths from the fPath root dir
        fileNames <- list.files(fPath,recursive = TRUE)
        filePaths <- fileNames[grep("ness_premium",fileNames)]
        filePaths <- paste(fPath,"/",filePaths,sep="")
        
        ## store data
        data <- list()
        
        ## iterate over files
        for (i in 1:length(filePaths)) {
                print(filePaths[i])
                res <- getRecovery(filePaths[i])
                res <- as.data.frame(res)
                data <- rbind(data,res)
                
        }
        
        ## massage results
        data <- data.frame(data,row.names = NULL)
        return(data)
} 

res <- recoveryMiner("/home/carlos/scripts/MiADN/data/genotyping/")
write.csv(res,"../data/recovery.csv",row.names = FALSE)
