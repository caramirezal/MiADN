
## data mining of page 12 of fitness_premium.pdf report
## Data must be stored in data/genotyping folder

filePath <- "/home/carlos/data/miadn/genotyping/Abigail Canada Torres/fitness_premium.pdf"



########################################################################
## Extract genotype
getAerobic <- function(filePath){
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
        
        ## Convert page 8 pdf to text
        oPath <- "page8.txt"
        command <- paste("pdftotext '",filePath,
                         "' '",oPath,"' -layout -f 8 -l 8",
                         sep="")
        system(command)
        
        text <- readLines(oPath,warn = FALSE)
        
        if ( language == "Spanish" ){
                level <- text[grep("ha determinado que su perfil",text)]
                level <- sub(".*máx.","",level)
                level <- sub(",.*","",level)
                level <- gsub(" ","",level)
        } 
        if ( language == "English" ){
                level <- text[grep("towards a",text)][1]
                level <- sub(".*towards a","",level)
                level <- sub("VO2 max .*","",level)
                level <- gsub(" ","",level)
                
                if ( level == "high" ) {
                        level <- "alto"
                } 
                if ( grepl("mediate",level ) ){
                        level <- "intermedio"
                }
                if ( level == "low" ){
                        level <- "bajo"
                }
        }
        
        genotyping <- c(genotyping,"level"=level)
        
        ## snps to extract
        snps <- c("ADRB2","CRP","PPARGC1A","VEGF")
        
        for (i in 1:length(snps)){
                snpline.index <- grep(snps[i],text)
                snpLine <- text[snpline.index]
                
                alleles <- substr(snpLine,60,80)
                alleles <- gsub(" ","",alleles)
                alleles
                
                effect <- sapply(snpLine,function(x) substr(x,80,nchar(x)))
                effect <- unname(effect)
                effect <- gsub(" ","",effect)
                effect <- gsub("•","+",effect)
                
                if ( length(alleles) > 1 ){
                        result <- list(alleles[1],effect[1],
                                       alleles[2],effect[2])
                        snp2 <- paste(snps[i],"snp2",sep="")
                        effect_snp <- paste(snps[i],"effect",sep="")
                        effect_snp2 <- paste(snp2,"effect",sep="")
                        names(result) <- c(snps[i],effect_snp,
                                           snp2,effect_snp2)
                } 
                if ( length(alleles) == 1 ){
                        result <- list(alleles,effect)
                        effect_snp <- paste(snps[i],"effect",sep="")
                        names(result) <- c(snps[i],effect_snp)
                }
                genotyping <- c(genotyping,result)
        }
        
        return(genotyping)
}



####################################################################################
## extracting table of power and endurance from page 12
## iteration over all files
aerobicMiner <- function(fPath){
        ## getting pdf paths from the fPath root dir
        fileNames <- list.files(fPath,recursive = TRUE)
        filePaths <- fileNames[grep("ness_premium",fileNames)]
        filePaths <- paste(fPath,"/",filePaths,sep="")
        
        ## store data
        data <- list()
        
        ## iterate over files
        for (i in 1:length(filePaths)) {
                print(filePaths[i])
                res <- getAerobic(filePaths[i])
                res <- as.data.frame(res)
                data <- rbind(data,res)
                
        }
        
        ## massage results
        data <- data.frame(data,row.names = NULL)
        return(data)
} 

#data <- getPowerEndurance("/home/carlos/data/miadn/genotyping/")
#write.csv(data,"../data/power_endurance.csv",row.names = FALSE)