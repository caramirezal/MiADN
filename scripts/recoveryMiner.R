
## data mining of page 12 of fitness_premium.pdf report
## Data must be stored in data/genotyping folder

filePath <- "/home/carlos/data/miadn/genotyping/Adam Grant/fitness_premium.pdf"



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
        
        ## convertinf pdf to text
        oPath <- "page10.txt"
        command <- paste("pdftotext '",filePath,
                         "' '",oPath,"' -layout -f 10 -l 10",
                         sep="")
        system(command)
        
        text <- readLines(oPath,warn = FALSE)
        
        ## snps to extract
        snps <- c("GSTM1","GSTT1","SOD2","IL6","IL6R","CRP","TNF")
        
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