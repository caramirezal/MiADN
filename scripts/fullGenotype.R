## data mining of page 12 of fitness_premium.pdf report
## Data must be stored in data/genotyping folder


########################################################################
## Extract genotype
powerEndurance <- function(filePath,output="default"){
        ## set output path
        if (output=="default") {
                oPath <- "genotypingMiner.txt" 
        } else {
                oPath <- output
        }
        
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
        
        oPath <- "../data/genotypingMinerpage13.txt"
        ## Convert pdf to text
        ## pdftotext <pdfFile> <txtOutput>
        command <- paste("pdftotext '",filePath,
                         "' '",oPath,"' -layout -f 13 -l 13",
                         sep="")
        system(command)
        
        ## data mining
        genotyping.txt <- readLines(oPath,warn = FALSE)
        
        ## select first and last table line
        if ( language == "Spanish" ){
                firstLine <- grep("Resultado de alelos",genotyping.txt)
                finalLine <- grep("Recuperación posterior al ejercicio",
                                  genotyping.txt)
        }
        if ( language == "English" ){
                firstLine <- grep("Allele Result",genotyping.txt)
                finalLine <- grep("Post Exercise Recovery",
                                  genotyping.txt)
        }
        
        
        ## select lines containing the table
        powerEndur <- genotyping.txt[firstLine:finalLine]
        
        ## snps to extract
        snps <- c("rs4646994","rs1042713","rs1042714","rs699","rs1815739",
                  "rs1799722","rs12722","rs1205","rs1800795","rs7181866",
                  "rs4253778","rs8192678","rs16892496","rs2010963","rs731236")
        
        for (i in 1:length(snps)){
                ## find the line where the snp is
                snpline.index <- grep(snps[i],powerEndur)
                snpLine <- powerEndur[snpline.index]
                
                ## get the alleles field
                if (language == "Spanish") {
                        alleles <- substr(snpLine,43,72)
                }
                if (language == "English") {
                        alleles <- substr(snpLine,38,65)
                }
                alleles <- gsub(" ","",alleles)
                
                ## extract effect field
                if ( nchar(snpLine) > 50 ){
                        if (language == "Spanish"){
                                effect <- substr(snpLine,74,nchar(snpLine))
                        }
                        if (language == "English"){
                                effect <- substr(snpLine,66,nchar(snpLine))
                                effect <- sub("  ","",effect)
                        }
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



####################################################################################
## extracting table of power and endurance from page 12
## iteration over all files
getPowerEndurance <- function(fPath){
        
        fileNames <- list.files(fPath,recursive = TRUE)
        filePaths <- fileNames[grep("ness_premium",fileNames)]
        filePaths <- paste(fPath,filePaths,sep="")
        
        ## store data
        data <- list()
        
        ## iterate over files
        for (i in 1:length(filePaths)) {
                print(filePaths[i])
                res <- powerEndurance(filePaths[i])
                res <- as.data.frame(res)
                data <- rbind(data,res)
                
        }
        
        ## massage results
        data <- data.frame(data,row.names = NULL)
        return(data)
} 

#data <- getPowerEndurance("/home/carlos/data/miadn/genotyping/")
#write.csv(data,"../data/power_endurance.csv",row.names = FALSE)