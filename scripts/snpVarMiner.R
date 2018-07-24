## http://genome.ucsc.edu/cgi-bin/hgTables
## See downloadSNPsUCSCparameters.png to see the parameters of the search

n <- 5000
data <- read.table("../data/allSNPs.tsv",sep = "\t",nrows = n,header = FALSE)
snps <- as.character(data$V5[1:n])
snps <- data.frame("dbsnp"=rep("dbsnp",n),"snps"=snps)
write.table(snps,"../data/snpsIds.csv",row.names = FALSE,col.names = FALSE,sep = "\t",
            quote = FALSE)

## Then snps were copy and pasted in the nexus tool to retrieve annotated
## snps
## http://snp-nexus.org/index.html


########################################################################################
## Data Mining from snpedia
library(SNPediaR)

## getting all snps from snpedia using SNPediaR appi
#snps <- getCategoryElements(category = "Is_a_snp")

## saving the data
## write.table(snps,"../data/snpedia.csv",row.names = FALSE,col.names = FALSE,sep = "\t",
##            quote = FALSE)

## read data
snps <- read.table("../data/snpedia.csv",header = FALSE,sep = "\t")
snps <- as.character(snps$V1)

## fields to extract
fields <- c("Title=","Chromosome=","PMID=","PMID=","Assembly=",
            "StabilizedOrientation=")



n <- 1

## dataframe to store results
snpsTable <- matrix(0,n,length(fields)+2)
snpsTable <- as.data.frame(snpsTable)
colnames(snpsTable) <- c("snps","summary",fields)

## function to extract fields fron snp
extractFields <- function(field,page){
        res <- grep(field,page[[1]],value = TRUE)
        res <- gsub(paste(field,"\n",sep="|"),"",res)
        res <- ifelse(length(res)==1,res,NA)
        res
}

initialTime <- Sys.time()
for (i in 1:n){
        page <- getPages(snps[i])
        
        ## extract summary from pages
        summary.snp <- extractSnpTags(page[[1]])
        summary.snp <- summary.snp["Summary"] 
        
        ## process snpTags
        page <- strsplit(page[[1]],"|",fixed = TRUE)
        
        ## extracting fields for a single snp
        snpFields <- sapply(fields, function(x) extractFields(x,page))
        snpsTable[i,] <- c(snps[i],summary.snp,snpFields) 
}
head(snpsTable)

totalTime <- Sys.time() - initialTime


library(xml2)

read_xml("https://www.snpedia.com/index.php?title=Special%3ASearch&search=fitness|diabetes&go=Ir")

