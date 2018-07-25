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
snps <- getCategoryElements(category = "Is_a_snp")

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

baseURL <- "https://bots.snpedia.com/api.php"
format <- "format=json"
query <- "action=query&cmlimit=max&cmprop=title&list=categorymembers&cmcontinue="

paste(baseURL,format,query,sep="")


limit <- "max"
category <- "Is_a_snp"
baseURL <- "https://bots.snpedia.com/api.php"
format <- "format=json"
query <- "action=query&list=categorymembers&cmlimit=___LIMIT___&cmprop=title&cmtitle=Category:___CATEGORY___"
continue <- "cmcontinue="
query <- sub("___LIMIT___", limit, query)
query <- sub("___CATEGORY___", category, query)
baseURL <- paste0(baseURL, "?", format, "&", query, "&", 
                  continue)
res <- NULL
cont <- ""
continueURL <- paste0(baseURL, cont)
lineas <- getURL(continueURL)
#library(jsonlite)
lineas <- gsub("\\n", "\\\\n", lineas)
js <- fromJSON(lineas)
res <- rbind(js[["query"]][["categorymembers"]], res)
cont <- js[["continue"]][["cmcontinue"]]
touse <- res[, "ns"] == 0
res <- res[touse, "title"]
res


###################################################################################

getsnps <- function(category,
                                verbose = FALSE,
                                includeTemplates = FALSE,
                                limit,
                                baseURL,
                                format,
                                query,
                                continue
) {
        
        ## default URL parameters
        if (missing(limit))     limit    <- 550
        if (missing(baseURL))   baseURL  <- "https://bots.snpedia.com/api.php"
        if (missing(format))    format   <- "format=json"
        if (missing(query))     query    <- "action=query&list=categorymembers&cmlimit=___LIMIT___&cmprop=title&cmtitle=Category:___CATEGORY___"
        if (missing(continue))  continue <- "cmcontinue="
        
        ## replace query parameters
        query <- sub("___LIMIT___",       limit, query)
        query <- sub("___CATEGORY___", category, query)
        
        ## base url
        baseURL <- paste0(baseURL, "?", format, "&", query, "&", continue)
        print(baseURL)
        
        ## iterate across pages 
        res <- NULL
        cont <- ""
        while (!is.null(cont)) {
                continueURL <- paste0(baseURL, cont)
                if (verbose) {
                        cat("Downloading...", continueURL, fill = TRUE)
                }
                lineas <- getURL(continueURL)
                lineas <- gsub("\\n", "\\\\n", lineas) ##Funny line ends break json
                js <- fromJSON(lineas)
                res <- rbind(js[["query"]][["categorymembers"]], res)
                ##cont <- js[["query-continue"]][["categorymembers"]][["cmcontinue"]]
                cont <- js[["continue"]][["cmcontinue"]]
        }
        print(continueURL)
        
        if (!includeTemplates) {
                if (verbose) {
                        cat("Template pages not included", fill = TRUE)
                }
                touse <- res[,"ns"] == 0
                res <- res[touse, "title"]
        }
        
        ## output
        return(res)
}

"https://bots.snpedia.com/api.php?format=json&action=query&list=categorymembers&cmlimit=550&cmprop=title&cmtitle=Category:Is_a_snp&cmcontinue="