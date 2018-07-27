## query opensnp.com
library(httr)
library(dplyr)
library(XML)
library(dplyr)

## energy efficiency Universe
keywords <- c("diabetes","carbohydrates","glucose","glycolisis","pentose","phosphate",
            "Piruvate","insulin","glucogen","lipids","lipogenesis","trigliceride",
            "Isocitrate","proteins","amino+acids","BCAA","T1DM","T2DM","TCA+cycle",
            "Acetil+CoA","isocitrato","obesity")

###########################################################################################
## Data mining based on keywords

## perform a single query
findSnpsByKeywordOpenSnp <- function(keyword){
        ## Query definition
        baseURL <- "https://opensnp.org/search?utf8=%E2%9C%93&search="
        url <- paste0(baseURL,keyword)
        
        ## read html to tree parsed structure
        html2 <- GET(url)
        cont <- content(html2,as = "text")
        parsedHtml <- htmlParse(cont,asText = TRUE)
        
        ## getting rsIds inside tag: <a><td> rsIds </td></a>
        fields <- xpathSApply(parsedHtml,
                              "//td[@class='table-cell vertical-centered']//a", 
                              xmlValue)
        
        ## filtering rsIds
        if ( length(fields) > 0 ){
                rsIds <- grep("rs[[:digit:]]",fields,value = TRUE)      ## getting rs1-9 pattern
                rsIds <- rsIds[nchar(rsIds) < 11]                       ## filtering string by size
                head(rsIds)
                length(rsIds)
                
                ## formatting output dataframe of three columns containing
                ## rsIds, ocurrences, and keyword
                idFrequencies <- table(rsIds)
                snps <- data.frame("rsIds"=names(idFrequencies),
                                   "Ocurrences"=as.vector(idFrequencies),
                                   "query"=rep(keyword,length(idFrequencies)))
                
                return(snps)
                
        } else {
                ## No query results case
                cat("Query = ",keyword,"have no results in opensnp.com data bases")
                snps <- NULL
                
                return(snps)
        }
}

#########################################################################################
## Perform query

## iterate keyword query
openSnpQuery <- function(keywords){
        ## dataframe to store data
        snps <- data.frame()
        
        ## performing query and appending results to snps
        for (k in keywords){
                res <- findSnpsByKeywordOpenSnp(k)
                if ( length(res) > 0 ){
                        snps <- rbind(snps,res)
                }
        }
        
        return( arrange(snps,desc(Ocurrences)) )
}

## performing query to opensnp
snps <- openSnpQuery(keywords)

## processing rs Ids
snps[,"rsIds"] <- sapply(snps[,"rsIds"], function(x) gsub("A|C|Ŧ|G|/","",x))

## saving data
write.csv(snps,
          "../data/opensnp.csv",
          row.names=FALSE)

## reading pre calculated data
snps <- read.csv("../data/opensnp.csv")

## snpnexus query format
nexusQuery <- data.frame("dbsnp"=rep("dbsnp",nrow(snps)),
                         "rsIds"=snps[,"rsIds"])
head(nexusQuery)
write.table(nexusQuery,"../data/nexusQuery.tsv",
            col.names = FALSE,row.names = FALSE,quote = FALSE)


## dbSNP query format
write.table(snps[,"rsIds"],"../data/dbSNPQuery.tsv",
            col.names = FALSE,row.names = FALSE,quote = FALSE)


