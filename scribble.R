load("texturen/cdf.RData")

co<-"Andorra"
id<-"1"
subset<-cdf[cdf$Country_Name==co,]
    story<-subset$Story[id]
    print(dim(subset))
    print(colnames(subset))
    cat("--- story:",co,id,"\n\n",substr(story,1,100),"\n")

library(markdown)
?markdown
?rmarkdown::render
