
get.pdfs<-function(){
  fs<-list.files("www",pattern=".pdf",full.names=T)
  cat("--- new files fetch ---\n")
  li<-lapply(fs,function(x){
    # print(x)
    n<-basename(x)
    d<-paste0('<li><a href="',n,'" target="_blank">',n,"</a></li>")
  })
ul<-paste0("<ul>",paste0(unlist(li),collapse="\n"),"</ul>")
cat(ul)
div<-paste0("<div>",ul,"</div>")
td<-lapply(fs,function(x){
    # print(x)
    n<-basename(x)
  d<-data.frame(link=paste0('<a href="',n,'" target="_blank">',n,'</a>'))
  # d<-paste0('<li><a href="',n,'" target="_blank">',n,"</a></li>")
    # d<-data.frame(link=paste0('[',n,'](',n,')'))

})
tdf<-data.frame(abind(td,along=1))
  return(list(div=div,td=tdf))
}

get.cdirs<-function(cp,local){
  print("--- get.cdirs() ---")
### tempfile to store zip
sbctemp<-tempfile("SBCtemp.zip")
sbctempdir<-tempdir()
f<-list.files(".")
m<-grepl("^SBCtemp.zip$",f)
dload<-ifelse(sum(m)>0,F,T)
m2<-grepl("cdf.RData",f)
m2<-sum(m2>0)
if(m2){
  cat("--- loading from saved cdf ---\n")
  load("cdf.RData")
  print(dim(cdf))
  # print(colnames(cdf))
  return(list(cdf=cdf))
}
dload<-ifelse(sum(m)>0,F,T)
fc<-f[m]
ifelse(!local&dload,download.file(q,sbctemp),file.copy(fc,sbctemp))
file.copy(sbctemp,"SBCtemp.zip")
unzip(sbctemp,exdir = sbctempdir)
cat("--- top zip ---\n")
# print(list.files(sbctempdir))
sbctrn<-paste0(sbctempdir,"/")
filestrn<-list.files(sbctrn)
cat("--- filestrn ---\n")

# print(filestrn)
f<-list.dirs(paste0(sbctempdir,"/",filestrn[2]),full.names=T)
cat("--- country dirs ---\n")
f<-f[2:length(f)]
# print(f)
cdf<-lapply(f,function(x){
  #read.csv()
    cat("--- lapply files --- :",x,":\n")
    csf<-list.files(paste0(x),full.names=T)
    # print(csf)

    m<-grep("stories",basename(csf))
    # print(csf[m])
    cdf<-read.csv(csf[m])
    #stories<-cdf$Story
})
library(abind)
cat("dim cdf:",dim(cdf),"\n")
cdf<-data.frame(abind(cdf,along=1))

save(cdf,file="cdf.RData")
return(list(cdflist=f,cdf=cdf))
}
get.stories<-function(country,cdf){
 # print(country)
cc<-countries[countries$country_name==country]
cc<-cc[cc!=""]
cc<-cc[!is.na(cc)]
cat("--- extrcating... ---\n")
sbctemp<-tempfile("SBCtemp.zip")
# sbctempdir<-tempdir()
#download.file(q,sbctemp)
#ifelse(!local,download.file(q,sbctemp),file.copy(cp,sbctemp))
#unzip(sbctemp,exdir = sbctempdir)
#print(list.files(sbctempdir))
#sbctrn<-paste0(sbctempdir,"/")
#filestrn<-list.files(sbctrn)
#print(filestrn)
#f<-list.files(paste0(sbctempdir,"/",filestrn[2]))
f
cl<-f

print("---- codes ---")
# print(cc)
sbctrn<-paste0(sbctempdir,"/")
filestrn<-list.files(sbctrn)
cat("--- folders---\n")
# print(filestrn)

    csf<-list.files(paste0(sbctempdir))#,"/",filestrn[2],"/",cc),full.names=T)
    # print(csf)

    m<-grep("stories",basename(csf))
   # print(csf[m])
    cdf<-read.csv(csf[m])
   # stories<-cdf$Story
}
