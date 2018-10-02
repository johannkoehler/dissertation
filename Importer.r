wok2dbl.f <- function(
  dir=stop("Choose input directory containing WOK batches.")
  ,out=stop("Specify output directory for your project.")
  ,art_rev_only=T
  ,sample.batches=F
  ,sample.size=1000
  ,save=T
  ,verbose=T
  ,check.for.saved.output=F
  ,imp.rul=list( #import rules
    collapse=c(TI=' ',WC=' ',SC=' ')
    ,split=c(WC='; ',SC='; '))
)
{
  if(check.for.saved.output) if(any(grepl('wok2dbl.RData',dir(path=out,recursive=T,full.names=T,ignore.case=T)))) {
    warning('Loading and returning saved wok2dbl.RData.',call. = F)
    load(dir(path=out,pattern='wok2dbl.RData',recursive=T,full.names=T,ignore.case=F)[1])
    return(wok2dbl)
  }

  #NEWER WOK Database Import
  library(data.table)

  files<-list.files(dir,full.names=T,recursive=T)

  c<-0
  n<-length(files)

  cat("\n",n," batches detected.",sep="")

  if(sample.batches) {
    files<-sort(sample(x=files,size=sample.size))
    cat("\n",sample.size," or ",round(sample.size/n*100,3)," % of batches drawn at random.\n\n",sep="")
  }

  n<-length(files)

  wok2dbl<-list()
  for(i in files){
    c<-c+1
    if(verbose) {flush.console();cat("\r",round(c/n,3),i,sep=" ")}
    b<-readLines(i,warn=F)
    field<-sub("^(.{2}).+","\\1",b)
    cut<-field%in%c("FN","VR","","ER","EF")
    b<-b[!cut]
    field<-field[!cut]
    t<-field=="  "
    x<-which(t)
    y<-which(!t)
    if(any(t)) {
      for(j in 1:length(x)) x[j]<-y[sum(y<x[j])]
      field[t]<-field[x]
    }
    b<-sub("^.. ?(.*)","\\1",b)
    ind<-1:length(b)
    t<-field!="UT"
    x<-which(t)
    y<-which(!t)
    ind[t]<-ind[y[sapply(lapply(lapply(as.list(x),"<",y),which),min)]]
    d<-data.table(b.ind=b[ind],field,b)
    if(art_rev_only){
      dt<-which(d$field=="DT")
      dtt<-grepl("(Article)|(Review)",d$b[dt])
      if(!all(dtt)) {
        dt<-d$b.ind[dt[dtt]]
        setkey(d,b.ind)
        d<-d[dt]
      }
    }
    wok2dbl[[i]]<-copy(d)
  }
  wok2dbl<-rbindlist(wok2dbl)
  col.ord<-unique(wok2dbl$field)
  setnames(wok2dbl,c("b.ind","b"),c("id","val"))
  if(!is.null(imp.rul)){
    setkey(wok2dbl,field)
    imp<-list()
    for(i in names(imp.rul$collapse)) {
      imp[[i]]<-wok2dbl[list(i),list(val=paste(val,collapse=imp.rul$collapse[[i]])),by=c('id','field')]
      wok2dbl<-wok2dbl[!list(i)]
    }
    for(i in names(imp.rul$split)) {
      if(i%in%names(imp)) {
        imp[[i]]<-imp[[i]][,list(val=unlist(strsplit(val,split = imp.rul$split[[i]]))),by=c('id','field')]
      } else {
        imp[[i]]<-wok2dbl[list(i),list(val=unlist(strsplit(val,split = imp.rul$split[[i]]))),by=c('id','field')]
        wok2dbl<-wok2dbl[!list(i)]
      }
    }
    wok2dbl<-rbindlist(c(list(wok2dbl),imp))
  }
  setkey(wok2dbl,id,field)
  o<-wok2dbl[,list(o=1:.N),keyby=c('id','field')]$o
  wok2dbl[,o:=o]
  setkey(wok2dbl,id,field,val)
  dup<-duplicated(wok2dbl)
  if(any(dup)){
    ud<-unique(wok2dbl$id[dup])
    cat('\n\n',length(ud),' duplicate records detected and removed, e.g.:\n',sep='')
    if(length(ud)>5) {cat(sample(ud,5),sep='\n')} else {cat(ud,sep='\n')}
    wok2dbl<-unique(wok2dbl)
    setkey(wok2dbl,id,field,o)
    o<-wok2dbl[,list(o=1:.N),keyby=c('id','field')]$o
    wok2dbl[,o:=o]
  }
  wok2dbl<-droplevels(wok2dbl)
  attributes(wok2dbl)$col.ord<-col.ord
  setkey(wok2dbl,id,field,o,val)
  if(save) save(wok2dbl,file=paste(out,.Platform$file.sep,"wok2dbl.RData",sep=""))
  wok2dbl
}
