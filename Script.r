library(magrittr)
library(data.table)
library(ggplot2)
source("Importer.r")
dat<-wok2dbl.f(dir = 'WoS.TextFiles.19Sept2018.61551/', out = 'Out')
dat[,o:=NULL] #if we ever need to remember first author, we would need this back
head(dat[field=="CR"], 50)
setkey(dat,field,id) # key the database on the field to let me query things quickly by field


# Data Audit --------------------------------------------------------------
dat['UT'][duplicated(val),val] %>% cat(sep='\n')

# Descriptives ------------------------------------------------------------

# average page numbers and number of works cited by year

  des<-dcast(data = dat[c("PY", "PG", "NR")]
             ,formula = id~field
             ,value.var = "val",
             ,fun.aggregate = function(x) x[1]
  )[,`:=`(
               NR=as.numeric(NR)
               ,PG=as.numeric(PG)
               ,PY=as.numeric(PY)
             )]
  
  des<-des[,.(cites=mean(NR),pages=mean(PG)),by=PY]
  
  ggplot(data=melt(des,id.vars = "PY")) + geom_line(aes(x=PY,y=value,color=variable))

ggsave("citespages.png")

