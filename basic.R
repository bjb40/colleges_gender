#Dev R 3.3.0 "Supposedly Educational"
#Bryce Bartlett

#@@@
#preliminaries
#@@@

library('ggplot2')
library('gridExtra')

#@@@@@@@@@@
#Read Data
#Data from IPEDS
#https://nces.ed.gov/ipeds/datacenter/Data.aspx
#downloaded 9/17/2016 -- all employees engaged in teaching by female and total
#no idea what imputation status flag means
#@@@@@@@@@@@

dat = read.csv("Data_9-17-2016.csv")
dat = dat[,1:ncol(dat)-1] #wierd "X" variable that appears useless

#@@@@@@@@@@
#Make data useful
#@@@@@@@@@@

#helper functions

pcol = function(df,pattern){
  #helper funciton that looks for string based on regex pattern
  #this will return a list of columns based on the *name* of the column  
  
  col = grepl(pattern,colnames(df),ignore.case=TRUE,perl=TRUE)
  return(df[,col])
}

#dunno what imputation status is; drop
dat = pcol(dat,'^(?!Imputation).*$')

fe=pcol(dat,'instnm|total.women')
t=pcol(dat,'^(?!Grand.total.women).*$')

#rename top variables to include only year
colnames(fe)[2:ncol(fe)] = 
  paste0('fe.',
         na.omit(as.numeric(unlist(strsplit(colnames(fe[2:ncol(fe)]),"[^0-9]+"))))
         )
fe=reshape(fe,direction='long',varying=2:11)

colnames(t)[2:ncol(t)] = 
  paste0('tot.',
    na.omit(as.numeric(unlist(strsplit(colnames(t[2:ncol(t)]),"[^0-9]+"))))
    )
t=reshape(t,direction='long',varying=2:11)

dat=merge(fe,t,by=c('id','instnm','time'));rm(t,fe)
dat$prop_female=dat$fe/dat$tot

#generate plots by year
byus=grepl('Brigham',dat$instnm,ignore.case=TRUE)
grace=grepl('Graceland',dat$instnm,ignore.case=TRUE)
dat$Affiliation=0
dat$Affiliation[byus]=1
dat$Affiliation[grace]=2
dat$Affiliation = factor(dat$Affiliation,
                         levels=c(0,1,2),
                         labels=c('Other','BYU','Graceland (RLDS)')) #for plotting



j= ggplot(dat, aes(x = time, y=prop_female,col=Affiliation)) + geom_jitter()
l= ggplot(dat, aes(x=time,y=prop_female)) + expand_limits(y=1) + stat_smooth(aes(col = Affiliation)) 

png('plot.png')
  grid.arrange(j,l,nrow=2)
dev.off()
  
#print # of schools and those with missing values
length(unique(dat$id))
length(unique(dat[complete.cases(dat),]$id))

write.csv(dat,'bjb_clean.csv')

