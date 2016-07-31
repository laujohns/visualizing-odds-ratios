setwd ("") #set working directory
library(gridExtra)
library(grid)
library(ggplot2)

#create dataset for an example
chem=rep(c("A",times=5),c("B", times=5) #chemical exposure
quintile=rep(c("1","2","3","4","5"), times=2)#quintile of exposure
beta=c(-3:1,-1:3)#exp(beta estimates) from model=odds ratios (ORs)
lcl=c(-4:0,-2:2)#lower confidence limits from ORs
ucl=c(-1:3,2:4)#upper confidence limits from ORs
df=data.frame(chem, quintile, beta, lcl, ucl)#create data frame
df#take a look at data frame

library("plyr")
neworder <- c("A","B")
df2<- arrange(transform(df,chem=factor(chem,levels=neworder)),chem)#reorder by A and B
df2#take a look at data frame

pd <- position_dodge(width=8)

#create text to denote significant ORs
sig1<-data.frame(quintile=1,beta=-0.8,chem=factor("A",levels=c("A","B")))
sig2<-data.frame(quintile=4,beta=4.2,chem=factor("A",levels=c("A","B")))
sig3<-data.frame (quintile=5,beta=5.2,chem=factor("B",levels=c("A","B")))

#black and white panel plot 
quint<-ggplot(df2, aes(quintile, beta, shape=chem)) +
  theme_bw() +    
  theme(legend.position="none") +
  facet_grid(~ chem, labeller=label_parsed)+
  geom_point(size=2.5)+ 
  scale_shape_manual(values=c(9, 16, 15, 17, 18))+
  scale_x_discrete(name="Quintile of Exposure", breaks=1:5, labels=c("1", "2", "3", "4","5")) + 
  scale_y_continuous(name="Change in dependent variable", breaks=c(-4, -2, 2, 4)) +
  geom_errorbar(aes(ymin=lcl,ymax=ucl),width=0.6,position=pd, size=0.7) +
  geom_hline(aes(yintercept=0),linetype="dashed")+
  ggtitle("Figure 1. Adjusted regression coeficients for change in outcome variable\nin relation to exposure quintiles (*p<0.05)")+
  geom_text(data=sig1, aes(label="*"), size=8, colour="black",family="sans")+
  geom_text(data=sig2, aes(label="*"), size=8, colour="black",family="sans")+
  geom_text(data=sig3, aes(label="*"), size=8, colour="black",family="sans")+
  theme(plot.title=element_text(face="bold",size=14, family="serif"), #mono=courier new, serif=times new roman
        axis.text.x = element_text(angle = 0, hjust = 1,vjust=1, size=10,family="serif"), 
        axis.text.y=element_text(vjust=1, size=10,family="serif"),
        text=element_text(size=14,family="serif"), 
        strip.text.x=element_text(size=14, face="bold",family="serif"))
print(quint)
ggsave("Fig1_quintiles.tiff", height=4, width=8.5, units='in', dpi=600)

