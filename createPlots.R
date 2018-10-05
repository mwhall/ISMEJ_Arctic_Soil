#!/usr/bin/env Rscript
library(ggplot2)
library(reshape2)
library(vegan)
library(grid)
library(gridExtra)

grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {

  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)

  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))

  #grid.newpage()
  grid.draw(combined)

  # return gtable invisibly
  invisible(combined)

}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    legend
}

wudata<-read.table("data/WeightedUniFracCoordsCollated.csv",sep="\t",header=TRUE)

depth <- c(5,10,20,30,40,50,60)

PC1<-c(31.4,32.9,31.7,31.7,33.5,31.5,57.1)
PC2<-c(22.1,16.5,16.9,19.2,15.6,18.3,28.5)

pcx<-data.frame(Depth=depth,PC1=PC1,PC2=PC2)

p0<-ggplot(wudata[wudata$Depth==5,], aes(x=PC1, y=PC2, fill=factor(Frozen)))+facet_wrap(~Depth,ncol=3)+geom_point(size=3,alpha=0.8,pch=21)+scale_fill_manual(values=c("white","black"),name="Frozen State", breaks=c("0","1"), labels=c("Frozen","Thawed"))+theme_bw()+theme(legend.position='none',axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ylab("PC2 22.1%")+xlab("PC1 31.4%")
p1<-ggplot(wudata[wudata$Depth==10,], aes(x=PC1, y=PC2, fill=factor(Frozen)))+facet_wrap(~Depth,ncol=3)+geom_point(size=3,alpha=0.8,pch=21)+scale_fill_manual(values=c("white","black"),name="Frozen State", breaks=c("0","1"), labels=c("Frozen","Thawed"))+theme_bw()+theme(legend.position='none',axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ylab("PC2 16.5%")+xlab("PC1 32.9%")
p2<-ggplot(wudata[wudata$Depth==20,], aes(x=PC1, y=PC2, fill=factor(Frozen)))+facet_wrap(~Depth,ncol=3)+geom_point(size=3,alpha=0.8,pch=21)+scale_fill_manual(values=c("white","black"),name="Frozen State", breaks=c("0","1"), labels=c("Frozen","Thawed"))+theme_bw()+theme(legend.position='none',axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ylab("PC2 16.9%")+xlab("PC1 31.7%")
p3<-ggplot(wudata[wudata$Depth==30,], aes(x=PC1, y=PC2, fill=factor(Frozen)))+facet_wrap(~Depth,ncol=3)+geom_point(size=3,alpha=0.8,pch=21)+scale_fill_manual(values=c("white","black"),name="Frozen State", breaks=c("0","1"), labels=c("Frozen","Thawed"))+theme_bw()+theme(legend.position='none',axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ylab("PC2 19.2%")+xlab("PC1 31.7%")
p4<-ggplot(wudata[wudata$Depth==40,], aes(x=PC1, y=PC2, fill=factor(Frozen)))+facet_wrap(~Depth,ncol=3)+geom_point(size=3,alpha=0.8,pch=21)+scale_fill_manual(values=c("white","black"),name="Frozen State", breaks=c("0","1"), labels=c("Frozen","Thawed"))+theme_bw()+theme(legend.position='none',axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ylab("PC2 15.6%")+xlab("PC1 33.5%")
p5<-ggplot(wudata[wudata$Depth==50,], aes(x=PC1, y=PC2, fill=factor(Frozen)))+facet_wrap(~Depth,ncol=3)+geom_point(size=3,alpha=0.8,pch=21)+scale_fill_manual(values=c("white","black"),name="Frozen State", breaks=c("0","1"), labels=c("Frozen","Thawed"))+theme_bw()+theme(legend.position='none',axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ylab("PC2 18.3%")+xlab("PC1 31.5%")
p6<-ggplot(wudata[wudata$Depth==60,], aes(x=PC1, y=PC2, fill=factor(Frozen)))+facet_wrap(~Depth,ncol=3)+geom_point(size=3,alpha=0.8,pch=21)+scale_fill_manual(values=c("white","black"),name="Frozen State", breaks=c("0","1"), labels=c("Frozen","Thawed"))+theme_bw()+theme(legend.position='none',axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ylab("PC2 28.5%")+xlab("PC1 57.1%")

pdf("PCoA_WeightedUniFracByDepth.pdf",width=6,height=4)
grid_arrange_shared_legend(p0,p1,p2,p3,p4,p5,ncol=3,nrow=2)
dev.off()

for (depth in c(5,10,20,30,40,50)) {
    subsetData <- wudata[wudata$Depth==depth,]
    vfit<-vectorfit(as.matrix(subsetData[,c("PC1","PC2")]), subsetData[,c("total_c","no3","nh4","po4","mbn","mbc","mbp","ph")],permutations=9999)
    print(paste("UniFrac Depth",depth))
    print(vfit)
}

bdata<-read.table("data/braycurtis_bydepth.tsv",sep="\t",header=TRUE)

p0<-ggplot(bdata[bdata$Depth==5,], aes(x=PC1, y=PC2, fill=factor(Frozen)))+facet_wrap(~Depth,ncol=3)+geom_point(size=3,alpha=0.8,pch=21)+scale_fill_manual(values=c("white","black"),name="Frozen State", breaks=c("0","1"), labels=c("Frozen","Thawed"))+theme_bw()+theme(axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ylab("PC2 11.1%")+xlab("PC1 37.7%")
p1<-ggplot(bdata[bdata$Depth==10,], aes(x=PC1, y=PC2, fill=factor(Frozen)))+facet_wrap(~Depth,ncol=3)+geom_point(size=3,alpha=0.8,pch=21)+scale_fill_manual(values=c("white","black"),name="Frozen State", breaks=c("0","1"), labels=c("Frozen","Thawed"))+theme_bw()+theme(axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ylab("PC2 13.0%")+xlab("PC1 26.8%")
p2<-ggplot(bdata[bdata$Depth==20,], aes(x=PC1, y=PC2, fill=factor(Frozen)))+facet_wrap(~Depth,ncol=3)+geom_point(size=3,alpha=0.8,pch=21)+scale_fill_manual(values=c("white","black"),name="Frozen State", breaks=c("0","1"), labels=c("Frozen","Thawed"))+theme_bw()+theme(axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ylab("PC2 15.8%")+xlab("PC1 22.9%")
p3<-ggplot(bdata[bdata$Depth==30,], aes(x=PC1, y=PC2, fill=factor(Frozen)))+facet_wrap(~Depth,ncol=3)+geom_point(size=3,alpha=0.8,pch=21)+scale_fill_manual(values=c("white","black"),name="Frozen State", breaks=c("0","1"), labels=c("Frozen","Thawed"))+theme_bw()+theme(axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ylab("PC2 15.5%")+xlab("PC1 22.2%")
p4<-ggplot(bdata[bdata$Depth==40,], aes(x=PC1, y=PC2, fill=factor(Frozen)))+facet_wrap(~Depth,ncol=3)+geom_point(size=3,alpha=0.8,pch=21)+scale_fill_manual(values=c("white","black"),name="Frozen State", breaks=c("0","1"), labels=c("Frozen","Thawed"))+theme_bw()+theme(axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ylab("PC2 16.5%")+xlab("PC1 21.8%")
p5<-ggplot(bdata[bdata$Depth==50,], aes(x=PC1, y=PC2, fill=factor(Frozen)))+facet_wrap(~Depth,ncol=3)+geom_point(size=3,alpha=0.8,pch=21)+scale_fill_manual(values=c("white","black"),name="Frozen State", breaks=c("0","1"), labels=c("Frozen","Thawed"))+theme_bw()+theme(axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ylab("PC2 15.1%")+xlab("PC1 34.3%")

pdf("PCoA_BrayCurtisByDepth.pdf",width=6,height=4)
grid_arrange_shared_legend(p0,p1,p2,p3,p4,p5,ncol=3,nrow=2)
dev.off()

for (depth in c(5,10,20,30,40,50)) {
    subsetData <- bdata[bdata$Depth==depth,]
    vfit<-vectorfit(as.matrix(subsetData[,c("PC1","PC2")]), subsetData[,c("total_c","no3","nh4","po4","mbn","mbc","mbp","ph")],permutations=9999)
    print(paste("Bray Curtis Depth",depth))
    print(vfit)
}

alphaData<-read.table("data/combined_alpha.txt",sep="\t",header=TRUE)
meltedAlpha<-melt(alphaData,id.vars=c("SampleName","iteration","Depth"))

alphaPlot<-ggplot(meltedAlpha[meltedAlpha$variable!="Chao1",],aes(y=value,x=factor(Depth)))+geom_boxplot()+facet_wrap(~variable,scales="free")+xlab("Depth (cm)")+ylab("Alpha diversity measure")+theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

pdf("AlphaByDepth.pdf",width=5,height=3)
alphaPlot
dev.off()

#Weighted UniFrac main PCOA plot/biplot

mainPcoa<-read.table("data/MainWeightedUniFracCoordsCollated.csv",sep="\t",header=TRUE, row.names=1)
#Reflect the axis
mainPcoa$PC1 <- mainPcoa$PC1 * -1
p<-ggplot(data=mainPcoa, aes(x=PC2, y=PC1, colour=factor(soil_depth)))+theme_bw()+theme(axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), legend.background=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+labs(colour="Soil Depth (cm)")+theme(legend.justification=c(0,1), legend.position=c(0,1))+ylab("PC1 (44.1%)")+xlab("PC2 (18.7%)")
p1<-p+geom_point(size=3,alpha=0.6)+scale_color_manual(values=c("5" = "#88CCEE", "10" = "#44AA99", "20" = "#999933","30" = "#DDCC77","40" = "#CC6677","50" = "#882255","60" = "#332288"))
PC1max<-max(abs(mainPcoa$PC1))-0.05
PC2max<-max(abs(mainPcoa$PC2))-0.05

vfit1<-vectorfit(as.matrix(mainPcoa[,c("PC2","PC1")]), mainPcoa[,c("total_c","no3","nh4","po4")],permutations=9999)
print(vfit1)
p2<-p+theme(legend.position="none")+geom_point(size=3,alpha=0.6,colour='grey')
p2<-p2+annotate("segment",x=0,y=0,yend=vfit1$arrows["total_c","PC1"]*PC1max,xend=vfit1$arrows["total_c","PC2"]*PC2max, arrow=arrow())
p2<-p2+annotate("segment",x=0,y=0,yend=vfit1$arrows["no3","PC1"]*PC1max,xend=vfit1$arrows["no3","PC2"]*PC2max, arrow=arrow())
p2<-p2+annotate("segment",x=0,y=0,yend=vfit1$arrows["nh4","PC1"]*PC1max,xend=vfit1$arrows["nh4","PC2"]*PC2max, arrow=arrow())
p2<-p2+annotate("segment",x=0,y=0,yend=vfit1$arrows["po4","PC1"]*PC1max,xend=vfit1$arrows["po4","PC2"]*PC2max, arrow=arrow())
p2<-p2+annotate("text",label="Total C",y=vfit1$arrows["total_c","PC1"]*PC1max+0.02,x=vfit1$arrows["total_c","PC2"]*PC2max+0.01)
p2<-p2+annotate("text",label="NO3",y=vfit1$arrows["no3","PC1"]*PC1max+0.03,x=vfit1$arrows["no3","PC2"]*PC2max,colour="black")
p2<-p2+annotate("text",label="NH4",y=vfit1$arrows["nh4","PC1"]*PC1max+0.03,x=vfit1$arrows["nh4","PC2"]*PC2max-0.06,colour="black")
p2<-p2+annotate("text",label="PO4",y=vfit1$arrows["po4","PC1"]*PC1max-0.01,x=vfit1$arrows["po4","PC2"]*PC2max-0.06,colour="black")

vfit2<-vectorfit(as.matrix(mainPcoa[,c("PC2","PC1")]), mainPcoa[,c("mbn","mbc","mbp","ph")],permutations=9999)
print(vfit2)
p3<-p+theme(legend.position="none")+geom_point(size=3,alpha=0.6,colour='grey')
p3<-p3+annotate("segment",x=0,y=0,yend=vfit2$arrows["ph","PC1"]*PC1max,xend=vfit2$arrows["ph","PC2"]*PC2max, arrow=arrow())
p3<-p3+annotate("segment",x=0,y=0,yend=vfit2$arrows["mbn","PC1"]*PC1max,xend=vfit2$arrows["mbn","PC2"]*PC2max, arrow=arrow())
p3<-p3+annotate("segment",x=0,y=0,yend=vfit2$arrows["mbc","PC1"]*PC1max,xend=vfit2$arrows["mbc","PC2"]*PC2max, arrow=arrow())
p3<-p3+annotate("segment",x=0,y=0,yend=vfit2$arrows["mbp","PC1"]*PC1max,xend=vfit2$arrows["mbp","PC2"]*PC2max, arrow=arrow())
p3<-p3+annotate("text",label="pH",y=vfit2$arrows["ph","PC1"]*PC1max-0.04,x=vfit2$arrows["ph","PC2"]*PC2max,colour="black")
p3<-p3+annotate("text",label="MBN",y=vfit2$arrows["mbn","PC1"]*PC1max+0.01,x=vfit2$arrows["mbn","PC2"]*PC2max+0.02,colour="black")
p3<-p3+annotate("text",label="MBC",y=vfit2$arrows["mbc","PC1"]*PC1max+0.01,x=vfit2$arrows["mbc","PC2"]*PC2max+0.16,colour="black")
p3<-p3+annotate("text",label="MBP",y=vfit2$arrows["mbp","PC1"]*PC1max,x=vfit2$arrows["mbp","PC2"]*PC2max-0.08,colour="black")

pdf("MainPCoABiplot_WeightedUniFrac.pdf",width=8,height=5)
multiplot(p1,p2,p3,cols=2,layout=matrix(c(1,1,1,1,3,2),nrow=2))
dev.off()

#Bray-Curtis main PCoA plot/biplot

mainPcoa<-read.table("data/bray_coords.txt",sep="\t",header=TRUE, row.names=1)
mainPcoa$PC1 <- mainPcoa$PC1 * -1
p<-ggplot(data=mainPcoa, aes(x=PC2, y=PC1, colour=factor(soil_depth)))+theme_bw()+theme(axis.ticks=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), legend.background=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+labs(colour="Soil Depth (cm)")+theme(legend.justification=c(0,1), legend.position=c(0,1))+ylab("PC1 (39.6%)")+xlab("PC2 (11.9%)")+scale_color_manual(values=c("5" = "#88CCEE", "10" = "#44AA99", "20" = "#999933","30" = "#DDCC77","40" = "#CC6677","50" = "#882255","60" = "#332288"))
p1<-p+geom_point(size=3,alpha=0.6)
PC1max<-max(abs(mainPcoa$PC1))-0.1
PC2max<-max(abs(mainPcoa$PC2))-0.1

vfit1<-vectorfit(as.matrix(mainPcoa[,c("PC2","PC1")]), mainPcoa[,c("total_c","no3","nh4","po4")],permutations=9999)
print(vfit1)
p2<-p+theme(legend.position="none")+geom_point(size=3,alpha=0.6,colour='grey')
p2<-p2+annotate("segment",x=0,y=0,yend=vfit1$arrows["total_c","PC1"]*PC1max,xend=vfit1$arrows["total_c","PC2"]*PC2max, arrow=arrow())
p2<-p2+annotate("segment",x=0,y=0,yend=vfit1$arrows["no3","PC1"]*PC1max,xend=vfit1$arrows["no3","PC2"]*PC2max, arrow=arrow())
p2<-p2+annotate("segment",x=0,y=0,yend=vfit1$arrows["nh4","PC1"]*PC1max,xend=vfit1$arrows["nh4","PC2"]*PC2max, arrow=arrow())
p2<-p2+annotate("segment",x=0,y=0,yend=vfit1$arrows["po4","PC1"]*PC1max,xend=vfit1$arrows["po4","PC2"]*PC2max, arrow=arrow())
p2<-p2+annotate("text",label="Total C",y=vfit1$arrows["total_c","PC1"]*PC1max+0.07,x=vfit1$arrows["total_c","PC2"]*PC2max+0.05)
p2<-p2+annotate("text",label="NO3",y=vfit1$arrows["no3","PC1"]*PC1max+0.05,x=vfit1$arrows["no3","PC2"]*PC2max-0.03,colour="black")
p2<-p2+annotate("text",label="NH4",y=vfit1$arrows["nh4","PC1"]*PC1max+0.03,x=vfit1$arrows["nh4","PC2"]*PC2max-0.05,colour="black")
p2<-p2+annotate("text",label="PO4",y=vfit1$arrows["po4","PC1"]*PC1max,x=vfit1$arrows["po4","PC2"]*PC2max-0.08,colour="black")

vfit2<-vectorfit(as.matrix(mainPcoa[,c("PC2","PC1")]), mainPcoa[,c("mbn","mbc","mbp","ph")],permutations=9999)
print(vfit2)
p3<-p+theme(legend.position="none")+geom_point(size=3,alpha=0.6,colour='grey')
p3<-p3+annotate("segment",x=0,y=0,yend=vfit2$arrows["ph","PC1"]*PC1max,xend=vfit2$arrows["ph","PC2"]*PC2max, arrow=arrow())
p3<-p3+annotate("segment",x=0,y=0,yend=vfit2$arrows["mbn","PC1"]*PC1max,xend=vfit2$arrows["mbn","PC2"]*PC2max, arrow=arrow())
p3<-p3+annotate("segment",x=0,y=0,yend=vfit2$arrows["mbc","PC1"]*PC1max,xend=vfit2$arrows["mbc","PC2"]*PC2max, arrow=arrow())
p3<-p3+annotate("segment",x=0,y=0,yend=vfit2$arrows["mbp","PC1"]*PC1max,xend=vfit2$arrows["mbp","PC2"]*PC2max, arrow=arrow())
p3<-p3+annotate("text",label="pH",y=vfit2$arrows["ph","PC1"]*PC1max-0.02,x=vfit2$arrows["ph","PC2"]*PC2max-0.04,colour="black")
p3<-p3+annotate("text",label="MBN",y=vfit2$arrows["mbn","PC1"]*PC1max+0.03,x=vfit2$arrows["mbn","PC2"]*PC2max+0.02,colour="black")
p3<-p3+annotate("text",label="MBC",y=vfit2$arrows["mbc","PC1"]*PC1max+0.02,x=vfit2$arrows["mbc","PC2"]*PC2max-0.07,colour="black")
p3<-p3+annotate("text",label="MBP",y=vfit2$arrows["mbp","PC1"]*PC1max,x=vfit2$arrows["mbp","PC2"]*PC2max-0.07,colour="black")

pdf("MainPCoABiplot_BrayCurtis.pdf",width=8,height=5)
multiplot(p1,p2,p3,cols=3,layout=matrix(c(1,1,1,1,3,2),nrow=2))
dev.off()
