library(xlsx)
library(ggplot2)
library(ggthemes)
library(scales)
library(grid)
library(gridExtra)
library(dplyr)
library(tidyr)
library(plotly)
library(extrafont)

Sys.setenv("plotly_username"="reidhaefer")
Sys.setenv("plotly_api_key"="6g0piiyf82")
font_import()
loadfonts(device = "win")
setwd("X:/Trans/STAFF/Reid/Project Selection/EJ Analysis")

data<-read.xlsx("DemogDataEJ.xlsx", sheetName="DemogDataEJ")


data$pct_eld_whole<-data$pct_eld * 100
data$pct_dis_whole<-data$pct_dis * 100
data$pct_pov_whole<-data$pct_pov * 100
data$pct_min_whole<-data$pct_min * 100

a<-ggplot(data, aes(pct_eld_whole)) + geom_histogram(binwidth=1, colour="white", fill="#F05A28", alpha=0.7) + theme_economist() + 
  theme(plot.background=element_rect(fill = alpha("#00A7A0", 0.25), colour="white"), panel.background=element_blank(),axis.text.y=element_text(color="black", face="bold",size=16),
        axis.text.x=element_text(color="black", face="bold",size=16),axis.title.x=element_text(color="black", face="bold",margin=margin(20,0,0,0),size=16) ,
        axis.title.y=element_text(color="black", face="bold",margin=margin(0,20,0,0),size=16) ) + 
  annotate("segment", x=10.75, xend = 10.75, y=0, yend=83,colour= "#91268F", linetype="longdash", size=1)+
  annotate("segment", x=21, xend = 29, y=35, yend=35,colour= "#91268F", linetype="longdash", size=1)+
  annotate("text", x=25, xend = 29, y=33, yend=33,label="REGIONAL THRESHOLD",colour= "#91268F", fontface="bold",size=6)+
  ylab("NUMBER OF CENSUS TRACTS") +xlab("PERCENT ELDERLY PER CENSUS TRACT") +
  scale_x_continuous(breaks=c(0,5,10,15,20,25,30,35,40),labels=c("0","5","10%","15%","20%","25%","30%","35%","40%"))
  ggsave(a, file= "elderly.jpg", width=15,height=9,dpi=1000)
  
b<-ggplot(data, aes(pct_dis_whole)) + geom_histogram(binwidth=1, colour="white", fill="#F05A28", alpha=0.7) + theme_economist() + 
    theme(plot.background=element_rect(fill = alpha("#00A7A0", 0.25), colour="white"), panel.background=element_blank(),axis.text.y=element_text(color="black", face="bold",size=16),
          axis.text.x=element_text(color="black", face="bold",size=16),axis.title.x=element_text(color="black", face="bold",margin=margin(20,0,0,0),size=16) ,
          axis.title.y=element_text(color="black", face="bold",margin=margin(0,20,0,0),size=16) ) + 
    annotate("segment", x=11, xend = 11, y=0, yend=80,colour= "#91268F", linetype="longdash", size=1)+
    annotate("segment", x=22, xend = 28, y=35, yend=35,colour= "#91268F", linetype="longdash", size=1)+
    annotate("text", x=25, xend = 29, y=33, yend=33,label="REGIONAL THRESHOLD",colour= "#91268F", fontface="bold",size=6)+
   ylab("NUMBER OF CENSUS TRACTS") +xlab("PERCENT DISABLED PER CENSUS TRACT") +
    scale_x_continuous(breaks=c(0,5,10,15,20,25,30,35),labels=c("0","5","10%","15%","20%","25%","30%","35%"))
  ggsave(b, file= "disabled.jpg", width=15,height=9, dpi=1000)
  
  c<-ggplot(data, aes(pct_pov_whole)) + geom_histogram(binwidth=1, colour="white", fill="#F05A28", alpha=0.7) + theme_economist() + 
    theme(plot.background=element_rect(fill = alpha("#00A7A0", 0.25), colour="white"), panel.background=element_blank(),axis.text.y=element_text(color="black", face="bold",size=16),
          axis.text.x=element_text(color="black", face="bold",size=16),axis.title.x=element_text(color="black", face="bold",margin=margin(20,0,0,0),size=16) ,
          axis.title.y=element_text(color="black", face="bold",margin=margin(0,20,0,0),size=16) ) + 
    annotate("segment", x=11, xend = 11, y=0, yend=65,colour= "#91268F", linetype="longdash", size=1)+
    annotate("segment", x=20, xend = 30, y=35, yend=35,colour= "#91268F", linetype="longdash", size=1)+
    annotate("text", x=25, xend = 29, y=33, yend=33,label="REGIONAL THRESHOLD",colour= "#91268F", fontface="bold",size=6)+
   ylab("NUMBER OF CENSUS TRACTS") +xlab("PERCENT POVERTY PER CENSUS TRACT") +
    scale_x_continuous(breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65),labels=c("0","5","10%","15%","20%","25%","30%","35%","40%","45%","50%","55%","60%","65%"))
  ggsave(c, file= "poverty.jpg", width=15,height=9,dpi=1000)
  
  d<-ggplot(data, aes(pct_min_whole)) + geom_histogram(binwidth=1, colour="white", fill="#F05A28", alpha=0.7) + theme_economist() + 
    theme(plot.background=element_rect(fill = alpha("#00A7A0", 0.25), colour="white"), panel.background=element_blank(),axis.text.y=element_text(color="black", face="bold",size=16),
          axis.text.x=element_text(color="black", face="bold",size=16),axis.title.x=element_text(color="black", face="bold",margin=margin(20,0,0,0),size=16) ,
          axis.title.y=element_text(color="black", face="bold",margin=margin(0,20,0,0),size=16) ) + 
    annotate("segment", x=33.1, xend = 33.1, y=0, yend=33,colour= "#91268F", linetype="longdash", size=1)+
    annotate("segment", x=47, xend = 62.5, y=26, yend=26,colour= "#91268F", linetype="longdash", size=1)+
    annotate("text", x=55, xend = 55, y=25, yend=25,label="REGIONAL THESHOLD",colour= "#91268F", fontface="bold",size=6)+
   ylab("NUMBER OF CENSUS TRACTS") +xlab("PERCENT MINORITY PER CENSUS TRACT") +
    scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100),labels=c("0","10%","20%","30%","40%","50%","60%","70%","80%","90%","100%"))
  ggsave(d, file= "minority.jpg", width=15,height=9, dpi = 1000)
  
  ggsave("histogram.jpg", arrangeGrob(a, b,c,d), width=25,height=15, dpi=1000)
  
  
  ##########################
  
  e<-ggplot(data, aes(ej_comb)) + geom_bar(colour="white", fill="#F05A28", alpha=0.7) + theme_economist() + 
    theme(plot.background=element_rect(fill = alpha("#00A7A0", 0.25), colour="white"), panel.background=element_blank(),axis.text.y=element_blank(),
          axis.text.x=element_text(color="black", face="bold",size=15),axis.title.x=element_text(color="black", face="bold",margin=margin(20,0,0,0)) ,
          axis.title.y=element_text(color="black", face="bold",margin=margin(0,20,0,0),size=12) ) + 
    ggtitle("Distribution of Equity Measures") +ylab("NUMBER OF CENSUS TRACTS") +xlab("NUMBER OF MEASURES ABOVE REGIONAL THRESHOLD") +
    scale_x_continuous(breaks=c(0,1,2,3,4), labels= c("zero", "one", "two", "three", "four")) +
     geom_text(stat='count',aes(label=..count..),vjust=2,size=8,fontface="bold")
  
   ggsave(e, file= "measures_distribution.pdf", width=8,height=8)
   
   
   ####################################
   
   perc_touched<-data.frame(perc= c(53.10, 49.14,51.69,61.44,73.53), group=c(0,1,2,3,4))
   
   f<-ggplot(perc_touched, aes(group, perc)) + geom_bar(stat="identity",colour="white", fill="#F05A28", alpha=0.7) + theme_economist() + 
     theme(plot.background=element_rect(fill = alpha("#00A7A0", 0.25), colour="white"), panel.background=element_blank(),
           axis.text.x=element_text(color="black", face="bold",size=15),axis.title.x=element_text(color="black", face="bold",margin=margin(20,0,0,0)) ,
           axis.title.y=element_blank(),axis.text.y=element_text(color="black", face="bold",size=12),plot.title=element_blank(),
           axis.ticks.x=element_blank()) + 
     ggtitle("PERCENT OF MEASURE GROUPS TOUCHED BY PROJECTS") +xlab("NUMBER OF MEASURES ABOVE REGIONAL THRESHOLD") +
     scale_x_continuous(breaks=c(0,1,2,3,4), labels= c("ZERO", "ONE", "TWO", "THREE", "FOUR")) +
     scale_y_continuous(limits= c(0,80),breaks=c(0,10,20,30,40,50,60,70,80), labels=c("0","10%","20%","30%","40%","50%","60%","70%","80%"))+
     geom_text(aes(label=paste(factor(perc), "%")), colour="white", fontface="bold",size=7,position=position_dodge(width=0.9), vjust=2)
     
   
   ggsave(f, file= "measures_projects_touched.jpg", width=10,height=8, dpi=900)
   
   
   ####################################
   
   
   perc_ej_touched<-data.frame(perc_tract=c(44.5,45.6, 22.1,45.6,38.9,40.1,55.4,43.7), 
                                measure=c("poverty", "minority", "elderly", "disabled","poverty", "minority", "elderly", "disabled"),
                               tract=c("TOUCHED EJ TRACT", "TOUCHED EJ TRACT", "TOUCHED EJ TRACT", "TOUCHED EJ TRACT","EJ TRACT", "EJ TRACT", "EJ TRACT", "EJ TRACT"))
 
   perc_ej_touched$measure<-toupper(perc_ej_touched$measure)
   
   
   g<-ggplot(perc_ej_touched, aes(measure, perc_tract, fill=tract) ) +geom_bar(stat="identity",position="dodge", colour="white",alpha=.8) +theme_economist() +
     theme(plot.background=element_rect(fill = alpha("#00A7A0", 0.25), colour="white"), panel.background=element_blank(),
           axis.text.x=element_text(color="black", face="bold",size=15,family="Arial"),axis.title.x=element_blank() ,
           axis.title.y=element_blank(), axis.text.y=element_text(color="black", face="bold",size=12,family="Arial"),
           axis.ticks.x=element_blank(),plot.title=element_blank(),legend.title=element_blank(), legend.background=element_blank(),
           legend.position = c(0.8, 0.85)) +
          ggtitle("All EJ tracts vs. touched EJ tracts (Percent of Total)")+
          geom_text(aes(label=paste(factor(perc_tract), "%")), colour="white", fontface="bold",size=7,position=position_dodge(width=0.9), vjust=2) +
     scale_y_continuous(limits= c(0,63),breaks=c(0,10,20,30,40,50,60), labels=c("0","10%","20%","30%","40%","50%","60%"))+
          scale_fill_manual(values=c("#F05A28", "#91268F"))
   
   ggsave(g, file= "tracts_touched.jpg", width=10,height=8,dpi = 900)
  
   
   ############################################# pie chart

 tracts_proportion<- data.frame(perc=c(14,27,24,22,13), group=c("ZERO","ONE","TWO","THREE","FOUR"))
 tracts_proportion <- tracts_proportion %>%  mutate(pos = cumsum(perc)- perc/2)

 
 h<-ggplot(tracts_proportion, aes(x=factor(1), y=perc, fill=factor(group))) +
   geom_bar(stat="identity", width = 1, colour="white") +
   geom_text(aes(x= factor(1), y=pos, label = paste(group,"\n", factor(perc),"%", sep="")), size=9, colour="white", fontface="bold" ) +  
   coord_polar(theta = "y") +theme_classic() + 
   theme(axis.title=element_blank(), axis.ticks.x=element_blank(),axis.ticks=element_blank(), axis.text=element_blank(), panel.grid=element_blank(),
         legend.title=element_blank(),plot.title=element_blank(), legend.position="none")+
   scale_fill_manual(values=c("#00A7A0", "#91268F","#F05A28","#8CC63E","black"))
  
 
 ggsave("pie_chart.jpg",h, width=10,height=10,dpi=1000)

 ######################################## project type
 
 setwd("X:/Trans/STAFF/Reid/Project Selection/EJ Analysis/data")
 type.a<- read.xlsx("EJ data summaries.xlsx", sheetName = "Sheet1")
 type.a.tidy <- type.a %>%
   gather(measure, percent, -Improvement.Types, -Project.Count)
type.a.tidy$percent<-type.a.tidy$percent *100

colors<-c("#91268F","#F05A28","#8CC63E","#00A7A0", "black")

type.a.tidy$measure<-factor(type.a.tidy$measure, levels=c("ZERO","ONE", "TWO","THREE", "FOUR"))



type.x<-ggplot(type.a.tidy, aes(Improvement.Types, percent)) +  
  geom_rect(xmin=0.55, xmax=1.45, ymin=0, ymax=63,fill = alpha("black", 0.01) )+
 geom_rect(xmin=1.55, xmax=2.45, ymin=0, ymax=63,fill = alpha("black", 0.01) )+
 geom_rect(xmin=2.55, xmax=3.45, ymin=0, ymax=63,fill = alpha("black", 0.01) )+
  geom_rect(xmin=3.55, xmax=4.45, ymin=0, ymax=63,fill = alpha("black", 0.01) )+
  geom_rect(xmin=4.55, xmax=5.45, ymin=0, ymax=63,fill = alpha("black", 0.01) )+
  geom_rect(xmin=5.55, xmax=6.45, ymin=0, ymax=63,fill = alpha("black", 0.01) )+
  geom_rect(xmin=6.55, xmax=7.45, ymin=0, ymax=63,fill = alpha("black", 0.01) )+
  geom_rect(xmin=7.55, xmax=8.45, ymin=0, ymax=63,fill = alpha("black", 0.01) )+
  geom_bar(stat = "identity", aes(fill=measure),position="dodge") +theme_economist() +
  theme(legend.title=element_blank(), panel.background=element_rect(fill="white"), axis.title.y=element_blank(), axis.text.y=element_text(size=18),
        axis.text.x=element_text(size=22), axis.title.x=element_blank(), axis.ticks.x=element_blank(), plot.title=element_text(size=24),
        plot.background=element_rect(fill="white"),panel.grid.major.y=element_line(color="black"),legend.background=element_blank(),
        legend.text=element_text(size=18)) +
  scale_fill_manual(values=colors) +
  scale_y_continuous(breaks=c(0,20,40,60), labels=c("0%", "20%", "40%", "60%"))+
  scale_x_discrete(labels=c("Bike/\nPedestrian", "Highway\nCapacity","Multimodal\nCapacity","Other","Preservation","Safety/\nEfficiency",
        "Transit Capital &\nExpansion","Vehicles/\nEquipment"))+
  ggtitle("Measure Groups that Touch Projects (Percent of Total)")

ggsave(type.x,file="bar_measure_type1.jpg", width = 26, height=12, dpi=1000)


##############################

type.b<-type.a.tidy %>% filter (Improvement.Types %in% c("Bicycle/Pedestrian","Highway Capacity Improvement", "Multimodal Capacity","Other"))

type.x<-ggplot(type.b, aes(Improvement.Types, percent)) +  
  geom_rect(xmin=0.55, xmax=1.45, ymin=0, ymax=63,fill = alpha("black", 0.01) )+
  geom_rect(xmin=1.55, xmax=2.45, ymin=0, ymax=63,fill = alpha("black", 0.01) )+
  geom_rect(xmin=2.55, xmax=3.45, ymin=0, ymax=63,fill = alpha("black", 0.01) )+
  geom_rect(xmin=3.55, xmax=4.45, ymin=0, ymax=63,fill = alpha("black", 0.01) )+
  geom_bar(stat = "identity", aes(fill=measure),position="dodge") +theme_economist() +
  theme(legend.title=element_blank(), panel.background=element_rect(fill="white"), axis.title.y=element_blank(), axis.text.y=element_text(size=18),
        axis.text.x=element_text(size=22), axis.title.x=element_blank(), axis.ticks.x=element_blank(), plot.title=element_text(size=24),
        plot.background=element_rect(fill="white"),panel.grid.major.y=element_line(color="black"),legend.background=element_blank(),
        legend.text=element_text(size=18)) +
  scale_y_continuous(breaks=c(0,20,40,60), labels=c("0%", "20%", "40%", "60%"))+
  scale_x_discrete(labels=c("Bike/\nPedestrian", "Highway\nCapacity","Multimodal\nCapacity","Other"))+
  ggtitle("Measure Groups that Touch Projects (Percent of Total)")

ggsave(type.x,file="bar_measure_type1.jpg", width = 26, height=12, dpi=1000)

###############

type.c<-type.a.tidy %>% filter (Improvement.Types %in% c("Preservation","safety/Efficiency", "Transit Capital & Expansion","Vehicles/Equipment"))

type.x<-ggplot(type.a.tidy, aes(Improvement.Types, percent)) +  
  geom_rect(xmin=0.55, xmax=1.45, ymin=0, ymax=63,fill = alpha("black", 0.01) )+
  geom_rect(xmin=1.55, xmax=2.45, ymin=0, ymax=63,fill = alpha("black", 0.01) )+
  geom_rect(xmin=2.55, xmax=3.45, ymin=0, ymax=63,fill = alpha("black", 0.01) )+
  geom_rect(xmin=3.55, xmax=4.45, ymin=0, ymax=63,fill = alpha("black", 0.01) )+
  geom_rect(xmin=4.55, xmax=5.45, ymin=0, ymax=63,fill = alpha("black", 0.01) )+
  geom_rect(xmin=5.55, xmax=6.45, ymin=0, ymax=63,fill = alpha("black", 0.01) )+
  geom_rect(xmin=6.55, xmax=7.45, ymin=0, ymax=63,fill = alpha("black", 0.01) )+
  geom_rect(xmin=7.55, xmax=8.45, ymin=0, ymax=63,fill = alpha("black", 0.01) )+
  geom_bar(stat = "identity", aes(fill=measure),position="dodge") +theme_economist() +
  theme(legend.title=element_blank(), panel.background=element_rect(fill="white"), axis.title.y=element_blank(), axis.text.y=element_text(size=18),
        axis.text.x=element_text(size=22), axis.title.x=element_blank(), axis.ticks.x=element_blank(), plot.title=element_text(size=24),
        plot.background=element_rect(fill="white"),panel.grid.major.y=element_line(color="black"),legend.background=element_blank(),
        legend.text=element_text(size=18)) +
  scale_fill_manual(values=colors) +
  scale_y_continuous(breaks=c(0,20,40,60), labels=c("0%", "20%", "40%", "60%"))+
  scale_x_discrete(labels=c("Bike/\nPedestrian", "Highway\nCapacity","Multimodal\nCapacity","Other","Preservation","Safety/\nEfficiency",
                            "Transit Capital &\nExpansion","Vehicles/\nEquipment"))+
  ggtitle("Measure Groups that Touch Projects (Percent of Total)")

ggsave(type.x,file="bar_measure_type1.jpg", width = 26, height=12, dpi=1000)



############################
type.a.tidy$measure<-gsub("zero",0 ,type.a.tidy$measure)
type.a.tidy$measure<-gsub("one",1 ,type.a.tidy$measure)
type.a.tidy$measure<-gsub("two",2 ,type.a.tidy$measure)
type.a.tidy$measure<-gsub("three",3 ,type.a.tidy$measure)
type.a.tidy$measure<-gsub("four",4 ,type.a.tidy$measure)
type.a.tidy$measure<-as.numeric(type.a.tidy$measure)

colors<-c("#91268F","#F05A28","#8CC63E","#00A7A0", "black", "brown","#A4A4A4", "#D7DF01")

ggplot(type.a.tidy, aes(measure, percent, group=Improvement.Types, colour=Improvement.Types)) +
  geom_line(size=1.5) +theme_economist() +
  theme(legend.title=element_blank()) +scale_colour_manual(values=colors)
 
ggsave("measure_type_line.pdf",width=14,height=10)

ggplot(type.a.tidy, aes(measure, percent, group=Improvement.Types, colour=Improvement.Types)) +
  theme_economist() +
  theme(legend.title=element_blank()) +scale_colour_manual(values=colors) +geom_jitter(size=5)
  

################################
setwd("X:/Trans/STAFF/Reid/Project Selection/EJ Analysis/data")
type.a<- read.xlsx("EJ data summaries.xlsx", sheetName = "Sheet1")
type.a.tidy <- type.a %>%
  gather(measure, percent, -Improvement.Types, -Project.Count)
type.a.tidy$percent<-type.a.tidy$percent *100

colors<-c("#91268F","#F05A28","#8CC63E","#00A7A0", "black")
type.a.tidy$measure<-factor(type.a.tidy$measure, levels=c("ZERO","ONE", "TWO","THREE", "FOUR"))
type.b<-type.a.tidy %>% filter (Improvement.Types %in% c("Bicycle/Pedestrian","Highway Capacity Improvement", "Multimodal Capacity","Other"))

ggplot(type.b, aes(measure, percent)) +  
  geom_bar(stat = "identity", aes(fill=Improvement.Types)) + theme_economist()+
  theme(panel.background=element_blank(),plot.background=element_rect(fill = alpha("#A8DEDB")),legend.title=element_blank(), axis.title.y=element_blank(), axis.text.y=element_text(size=24),
        axis.text.x=element_blank(), axis.title.x=element_blank(),axis.ticks.x=element_blank(), plot.title=element_blank(), 
        legend.background=element_blank(), legend.position="top",
        legend.text=element_text(size=18),strip.background = element_rect( fill="black",size=5),strip.text.y = element_text(size=18, colour="white",face="bold")) +
  scale_y_continuous(breaks=c(0,20,40,60), labels=c("0%", "20%", "40%", "60%"))+
  facet_grid(Improvement.Types~.)+
  ggtitle("Measure Groups that Touch Projects (Percent of Total)")+
  scale_fill_manual(values=c("#91268F","#F05A28","#8CC63E","black"))+
  geom_text(aes(label=measure),vjust=-.5,colour="black",fontface="bold",size=7)+
  xlab("Number of Measures Above Threshold") +
  geom_rect(aes(xmin=0.5, xmax=6, ymin=62,ymax=65), fill="#A8DEDB")+
  geom_hline(aes(yintercept=-15),size=1) 


##################################

type.c<-type.a.tidy %>% filter (Improvement.Types %in% c("Preservation","safety/Efficiency", "Transit Capital & Expansion","Vehicles/Equipment"))
type.c$Improvement.Types<-as.character(type.c$Improvement.Types)
type.c$Improvement.Types[type.c$Improvement.Types == "safety/Efficiency"] <- "Safety/Efficiency"

type.xz<-ggplot(type.c, aes(measure, percent)) +  
  geom_bar(stat = "identity", aes(fill=Improvement.Types)) + theme_economist()+
  theme(panel.background=element_blank(),plot.background=element_rect(fill = alpha("#A8DEDB")),legend.title=element_blank(), axis.title.y=element_blank(), axis.text.y=element_text(size=24),
        axis.text.x=element_blank(), axis.title.x=element_blank(),axis.ticks.x=element_blank(), plot.title=element_blank(), 
        legend.background=element_blank(), legend.position="top",
        legend.text=element_text(size=18),strip.background = element_rect( fill="black",size=5),strip.text.y = element_text(size=18, colour="white",face="bold")) +
  scale_y_continuous(breaks=c(0,20,40,60), labels=c("0%", "20%", "40%", "60%"))+
  facet_grid(Improvement.Types~.)+
  ggtitle("Measure Groups that Touch Projects (Percent of Total)")+
  scale_fill_manual(values=c("#91268F","#F05A28","#8CC63E","black"))+
  geom_text(aes(label=measure),vjust=1.4,colour="white",fontface="bold",size=9)+
  xlab("Number of Measures Above Threshold") +
  geom_rect(aes(xmin=0.5, xmax=4, ymin=-9,ymax=-.5), fill="#A8DEDB")+
geom_hline(aes(yintercept=-15),size=1) 
ggsave(type.xz,file="bar_measure_type_facet4.jpg", width = 14, height=18, dpi=1000)

###########################################33

setwd("X:/Trans/STAFF/Reid/Project Selection/EJ Analysis/data")
data<-read.xlsx("bar_data.xlsx", sheetName = "data")
  data<-data %>%
    group_by(variable, touched) %>%
    mutate(countT= sum(tracts)) %>%
    mutate(per=paste0(round(100*tracts/countT,2),'%'))
data$touched<- toupper(data$touched)
data$touched<-gsub("ALL", "ALL TRACTS", data$touched)
data$touched<-gsub("TOUCHED", "TOUCHED TRACTS", data$touched)

ggplot(data, aes(factor(touched), tracts)) + geom_blank() + scale_y_continuous(limits=c(0,800), breaks=c(0,100,200,300,400,500,600,700,800)) + theme_economist()+
  theme(axis.line.x=element_blank(),
         axis.text.x=element_blank(),
         axis.ticks.x=element_blank(),
         axis.title.x=element_blank(),
         panel.grid.minor.x=element_blank(),
         panel.grid.major.x=element_blank())
  
count.1<-ggplot(data %>% filter(variable=="Min") , aes(factor(touched), tracts, fill=factor(ej))) +geom_bar(stat="identity",alpha=.8) +
  scale_fill_manual(values=c("#91268F","#F05A28")) + theme_economist() +
  geom_text(aes(1,100,label="NON EJ \n 59.9%"), colour="white",size=6) +
  geom_text(aes(1,670,label="EJ \n 40.1%"), colour="white",size=6) +
  geom_text(aes(2,100,label="NON EJ \n 54.35%"), colour="white",size=6) +
  geom_text(aes(2,300,label="EJ \n 45.65%"), colour="white",size=6) +
  theme(legend.position="none",axis.title.y=element_text(margin=margin(0,20,0,0), size=18), axis.title.x=element_blank(),axis.text.x=element_text(size=18), 
        axis.ticks.x=element_blank(),axis.text.y=element_text(size=18), plot.title=element_text(size=26))  +
  scale_y_continuous(limits=c(0,800), breaks=c(0,100,200,300,400,500,600,700,800)) +
  ggtitle("MINORITY") +ylab("NUMBER OF TRACTS") 

ggsave(count.1,file="minority_tracts.jpg", width = 6, height=8, dpi=1000)

count.2<-ggplot(data %>% filter(variable=="Pov") , aes(factor(touched), tracts, fill=factor(ej))) +geom_bar(stat="identity",alpha=.8, width=.8) +
  scale_fill_manual(values=c("#91268F","#F05A28")) + theme_economist() +
  geom_text(aes(1,100,label="NON EJ \n 61.06%"), colour="white",size=6) +
  geom_text(aes(1,670,label="EJ \n 38.94%"), colour="white",size=6) +
  geom_text(aes(2,100,label="NON EJ \n 55.53%"), colour="white",size=6) +
  geom_text(aes(2,270,label="EJ \n 44.47%"), colour="white",size=6) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.text.y=element_blank(),axis.title.x=element_blank(), axis.text.x=element_text(size=18),
        axis.ticks.x=element_blank(), plot.title=element_text(size=26))  +
  scale_y_continuous(limits=c(0,800), breaks=c(0,100,200,300,400,500,600,700,800)) +
  ggtitle("POVERTY") +ylab("NUMBER OF TRACTS")

ggsave(count.2,file="poverty_tracts.jpg", width = 6, height=8, dpi=1000)

count.3<-ggplot(data %>% filter(variable=="Dis") , aes(factor(touched), tracts, fill=factor(ej))) +geom_bar(stat="identity",alpha=.8, width=.8) +
  scale_fill_manual(values=c("#91268F","#F05A28")) + theme_economist() +
  geom_text(aes(1,100,label="NON EJ \n 56.27%" ), colour="white",size=6) +
  geom_text(aes(1,670,label="EJ \n 43.73%" ), colour="white",size=6) +
  geom_text(aes(2,100,label="NON EJ \n 54.35%"), colour="white",size=6) +
  geom_text(aes(2,270,label="EJ \n 45.65%"), colour="white",size=6) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.text.y=element_blank(),axis.title.x=element_blank(), 
        axis.text.x=element_text(size=18), axis.ticks.x=element_blank(), plot.title=element_text(size=26))  +
  scale_y_continuous(limits=c(0,800), breaks=c(0,100,200,300,400,500,600,700,800)) +
  ggtitle("DISABLED") +ylab("NUMBER OF TRACTS")

ggsave(count.3,file="senior_tracts.jpg", width = 6, height=8, dpi=1000)

count.4<-ggplot(data %>% filter(variable=="Sen") , aes(factor(touched), tracts, fill=factor(ej))) +geom_bar(stat="identity",alpha=.8, width=.8) +
  scale_fill_manual(values=c("#91268F","#F05A28")) + theme_economist() +
  geom_text(aes(1,100,label="NON EJ \n 44.63%"), colour="white",size=6) +
  geom_text(aes(1,670,label="EJ \n 55.37%"), colour="white",size=6) +
  geom_text(aes(2,100,label="NON EJ \n 77.88%"), colour="white",size=6) +
  geom_text(aes(2,370,label="EJ \n 22.12%"), colour="white",size=6) +
  theme(legend.position="none",axis.title.y=element_blank(),axis.text.y=element_blank(), axis.title.x=element_blank(),
        axis.text.x=element_text(size=18), axis.ticks.x=element_blank(), plot.title=element_text(size=26))  +
  scale_y_continuous(limits=c(0,800), breaks=c(0,100,200,300,400,500,600,700,800)) +
  ggtitle("SENIORS") +ylab("NUMBER OF TRACTS")

ggsave(count.4,file="disabled_tracts.jpg", width = 6, height=8, dpi=1000)

png("tracts.png", width=1800,height=880)
multiplot(count.1,count.2,count.3,count.4,cols=4)
dev.off()