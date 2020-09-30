

SP<-rnorm(100,0,3)
xb<-0+1.5*SP
p<-1/(1+exp(-xb))
y <- rbinom(n = 100, size = 1, prob = p)

data<-data.frame(COF=y,SelPres=SP)

p<-ggplot(data,aes(x=SelPres,y=COF))+
  geom_point(shape=21,size=1.5,stroke=1.5,fill="#a6bddb",
             color="#0570b0",alpha=0.75)+
  geom_smooth(se=F,color="black",size=2)+
  theme_classic()+
  scale_y_continuous(breaks=c(0,1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text( size=14, color="black",face="bold"),
        axis.title.x = element_text( size=12),
        axis.title.y = element_text( size=12),
        axis.text=(element_text(color="black", size=12)),
        legend.title = element_text(colour="black", size=12),
        legend.text = element_text( size = 12),
        legend.position = "none")+
  xlab("Calculated Selection Pressure for \n Costly Conservation Adoption")+
  ylab("Protected Area Observed \n(Y/N)")+ggtitle("Simulated Selection Pressure vs.\nProtected Area Adoption")

png(filename = "DissertationProposal/Figures/Ch3/SelPressureAdopt.png", pointsize=10, width=900, height=480, res=200)
p
dev.off()


data<-data.frame(CF=sort(rbeta(100,1,2)),WC=sort(rpois(100,7)),GD=rpois(100,5))

CSP1<-ggplot(data,aes(x=WC,y=CF))+
  geom_point(shape=21,size=1.5,stroke=1.5,fill="#a6bddb",
             color="#0570b0",alpha=0.75)+
  geom_smooth(se=F,color="black",size=2,method="lm")+
  theme_classic()+
  scale_y_continuous(label=comma)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text( size=14, color="black",face="bold"),
        axis.title.x = element_text( size=12),
        axis.title.y = element_text( size=12),
        axis.text=(element_text(color="black", size=12)),
        legend.title = element_text(colour="black", size=12),
        legend.text = element_text( size = 12),
        legend.position = "none")+
  xlab("Weekly Communication Between \n Original Stove Builder and Copier")+
  ylab("Copying Fidelity")+ggtitle("Simulated Copying Fidelity vs.\n Weekly Communication")

png(filename = "DissertationProposal/Figures/Ch3/CSP1.png", pointsize=10, width=900, height=480, res=200)
CSP1
dev.off()

CSP2<-ggplot(data,aes(x=WC,y=GD))+
  geom_jitter(shape=21,size=1.5,stroke=1.5,fill="#a6bddb",
              color="#0570b0",alpha=0.75)+
  geom_smooth(se=F,color="black",size=2,method="lm")+
  theme_classic()+
  scale_y_continuous(label=comma)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text( size=14, color="black",face="bold"),
        axis.title.x = element_text( size=12),
        axis.title.y = element_text( size=12),
        axis.text=(element_text(color="black", size=12)),
        legend.title = element_text(colour="black", size=12),
        legend.text = element_text( size = 12),
        legend.position = "none")+
  xlab("Geographic Distance Between \n Original Stove Builder and Copier")+
  ylab("Copying Fidelity")+ggtitle("Simulated Copying Fidelity vs.\n Geographic Distance")

png(filename = "DissertationProposal/Figures/Ch3/CSP2.png", pointsize=10, width=900, height=480, res=200)
CSP2
dev.off()




