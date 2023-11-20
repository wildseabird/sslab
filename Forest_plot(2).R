#setwd("/Users/nana/desktop/hcov共感染meta分析")
library(metafor)
library(readxl)
my_meta <- read_excel("D:/学习工作/工作/ALRI-meta-1120.xlsx")
View(my_meta)

a<-escalc(measure="OR", ai=ca, bi=cn, ci=sa, di=sn,  
          data = my_meta, add = 1/2, to = "only0", vtype = "LS", append = TRUE)
a
b<-rma(ai=ca,bi=cn,ci=sa,di=sn,data = a,measure = "OR")
b

mlabfun <- function(text, b) 
  list(bquote(paste(.(text),
                    I^2, " = ", .(formatC(b$I2, digits=1, format="f")),
                    "%, p ", .(metafor:::.pval(b$QEp, digits=3, showeq=TRUE, sep=" ")) ,")" 
  )))

forest(b, slab=paste(a$author, a$time, sep=" et al"),xlim = c(-18, 9),
       at=log(c(0.05, 0.25, 1, 4,42)), atransf=exp,alim = c(-4,6),
       ilab=cbind(a$ca, a$sa, a$cn, a$sn),
       ilab.xpos = c(-11.3, -8.9, -6.6, -4.2),order=num,mlab=mlabfun("Overall (", b),
       ylim = c(-1,24),
       cex = 1,showweights = T,annosym=c("(","-",")"))


op <- par(cex = 0.8, font = 2)
text(c(-11.3, -8.9, -6.6, -4.2), 22.75,c("Co-infection", "Mono-infection", "Co-infection", "Mono-infection"))
text(9, 23,"Odds Ratio(95% CI)", pos = 2)
text(5.95,23,"Weight ", pos = 2)
op <- par(cex = 0.85, font = 2)
text(c(-10, -5.4), 23.5,c("ALRI", "Non-ALRI"))
op <- par(cex = 0.9, font = 2)
text(-16.4, 23 ,"Study", pos = 2,cex = 1)



