
library(ggplot2)
## Make figures for precept 2

## Proper CDF choices
cdf1.data <- data.frame(
  x=seq(0,10,10/4),
  y1=c(0,seq(0,.9,.3)),
  y2=c(0,seq(.3,.9,.3),1)
)
ggplot(cdf1.data, aes(x=x)) + 
  geom_point(aes(y=y1),size=5,shape=1) +
  geom_point(aes(y=y2),size=5) +
  geom_segment(x=cdf1.data$x,xend=cdf1.data$x+10/4,
               y=cdf1.data$y2,yend=cdf1.data$y2) +
  scale_x_continuous(limits=c(.5,10),name="\nX") +
  scale_y_continuous(name="CDF\n") +
  ggtitle("Choice A") +
  theme(text=element_text(size=16)) +
  ggsave("/Users/ilundberg/Dropbox/My precepts/CDF1.pdf",
         height=4,width=6)
ggplot(cdf1.data, aes(x=x)) + 
  geom_point(aes(y=y1),size=5) +
  geom_point(aes(y=y2),size=5,shape=1) +
  geom_segment(x=cdf1.data$x,xend=cdf1.data$x+10/4,
               y=cdf1.data$y2,yend=cdf1.data$y2) +
  scale_x_continuous(limits=c(.5,10),name="\nX") +
  scale_y_continuous(name="CDF\n") +
  ggtitle("Choice B") +
  theme(text=element_text(size=16)) +
  ggsave("/Users/ilundberg/Dropbox/My precepts/CDF2.pdf",
         height=4,width=6)
ggplot(mutate(data.frame(x=0:10),y=(-(x/5)^3+x^2+4*x))/132,
       aes(x=x,y=y)) + 
  geom_line() +
  scale_x_continuous(name="\nX") +
  scale_y_continuous(name="CDF\n") +
  ggtitle("Choice C") +
  theme(text=element_text(size=16)) +
  ggsave("/Users/ilundberg/Dropbox/My precepts/CDF3.pdf",
         height=4,width=6)
ggplot(mutate(data.frame(x=0:10),y=.05+x/10-1/100*(x-1)^2),
       aes(x=x,y=y)) + 
  geom_line() +
  scale_x_continuous(name="\nX") +
  scale_y_continuous(name="CDF\n") +
  ggtitle("Choice D") +
  theme(text=element_text(size=16)) +
  ggsave("/Users/ilundberg/Dropbox/My precepts/CDF4.pdf",
         height=4,width=6)

  
  #x=c(0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,10),
  #y=c(0,0,.1,.1,.2,.2,.3,.3,.4,.4,.5,.5,.6,.6,.7,.7,.9,.9,1)

## Uniform plot
forplot <- mutate(
  data.frame(x=seq(.001,.999,.001)),
  y=1,
  ymin=0,
  ymax1=ifelse(x<(.25), 1, 0),
  ymax2=ifelse(x<(.75), 1, 0),
  ymax3=ifelse(x<(1), 1, 0)
)
ggplot(forplot,aes(x=x,y=y)) +
  geom_line() +
  scale_x_continuous(name = "\nX") +
  scale_y_continuous(name="Density (PDF)\n",limits=c(0,1.2)) +
  theme(text=element_text(size=16),
        legend.position="none") +
  geom_segment(x=0,xend=0,y=0,yend=1) +
  geom_segment(x=1,xend=1,y=0,yend=1) +
  ggsave("/Users/ilundberg/Dropbox/My precepts/Unif1a.pdf",
         height=3,width=6)
ggplot(forplot,aes(x=x,y=x)) +
  geom_line() +
  scale_x_continuous(name = "\nX") +
  scale_y_continuous(name="Cumulative density (CDF)\n",
                     limits=c(0,1.2)) +
  geom_point(aes(x=0,y=0),size=6) +
  theme(text=element_text(size=16),
        legend.position="none") +
  ggsave("/Users/ilundberg/Dropbox/My precepts/Unif1b.pdf",
         height=3,width=6)
ggplot(forplot,aes(x=x,y=y)) +
  geom_line() +
  scale_x_continuous(name = "\nX") +
  scale_y_continuous(name="Density (PDF)\n",limits=c(0,1.2)) +
  theme(text=element_text(size=16),
        legend.position="none") +
  geom_segment(x=0,xend=0,y=0,yend=1) +
  geom_segment(x=1,xend=1,y=0,yend=1) +
  geom_ribbon(aes(ymin=ymin,ymax=ymax1,alpha=.1)) +
  ggsave("/Users/ilundberg/Dropbox/My precepts/Unif2a.pdf",
         height=3,width=6)
ggplot(forplot,aes(x=x,y=x)) +
  geom_line() +
  scale_x_continuous(name = "\nX") +
  scale_y_continuous(name="Cumulative density (CDF)\n",
                     limits=c(0,1.2)) +
  geom_point(aes(x=.25,y=.25),size=6) +
  theme(text=element_text(size=16),
        legend.position="none") +
  ggsave("/Users/ilundberg/Dropbox/My precepts/Unif2b.pdf",
         height=3,width=6)
ggplot(forplot,aes(x=x,y=y)) +
  geom_line() +
  scale_x_continuous(name = "\nX") +
  scale_y_continuous(name="Density (PDF)\n",limits=c(0,1.2)) +
  theme(text=element_text(size=16),
        legend.position="none") +
  geom_segment(x=0,xend=0,y=0,yend=1) +
  geom_segment(x=1,xend=1,y=0,yend=1) +
  geom_ribbon(aes(ymin=ymin,ymax=ymax2,alpha=.1)) +
  ggsave("/Users/ilundberg/Dropbox/My precepts/Unif3a.pdf",
         height=3,width=6)
ggplot(forplot,aes(x=x,y=x)) +
  geom_line() +
  scale_x_continuous(name = "\nX") +
  scale_y_continuous(name="Cumulative density (CDF)\n",
                     limits=c(0,1.2)) +
  geom_point(aes(x=.75,y=.75),size=6) +
  theme(text=element_text(size=16),
        legend.position="none") +
  ggsave("/Users/ilundberg/Dropbox/My precepts/Unif3b.pdf",
         height=3,width=6)
ggplot(forplot,aes(x=x,y=y)) +
  geom_line() +
  scale_x_continuous(name = "\nX") +
  scale_y_continuous(name="Density (PDF)\n",limits=c(0,1.2)) +
  theme(text=element_text(size=16),
        legend.position="none") +
  geom_segment(x=0,xend=0,y=0,yend=1) +
  geom_segment(x=1,xend=1,y=0,yend=1) +
  geom_ribbon(aes(ymin=ymin,ymax=ymax3,alpha=.1)) +
  ggsave("/Users/ilundberg/Dropbox/My precepts/Unif4a.pdf",
         height=3,width=6)
ggplot(forplot,aes(x=x,y=x)) +
  geom_line() +
  scale_x_continuous(name = "\nX") +
  scale_y_continuous(name="Cumulative density (CDF)\n",
                     limits=c(0,1.2)) +
  geom_point(aes(x=1,y=1),size=6) +
  theme(text=element_text(size=16),
        legend.position="none") +
  ggsave("/Users/ilundberg/Dropbox/My precepts/Unif4b.pdf",
         height=3,width=6)

ggplot(forplot,aes(x=x,y=y)) +
  geom_line() +
  scale_x_continuous(name = "\nX") +
  scale_y_continuous(name="Density (PDF)\n",limits=c(0,1.2)) +
  theme(text=element_text(size=16),
        legend.position="none") +
  geom_segment(x=0,xend=0,y=0,yend=1) +
  geom_segment(x=1,xend=1,y=0,yend=1) +
  geom_ribbon(aes(ymin=ymin,ymax=ifelse(x<.25 | x>.5,0,1),
                  alpha=.1)) +
  ggsave("/Users/ilundberg/Dropbox/My precepts/Unif5a.pdf",
         height=3,width=4)
ggplot(forplot,aes(x=x,y=x)) +
  geom_line() +
  scale_x_continuous(name = "\nX") +
  scale_y_continuous(name="Cumulative density (CDF)\n",
                     limits=c(0,1.2)) +
  geom_point(aes(x=.25,y=.25),size=6) +
  geom_point(aes(x=.5,y=.5),size=6) +
  theme(text=element_text(size=16),
        legend.position="none") +
  ggsave("/Users/ilundberg/Dropbox/My precepts/Unif5b.pdf",
         height=3,width=4)



## Normal plots
forplot <- mutate(
  data.frame(p=seq(.001,.999,.001)),
  x=qnorm(p,sd=6),
  y=dnorm(x,sd=6),
  ymin=0,
  ymax1=ifelse(x<(-6) | x>6, 0, y),
  ymax2=ifelse(x<(-1) | x>1, 0, y),
  ymax3=ifelse(x<(-.01) | x>.01, 0, y)
)
ggplot(forplot, aes(x=x,y=y)) +
  geom_line() + 
  scale_x_continuous(name = "\nDistance from center of wall (inches)") +
  scale_y_continuous(name="Density\n") +
  theme(text=element_text(size=16),
        legend.position="none") +
  ggsave("/Users/ilundberg/Dropbox/My precepts/Normal0.pdf",
         height=3,width=6)
ggplot(forplot, aes(x=x,y=y)) +
  geom_line() + 
  scale_x_continuous(name = "\nDistance from center of wall (inches)") +
  scale_y_continuous(name="Density\n") +
  theme(text=element_text(size=16),
        legend.position="none") +
  geom_ribbon(aes(ymin=ymin,ymax=ymax1,alpha=.1)) +
  ggsave("/Users/ilundberg/Dropbox/My precepts/Normal1.pdf",
         height=3,width=6)
ggplot(forplot, aes(x=x,y=y)) +
  geom_line() + 
  scale_x_continuous(name = "\nDistance from center of wall (inches)") +
  scale_y_continuous(name="Density\n") +
  theme(text=element_text(size=16),
        legend.position="none") +
  geom_segment(aes(x=-7,xend=-2,y=.02,yend=.02),
               arrow=arrow(length=unit(.3,"cm"))) +
  geom_segment(aes(x=7,xend=2,y=.02,yend=.02),
               arrow=arrow(length=unit(.3,"cm"))) +
  geom_ribbon(aes(ymin=ymin,ymax=ymax2,alpha=.1)) +
  ggsave("/Users/ilundberg/Dropbox/My precepts/Normal2.pdf",
         height=3,width=6)
ggplot(forplot, aes(x=x,y=y)) +
  geom_line() + 
  scale_x_continuous(name = "\nDistance from center of wall (inches)") +
  scale_y_continuous(name="Density\n") +
  theme(text=element_text(size=16),
        legend.position="none") +
  geom_segment(aes(x=-6,xend=-1,y=.02,yend=.02),
               arrow=arrow(length=unit(.3,"cm"))) +
  geom_segment(aes(x=6,xend=1,y=.02,yend=.02),
               arrow=arrow(length=unit(.3,"cm"))) +
  geom_ribbon(aes(ymin=ymin,ymax=ymax3,alpha=.1)) +
  ggsave("/Users/ilundberg/Dropbox/My precepts/Normal3.pdf",
         height=3,width=6)
ggplot(forplot, aes(x=x,y=y)) +
  geom_line() + 
  scale_x_continuous(name = "\nDistance from center of wall (inches)") +
  scale_y_continuous(name="Density\n") +
  theme(text=element_text(size=16),
        legend.position="none") +
  geom_segment(aes(x=-5,xend=0,y=.02,yend=.02),
               arrow=arrow(length=unit(.3,"cm"))) +
  geom_segment(aes(x=5,xend=0,y=.02,yend=.02),
               arrow=arrow(length=unit(.3,"cm"))) +
  ggsave("/Users/ilundberg/Dropbox/My precepts/Normal4.pdf",
         height=3,width=6)

ggplot(mutate(data.frame(p=seq(.001,.999,.01)),
              x=exp(qnorm(p)),
              y=dnorm(qnorm(p))),
       aes(x=x,y=y)) +
  geom_line() + 
  scale_x_continuous(name = "\nWealth",labels=NULL) +
  scale_y_continuous(name="Density\n",labels=NULL) +
  theme_bw() +
  theme(text=element_text(size=16)) +
  ggsave("/Users/ilundberg/Dropbox/My precepts/WealthDist.pdf",
         height=3,width=6)

library(ggExtra)
library(mvtnorm)
set.seed(08544)
forplot <- transmute(
  data.frame(
    rmvnorm(1000,c(0,0),cbind(c(.2,.2),c(.2,1)))
  ),
  X=exp(X1),
  Y=X2
)
p <- ggplot(forplot, aes(x=X, y=Y)) + 
  geom_point() +
  scale_x_continuous(labels=NULL) +
  scale_y_continuous(labels=NULL) +
  theme(text=element_text(size=20))
ggMarginal(p)
## Save that as Marginal_dists.pdf

ggplot(data.frame(x=c(0,1),y=c(0,1)),
       aes(x=x,y=y)) +
  geom_rect(aes(xmin=0,xmax=.5,ymin=0,ymax=.25,alpha=.1)) +
  scale_x_continuous(name="\nX") +
  scale_y_continuous(name="Y\n") +
  ggtitle("F(.5, .25) = P(X<.5, Y<.25)\n") +
  theme(text=element_text(size=20),
        legend.position="none") +
  ggsave("/Users/ilundberg/Dropbox/My precepts/Joint_CDF.pdf",
         width=5,height=5)
  
marathon.times <- cbind(c(.05,.1,.15),
                        c(.2,.25,.1),
                        c(.15,.05,0))
colnames(marathon.times) <- rownames(marathon.times) <- c(1.5,2,2.5)
xtable(marathon.times,caption="Distribution of marathon times")


## Die PMF plot
ggplot(data = data.frame(x = 1:6,
                         y = rep(1/6, 6),
                         yend = rep(0,6)),
       aes(x = x, y = y, xend = x, yend = yend)) +
  geom_point() +
  geom_segment() +
  scale_x_continuous(name="\nNumber on die", breaks=1:6) +
  scale_y_continuous(name="PMF p(x)\n") +
  ggtitle("PMF for rolling a die") +
  theme(text=element_text(size=20)) +
  ggsave("DiePMF.pdf",
         height=4,
         width=4)

## Die CDF plot
ggplot(data = data.frame(x = 1:6,
                         y = (1:6)/6,
                         xend = c(2:6,6))) +
  geom_point(aes(x = x, y = y), size = 4) +
  geom_segment(aes(x = x, xend = xend, y = y, yend = y)) +
  geom_point(aes(x = xend, y = y), shape = 1, size = 4) +
  scale_x_continuous(name="\nNumber on die",
                     breaks=1:6) +
  scale_y_continuous(name="CMF F(x)\n") +
  ggtitle("CDF for rolling a die") +
  theme(text=element_text(size=20)) +
  ggsave("DieCDF.pdf",
         height=4,
         width=4)

## Universality of the uniform
p <- ggplot(mutate(data.frame(p=seq(.001,.999,.001)),
                   z=qnorm(p))) +
  geom_line(aes(y=p, x=z)) +
  scale_x_continuous(name=bquote('\nZ ~ '*F^{-1}*'(p) ~ Normal(0,1)')) + 
  scale_y_continuous(name="p ~ F(Z) ~ Uniform(0,1)\n") +
  geom_segment(aes(x=1.96,xend=1.96,y=0,yend=.975),
               size=.5, linetype=2) +
  geom_segment(aes(x=qnorm(.001),xend=1.96,y=.975,yend=.975),
               size=.5, linetype=2) +
  theme(text=element_text(size=16)) +
  annotate("text",label="F(1.96) = .975",x=-2,y=.9,size=3) +
  annotate("text",label="F^{-1}*'(.975) = 1.96'",
           x=.4,y=.1,size=3,parse=T)
pdf(file="/Users/ilundberg/Dropbox/My precepts/UofU.pdf",
    height = 4, width = 5)
ggMarginal(p,type="histogram",bins=30)
dev.off()




draw.rnorm <- function(n, mean = 0, sd = 1) {
  u <- runif(n)
  z <- qnorm(u)
  return(z)
}
qplot(x=draw.rnorm(1000), geom="histogram",
      bins = 30, xlim = c(-3,3))
qplot(x=rnorm(1000), geom="histogram",
      bins = 30, xlim=c(-3,3))


## Dice sum and difference
draw.sum.diff <- function() {
  x <- sample(1:6,1)
  y <- sample(1:6,1)
  return(c(x+y,x-y))
}
samples <- matrix(nrow=5000,ncol=2)
colnames(samples) <- c("x","y")
set.seed(08544)
for (i in 1:5000) {
  samples[i,] <- draw.sum.diff()
}
ggplot(data.frame(samples), aes(x=x,y=y)) +
  geom_point() +
  scale_x_continuous(breaks=c(2:12),
                     name="\nSum of dice") +
  scale_y_continuous(breaks=c(-5:5),
                     name="Difference of dice\n") +
  ggtitle("Sum and difference\nof two dice") +
  theme(text=element_text(size=20)) +
  ggsave("SumDiff.pdf",
         height=4, width=5)




