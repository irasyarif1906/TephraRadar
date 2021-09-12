list.of.packages <- c("truncdist", "ggplot2", "plyr", "dplyr", "gridExtra", "naivebayes", "psych")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(truncdist)
library(ggplot2)
library(plyr)
library(dplyr)
library(gridExtra)
library(naivebayes)
library(psych)

set.seed(234)
D1 = rtrunc(30000, spec="gamma", a=0.007,b=0.015, 1, 100)
set.seed(123)
D2 = rtrunc(30000, spec="gamma", a=0.015,b=0.35, 1, 10)
set.seed(122)
D3 = rtrunc(30000, spec="gamma", a=0.35,b=6.0, 1, 1.52)

mysamp <- function(n, m, s, lwr, upr, rounding) {
  samp <- round(rnorm(n, m, s), rounding)
  samp[samp < lwr] <- lwr
  samp[samp > upr] <- upr
  samp
}

set.seed(121)
C1 = mysamp(n=30000, m=0.1, s=0.15, lwr=0.01, upr=0.5, rounding=3)
set.seed(125)
C2 = mysamp(n=30000, m=1, s=0.5, lwr=0.5, upr=2.0, rounding=3)
set.seed(126)
C3 = mysamp(n=30000, m=5, s=1, lwr=2, upr=8.0, rounding=3)

#C1 = 0.1
#C2 = 1
#C3 = 5



#fine ash
fa1 = 6*C1*0.01/(pi*10^6*factorial(4))
fa1 = fa1*(factorial(2)/0.01)^5
b_fa = D1/0.01
c_fa = exp(-2*(D1/0.01))
Nd_fa1= fa1*b_fa*c_fa*10^9
fa2 = 6*C2*0.01/(pi*10^6*factorial(4))
fa2 = fa2*(factorial(2)/0.01)^5
Nd_fa2= fa2*b_fa*c_fa*10^9
fa3 = 6*C3*0.01/(pi*10^6*factorial(4))
fa3 = fa3*(factorial(2)/0.01)^5
Nd_fa3= fa3*b_fa*c_fa*10^9

#coarse ash
ca1 = 6*C1*0.1/(pi*10^6*factorial(4))
ca1 = ca1*(factorial(2)/0.1)^5
b_ca = D2/0.1
c_ca = exp(-2*(D2/0.1))
Nd_ca1= ca1*b_ca*c_ca*10^9
ca2 = 6*C2*0.1/(pi*10^6*factorial(4))
ca2 = ca2*(factorial(2)/0.1)^5
Nd_ca2= ca2*b_ca*c_ca*10^9
ca3 = 6*C3*0.1/(pi*10^6*factorial(4))
ca3 = ca3*(factorial(2)/0.1)^5
Nd_ca3= ca3*b_ca*c_ca*10^9


#lapilli
la1 = 6*C1*1/(pi*10^6*factorial(4))
la1 = la1*(factorial(2)/1)^5
b_la = D3/1
c_la = exp(-2*(D3/1))
Nd_la1= la1*b_la*c_la*10^9
la2 = 6*C2*1/(pi*10^6*factorial(4))
la2 = la2*(factorial(2)/1)^5
Nd_la2= la2*b_la*c_la*10^9
la3 = 6*C3*1/(pi*10^6*factorial(4))
la3 = la3*(factorial(2)/1)^5
Nd_la3= la3*b_la*c_la*10^9

#calculate ND
log_NDfa1 = log10(Nd_fa1)
log_NDfa2 = log10(Nd_fa2)
log_NDfa3 = log10(Nd_fa3)
log_NDca1 = log10(Nd_ca1)
log_NDca2 = log10(Nd_ca2)
log_NDca3 = log10(Nd_ca3)
log_NDla1 = log10(Nd_la1)
log_NDla2 = log10(Nd_la2)
log_NDla3 = log10(Nd_la3)

data.frame(D1, Nd_fa1, Nd_fa2, Nd_fa3, log_NDfa1,log_NDfa2,log_NDfa3) -> fa
data.frame(D2, Nd_ca1, Nd_ca2, Nd_ca3, log_NDca1,log_NDca2,log_NDca3) -> ca
data.frame(D3, Nd_la1, Nd_la2, Nd_la3, log_NDla1,log_NDla2,log_NDla3) -> la

p1 <- ggplot(fa, aes(D1, log_NDfa1)) +
  geom_point(cex=1, alpha=0.4) + 
  geom_smooth(method = "loess", color="red") +
  scale_x_continuous(limits=c(0,0.20), expand=c(0,0), breaks = seq(0, 0.20, by = 0.05)) +
  #scale_y_continuous(limits=c(-4,8), expand=c(0,0), breaks = seq(-4, 8, by = 4)) +
  theme_linedraw() +
  ggtitle("Dn = 0.01 mm, Ca= 0.1 g/m3")
p2 <- ggplot(fa, aes(D1, log_NDfa2)) +
  geom_point(cex=1, alpha=0.4) + 
  geom_smooth(method = "loess", color="red") +
  scale_x_continuous(limits=c(0,0.15), expand=c(0,0), breaks = seq(0, 0.20, by = 0.05)) +
  #scale_y_continuous(limits=c(-4,8), expand=c(0,0), breaks = seq(-4, 8, by = 4)) +
  theme_linedraw() +
  ggtitle("Dn = 0.01 mm, Ca= 1 g/m3")
p3 <- ggplot(fa, aes(D1, log_NDfa3)) +
  geom_point(cex=1, alpha=0.4) + 
  geom_smooth(method = "loess", color="red") +
  scale_x_continuous(limits=c(0,0.15), expand=c(0,0), breaks = seq(0, 0.15, by = 0.05)) +
  #scale_y_continuous(limits=c(-4,8), expand=c(0,0), breaks = seq(-4, 8, by = 4)) +
  theme_linedraw() +
  ggtitle("Dn = 0.01 mm, Ca= 5 g/m3")


p4 <- ggplot(ca, aes(D2, log_NDca1)) +
  geom_point(cex=1, alpha=0.4) + 
  geom_smooth(method = "loess", color="red") +
  scale_x_continuous(limits=c(0,0.35), expand=c(0,0), breaks = seq(0, 0.35, by = 0.05)) +
  #scale_y_continuous(limits=c(-4,8), expand=c(0,0), breaks = seq(-4, 8, by = 4)) +
  theme_linedraw() +
  ggtitle("Dn = 0.1 mm, Ca= 0.1 g/m3")

p5 <- ggplot(ca, aes(D2, log_NDca2)) +
  geom_point(cex=1, alpha=0.4) + 
  geom_smooth(method = "loess", color="red") +
  scale_x_continuous(limits=c(0,0.35), expand=c(0,0), breaks = seq(0, 0.35, by = 0.05)) +
  #scale_y_continuous(limits=c(-4,8), expand=c(0,0), breaks = seq(-4, 8, by = 4)) +
  theme_linedraw() +
  ggtitle("Dn = 0.1 mm, Ca= 1 g/m3")
p6 <- ggplot(ca, aes(D2, log_NDca3)) +
  geom_point(cex=1, alpha=0.4) + 
  geom_smooth(method = "loess", color="red") +
  scale_x_continuous(limits=c(0,0.35), expand=c(0,0), breaks = seq(0, 0.35, by = 0.05)) +
  #scale_y_continuous(limits=c(-4,8), expand=c(0,0), breaks = seq(-4, 8, by = 4)) +
  theme_linedraw() +
  ggtitle("Dn = 0.1 mm, Ca= 5 g/m3")

p7 <- ggplot(la, aes(D3, log_NDla1)) +
  geom_point(cex=1, alpha=0.4) + 
  geom_smooth(method = "loess", color="red") +
  scale_x_continuous(limits=c(0,6), expand=c(0,0), breaks = seq(0, 6, by = 1)) +
  #scale_y_continuous(limits=c(-4,8), expand=c(0,0), breaks = seq(-4, 8, by = 4)) +
  theme_linedraw() +
  ggtitle("Dn = 1 mm, Ca= 0.1 g/m3")
p8 <- ggplot(la, aes(D3, log_NDla2)) +
  geom_point(cex=1, alpha=0.4) + 
  geom_smooth(method = "loess", color="red") +
  scale_x_continuous(limits=c(0,6), expand=c(0,0), breaks = seq(0, 6, by = 1)) +
  #scale_y_continuous(limits=c(-4,8), expand=c(0,0), breaks = seq(-4, 8, by = 4)) +
  theme_linedraw() +
  ggtitle("Dn = 1 mm, Ca= 1 g/m3")
p9 <- ggplot(la, aes(D3, log_NDla2)) +
  geom_point(cex=1, alpha=0.4) + 
  geom_smooth(method = "loess", color="red") +
  scale_x_continuous(limits=c(0,6), expand=c(0,0), breaks = seq(0, 6, by = 1)) +
  #scale_y_continuous(limits=c(-4,8), expand=c(0,0), breaks = seq(-4, 8, by = 4)) +
  theme_linedraw() +
  ggtitle("Dn = 1 mm, Ca= 5 g/m3")

ggsave(
  filename = "plot_D_logND.pdf", 
  plot = marrangeGrob(list(p1,p2,p3, p4,p5,p6,p7,p8,p9), nrow=3, ncol=3), 
  width = 15, height = 9)

# write.table(fa, "GSDfa.csv", col.names=T, row.names=T, sep=",")
# write.table(ca, "GSDca.csv", col.names=T, row.names=T, sep=",")
# write.table(la, "GSDla.csv", col.names=T, row.names=T, sep=",")

ZH_fa1 = (Nd_fa1*10/2^8)*(0.01^7)*factorial(7)
dbz_fa1 = 10*log10(ZH_fa1)
ZH_fa2 = (Nd_fa2*10/2^8)*(0.01^7)*factorial(7)
dbz_fa2 = 10*log10(ZH_fa2)
ZH_fa3 = (Nd_fa3*10/2^8)*(0.01^7)*factorial(7)
dbz_fa3 = 10*log10(ZH_fa3)
ZH_ca1 = (Nd_ca1*10/2^8)*(0.1^7)*factorial(7)
dbz_ca1 = 10*log10(ZH_ca1)
ZH_ca2 = (Nd_ca2*10/2^8)*(0.1^7)*factorial(7)
dbz_ca2 = 10*log10(ZH_ca2)
ZH_ca3 = (Nd_ca3*10/2^8)*(0.1^7)*factorial(7)
dbz_ca3 = 10*log10(ZH_ca3)
ZH_la1 = (Nd_la1*10/2^8)*(1^7)*factorial(7)
dbz_la1 = 10*log10(ZH_la1)
ZH_la2 =(Nd_la2*10/2^8)*(1^7)*factorial(7)
dbz_la2 = 10*log10(ZH_la2)
ZH_la3 = (Nd_la3*10/2^8)*(1^7)*factorial(7)
dbz_la3 = 10*log10(ZH_la3)

Ca_fa1 = ((Nd_fa1*10^-8)/2^5)*(0.1^4)*factorial(4)*(pi/6)*(10^6)
Ca_fa2 = ((Nd_fa2*10^-8)/2^5)*(0.1^4)*factorial(4)*(pi/6)*(10^6)
Ca_fa3 = ((Nd_fa3*10^-8)/2^5)*(0.1^4)*factorial(4)*(pi/6)*(10^6)
Ca_ca1 = ((Nd_ca1*10^-8)/2^5)*(0.1^4)*factorial(4)*(pi/6)*(10^6)
Ca_ca2 = ((Nd_ca2*10^-8)/2^5)*(0.1^4)*factorial(4)*(pi/6)*(10^6)
Ca_ca3 = ((Nd_ca3*10^-8)/2^5)*(0.1^4)*factorial(4)*(pi/6)*(10^6)
Ca_la1 = ((Nd_la1*10^-8)/2^5)*(1^4)*factorial(4)*(pi/6)*(10^6)
Ca_la2 = ((Nd_la2*10^-8)/2^5)*(1^4)*factorial(4)*(pi/6)*(10^6)
Ca_la3 = ((Nd_la3*10^-8)/2^5)*(1^4)*factorial(4)*(pi/6)*(10^6)

Ra_fa1 = (((pi/6)*(10^6)*7.468*(Nd_fa1*10^-8)/2^5)*(0.1^4)*factorial(4)+1)*3.6*(10^-3)
Ra_fa2 = (((pi/6)*(10^6)*7.468*(Nd_fa2*10^-8)/2^5)*(0.1^4)*factorial(4)+1)*3.6*(10^-3)
Ra_fa3 = (((pi/6)*(10^6)*7.468*(Nd_fa3*10^-8)/2^5)*(0.1^4)*factorial(4)+1)*3.6*(10^-3)
Ra_ca1 = (((pi/6)*(10^6)*7.468*(Nd_ca1*10^-8)/2^5)*(0.1^4)*factorial(4)+1)*3.6*(10^-3)
Ra_ca2 = (((pi/6)*(10^6)*7.468*(Nd_ca2*10^-8)/2^5)*(0.1^4)*factorial(4)+1)*3.6*(10^-3)
Ra_ca3 = (((pi/6)*(10^6)*7.468*(Nd_ca3*10^-8)/2^5)*(0.1^4)*factorial(4)+1)*3.6*(10^-3)
Ra_la1 = (((pi/6)*(10^6)*7.468*(Nd_la1*10^-8)/2^5)*(1^4)*factorial(4)+1)*3.6*(10^-3)
Ra_la2 = (((pi/6)*(10^6)*7.468*(Nd_la2*10^-8)/2^5)*(1^4)*factorial(4)+1)*3.6*(10^-3)
Ra_la3 = (((pi/6)*(10^6)*7.468*(Nd_la3*10^-8)/2^5)*(1^4)*factorial(4)+1)*3.6*(10^-3)

Noisify <- function(data) {
  
  if (is.vector(data)) {
    noise <- runif(length(data), -1.4, 1.4)
    noisified <- data + noise
  } else {
    length <- dim(data)[1] * dim(data)[2]
    noise <- matrix(runif(length, -1.4, 1.4), dim(data)[1])
    noisified <- data + noise
  }
  return(noisified)
}
Zhm_fa1 = Noisify(dbz_fa1)
Zhm_fa2 = Noisify(dbz_fa2)
Zhm_fa3 = Noisify(dbz_fa3)
Zhm_ca1 = Noisify(dbz_ca1)
Zhm_ca2 = Noisify(dbz_ca2)
Zhm_ca3 = Noisify(dbz_ca3)
Zhm_la1 = Noisify(dbz_la1)
Zhm_la2 = Noisify(dbz_la2)
Zhm_la3 = Noisify(dbz_la3)

data.frame(fa, zh=Zhm_fa1, ca=Ca_fa1, ra=Ra_fa1) -> fa1
data.frame(fa, zh=Zhm_fa2, ca=Ca_fa2, ra=Ra_fa2) -> fa2
data.frame(fa, zh=Zhm_fa3, ca=Ca_fa3, ra=Ra_fa3) -> fa3

data.frame(ca, zh=Zhm_ca1, ca=Ca_ca1, ra=Ra_ca1) -> ca1
data.frame(ca, zh=Zhm_ca2, ca=Ca_ca2, ra=Ra_ca2) -> ca2
data.frame(ca, zh=Zhm_ca3, ca=Ca_ca3, ra=Ra_ca3) -> ca3

data.frame(la, zh=Zhm_la1, ca=Ca_la1, ra=Ra_la1) -> la1
data.frame(la, zh=Zhm_la2, ca=Ca_la2, ra=Ra_la2) -> la2
data.frame(la, zh=Zhm_la3, ca=Ca_la3, ra=Ra_la3) -> la3

# ##coarse class zhh-ca
# p4 <- ggplot(ca1, aes(zhh, ca)) +
#   geom_point(cex=1, alpha=0.4) + 
#   geom_smooth(method = "loess", color="red") +
#   scale_x_continuous(limits=c(-10,60), expand=c(0,0), breaks = seq(-10, 60, by = 20)) +
#   scale_y_continuous(limits=c(0,1), expand=c(0,0), breaks = seq(0, 1, by = 0.25)) +theme_linedraw() +
#   ggtitle("Dn = 0.1 mm, Ca= 0.1 g/m3")
# p5 <- ggplot(ca2, aes(zhh, ca)) +
#   geom_point(cex=1, alpha=0.4) + 
#   geom_smooth(method = "loess", color="red") +
#   scale_x_continuous(limits=c(-10,60), expand=c(0,0), breaks = seq(0, 60, by = 20)) +
#   scale_y_continuous(limits=c(0,3), expand=c(0,0), breaks = seq(0, 3, by = 0.50)) +theme_linedraw() +
#   ggtitle("Dn = 0.1 mm, Ca= 1 g/m3")
# p6 <- ggplot(ca3, aes(zhh, ca)) +
#   geom_point(cex=1, alpha=0.4) + 
#   geom_smooth(method = "loess", color="red") +
#   scale_x_continuous(limits=c(-10,60), expand=c(0,0), breaks = seq(0, 60, by = 20)) +
#   scale_y_continuous(limits=c(0,15), expand=c(0,0), breaks = seq(0, 15, by = 5)) +theme_linedraw() +
#   ggtitle("Dn = 0.1 mm, Ca= 5 g/m3")
# 
# #lapilli class zhh, ca
# p7 <- ggplot(la1, aes(zhh, ca)) +
#   geom_point(cex=1, alpha=0.4) + 
#   geom_smooth(method = "loess", color="red") +
#   scale_x_continuous(limits=c(-10,60), expand=c(0,0), breaks = seq(-10, 60, by = 20)) +
#   scale_y_continuous(limits=c(0,1), expand=c(0,0), breaks = seq(0, 1, by = 0.25)) +theme_linedraw() +
#   ggtitle("Dn = 1 mm, Ca= 0.1 g/m3")
# 
# p8 <-ggplot(la3, aes(zhh, ca)) +
#   geom_point(cex=1, alpha=0.4) + 
#   geom_smooth(method = "loess", color="red") +
#   scale_x_continuous(limits=c(-10,60), expand=c(0,0), breaks = seq(0, 60, by = 20)) +
#   scale_y_continuous(limits=c(0,3), expand=c(0,0), breaks = seq(0, 3, by = 0.50)) +theme_linedraw() +
#   ggtitle("Dn = 1 mm, Ca= 1 g/m3")
# p9 <- ggplot(la3, aes(zhh, ca)) +
#   geom_point(cex=1, alpha=0.4) + 
#   geom_smooth(method = "loess", color="red") +
#   scale_x_continuous(limits=c(-10,60), expand=c(0,0), breaks = seq(-10, 60, by = 20)) +
#   scale_y_continuous(limits=c(0,15), expand=c(0,0), breaks = seq(0, 15, by = 5)) +theme_linedraw() +
#   ggtitle("Dn = 1 mm, Ca= 5 g/m3")
# 
# ggsave(
#   filename = "plotCA_Concentration2.pdf", 
#   plot = marrangeGrob(list(p4,p5, p6), nrow=1, ncol=3), 
#   width = 15, height = 9)
# 
# ggsave(
#   filename = "plotLA_Concentration2.pdf", 
#   plot = marrangeGrob(list(p7,p8, p9), nrow=1, ncol=3), 
#   width = 15, height = 9)


#Naive bayessian classification

data.frame(zh =Zhm_ca1, c= rep(4,length(Zhm_ca1))) -> class04
data.frame(zh =Zhm_ca2, c= rep(5,length(Zhm_ca2))) -> class05
data.frame(zh =Zhm_ca3, c= rep(6,length(Zhm_ca3))) -> class06
data.frame(zh =Zhm_la1, c= rep(7,length(Zhm_la1))) -> class07
data.frame(zh =Zhm_la2, c= rep(8,length(Zhm_la2))) -> class08
data.frame(zh =Zhm_la3, c= rep(9,length(Zhm_la3))) -> class09

#list(class01, class02, class03,class04, class05, class06,class07, class08, class09) -> cl.all
cl.all <- mget(ls(pattern = "class0")) 
list(fa1, fa2, fa3, ca1, ca2, ca3,la1, la2, la3) -> classes
write <-mapply(write.table,
               x=classes, file=paste0(names(cl.all), ".csv"),
               MoreArgs=list(row.names=FALSE, col.names=T,sep=","))

set.seed(101)
sample <- lapply(cl.all, function(x)
  sample.int(n = nrow(x), size = floor(.75*nrow(x)), replace = F))

#fun <- function(x, y) x[x$x %in% seq(y[1], y[2]), ]
f1 <- function(x, y) x[y,]
f2 <- function(x, y) x[-y,]
Map(f1, x=cl.all, y=sample) -> train
Map(f2, x=cl.all, y=sample) -> test

do.call(rbind, train) -> train
do.call(rbind, test) -> test

as.numeric(train$zh) -> train$zh
as.factor(train$c) -> train$c
as.numeric(test$zh) -> test$zh
as.factor(test$c) -> test$c

class_all <- do.call(rbind, cl.all)
class_all$c <- as.factor(class_all$c)

png(filename = "pair_zhh_class.png", width = 600, height = 600, units = "px", pointsize = 12,
    bg = "white")
pairs.panels(class_all)
dev.off()

box <- class_all %>%
  ggplot(aes(x=c, y = zh, fill= c)) +
  geom_boxplot() +
  ggtitle('Box plot') + theme_bw()
ggsave("boxplot_synthetic.pdf", box, width = 15, height = 9)

dens <- class_all %>%
  ggplot(aes(x=zh, fill= c)) +
  geom_density(alpha=0.5, color='black') +
  ggtitle("Density plot") + theme_bw()
ggsave("densplot_synthetic.pdf", dens, width = 15, height = 9)

model <- naive_bayes(c ~ zh, data = train, usekernel=T)
pr1 <- predict(model, train, type=('c'))
(tab<- table(pr1, train$c))
round(tab/rowSums(tab), 2) -> tab
1 - sum(diag(tab)/sum(tab)) -> mean_error

print(paste0("mean error rate=", round(mean_error,3)))
write.csv(tab, "table.contingency.csv")

saveRDS(model, "class_bayesian2.rds")

source("scr_powerlaw_model.R")

#my_model <- readRDS("class_bayesian.rds")
