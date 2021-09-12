read.csv("class04.csv") -> class04
subset(class04, zh >0) -> class04
Mod_ca4 <- nls(ca ~ a*zh^b, data=class04, start = list(a=1, b=1))
Mod_ra4 <- nls(ra ~ a*zh^b, data=class04, start = list(a=1, b=1))

read.csv("class05.csv") -> class05
subset(class05, zh >0) -> class05
Mod_ca5 <- nls(ca ~ a*zh^b, data=class05, start = list(a=1, b=1))
Mod_ra5 <- nls(ra ~ a*zh^b, data=class05, start = list(a=1, b=1))

read.csv("class06.csv") -> class06
#subset(class06, zh >0) -> class06
m6 <- lm(log(ca) ~ log(zh), data=class06)
start <- list (a=coef(m6)[1], b= coef(m6)[2])
Mod_ca6 <- nls(ca ~ a*zh^b, data=class06, start = start)
Mod_ra6 <- nls(ra ~ a*zh^b, data=class06, start = start)

read.csv("class09.csv") -> class09
m9 <- lm(log(class09$ca) ~ log(class09$zh))
start <- list (a=coef(m9)[1], b= coef(m9)[2])
Mod_ca9 <- nls(ca ~ a*zh^b, data=class09, start = start)
Mod_ra9 <- nls(ra ~ a*zh^b, data=class09, start =  start)

read.csv("class08.csv") -> class08
# m9 <- lm(log(class08$ca) ~ log(class08$zh))
# start <- list (a=coef(m9)[1], b= coef(m9)[2])
Mod_ca8 <- nls(ca ~ a*zh^b, data=class08, start = start)
Mod_ra8 <- nls(ra ~ a*zh^b, data=class08, start =  start)

read.csv("class07.csv") -> class07
subset(class07, zh >=0) -> class07
# write.table(class07, "class07b.csv", sep=",", row.names=F)
Mod_ca7 <- nls(ca ~ a*zh^b, data=class07, start = start)
control1 <- nls.control(maxiter= 1000,tol=1e-02, warnOnly=TRUE)
m7 <- lm(log(ra) ~ log(zh), data=class08)
start <- list (a=coef(m7)[1], b= coef(m7)[2])
Mod_ra7 <- nls(ra ~ a*zh^b, data=class07, start = start, control=control1)

saveRDS(Mod_ca4, "class_04Ca.rds")
saveRDS(Mod_ca5, "class_05Ca.rds")
saveRDS(Mod_ca6, "class_06Ca.rds")
saveRDS(Mod_ca7, "class_07Ca.rds")
saveRDS(Mod_ca8, "class_08Ca.rds")
saveRDS(Mod_ca9, "class_09Ca.rds")

saveRDS(Mod_ra4, "class_04Ra.rds")
saveRDS(Mod_ra5, "class_05Ra.rds")
saveRDS(Mod_ra6, "class_06Ra.rds")
saveRDS(Mod_ra7, "class_07Ra.rds")
saveRDS(Mod_ra8, "class_08Ra.rds")
saveRDS(Mod_ra9, "class_09Ra.rds")

coef_ca<- rbind(coef(Mod_ca4), coef(Mod_ca5), coef(Mod_ca6), 
      coef(Mod_ca7), coef(Mod_ca8), coef(Mod_ca9))
coef_ra<- rbind(coef(Mod_ra4), coef(Mod_ra5), coef(Mod_ra6), 
                coef(Mod_ra7), coef(Mod_ra8), coef(Mod_ra9))

seq(4,9,1) -> num
data.frame(class=num, coef_ca) -> coef_ca
data.frame(class=num, coef_ra) -> coef_ra

write.csv(coef_ca, "coefficient_ca.csv")
write.csv(coef_ra, "coefficient_ra.csv")