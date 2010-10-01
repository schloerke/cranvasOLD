

###################################

# mosaics on their own
source("mosaic-hilite.r")

require(productplots)
qhappy <- qmutaframe(happy)

ra <- get_row_attr(qhappy)
ra$.brushed <- qhappy$marital =="married"

plot1 <- qmosaic(qhappy, ~ health+sex+happy, c("vspine","hspine","hspine"))  
print(plot1)
plot2 <- qmosaic(qhappy, ~ degree+sex+happy, c("vspine","hspine","hspine"))  
print(plot2)
#print(plot2)
#happym <- mutaframe(happy)
#qmosaic(happym, ~ health+sex+happy, c("vspine","hspine","hspine"))  

#qmosaic(mutaframe(happy), ~ health+sex+happy, c("vspine","hspine","hspine"))  

#tc <- as.data.frame(Titanic)
#plot1 <- qmosaic(tc, Freq~Survived+Sex+Class+Age, c("vspine","hspine","hspine","hspine"))
#print(plot1)