source("Heike/mosaic.r")

# Some Examples  
qtmosaic(happy, ~ health+sex+happy, c("vspine","hspine","hspine"), subset=level==3)  

happy$highlight <- happy$sex=="male" 
qtmosaic(happy, ~ highlight+health+sex+happy, c("vspine","hspine","hspine","hspine"), subset=level==4)  

qtmosaic(happy, ~ highlight+health+sex+happy, c("vspine","hspine","hspine","hspine"), subset=level==4)  


qtmosaic(happy, ~ health+sex+happy, c("vspine","hspine","hspine"), subset=level==3)  

qtmosaic(happy, ~ health, c("vbar"))  
qtmosaic(happy, ~ sex+health, flucts())  
