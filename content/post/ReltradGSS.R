## @knitr ReltradCode

library(car)
#library(tidyverse)
library(descr) #Get the rocking CrossTable Function! Weighted! crosstab
#This is where the R dataset will live:
urldata = url("https://github.com/thebigbird/academic/raw/master/static/files/gss7216.data")
#
load(urldata)
#recode into 5 major categories of religious affiliation
# 1) Protestant [Ask DENOM]	1371	47.8
# 2) Catholic
# 3) Jewish	
# 4) None	
# 5) Other (specify)
# 6) Buddhism
# 7) Hinduism
# 8) Other Eastern religion
# 9) Muslim/Islam
# 10) Orthodox Christian
# 11) Christian
# 12) Native American
# 13) Inter-/non-denominational
# 98) Don't know
# 99) No answer
gss$xaffil = car::recode(gss$relig, "1=1;2=4;3=5;4=9;5:10=6;11=1;12=6;13=1")
gss$xaffil = as.factor(gss$xaffil)
levels(gss$xaffil) = c("prot", "cath", "jew", "other", "nonaf")

# The following code breaks down religious groups by evangelicals, black
# Protestants, mainline, liberal and conservative nontraditional,
# and Protestant nondenomination/no denomination.

#####Black Protestants
#Create a racial indicator
gss$black = ifelse(gss$race == "black", 1, 0)
gss$white = ifelse(gss$race == "white", 1, 0)
#Take the "other" Protestant denominations and pull out the 
#historical Black denominations, e.g. COGIC
gss$xbp = gss$other
gss$xbp = ifelse(gss$xbp %in% c(7, 14, 15, 21, 37, 38, 56, 78, 79, 85, 86, 87, 88, 98, 103, 104, 128, 133), 1, 0)
#National baptists and AME, AMEZ
gss$xbp = ifelse(gss$denom %in% c(12, 13, 20, 21), 1, gss$xbp)
#Blacks in certain denoms get recoded as Black Protestant
#Other baptist, amer. baptist, south. bap, other Methodists
gss$xbp[gss$black == 1] = ifelse(gss$denom[gss$black == 1] %in%
                                   c(10, 11, 18, 23, 13, 14), 1, gss$xbp[gss$black == 1])
#Black missionary baptists
gss$xbp[gss$black == 1] = ifelse(gss$other[gss$black == 1] %in%
                                   c(93), 1, gss$xbp[gss$black == 1])

#Evangelical Protestants#
#Recode the evangelicals in the other variable
gss$xev=gss$other
evother=c(2, 3, 5, 6, 9, 10, 12, 13, 16, 18, 20, 22, 24, 26, 27, 28, 31, 32, 34, 35, 36, 39, 41, 42, 43, 45, 47,51, 52, 53, 55, 57, 63, 65, 66, 67, 68, 69, 76, 77, 83,
          84, 90, 91, 92, 94, 97, 100, 101, 102, 106, 107, 108, 109, 110, 111, 112, 115, 116, 117, 118, 120, 121, 122, 124, 125, 127, 129, 131, 132, 134, 135, 138, 139, 140, 146)
gss$xev=ifelse(gss$xev %in% evother,1,0)
#Cons Lutherans, cons presbyterians
gss$xev=ifelse(gss$denom %in% c(32,33,34,42), 1, gss$xev)
#White baptists, white other methodists
gss$xev[gss$black==0]=ifelse(gss$denom[gss$black==0] %in% 
                               c(10,18,15,23,14),1,gss$xev[gss$black==0])
#Missionary baptist
gss$xev[gss$black==0]=ifelse(as.numeric(gss$other[gss$black==0]) %in%
                               c(93),1,gss$xev[gss$black==0])

#Lifeway correction to reltrad
gss$xtn = gss$relig
gss$denom2 = gss$denom
#70 = No denomination or non-denominations
gss$denom2 = recode(gss$denom2, "70=1; else=0")
gss$xtn = recode(gss$xtn, "11=1; else=0")
gss$xtn[gss$denom2 == 1] = 2
gss$xtn = recode(gss$xtn, "1=1; 2=0")
#Only weekly or +weekly attenders
gss$xtn[gss$attend < 4|gss$attend==3|gss$attend==0|is.na(gss$attend)] <- 0
gss$xev[gss$xtn ==1] <- 1

gss$inter <- gss$relig
#Interdenominationals
gss$inter <- recode(gss$inter, "13=1; else=0")
gss$inter[gss$attend < 4|gss$attend==3|gss$attend==0|is.na(gss$attend)] <- 0
gss$xev[gss$inter ==1] <- 1

# Mainline Protestants
#The other category
gss$xml = NA
gss$xml = gss$other
mpother=c(1,8,19,23,25,40, 44, 46, 48, 49, 50, 54, 70, 71, 72, 73, 81, 89, 96, 99, 105, 119, 148)
gss$xml=ifelse(gss$xml %in% mpother,1,0) 
#The denom category
gss$xml = ifelse(gss$denom %in% 
                   c(30, 50, 35, 31, 38, 40, 48, 43, 22, 41),1,gss$xml)
#Mainline baptist denom and methodists - if the R is white, they get coded mainline
gss$xml[gss$black==0] = ifelse(gss$denom[gss$denom[gss$black==0]] %in%
                                 c(11, 28),1,gss$xml[gss$black==0])

#Catholics
gss$xcath = gss$other 
#Polish National Church and Catholic
gss$xcath = ifelse(gss$denom %in% c(123, 28),1,0)
#People who say that they are other get coded zero
gss$xcath=ifelse(gss$xaffil=="cath", 1, gss$xcath) 

#Jews
gss$xjew=0 
gss$xjew=ifelse(gss$xaffil=="jew",1,0)

#Adherents of other religions.
gss$xother = gss$other
gss$xother = ifelse(gss$xother %in% 
                      c(11, 17, 29, 30, 33, 58, 59, 60, 61, 62, 64, 74, 75, 80, 82,
                        95, 113, 114, 130, 136, 141, 145),1,0)
#Adds others from main religious recoding
gss$xother=ifelse(gss$xaffil=="other" & gss$xev==0,1,0)

#Unaffiliateds/Nonaffiliateds
gss$xnonaff=0
gss$xnonaff[gss$xaffil=="nonaf"]=1

#The recodes non-denoms based on their attendance 
#Non active Don't Know Protestants coded to nonaffil
gss$xprotdk = ifelse(gss$denom == 70,1,0)
gss$xprotdk[gss$xprotdk == 1 & gss$attend >= 4] = 0
#Active Don't Know Protestants coded to evangelicals
gss$xnonaff[gss$xprotdk]=1
gss$xev[gss$xprotdk == 1 & gss$attend >= 4] = 1

#All these folks get coded evangelical
gss$reltrad = factor(NA, levels=c("Conservative Protestant",
                                  "Mainline Protestant",
                                  "Black Protestant",
                                  "Roman Catholic",
                                  "Other",
                                  "None"))
gss$reltrad[gss$xev==1]="Conservative Protestant"
gss$reltrad[gss$xml==1]="Mainline Protestant"
gss$reltrad[gss$xbp==1]="Black Protestant"
gss$reltrad[gss$xcath==1]="Roman Catholic"
gss$reltrad[gss$xother==1]="Other"
gss$reltrad[gss$xnonaff==1]="None"
save(gss,file="gss7216_reltrad.data")
gss$year = as.factor(gss$year)
#End of my poorly written R code! Sorry - I'll clean it up some day!
