is.installed = function(pkg) {
    is.element(pkg, installed.packages()[,1])
} 

if (!is.installed("ggplot2")) { install.packages("ggplot2") }
if (!is.installed("gtools")) { install.packages("gtools") }
if (!is.installed("gridExtra")) { install.packages("gridExtra") }
if (!is.installed("RColorBrewer")) { install.packages("RColorBrewer") }
if (!is.installed("doBy")) { install.packages("doBy") }
if (!is.installed("nortest")) { install.packages("nortest") }
if (!is.installed("coin")) { install.packages("coin") }
if (!is.installed("reshape2")) { install.packages("reshape2") }
if (!is.installed("cowplot")) { install.packages("cowplot") }

library(ggplot2)
library(gtools)
library(gridExtra)
library(grid)
library(RColorBrewer)
library(doBy)
library(nortest)
library(coin)
library(reshape2)
library(cowplot)

require(reshape2)


readcsv = function(filename) {
    read.csv(filename, header = FALSE, sep = " ", as.is = TRUE)
}


q1 <- readcsv("results/smjoin/smjoin_ls_q2.tsv")
#q1 <- q1[-c(5,10,15,20),]
#q1 <- q1[-c(3+2,2*3+4,3*3+6,4*3+8,5*3+10),]

q1_nlde <- read.csv("results/nlde_new/nlde_ls_q2.tsv", header = FALSE, sep = "\t", as.is = TRUE)
#q1_nlde <- q1_nlde[-c(5,10,15,20),]
#q1_nlde <- q1_nlde[-c(3+2,2*3+4,3*3+6,4*3+8,5*3+10),]

# q11 <- q1[1:(3+1),]
# q12 <- q1[(3+2):(2*3+2),]
# q13 <- q1[(2*3+3):(3*3+3),]
# q14 <- q1[(3*3+4):(4*3+4),]
# q15 <- q1[(4*3+5):(5*3+5),]

q11 <- q1[1:4,]
q12 <- q1[5:8,]
q13 <- q1[9:12,]
q14 <- q1[13:16,]
q15 <- q1[17:20,]

q11_nlde <- q1_nlde[1:4,]
q12_nlde <- q1_nlde[5:8,]
q13_nlde <- q1_nlde[9:12,]
q14_nlde <- q1_nlde[13:16,]
q15_nlde <- q1_nlde[17:20,]
# q11_nlde <- q1_nlde[1:(3+1),]
# q12_nlde <- q1_nlde[(3+2):(2*3+2),]
# q13_nlde <- q1_nlde[(2*3+3):(3*3+3),]
# q14_nlde <- q1_nlde[(3*3+4):(4*3+4),]
# q15_nlde <- q1_nlde[(4*3+5):(5*3+5),]

q11[1:3,1] <- c(1:3)
q11[4,1] <- 3
q11_nlde[1:3,1] <- c(1:3)
q11_nlde[4,1] <- 4

q12[1:3,1] <- c(1:3)
q12[3+1,1] <- 3
q12_nlde[1:3,1] <- c(1:3)
q12_nlde[3+1,1] <- 3

q13[1:3,1] <- c(1:3)
q13[3+1,1] <- 3

q13_nlde[1:3,1] <- c(1:3)
q13_nlde[3+1,1] <- 3

q14[1:3,1] <- c(1:3)
q14[3+1,1] <- 3

q14_nlde[1:3,1] <- c(1:3)
q14_nlde[3+1,1] <- 3

q15[1:3,1] <- c(1:3)
q15[3+1,1] <- 3

q15_nlde[1:3,1] <- c(1:3)
q15_nlde[3+1,1] <- 3

q_avg <- q15
q_nlde_avg <- q15_nlde

for (i in c(1:(3+1))){
    q_avg$V3[i] <- mean(c(q11$V3[i],q12$V3[i],q13$V3[i],q14$V3[i],q15$V3[i]))
    q_nlde_avg$V3[i] <- mean(c(q11_nlde$V3[i],q12_nlde$V3[i],q13_nlde$V3[i],q14_nlde$V3[i],q15_nlde$V3[i]))
    print(q_avg$V3[i])
}

quplot <- ggplot()+
    geom_line(data=q_avg, aes(x=q_avg$V3, y=(as.integer(q_avg$V1)/3)*100, color="SMJoin"), size=1) +
    geom_line(data = q_nlde_avg, aes(x=q_nlde_avg$V3, y=(as.integer(q_nlde_avg$V1)/3)*100, color="nLDE"), size=1) +
    geom_point()+
    #stat_smooth(aes(x=q_avg$V3, y=(as.integer(q_avg$V1)/3)*100), method = lm, formula = y ~ poly(x, 2), size=0.2, color="#E2150A")+
    #stat_smooth(aes(x=q_nlde_avg$V3, y=(as.integer(q_nlde_avg$V1)/3)*100), method = lm, formula = y ~ poly(x, 2), size=0.2, color="#337CB9")+
    xlab("Time, sec")+
    ylab("Completeness, %")+
    #ggtitle("Q1") +
    theme_bw()+
    scale_x_continuous(breaks = seq(0, 3, 3/10))+
    #scale_y_continuous(breaks = seq(0, 100, 20))+
    ylim(0,100)+
    scale_colour_manual(values=c("#FD8D5B","#62C3A4"))+
    scale_fill_manual(values=c("#FD8D5B","#62C3A4"))+
    theme(legend.position="top",legend.title=element_blank())

pdf("smjoin_ls2.pdf", width = 6, height = 4)
grid.arrange(quplot)
dev.off()
