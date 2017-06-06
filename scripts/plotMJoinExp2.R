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

plot_charts <- function(name1, name2, numAnswers, maxTime, outputName) {
    q1 <- readcsv(name1)
    #q1 <- q1[-c(13,26,39,52,65),]
    q1 <- q1[-c(numAnswers+2,2*numAnswers+4,3*numAnswers+6,4*numAnswers+8,5*numAnswers+10),]
    
    q1_nlde <- read.csv(name2, header = FALSE, sep = "\t", as.is = TRUE)
    #q1_nlde <- q1_nlde[-c(13,26,39,52,65),]
    q1_nlde <- q1_nlde[-c(numAnswers+2,2*numAnswers+4,3*numAnswers+6,4*numAnswers+8,5*numAnswers+10),]
    
    q11 <- q1[1:(numAnswers+1),]
    q12 <- q1[(numAnswers+2):(2*numAnswers+2),]
    q13 <- q1[(2*numAnswers+3):(3*numAnswers+3),]
    q14 <- q1[(3*numAnswers+4):(4*numAnswers+4),]
    q15 <- q1[(4*numAnswers+5):(5*numAnswers+5),]
    
    # q11 <- q1[1:12,]
    # q12 <- q1[13:24,]
    # q13 <- q1[25:36,]
    # q14 <- q1[37:48,]
    # q15 <- q1[49:60,]
    
    # q12_nlde <- q1_nlde[13:24,]
    # q13_nlde <- q1_nlde[25:36,]
    # q14_nlde <- q1_nlde[37:48,]
    # q15_nlde <- q1_nlde[49:60,]
    q11_nlde <- q1_nlde[1:(numAnswers+1),]
    q12_nlde <- q1_nlde[(numAnswers+2):(2*numAnswers+2),]
    q13_nlde <- q1_nlde[(2*numAnswers+3):(3*numAnswers+3),]
    q14_nlde <- q1_nlde[(3*numAnswers+4):(4*numAnswers+4),]
    q15_nlde <- q1_nlde[(4*numAnswers+5):(5*numAnswers+5),]
    
    q11[1:numAnswers,1] <- c(1:numAnswers)
    q11[numAnswers+1,1] <- numAnswers
    q11_nlde[1:numAnswers,1] <- c(1:numAnswers)
    q11_nlde[numAnswers+1,1] <- numAnswers
    
    q12[1:numAnswers,1] <- c(1:numAnswers)
    q12[numAnswers+1,1] <- numAnswers
    q12_nlde[1:numAnswers,1] <- c(1:numAnswers)
    q12_nlde[numAnswers+1,1] <- numAnswers
    
    q13[1:numAnswers,1] <- c(1:numAnswers)
    q13[numAnswers+1,1] <- numAnswers
    
    q13_nlde[1:numAnswers,1] <- c(1:numAnswers)
    q13_nlde[numAnswers+1,1] <- numAnswers
    
    q14[1:numAnswers,1] <- c(1:numAnswers)
    q14[numAnswers+1,1] <- numAnswers
    
    q14_nlde[1:numAnswers,1] <- c(1:numAnswers)
    q14_nlde[numAnswers+1,1] <- numAnswers
    
    q15[1:numAnswers,1] <- c(1:numAnswers)
    q15[numAnswers+1,1] <- numAnswers
    
    q15_nlde[1:numAnswers,1] <- c(1:numAnswers)
    q15_nlde[numAnswers+1,1] <- numAnswers
    
    q_avg <- q15
    q_nlde_avg <- q15_nlde
    
    for (i in c(1:(numAnswers+1))){
        q_avg$V3[i] <- mean(c(q11$V3[i],q12$V3[i],q13$V3[i],q14$V3[i],q15$V3[i]))
        q_nlde_avg$V3[i] <- mean(c(q11_nlde$V3[i],q12_nlde$V3[i],q13_nlde$V3[i],q14_nlde$V3[i],q15_nlde$V3[i]))
        print(q_avg$V3[i])
    }
    
    quplot <- ggplot()+
        geom_line(data=q_avg, aes(x=q_avg$V3, y=(as.integer(q_avg$V1)/numAnswers)*100, color="SMJoin"), size=1) +
        geom_line(data = q_nlde_avg, aes(x=q_nlde_avg$V3, y=(as.integer(q_nlde_avg$V1)/numAnswers)*100, color="nLDE"), size=1) +
        geom_point()+
        #geom_smooth(method=lm)+
        stat_smooth(aes(x=q_avg$V3, y=(as.integer(q_avg$V1)/numAnswers)*100), method = lm, formula = y ~ poly(x, 2), size=0.2, color="#62C3A4")+
        stat_smooth(aes(x=q_nlde_avg$V3, y=(as.integer(q_nlde_avg$V1)/numAnswers)*100), method = lm, formula = y ~ poly(x, 2), size=0.2, color="#FD8D5B")+
        xlab("Time, sec")+
        ylab("Completeness, %")+
        #ggtitle("Q1") +
        theme_bw()+
        ylim(0,100)+
        #scale_fill_brewer(palette="Set1") +
        scale_colour_manual(values=c("#FD8D5B","#62C3A4"))+
        scale_fill_manual(values=c("#FD8D5B","#62C3A4"))+
        scale_x_continuous(breaks = seq(0, maxTime, maxTime/10))+
        #scale_y_continuous(breaks = seq(0, 100, 20))+
        theme(legend.position="top",legend.title=element_blank())
    
    pdf(outputName, width = 6, height = 4)
    grid.arrange(quplot)
    dev.off()
}

plot_charts("results/smjoin/smjoin_ls_q1.tsv","results/nlde_new/nlde_ls_q1.tsv",11,10,"smjoin_ls1.pdf")
plot_charts("results/smjoin/smjoin_ls_q2.tsv","results/nlde_new/nlde_ls_q2.tsv",3,3,"smjoin_ls2.pdf")
plot_charts("results/smjoin/smjoin_ls_q4.tsv","results/nlde_new/nlde_ls_q4.tsv",5,2,"smjoin_ls4.pdf")
plot_charts("results/smjoin/smjoin_ls_q5.tsv","results/nlde_new/nlde_ls_q5.tsv",5,3,"smjoin_ls5.pdf")


