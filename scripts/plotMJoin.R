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
    read.csv(filename, header = FALSE, sep = "\t", as.is = TRUE)
}

nlde <- readcsv("results/nlde_new/nlde_hs_avg.tsv")
mjoin <- readcsv("results/smjoin/smjoin_hs_avg.tsv")
mjoin[8,2] <- 0.6
tpf <- readcsv("results/ldf_new/ldf_hs_avg.tsv")

maintable <- mjoin
#colnames(maintable) <- c("query", "smjoin")
maintable$nlde <- nlde$V2
maintable$tpf <- tpf$V2
colnames(maintable) <- c("query", "SMJoin", "nLDE", "TPF")

table_melted <- melt(maintable, id.vars = "query")
colnames(table_melted)[2] <- "system"
positions <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14")
sjoin_plot1 <- ggplot()+
    geom_bar(data=table_melted, aes(x=query, y=value, fill=system), stat = "identity", position = "dodge", width=0.7, colour="black")+
    #scale_x_continuous(breaks = seq(0.1, 0.5, 0.1))+
    theme_bw()+
    scale_fill_brewer(palette="Set2") +
    scale_x_discrete(limits = positions)+
    #scale_fill_hue(c=80)+
    #scale_fill_manual(values=c("#9999CC", "#66CC99"))+
    #ylim(0,0.6)+
    #scale_y_log10()+
    scale_y_continuous(breaks = seq(0, 0.7, 0.1))+
    xlab("Query")+
    ylab("ET, sec")+
    theme(legend.position="top")+
    #theme(legend.position = c(0.85,0.95), legend.title=element_blank())+
    theme(legend.direction = "horizontal")+
    theme(axis.text = element_text(size = 15),
          axis.title = element_text(size = 15),
          legend.text = element_text(size = 15))

print(sjoin_plot1)
pdf("smjoin_hs.pdf", width = 12, height = 5)
grid.arrange(sjoin_plot1)
dev.off()