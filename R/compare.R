library(arrow)
library(dplyr)
dirDta <- 'N:/durable/_HQ/_Data/_2Processed/'
dirRes <- 'N:/durable/_HQ/_Syntax/andreasj/results/'
dta <- open_dataset(paste0(dirDta, 'LMR/parquetLMR/'))
atc5 <- dta %>% select(atckode5) %>% count(atckode5)
atc5 <- collect(atc5)
atc5 <- atc5[order(atc5€atckode5),]
n <- sum(atc5$n)
atc5$Percent <- 100*atc5$n/n
colnames(atc5) <- c('ATC', 'N', 'Percent')

atc4 <- dta %>% select(atckode4) %>% count(atckode4)
atc4 <- collect(atc4)
atc4 <- atc4[order(atc4$atckode4),]
n <- sum(atc4$n)
at4$Percent <- 100*atc4$n/n
colnames(atc4) <- c('ATC', 'N', 'Percent')

save(file=paste0(dirRes, 'results.Rdata'),
     atc5, atc5)
