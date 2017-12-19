getEnergy <- function(path){
  temp <- character()
  path %>%
    paste0("/freq/") %>%
    {function(x)
      dir(x) %>%
        grep(".out|.log", ., ignore.case = T, value = T) %>%
        paste0(x,.)
    }() %>%
    read_g09output %>%
    {function(x) x$Summary$HF}()

}

getPG <- function(path){
  temp <- character()
  path %>%
    paste0("/freq/") %>%
    {function(x)
      dir(x) %>%
        grep(".out|.log", ., ignore.case = T, value = T) %>%
        paste0(x,.)
    }() %>%
    read_g09output %>%
    {function(x) x$Summary$PG}()

}







library(tidyverse)
a<-Total$new()
dir(".") %>%lapply(function(x) a$add(IdenticalEntry$new(x, test,getEnergy(x),getPG(x))))
a$fixEnergy()
memo <- a$Ids %>% sapply(function(x) c(x$id, x$pointGroup, x$fixedEnergy))


getEnergy <- function(dirpath){
  dirpath %>%
    paste0("/file") %>%
    read_lines %>%
    as.numeric
}

a<-Total$new()
dir(".") %>%
  lapply(function(x) a$add(IdenticalEntry$new(x, dirNameParser, getEnergy(x))))
a$fixEnergy("10EQ1")
a$plot(1)
a$links
a$links %<>% rbind(c("33TS", "90EQ4"))
a$plot(-1)


links <- a$getLinks()
seq <- a$getLinks() %>% as.vector() %>% unique()
seq %<>% .[order(.)]
kv <- seq %>% sapply(function(x) a$getEntryFor(x)$fixedEnergy)

a$Ids
mat <- a$Ids %>% lapply(function(x) c(x$id,as.numeric(x$fixedEnergy))) %>% do.call(rbind, .)
mat %<>% cbind(c(NA,NA,"A","A","A","A","A","A","B","B","A"))
mat %<>% rbind(c("20EQ2",  "10", "B"),c("90EQ10", "-30", "B"))
mat <- mat[order(mat[,1]),]

library(magrittr)
data <- mat[c(-1,-2),] %>% as.data.frame
data$V2 %<>% as.character() %>% as.numeric()
data$V1 %<>% as.numeric()
data$V3

library(tidyverse)
gp <- ggplot(data, aes(x=V1, y=V2, colour = V3))
gp + geom_path(size=2)

gp <- ggplot()
gp = gp + ylim(-30, 30) + xlim(0,20)
for(i in 1:9){
  eval(parse(text = paste0("gp <- gp + geom_segment(aes(x = ",2*i -2," , y=",kv[seq[i]],", xend = ",2*i -1,", yend=",kv[seq[i]],"))")))
}

for(i in 1:8){
  eval(parse(text = paste0("gp <- gp + geom_segment(aes(x = ",2*(1:length(seq))[seq==links[i,1]] -1," , y=",kv[links[i,1]],", xend = ",(1:length(seq))[seq==links[i,2]]*2 -2,", yend=",kv[links[i,2]],"))")))
}

for(i in 1:9){
  eval(parse(text = paste0("gp <- gp + geom_text(aes(x = ",2*i -1.5," , y=",kv[seq[i]] ,",label=\"",seq[i],"\"))")))
}

gp

a <- Profiler$new()
dir() %>% lapply(Entry$new) %>% lapply(a$add_entry)
a$get_links()

a %>% profiler_extract_energy_with_("HF", get_energy_by)
a$compensate_energy("HF")
a$plot("HF")

a$link_info %<>% rbind(c("33TS", "90EQ4", "solid", "black"))
a$link_info
a$plot("HF")



#if(is.na(self$links)) self$getLinks() #この辺どうやって場合分けするのだろうか
seq <- a$links %>% as.vector() %>% unique()
seq %<>% .[order(.)]
kv <- seq %>% sapply(function(x) a$entries[[x]]$corrected_energies[["HF"]])
labelpos <- 1

gp <- ggplot()
gp <- gp + ylim(min(kv) - abs(labelpos), max(kv) + abs(labelpos)) + xlim(0,length(seq)*2)

for(i in 1:length(seq)){
  closure <- list(x_ = 2 * i - 2, y_ = kv[seq[i]], xend_ = 2 * i - 1, yend_ = kv[seq[i]])
  eval(substitute(gp <- gp + geom_segment(aes(x = x_, y= y_, xend = xend_, yend = yend_)), closure))
}
for(i in 1:length(seq)){
  closure <- list(x_ = 2*i - 1.5, y_ = kv[seq[i]] + labelpos, label_ = a$entries[[seq[i]]]$label)
  eval(substitute(gp <- gp + geom_text(aes(x = x_, y = y_, label = label_)), closure))
}
for(i in 1:length(a$links[,1])){
  closure <- list(x_ = 2 * (1:length(seq))[seq == a$links[i,1]] - 1,
               y_ = kv[a$links[i,1]],
               xend_ = 2 * (1:length(seq))[seq == a$links[i,2]] - 2,
               yend_ = kv[a$links[i,2]])
  eval(substitute(gp <- gp + geom_segment(aes(x = x_, y= y_, xend = xend_, yend = yend_)), closure))
}
title <- getwd() %>% {function(x) rev(strsplit(x,split = "/")[[1]])[1]}()
gp <- gp + labs(title = title) + xlab("") + ylab("")
gp
