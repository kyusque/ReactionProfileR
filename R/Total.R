library(R6)

Total <- R6Class("Total",
                 public = list(
                   Ids = list(),
                   init = function() self$Ids <- list(),
                   add = function(x){
                     self$Ids <<- c(self$Ids, x)
                   },
                   getEntryFor = function(id){
                     for(x in self$Ids){
                       if(x$id == id) return(x)
                     }
                     return(NULL)
                   },
                   getTotalStrings = function(strings, list = NULL){
                     if(is.null(list)) list <- strsplit(strings, split = "\\+|\\-")[[1]]
                     if(length(list) == 0 || is.na(list)){
                       return(strings)
                     }else{
                       firstInout <- self$getEntryFor(list[1])$atomsInout
                       if(is.na(firstInout) && length(list) == 1) return(Recall(strings, list()))
                       if(firstInout == list[1] && length(list) == 1) return(Recall(strings, list()))
                       if(firstInout == list[1] || is.na(firstInout)) return(Recall(strings, list[-1]))
                       strings <- gsub(list[1], firstInout, strings)
                       list <- c(strsplit(firstInout, split = "\\+|\\-")[[1]], list[-1])
                       return(Recall(strings, list))
                     }

                   },
                   fixEnergy = function(){
                     for(x in self$Ids){
                       strings <- self$getTotalStrings(x$atomsInout)
                       if(is.na(strings)){
                         srtings <- "0"
                       }else if(strings == x$id){#一番最初の場合
                         strings <- paste0("as.numeric(self$getEntryFor(\"",strings,"\")$energy) * 2")
                       }else{
                         strings <- paste0(x$id,"-",strings)
                         for(str in unique(strsplit(strings, split = "\\+|\\-")[[1]])){
                           if(str == "NA"){
                             strings <- gsub("NA", "0", strings)
                           }
                           else {
                             strings <- gsub(str, paste0("as.numeric(self$getEntryFor(\"",str,"\")$energy)"), strings)#エネルギーが取れなかったときの例外処理考える
                           }
                         }

                       }
                       strings <- gsub("\\+", "=", strings)
                       strings <- gsub("\\-", "\\+", strings)
                       strings <- gsub("=", "\\-", strings)
                       x$fixedEnergy <- eval(parse(text = strings))
                     }
                   },
                   plot = function(){

                     for(x in self$Ids){

                     }
                   }
                 )
)

IdenticalEntry <- R6Class("IdenticalEntry",
                          public = list(
                            initialize = function(path, parser = NULL, energy = NULL, pointGroup = NULL){
                              self$path <- path
                              self$energy <- energy
                              self$pointGroup <- pointGroup
                              if(!is.null(parser)){
                                res <- self$parseInfoFromPath(parser)
                                self$id = res[1]
                                self$title = res[2]
                                self$atomsInout = res[3]
                              }

                            },
                            parseInfoFromPath = function(parser){
                              parser(self$path)#parserはidとtitleとinoutのベクター
                            },
                            parseEnergyFromPath = function(parser){
                              self$energy <- parser(self$path)
                            },
                            parsepointGroupFromPath = function(parser){
                              self$pointGroup <- parser(self$path)
                            },
                            path = character(),
                            id = character(),
                            title = character(),
                            atomsInout = character(),
                            energy = numeric(),
                            fixedEnergy = numeric(),
                            pointGroup = character()
                          )
)

test <- function(path){
  path %>%
  {function(x) strsplit(x,"_")[[1]]}() %>%
  {function(x) c(paste0(x[1:3],collapse = ""),x[4],x[5])}()
}

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





a<-Total$new()
dir(".") %>%lapply(function(x) a$add(IdenticalEntry$new(x, test,getEnergy(x),getPG(x))))
a$fixEnergy()
memo <- a$Ids %>% sapply(function(x) c(x$id, x$pointGroup, x$fixedEnergy))

