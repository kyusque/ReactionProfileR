library(R6)
library(purrr)

Profiler <- R6Class("Profiler",
                    public = list(
                      entries = NULL,
                      link_info = NA,
                      initialize = function(){
                        self$entries <- new.env()
                      },
                      compensate_energy = function(key){
                        self$entries %>%
                          lapply(function(x) (self$calc_inout(x,key) + x$energies[[key]]) %>%
                                   x$set_corrected_energy_env_(key, .))
                      },
                      get_links = function(){
                        mat <- self$entries %>%
                          lapply(function(x) x$link_info) %>%
                          do.call(rbind,.)
                        mat <- mat[!is.na(mat[,1]),]
                        mat <- mat[!is.na(mat[,2]),]
                        mat <- mat[mat[,1] != mat[,2],]
                        mat <- cbind(mat, "black")
                        rownames(mat) <- NULL
                        self$link_info <- mat
                      },
                      get_entries = function(){
                        ls(self$entries) %>%
                          sapply(function(x) self$entries[[x]])
                      },
                      add_entry = function(entry){
                        id <- entry$id
                        self$entries[[id]] <- entry
                      },
                      get_inouts_extended_for_profiler = function(entry, strings = NULL ,inouts = NULL){
                        if(is.null(strings)){
                          strings <- strsplit(entry$atoms_inout, split = "&")[[1]][1]
                          if(is.na(strings) || is_empty(strings)) return(NA)
                          inouts <- strsplit(strings, split = "\\+|\\-")[[1]]
                        }
                        if(is_empty(inouts) || is.na(inouts)) return(strings)
                        firstInout <- self$entries[[inouts[1]]]$atoms_inout
                        if(length(inouts) == 1 && (is.na(firstInout) || firstInout == inouts[1]))
                          return(Recall(entry, strings, NA))
                        if(firstInout == inouts[1] || is.na(firstInout))
                          return(Recall(entry, strings, inouts[-1]))
                        strings <- gsub(inouts[1], firstInout, strings)
                        inouts <- c(strsplit(firstInout, split = "\\+|\\-")[[1]], inouts[-1])
                        return(Recall(entry, strings, inouts))
                      },
                      eval_inouts_for_correction_energy_ = function(strings, key, list = NULL){
                        if(is.null(list)){
                          if(is.na(strings)) return(NA)
                          list <- strsplit(strings, split = "\\+|\\-")[[1]]
                          strings <- strings %>%
                            gsub("\\+", "=", .) %>%
                            gsub("\\-", "\\+", .) %>%
                            gsub("=", "\\-", .)
                          strings <- paste0("-", strings)
                        }
                        if(is_empty(list)) return(eval(parse(text = strings)))
                        if(is.na(list[1])) return(Recall(strings, key, list[-1]))
                        energy <- self$entries[[list[1]]]$energies[[key]]
                        strings <- gsub(list[1], energy, strings)
                        return(Recall(strings, key, list[-1]))
                      },
                      calc_inout = function(entry, key){
                        entry %>%
                          self$get_inouts_extended_for_profiler() %>%
                          self$eval_inouts_for_correction_energy_(key)
                      },
                      extract_energy_with_ = function(key, energy_parser){
                        self$entries %>% lapply(function(x) x$set_energy_env_(key, energy_parser(x$path)))
                      },

                      plot = function(key, labelpos = 1){
                        if(is.na(self$link_info[1])) self$get_links()
                        seq <- self$link_info[,c(1, 2)] %>% as.vector() %>% unique()
                        seq %<>% .[order(.)]
                        kv <- seq %>% sapply(function(x) self$entries[[x]]$corrected_energies[[key]])
                        gp <- ggplot()
                        gp <- gp + ylim(min(kv) - abs(labelpos), max(kv) + abs(labelpos)) + xlim(0,length(seq)*2)

                        for(i in 1:length(seq)){
                          env <- list(x_ = 2 * i - 2,
                                      y_ = kv[seq[i]],
                                      xend_ = 2 * i - 1,
                                      yend_ = kv[seq[i]])
                          eval(substitute(gp <- gp + geom_segment(aes(x = x_, y= y_, xend = xend_, yend = yend_)), env))
                        }
                        for(i in 1:length(seq)){
                          env <- list(x_ = 2 * i - 1.5,
                                      y_ = kv[seq[i]] + labelpos,
                                      label_ = self$entries[[seq[i]]]$label)
                          eval(substitute(gp <- gp + geom_text(aes(x = x_, y = y_, label = label_)), env))
                        }
                        for(i in 1:length(self$link_info[,1])){
                          env <- list(x_ = 2 * (1:length(seq))[seq == self$link_info[i,1]] - 1,
                                      y_ = kv[self$link_info[i,1]],
                                      xend_ = 2 * (1:length(seq))[seq == self$link_info[i,2]] - 2,
                                      yend_ = kv[self$link_info[i,2]],
                                      color_ = self$link_info[i,4],
                                      linetype_ = self$link_info[i,3])
                          eval(substitute(gp <- gp + geom_segment(aes(x = x_, y= y_, xend = xend_, yend = yend_), color = color_, linetype = linetype_), env))
                        }
                        title <- getwd() %>% {function(x) rev(strsplit(x,split = "/")[[1]])[1]}()
                        title <- paste(title, key)
                        gp <- gp + labs(title = title) + xlab("") + ylab("")
                        gp
                      }
                    )
)


profiler_add_entry <- function(profiler, entry){
  profiler$add_entry(entry)
}

profiler_get_entries <- function(profiler){
  lprofiler$get_entries()
}

profiler_extract_energy_with_ <- function(profiler, key, energy_parser){
  profiler$extract_energy_with_(key, energy_parser)
}


Entry <- R6Class("Entry",
                 public = list(
                   path = "",
                   file = "",
                   id = "",
                   title = "",
                   atoms_inout = "",
                   link_info = "",
                   energies = NULL,
                   corrected_energies = NULL,
                   point_group = "",
                   label = "",

                   initialize = function(path, info_parser = self$info_parser){
                     self$path <- path
                     self$file <- self$path2file(self$path)
                     self$parse_info(info_parser)
                     self$energies <- new.env()
                     self$corrected_energies <- new.env()
                   },
                   set_energy_env_ = function(key, energy){
                     self$energies[[key]] <- energy
                   },
                   set_corrected_energy_env_ = function(key, energy){
                     self$corrected_energies[[key]] <- energy
                   },
                   info_parser = function(dirpath){
                     dirpath %>%
                     {function(x) strsplit(x,"_")[[1]]}() %>%
                     {function(x) c(paste0(x[1:3],collapse = ""),x[4],x[5])}()
                   },
                   parse_info = function(parser){
                     res <- parser(self$file)

                     self$id <- res[1]
                     self$title <- res[2]
                     self$atoms_inout <- res[3]

                     inout_list <- strsplit(self$atoms_inout, split = "&")[[1]]
                     link_info <- strsplit(inout_list, split = "\\+|\\-") %>%
                       lapply(function(x) c(x[1], self$id, ifelse(length(x) == 1, "solid", "dashed"))) %>%
                       do.call(rbind, .)
                     names(link_info) <- NULL
                     self$link_info <- link_info
                     label <- strsplit(self$id, split = "EQ|TS")[[1]][2]
                     self$label <- ifelse(is.na(label), "", label)
                   },
                   parse_pointgroup = function(parser){
                     self$point_group <- parser(self$path)
                   },
                   path2file = function(path) tail(strsplit(path, split = "/")[[1]], n = 1)
                 )
)

entry_set_energy_ <- function(entry, key, value){
  entry$set_energy_env_(key, value)
}
