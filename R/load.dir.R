load.dir <- function(directory) {
  all.files <- list.files(directory)
  nc <- nchar(all.files)
  data.file <- all.files[substr(all.files, start=nc-5, stop=nc)==".RData"]
  if (length(data.file)!=0) {
    for (i in 1:length(data.file)) {
      load(paste(directory,"/",data.file[i],sep=""),envir=.GlobalEnv)
    }
  }
}

