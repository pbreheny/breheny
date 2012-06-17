source.dir <- function(directory)
  {
    all.files <- list.files(directory)
    nc <- nchar(all.files)
    source.file <- all.files[substr(all.files,start=nc-1,sto=nc)==".R"]
    if (length(source.file)!=0)
      {
        for (i in 1:length(source.file))
          {
            source(paste(directory,"/",source.file[i],sep=""))
          }
      }
  }

