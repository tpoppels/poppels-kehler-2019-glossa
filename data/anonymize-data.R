## Packages
require("tidyverse")

## Data files to anonymize
files <- c("expt-01-raw-data",
           "expt-02-raw-data",
           "expt-03-raw-data",
           "expt-04-raw-data")

## Loop over files
for (f in files) {
    ## Function for extracting relevant lines from raw-data
    grep.csv <- function(pattern,opts="-E", ...) {
        p <- pipe(sprintf("grep %s '%s' %s",opts,pattern,f))
        read.csv(p,as.is=T,header=F, ...)
    }

    ## Get a list of all participant IPs
    grep.csv(",code,")$V2 -> ip ## ',code,' occurs exactly once per participant

    ## Create pseudonyms ('Participant_IP_01' etc.)
    pseudonym <- paste0("Participant_IP_",
                        sprintf(paste0("%0",nchar(length(ip)),"d"),
                                1:length(ip)))

    ## Replace each ip with corresponding pseudonym in raw-data file
    tmp <- readLines(f)
    writeLines(tmp,con=paste0(f,".bk")) ## Create back-up
    for (i in 1:length(ip)) {
        tmp <- str_replace(tmp,ip[i],pseudonym[i])
    }
    writeLines(tmp,con=f)
}

