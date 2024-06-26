## setup marks file
# read moodle participant download and create grades file
library(tidyverse)
library(fs)
parts_fname <- "Grades-BUSI4428-1-UNUK-SPR-2324-Individual Coursework Submission Link-7366237.csv"
file.copy(parts_fname, "Grades.csv", overwrite = TRUE)
#parts <- read_csv(parts_fname) %>%
#  dplyr::select(Identifier, Grade)
#parts$Identifier <- sapply(strsplit(parts$Identifier, " "), function(x) x[2])
#parts
#write_csv(parts, "Grades.csv")

#init record.rds
l <- list()
saveRDS(l, "record.rds")

## unzip submissions into Participants folder

subs <- "BUSI4428-1-UNUK-SPR-2324-Individual Coursework Submission Link-7366237.zip" 
unzip(subs, exdir = "Participants")

## FInd all pdfs and make copy of pdf for commenting
cpdfs <- list.files("Participants", pattern = glob2rx("*.pdf"),
                    recursive = TRUE, 
                    full.names = FALSE)
bn <- basename(cpdfs)
pfx <- sapply(strsplit(bn, "\\."), function(x) x[1])
npfx <- paste0(pfx, "_commented")
nbn <- paste0(npfx, ".pdf")
npdfs <- paste0("Participants/", dirname(cpdfs), "/", nbn)
ck <- data.frame(cpdfs, npdfs)
orig <- paste0("Participants/", cpdfs)
file_copy(orig, npdfs)


## copy app and others into participants
## unpack submission folder into Particpants folder

pdirs <- file.path(getwd(), list.files("Participants", full.names = TRUE))
cfiles <- c("header.tex", "setup.R", "app.R","qrm_rubric.rmd", "comments.xlsx")

targs <- paste0(rep(pdirs, each = length(cfiles)),"/", cfiles)
source <- rep(cfiles,length(pdirs))
## very slow in windows
#file.copy(from = source, to = targs, overwrite = TRUE)
# use this instead

file_copy(path = source, new_path = targs, overwrite = TRUE)
