## setup marks file
# read moodle participant download and create grades file
# 
# d <- read.csv("Grades.csv")
# ident <- paste0("Participant ", d$Identifier)
# d$Identifier <- ident
# #write.csv(d, file = "Grades_out.csv", row.names = FALSE)
# 
# #tst <- read.csv("Grades_outv2.csv")
# #tst$Identifier == d$Identifier
# # 1, tidy up the folder by removing the app files
# #tst$Grade
# 
# tst2 <- readr::read_csv("Grades_orig.csv")
# tst2$Identifier == d$Identifier
# tst2$Grade <- d$Grade
# #tst2
# write.csv(tst2, file = "Grades_final.csv", row.names = FALSE)

library(tidyverse)
library(fs)

#step 1 go through dirs and delete 
# unpack submission folder into Particpants folder

pdirs <- file.path(getwd(), list.files("Participants/Return", full.names = TRUE))
#cfiles <- c("header.tex", "setup.R", "app.R","qrm_rubric.rmd", "comments.xlsx")
#targs <- paste0(rep(pdirs, each = length(cfiles)),"/", cfiles)
#file_delete(targs)

#grep("_commented", list.files(pdirs), value = TRUE)
gwrap <- function(x, y){
  grep(y, x, value = TRUE)
}
filecontents <- lapply(pdirs,list.files)
comfiles <- lapply(filecontents, gwrap, "commented")
compaths <- paste0(pdirs,"/",unlist(comfiles))
feedpaths <- paste0(pdirs,"/feedback.pdf")
outdirs <- gsub("Return", "Final", pdirs)
fs::dir_create(outdirs)
origins <- c(compaths, feedpaths)
targs <- c(paste0(outdirs, "/", comfiles), paste0(outdirs,"/feedback.pdf"))

fs::file_copy(origins, targs)
# zip files up
targs <- paste0(rep(pdirs, each = length(cfiles)),"/", cfiles)
source <- rep(cfiles,length(pdirs))

d <- read.csv("Grades.csv")

d <- d[!is.na(d$Grade),]


sel <- d[d$Grade < 50, "Identifier"]

sel <- c(sel, "Participant 7248814", "Participant 7248776", 
         "Participant 7248812")
d <- d[d$Identifier %in% sel,]
write.csv(d, file=  "mods.csv", row.names = FALSE)
#source <- rep(cfiles,length(pdirs))

