# Run this script first. It extracts and formats data from raw tables to analysis-ready outcomes.

# clean environment
rm( list = ls() )

# read libraries
library(here)
library(readxl)
library(tidyverse)

# prepare folders for imported data
if( !dir.exists("_data") ) dir.create("_data")


# IN-HOUSE FUNCTIONS ----

# read excel data
dread <- function( f = "Validacni_studie", h = head ) {
    
  # prepare the pathway to data
  p <- here( "_raw", "data", "Validační studie_matice", paste0(f,".xlsx") )
    
  # keep header information of included lists only
  h <- h[ h[,f] > 0, ]
    
  # read it
  out <- with(
    
    h,
    lapply(
      setNames(1:length(sheet),sheet),
      function(i)
        read_excel( path = p, sheet = sheet[i], skip = get(f)[i]-1, na = c(" ","NA","NV","N/A","*","#REF!") )
    )
  )
    
  # return it
  return(out)
    
}

# check ID names
idcheck <- function(d) {
  
  lapply(
    setNames( names(d), names(d) ),
    function(i)
      unique( sapply( names(d[[i]]), function(j) colnames(d[[i]][[j]])[1]) )
  )
}

# save as csv
saveit <- function(d) {
  
  invisible(
    
    sapply(
      names(d),
      function(i) {
        
        if( !dir.exists( here("_data",i) ) ) dir.create( here("_data",i) ) # prepare folders for data
        
        # saving proper
        sapply(
          names(d[[i]]),
          function(j) {
            
            dir <- here( "_data", i, paste0(j,".csv") )
            write.table( d[[i]][[j]], dir, sep = "\t", row.names = F, quote = F )
            print( paste0(j," from ",i," saved into ",dir) )

          })

      })

  )
    
}

# move based on mover csv file
moveit <- function(origin, target, files = "all", folder = NA) {
  
  # prepare a _raw folder in target directory
  if( !dir.exists(paste0(target,"/_raw") ) ) dir.create( paste0(target,"/_raw") )
  if( !is.na(folder) ) if( !dir.exists(paste0(target,"/_raw/",folder) ) ) dir.create( paste0(target,"/_raw/",folder) )
  
  # extract files to move
  # if none are specified, move all files from the origin directory
  if("all" %in% files) list <- list.files(origin) else list <- files
  
  # prepare origina and target files
  temp <- data.frame(
    
    from = paste0(origin,"/",list),
    to = if( !is.na(folder) ) paste0(target,"/_raw/",folder,"/",list) else paste0(target,"/_raw/",list)
    
  )
  
  # loop through all data files moving them origin->target
  invisible(
    
    sapply(
      1:nrow(temp),
      function(i) {
        
        file.copy( from = temp[i,"from"], to = temp[i,"to"], overwrite = T )
        print( paste0("Copied from ",temp[i,"from"]," to ",temp[i,"to"]) )
        
      })

  )
  
}


# READ DATA-SETS ----

# Normative data sets & clinical cases saved in the same format ----

# set-up paths and data set names 
head <- read.csv( here("_raw","helpers","header_rows.csv"), sep = "," ) %>% `names<-`( gsub( ".", " ", names(.), fixed = T ) )

# list data sets names
dnms <- names(head)[-1]

# import the data 
d0 <- lapply( setNames(dnms,dnms), function(i) dread( f = i, h = head ) )

# list all unique id column names across data sets (should be of length one for each)
idcheck(d0)

# "Validacni_studie" data set
# NOTE: PRESENT IN A LOOP IN CASE ANY NEW DATA SET NEED THIS TYPE OF PRE-PROCESSING
for ( i in c("Validacni_studie") ) d0[[i]]$`0_anamneza` <- d0[[i]]$`0_anamneza` %>% rename( "kod_ditete" = "...1" ) %>% mutate( gender = ifelse( gender == "1=zena", "1_zena", gender ) )

# "NBD_LMR" data set
names(d0$NBD_LMR)[ names(d0$NBD_LMR) == "List1" ] <- "0_anamneza" # rename NBD_LMR "List1"
for ( i in names(d0$NBD_LMR) ) d0$NBD_LMR[[i]] <- d0$NBD_LMR[[i]] %>% select(-1) %>% `colnames<-`( c( "kod_ditete", colnames(.)[-1] ) )

# "NBDvsIDS_pozornost_deti s CF" data set
names(d0$`NBDvsIDS_pozornost_deti s CF`) <- "dAtta"
d0$`NBDvsIDS_pozornost_deti s CF`$dAtta <- d0$`NBDvsIDS_pozornost_deti s CF`$dAtta %>% select(-1) %>% rename( "kod_ditete" = "ID" )

# "Neverbální paměť_validace_Sadecká" data set
d0$`Neverbální paměť_validace_Sadecká`$Anamnéza <- d0$`Neverbální paměť_validace_Sadecká`$Anamnéza %>% rename( "kod_ditete" = "Kod ucastnika" )
d0$`Neverbální paměť_validace_Sadecká`$Testy <- d0$`Neverbální paměť_validace_Sadecká`$Testy %>% relocate( kod_ditete, .before = 1 )

# re-check IDs and save the results
idcheck(d0)
saveit(d0)


# VARIABLES CHARACTERISTICS ----

v <- read_xlsx( here("_raw","helpers","vars.xlsx"), skip = 1 ) # read them

if( !dir.exists( here("_data","helpers") ) ) dir.create( here("_data","helpers") ) # prepare a folder for the file
write.table( v, here("_data","helpers","vars.csv"), sep = ";", quote = F, row.names = F ) # save as.csv

# MOVE DATA-SETS ----

# read movers info
mov <- read.csv( here("_raw","helpers","movers.csv"), sep = ";" )

# move them
for ( i in 1:nrow(mov) )  moveit(
  
  origin = mov[i,"from"],
  target = mov[i,"to"],
  files = strsplit( mov[i,"files"], "," )[[1]],
  folder = mov[i,"folder"]
  
) 

