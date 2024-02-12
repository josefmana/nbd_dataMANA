# Run this script first. It extracts and formats data from raw tables to analysis-ready outcomes.

# clean environment
rm( list = ls() )

# list required packages into a character object
pkgs <- c("here","readxl","tidyverse")

# load or install packages as needed
for ( i in pkgs ) {
  if ( i %in% rownames( installed.packages() ) == F ) install.packages(i) # install if it ain't installed yet
  if ( i %in% names( sessionInfo()$otherPkgs ) == F ) library( i , character.only = T ) # load if it ain't loaded yet
}

# prepare folders for imported data
if( !dir.exists("_data") ) dir.create("_data")


# IN-HOUSE FUNCTIONS ----

# read excel data
dread <- function( f = "Validacni_studie", h = head ) {
  
  # prepare the pathway to data
  p <- here( "_raw", "datafiles", paste0(f,".xlsx") )
  
  # keep header information of included lists only
  h <- h[ h[,f] > 0, ]
  
  # read it
  out <-
    
    with(
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
idcheck <- function(d){
  
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
            
          }
        )
        
      }
    )
    
  )

}


# READ DATA-SETS ----

# Normative data sets & clinical cases saved in the same format ----

# set-up paths and data set names 
head <- read.csv( here("_raw","helpers","header_rows.csv"), sep = "," ) # header rows

# list data sets names
dnms <- c("Validacni_studie","NBD_ADHD","NBD_LMR","test_retest")

# import the data 
d0 <- lapply( setNames(dnms,dnms), function(i) dread( f = i, h = head ) )

# list all unique id column names across data sets (should be of length one for each)
idcheck(d0)

# re-code id columns
attach(d0) # will be working in d0 only for a while

# "Validacni_studie" data set
# NOTE: PRESENT IN A LOOP IN CASE ANY NEW DATA SET NEED THIS TYPE OF PRE-PROCESSING
for ( i in c("Validacni_studie") ){
  
  d0[[i]]$`0_anamneza` <-
    
    get(i)$`0_anamneza` %>%
    rename( "kod_ditete" = "...1" ) %>%
    mutate( gender = ifelse( gender == "1=zena", "1_zena", gender ) )
  
}

# "NBD_LMR" data set
for ( i in names(NBD_LMR) ) {
  d0$NBD_LMR[[i]][ , 1] <- NULL # get rid of the name variable
  colnames(d0$NBD_LMR[[i]])[1] <- "kod_ditete" # re-name the "dg.cislo" variable as the new id
                                               # NOTE THAT IN THE LIST1, THE NAMING CONVENTION WAS DIFFERENT
}

# rename NBD_LMR "List1"
names(d0$NBD_LMR)[ names(NBD_LMR) == "List1" ] <- "0_anamneza"

detach(d0)
idcheck(d0) # (re-)check it
saveit(d0)


# Nonverbal memory test ----

# path to the file
p1 <- here("_raw","datafiles","Neverbální paměť_validace_Sadecká.xlsx") 
#p1 <- here("_raw","datafiles","Tabulka-s-info-participantů.xlsx") # double-check with original (typos) data

# read each list separately
# need to create a two-level list for saveit() to work properly
d1 <-
  
  list(
    nonverbal_mem =
      list(
        
        anam =
          read_excel( path = p1, sheet = "Anamnéza", skip = 0, na = c(" ","NA","NV","N/A","*","#REF!") ) %>%
          rename( "kod_ditete" = "Kod ucastnika" ),
        
        test =
          read_excel( path = p1, sheet = "Testy", skip = 1, na = c(" ","NA","NV","N/A","*","#REF!") )
        
      )
  )

# save it
saveit(d1)


# Variables of interest ----

# reload the data as they were saved
d0$Validacni_studie <-
  
  lapply(
    setNames( names(d0$Validacni_studie), names(d0$Validacni_studie) ),
    function(i)
      read.csv( here("_data","Validacni_studie",paste0(i,".csv") ), sep = "\t" )
  )

# list variables of interest in each data set/test
v <-
  
  with(
    
    d0$Validacni_studie,
    list(

      `0_anamneza` = cbind( c( "vek_roky", "skola", "gender", "rukovost", "poradi" ), c( "cont", rep("cat",4) ) ),
      `1_verbalni_pamet_a_uceni` = cbind( names(`1_verbalni_pamet_a_uceni`)[c(3:7,12,21:24,34:36)], c( rep("count",4),"cont",rep("count",8) ) ),
      `2_pamet_na_pribehy_OKAMZITE` = cbind( names(`2_pamet_na_pribehy_OKAMZITE`)[2:4], rep("cont",3) ),
      `2_pamet_na_pribehy_ODDALENE` = cbind( names(`2_pamet_na_pribehy_ODDALENE`)[2:4], rep("cont",3) ),
      `3_neverbalni_pamet` = cbind( names(`3_neverbalni_pamet`)[c(28:29,55:56,82:83,109:110,136:137)], rep("cont",10) ),
      `4_sluchova_pozornost` = cbind( names(`4_sluchova_pozornost`)[2:5], c( "cont", rep("count",3) ) ),
      `5_zrakova_pozornost` = cbind( names(`5_zrakova_pozornost`)[2:5], c( "cont", rep("count",3) ) ),
      `6_trideni` = cbind( names(`6_trideni`)[c(2:3)], c( rep("cont",2) ) ),
      `7_inhibice_a_presun_pozornosti` = cbind( names(`7_inhibice_a_presun_pozornosti`)[2:16], rep( c( "cont", "cat", rep("count",3) ), 3 ) ),
      `8_pracovni_pamet` = cbind( rep( names(`8_pracovni_pamet`)[18], 2 ), c( "count", "cont" ) ),
      `9_verbalni_fluence` = cbind( names(`9_verbalni_fluence`)[c(8,15,22,29,30)], rep("count",5) ),
      `10_konfrontacni_pojmenovani` = cbind( names(`10_konfrontacni_pojmenovani`)[3:6], rep("count",4) ),
      `11_vybaveni_s_napovedou` = cbind( names(`11_vybaveni_s_napovedou`)[c(3:8)], rep("count",6) ),
      `12_prospektivni_pamet` = cbind( names(`12_prospektivni_pamet`)[2:3], rep("count",2) ),
      `13_porozumeni_pokynum` = cbind( rep( names(`13_porozumeni_pokynum`)[3], 2 ), c( "count", "cont" ) ),
      `15_motoricka_koordinace` = cbind( names(`15_motoricka_koordinace`)[c(3:6,11:14)], rep("cont",8) ),
      `16_zrakove_motoricka_presnost` = cbind( names(`16_zrakove_motoricka_presnost`)[3:6], rep("count",4) ),
      `17_zrakove_vnimani` = cbind( rep( names(`17_zrakove_vnimani`)[3], 2 ), c("count","cont") ),
      `18_zrakove_prostorove_vnimani` = cbind( rep( names(`18_zrakove_prostorove_vnimani`)[3], 2 ), c( "count", "cont" ) ),
      `19_orientace_v_prostoru` = cbind( rep( names(`19_orientace_v_prostoru`)[3], 2 ), c( "count", "cont" ) ),
      `20_rozpoznavani_emoci` = cbind( rep( names(`20_rozpoznavani_emoci`)[3], 2 ), c( "count", "cont" ) ),
      `21_teorie_mysli` = cbind( rep( names(`21_teorie_mysli`)[3], 2 ), c( "count", "cont" ) )

    )
  )

# collapse all variables of interest into a single data frame
v <- 
  
  lapply(
    
    names(v), # loop through all data sets/tests
    function(i)
      
      v[[i]] %>%
      as.data.frame() %>% # re-format
      mutate( test = i ) %>% # add variable designing the test
      `colnames<-` ( c("variable","type","test") )
    
  ) %>%
  
  do.call( rbind.data.frame, . ) # pull them all together

# save it
saveit( d = list( helpers = list( vars = v ) ) )


# SESSION INFO ----
capture.output( sessionInfo(), file = "import_envir.txt" )

