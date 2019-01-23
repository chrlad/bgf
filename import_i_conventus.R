#####################################################################################
###########  conventus konventioner  ################################################
#####################################################################################
##
##
## Filen skal have endelsen ".csv" og skal være "Semikolonsepareret".

## Felt A: Dato (dd-mm-åååå) -> dato
## Felt B: Bilag -> bilag
## Felt C: Tekst -> tekst
## Felt D: Debet kontotype (se nedenfor) -> deb_knt_type
## Felt E: Debet kontonummer -> deb_knt_no
## Felt F: Debet medlemsid -> deb_mdl_id
## Felt G: Debet gruppeid -> deb_grp_id
## Felt H: Debet udligner (Opkrævningsid) -> deb_udl
## Felt I: Debet finansafdeling -> deb_afd
## Felt J: Kredit kontotype (se nedenfor) -> kre_knt_type
## Felt K: Kredit kontonummer -> kre_knt_no
## Felt L: Kredit medlemsid -> kre_mdl_id
## Felt M: Kredit gruppeid -> kre_grp_id
## Felt N: Kredit udligner (Opkrævningsid) -> kre_udl
## Felt O: Kredit finansafdeling -> kre_afd
## Felt P: Beløb (Dansk format) -> beloeb
## Felt Q: Momskonto -> momsknt
## Felt R: Debet udligner (Faktura nr.) ->  deb_udl_no
## Felt S: Kredit udligner (Faktura nr.) -> kre_udl_no

###########################################################################################
#export <- c("dato", "bilag", "tekst", "deb_knt_type", "deb_knt_no", "deb_mdl_id", "deb_grp_id", "deb_udl", "deb_afd",
#          "kre_knt_type", "kre_knt_no", "kre_mdl_id", "kre_grp_id", "kre_udl", "kre_afd")

#libraries
library(readr)
library(dplyr)
library(stringr)

###############   import files    #########################################################
###########################################################################################

#set paths
folder_path = file.path("/Users","dkchlala","projects", "bgf_program","import")
export_path = file.path("/Users","dkchlala","projects", "bgf_program","export")

#filename
filename = list.files(path = folder_path, pattern = "*.csv")
for(files in filename){
  fil = paste(folder_path, files, sep="/")
  print(paste0("processing: ", fil))  
  export_filname <- paste0("conv_", files)
  
  export_fil <- paste(export_path, export_filname, sep = "/")
  
  import_fil <- as.data.frame(read_delim(fil, 
                                         ";", escape_double = FALSE, col_names = FALSE, 
                                         col_types = cols(X4 = col_character(), 
                                                          X5 = col_character()), trim_ws = TRUE, 
                                         locale = locale(encoding = "ISO-8859-1")))

#  file.remove(fil)
  
  #set names
  names(import_fil) = c("bogfoert", "tekst", "rentedato", "beloeb", "saldo")
  
  #set accountnumber
  if(str_sub(files,1,3) == '551'){
    acc = '5030'}
  if(str_sub(files,1,3) == '914'){
    acc = '5035'}
  if(str_sub(files,1,3) == '735'){
    acc = '5040'}
  
  #prepare data
  export <- import_fil %>%
    filter(substr(tekst, 1, 12) != "Dankort-salg") %>%
    mutate(dato = gsub("-", "/", bogfoert), bilag='', tekst = tekst, deb_knt_type=1, deb_knt_no=ifelse(beloeb > 0, acc, ''), 
           deb_mdl_id='', deb_grp_id='', deb_udl = 0, deb_afd='', kre_knt_type= 1, kre_knt_no=ifelse(beloeb < 0, acc, ''), 
           kre_mdl_id='', kre_grp_id='', kre_udl = 0, kre_afd = '', beloeb = if_else(beloeb > 0, beloeb, substring(beloeb,2)), 
           momsknt='', deb_udl_no = 0, kre_udl_no=0 ) %>% 
    select(dato, bilag, tekst, deb_knt_type, deb_knt_no, deb_mdl_id, deb_grp_id, deb_udl, deb_afd, 
           kre_knt_type, kre_knt_no, kre_mdl_id, kre_grp_id, kre_udl, kre_afd, beloeb, momsknt, deb_udl_no, kre_udl_no) %>% 
    arrange(dato)
  
  #save export fil
  write.table(export, file = export_fil, row.names = FALSE, col.names = FALSE, quote = FALSE ,sep = ";")
} 



