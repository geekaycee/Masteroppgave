## Kode 3 ##

# Kode for å slå sammen tilsigsdata og værdata for hver stasjon  

# Definer stier til mappene  
tilsig_dir <- "H:/OneDrive/MASTER/Masteroppgave/Sluttmodellering/Tilsig/Processed"  
vaer_dir <- "H:/OneDrive/MASTER/Masteroppgave/Sluttmodellering/Vaer/Combined"  
output_dir <- "H:/OneDrive/MASTER/Masteroppgave/Sluttmodellering/Sammenstilt"  

# Definer stasjonene  
stasjoner <- c("akslen", "nautsundv", "fiskum", "oyungen", "sogne")  

# Opprett output-mappen hvis den ikke eksisterer  
if (!dir.exists(output_dir)) {  
  dir.create(output_dir, recursive = TRUE)  
  cat("Opprettet mappe:", output_dir, "\n")  
}  

# Funksjon for å slå sammen data for en stasjon  
kombiner_data <- function(stasjon) {  
  # Les inn tilsigsdata  
  tilsig_fil <- file.path(tilsig_dir, paste0(stasjon, "_processed.csv"))  
  cat("Leser tilsigsdata fra:", tilsig_fil, "\n")  
  tilsig_data <- read.csv(tilsig_fil)  
  
  # Konverter dato-kolonnen til Date-format  
  tilsig_data$Dato <- as.Date(tilsig_data$Dato)  
  
  # Les inn værdata  
  vaer_fil <- file.path(vaer_dir, paste0(stasjon, "_vaerdata.csv"))  
  cat("Leser værdata fra:", vaer_fil, "\n")  
  vaer_data <- read.csv(vaer_fil)  
  
  # Konverter dato-kolonnen til Date-format  
  vaer_data$Dato <- as.Date(vaer_data$Dato)  
  
  # Slå sammen datasettene basert på dato  
  cat("Slår sammen data for", stasjon, "...\n")  
  kombinert_data <- merge(tilsig_data, vaer_data, by = "Dato", all = FALSE)  
  
  # Skriv resultatet til fil  
  output_fil <- file.path(output_dir, paste0(stasjon, "_komplett.csv"))  
  write.csv(kombinert_data, file = output_fil, row.names = FALSE)  
  
  cat("Opprettet kombinert fil for", stasjon, ":", output_fil, "\n")  
  cat("Antall rader i kombinert datasett:", nrow(kombinert_data), "\n")  
  cat("Kolonner i kombinert datasett:", paste(colnames(kombinert_data), collapse = ", "), "\n\n")  
  
  return(kombinert_data)  
}  

# Slå sammen data for hver stasjon  
for (stasjon in stasjoner) {  
  cat("\nBehandler stasjon:", stasjon, "\n")  
  tryCatch({  
    kombinert_data <- kombiner_data(stasjon)  
  }, error = function(e) {  
    cat("Feil ved behandling av", stasjon, ":", conditionMessage(e), "\n")  
  })  
}  

cat("\nSammenstilling av data fullført!\n")  