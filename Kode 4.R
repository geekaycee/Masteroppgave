## Kode 4 ## 

# Kode for å slå sammen tilsigsdata med vind- og soldata

# Koden henter ut vind- og soldata fra aktuelle datasett
# Koden setter inn nye kollonner inn tidligere sammenstilge datasett
# Koden lager 5 nye, komplette filer 

# Definer stier til mappene  
sammenstilt_dir <- "H:/OneDrive/MASTER/Masteroppgave/Sluttmodellering/Sammenstilt"  
output_dir <- "H:/OneDrive/MASTER/Masteroppgave/Sluttmodellering/Komplett"  

# Definer stasjonene og deres kommunenumre  
stasjoner <- c("akslen", "nautsundv", "fiskum", "oyungen", "sogne")  
kommune_numre <- c("3434", "4646", "3048", "5007", "4204")  
stasjon_til_kommune <- setNames(kommune_numre, stasjoner)  

# Les inn vind- og soldata  
wind_data <- read.csv("H:/OneDrive/MASTER/Masteroppgave/Sluttmodellering/VindogSol/WS_Nordic_onshore_SP199_150.csv")  
solar_data <- read.csv("H:/OneDrive/MASTER/Masteroppgave/Sluttmodellering/VindogSol/GHI.csv")  

# Konverter tidsstempel til Date-format  
wind_data$time <- as.POSIXct(wind_data$time, format="%Y-%m-%d %H:%M:%S")  
solar_data$time <- as.POSIXct(solar_data$time, format="%Y-%m-%d %H:%M:%S")  

# Opprett output-mappen hvis den ikke eksisterer  
if (!dir.exists(output_dir)) {  
  dir.create(output_dir, recursive = TRUE)  
  cat("Opprettet mappe:", output_dir, "\n")  
}  

# Funksjon for å slå sammen data for en stasjon  
kombiner_data <- function(stasjon, kommune_nr) {  
  cat("\nBehandler stasjon:", stasjon, "med kommunenummer:", kommune_nr, "\n")  
  
  # Les inn sammenstilt data  
  sammenstilt_fil <- file.path(sammenstilt_dir, paste0(stasjon, "_komplett.csv"))  
  cat("Leser sammenstilt data fra:", sammenstilt_fil, "\n")  
  sammenstilt_data <- read.csv(sammenstilt_fil)  
  sammenstilt_data$Dato <- as.Date(sammenstilt_data$Dato)  
  
  # Forbered vind- og soldata for denne stasjonen  
  cat("Forbereder vind- og soldata for", stasjon, "...\n")  
  
  # Hent vinddata for kommunen  
  vind_kolonne <- paste0("X", kommune_nr)  
  vind_data <- wind_data[, c("time", vind_kolonne)]  
  colnames(vind_data) <- c("Tidspunkt", "Vindstyrke")  
  
  # Hent soldata for kommunen  
  sol_kolonne <- paste0("X", kommune_nr)  
  sol_data <- solar_data[, c("time", sol_kolonne)]  
  colnames(sol_data) <- c("Tidspunkt", "Solstraaling")  
  
  # Legg til dato-kolonne for å forenkle sammenslåing  
  vind_data$Dato <- as.Date(vind_data$Tidspunkt)  
  sol_data$Dato <- as.Date(sol_data$Tidspunkt)  
  
  # Beregn daglige gjennomsnitt for vind og sol  
  cat("Beregner daglige gjennomsnitt for vind og sol...\n")  
  daglig_vind <- aggregate(Vindstyrke ~ Dato, data = vind_data, FUN = mean)  
  daglig_sol <- aggregate(Solstraaling ~ Dato, data = sol_data, FUN = mean)  
  
  # Slå sammen med sammenstilt data  
  cat("Slår sammen data...\n")  
  resultat <- merge(sammenstilt_data, daglig_vind, by = "Dato", all.x = TRUE)  
  resultat <- merge(resultat, daglig_sol, by = "Dato", all.x = TRUE)  
  
  # Skriv resultatet til fil med riktig tegnkoding  
  output_fil <- file.path(output_dir, paste0(stasjon, "_komplett_med_vind_sol.csv"))  
  write.csv(resultat, file = output_fil, row.names = FALSE, fileEncoding = "UTF-8")  
  
  cat("Opprettet komplett fil for", stasjon, ":", output_fil, "\n")  
  cat("Antall rader i komplett datasett:", nrow(resultat), "\n")  
  cat("Kolonner i komplett datasett:", paste(colnames(resultat), collapse = ", "), "\n\n")  
  
  return(resultat)  
}  

# Slå sammen data for hver stasjon  
for (stasjon in stasjoner) {  
  kommune_nr <- stasjon_til_kommune[stasjon]  
  tryCatch({  
    komplett_data <- kombiner_data(stasjon, kommune_nr)  
  }, error = function(e) {  
    cat("Feil ved behandling av", stasjon, ":", conditionMessage(e), "\n")  
  })  
}  

cat("\nSammenstilling av data fullført!\n")  

head(komplett_data)

test <- "H:/OneDrive/MASTER/Masteroppgave/Sluttmodellering/Komplett/akslen_komplett_med_vind_sol.csv"
head (test)
