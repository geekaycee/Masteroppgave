## Kode 1 ##

# Skript for å behandle tilsigsdata
# Dette skriptet fjerner:
# 1. Metadata (første linje)
# 2. Klokkeslett fra datoen
# 3. Negative verdier (-9999)
# 4. Data før 1980.01.01 og etter 2020.12.31

# Definer stier
input_path <- "H:/OneDrive/MASTER/Masteroppgave/Sluttmodellering/Tilsig"
output_path <- "H:/OneDrive/MASTER/Masteroppgave/Sluttmodellering/Tilsig/Processed"

# Opprett output-mappe hvis den ikke eksisterer
if (!dir.exists(output_path)) {
  dir.create(output_path)
  print(paste("Opprettet mappe:", output_path))
}

# Filnavn som skal behandles
filenames <- c("akslen", "nautsundv", "fiskum", "oyungen", "sogne")

# Funksjon for å behandle hver fil
process_file <- function(input_path, output_path, filename) {
  # Konstruer fullstendige filstier
  input_file <- file.path(input_path, filename)
  output_file <- file.path(output_path, paste0(filename, "_processed.csv"))
  
  # Les inn filen
  lines <- readLines(input_file, encoding = "UTF-8")
  
  # Fjern første linje (metadata)
  data_lines <- lines[-1]
  
  # Opprett en tom dataramme for å lagre resultatene
  result <- data.frame(Dato = character(), Tilsig = numeric(), stringsAsFactors = FALSE)
  
  # Definer start- og sluttdato for filtrering
  start_date <- as.Date("1980-01-01")
  end_date <- as.Date("2020-12-31")
  
  # Behandle hver linje
  for (line in data_lines) {
    # Del opp linjen ved semikolon
    parts <- strsplit(line, ";")[[1]]
    
    # Sjekk at linjen har riktig format
    if (length(parts) >= 2) {
      # Hent dato og klokkeslett
      datetime_parts <- strsplit(parts[1], " ")[[1]]
      dato_str <- datetime_parts[1]
      
      # Konverter dato til Date-objekt for sammenligning
      # Sjekk først hvilket datoformat som brukes (DD.MM.YYYY eller YYYY-MM-DD)
      if (grepl("\\.", dato_str)) {
        # Format: DD.MM.YYYY
        dato_parts <- strsplit(dato_str, "\\.")[[1]]
        if (length(dato_parts) == 3) {
          dato_date <- as.Date(paste(dato_parts[3], dato_parts[2], dato_parts[1], sep="-"))
        } else {
          next  # Hopp over hvis datoformatet ikke er som forventet
        }
      } else {
        # Format: YYYY-MM-DD
        dato_date <- as.Date(dato_str)
      }
      
      # Sjekk om datoen er innenfor ønsket tidsperiode
      if (is.na(dato_date) || dato_date < start_date || dato_date > end_date) {
        next  # Hopp over hvis datoen er utenfor ønsket tidsperiode
      }
      
      # Hent tilsig og fjern eventuelle mellomrom
      tilsig <- as.numeric(trimws(parts[2]))
      
      # Sjekk om tilsiget er gyldig (ikke -9999)
      if (!is.na(tilsig) && tilsig != -9999) {
        # Legg til i resultatdatarammen
        result <- rbind(result, data.frame(Dato = dato_str, Tilsig = tilsig, stringsAsFactors = FALSE))
      }
    }
  }
  
  # Skriv resultatet til fil
  write.csv(result, file = output_file, row.names = FALSE)
  
  return(paste("Behandlet", filename, "og lagret til", output_file, 
               "- Fjernet", length(data_lines) - nrow(result), "rader"))
}

# Behandle alle filer
for (filename in filenames) {
  result <- tryCatch({
    process_file(input_path, output_path, filename)
  }, error = function(e) {
    paste("Feil ved behandling av", filename, ":", e$message)
  })
  
  print(result)
}

print("Alle filer er behandlet!")

