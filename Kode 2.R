## Kode 2 ##

# Samlet kode for behandling av værdata  
# Dette skriptet fjerner data før 1980.01.01 og etter 2020.12.31
# Behandlet alle værdatafilene og aggregert dem til daglige verdier:
# Temperatur: gjennomsnitt over døgnet
# Nedbør: sum over døgnet
# Evaporasjon: sum over døgnet
# Snødybde: gjennomsnitt over døgnet
# Overflateavrenning: sum over døgnet

# Definer stien til mappen med værdata  
vaer_dir <- "H:/OneDrive/MASTER/Masteroppgave/Sluttmodellering/Vaer"  

# Definer stien til output-mappene  
output_dir <- file.path(vaer_dir, "Processed")  
combined_dir <- file.path(vaer_dir, "Combined")  

# Lag en funksjon for å behandle hver værdatafil  
process_weather_data <- function(input_file, output_file, stasjoner, aggregation_method) {  
  # Les inn data  
  df <- read.csv(file.path(vaer_dir, input_file), check.names = FALSE)  
  
  # Konverter tidsstempel til dato  
  df$date <- as.Date(substr(df$time, 1, 10))  
  
  # Filtrer ut data før 1980.01.01 og etter 2020.12.31  
  df <- df[df$date >= as.Date("1980-01-01") & df$date <= as.Date("2020-12-31"), ]  
  
  # Velg kun de kolonnene vi er interessert i  
  selected_cols <- c("date", stasjoner)  
  df_selected <- df[, c("date", stasjoner)]  
  
  # Aggreger data per dag basert på angitt metode  
  if (aggregation_method == "mean") {  
    # Beregn gjennomsnitt per dag  
    result <- aggregate(df_selected[, stasjoner], by = list(Date = df_selected$date), FUN = mean, na.rm = TRUE)  
  } else if (aggregation_method == "sum") {  
    # Beregn sum per dag  
    result <- aggregate(df_selected[, stasjoner], by = list(Date = df_selected$date), FUN = sum, na.rm = TRUE)  
  }  
  
  # Skriv resultatet til fil  
  write.csv(result, file = output_file, row.names = FALSE)  
  
  return(paste("Behandlet", input_file, "og lagret til", output_file))  
}  

# Definer stasjonene vi er interessert i  
stasjoner <- c("akslen", "nautsundv", "fiskum", "oyungen", "sogne")  

# Opprett en mappe for de behandlede filene hvis den ikke eksisterer  
if (!dir.exists(output_dir)) {  
  dir.create(output_dir)  
  print(paste("Opprettet mappe:", output_dir))  
}  

# Behandle temperaturdata (gjennomsnitt over døgnet)  
temp_result <- process_weather_data(  
  "Temperature.csv",   
  file.path(output_dir, "Temperature_processed.csv"),   
  stasjoner,   
  "mean"  
)  
print(temp_result)  

# Behandle nedbørsdata (sum over døgnet)  
precip_result <- process_weather_data(  
  "Total_precipitation_micrometers.csv",   
  file.path(output_dir, "Total_precipitation_processed.csv"),   
  stasjoner,   
  "sum"  
)  
print(precip_result)  

# Behandle evaporasjonsdata (sum over døgnet)  
evap_result <- process_weather_data(  
  "Evaporation_micrometers.csv",   
  file.path(output_dir, "Evaporation_processed.csv"),   
  stasjoner,   
  "sum"  
)  
print(evap_result)  

# Behandle snødybdedata (gjennomsnitt over døgnet)  
snow_result <- process_weather_data(  
  "Snow_depth_millimeters.csv",   
  file.path(output_dir, "Snow_depth_processed.csv"),   
  stasjoner,   
  "mean"  
)  
print(snow_result)  

# Behandle overflateavrenningsdata (sum over døgnet)  
runoff_result <- process_weather_data(  
  "Surface_runoff_micrometers.csv",   
  file.path(output_dir, "Surface_runoff_processed.csv"),   
  stasjoner,   
  "sum"  
)  
print(runoff_result)  

# Opprett en mappe for de kombinerte filene hvis den ikke eksisterer  
if (!dir.exists(combined_dir)) {  
  dir.create(combined_dir)  
  print(paste("Opprettet mappe:", combined_dir))  
}  

# Les inn alle de behandlede filene  
temp_df <- read.csv(file.path(output_dir, "Temperature_processed.csv"))  
precip_df <- read.csv(file.path(output_dir, "Total_precipitation_processed.csv"))  
evap_df <- read.csv(file.path(output_dir, "Evaporation_processed.csv"))  
snow_df <- read.csv(file.path(output_dir, "Snow_depth_processed.csv"))  
runoff_df <- read.csv(file.path(output_dir, "Surface_runoff_processed.csv"))  

# For hver stasjon, kombiner alle værparametrene  
for (stasjon in stasjoner) {  
  # Opprett en dataramme med dato  
  combined_df <- data.frame(Dato = temp_df$Date)  
  
  # Legg til temperatur  
  combined_df$Temperatur <- temp_df[[stasjon]]  
  
  # Legg til nedbør  
  combined_df$Nedbor <- precip_df[[stasjon]]  
  
  # Legg til evaporasjon  
  combined_df$Evaporasjon <- evap_df[[stasjon]]  
  
  # Legg til snødybde  
  combined_df$Snodybde <- snow_df[[stasjon]]  
  
  # Legg til overflateavrenning  
  combined_df$Avrenning <- runoff_df[[stasjon]]  
  
  # Skriv resultatet til fil  
  output_file <- file.path(combined_dir, paste0(stasjon, "_vaerdata.csv"))  
  write.csv(combined_df, file = output_file, row.names = FALSE)  
  
  print(paste("Opprettet kombinert værdatafil for", stasjon, ":", output_file))  
}  

print("Behandling av værdata fullført!")  
