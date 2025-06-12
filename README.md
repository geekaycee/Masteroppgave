# Statistisk modellering av vannkrafttilsig basert på historiske værdata i Norge

Dette repositoriet inneholder R-kode knyttet til masteroppgaven.

## Innhold og bruk

### 📁 Datasammenstilling
Følgende filer benyttes til å forberede og sammenstille data som senere brukes i modellering og analyser:

- **Kode 1.R**  
- **Kode 2.R**  
- **Kode 3.R**  
- **Kode 4.R**  

Disse skriptene dekker ulike steg i datasammenstillingen, som er detaljert beskrevet i underkapittel 3.2

### 📈 Modellering og validering
- **Sogne.R**  
  Inneholder alt som er nødvendig for å modellere én stasjon (Søgne), inkludert estimering av en statistisk modell, validering, samt sammenligning med HBV-modellen.

### 🌍 Klimascenarioanalyse
- **Klimascenario.R**  
  Inneholder kode for å utføre klimascenarioanalyse for én stasjon. Skriptet tar utgangspunkt i værdata for referanseperioden og scenario-data for år 2100 (RCP8.5), og produserer sesonsvise tilsigestimater.

---

## Filoversikt


Klimascenario.R         # Klimascenarioanalyse for én stasjon  
Kode 1.R                # Datasammenstilling, del 1  
Kode 2.R                # Datasammenstilling, del 2  
Kode 3.R                # Datasammenstilling, del 3  
Kode 4.R                # Datasammenstilling, del 4  
LICENSE                 # Lisens for bruk  
Sogne.R                 # Modellering og HBV-sammenligning for Søgne  
