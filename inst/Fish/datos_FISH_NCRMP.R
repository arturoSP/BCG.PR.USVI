#datos peces del NCRMP

PR_raw <- read_csv("National CRMP PR peces.csv") # datos Puerto Rico
USVI_raw <- read_csv("National CRMP USVI peces.csv") # DAtos USVI
temp1 <- bind_rows(PR_raw, USVI_raw)


## Extracción de parámetros de relación talla~peso a datos del NCRMP usando rfishbase

# Especies de interés para buscar en fish base
species <- temp1|>
  filter(LEN >= 1)|>
  group_by(SCIENTIFIC_NAME)|>
  summarise(n = n())

# Lista para albergar modelos
sp.list <- vector(mode = "list", length = length(species$SCIENTIFIC_NAME))
for(i in 1:length(sp.list)){
  xx <- length_weight(species$SCIENTIFIC_NAME[i])
  sp.list[[i]] <- xx
}

# Selección de modelos por cada especie y tabla de búsqueda para datos
temp2 <- bind_rows(sp.list)|>
  group_by(Species)|>
  filter(CoeffDetermination >= max(CoeffDetermination))|>
  summarise(across(a:b, ~ mean(.x, na.rm = TRUE)))|>
  rename(SCIENTIFIC_NAME =  Species)
 

## Función para identificar a y b especie, y determinar biomasa con ecuación W = a * L ^ b

weight <- function(SCIENTIFIC_NAME, LEN, ab){
  ab <- ab
  xx <- ab|>
  filter(Species == SCIENTIFIC_NAME)
    w <- xx$a * (LEN^ xx$b)
  return(w)
  } # función escrita en el avion sin acceso a internet, probar con conexión.


temp3 <- temp1|>
  filter(LEN >= 1)|>
  left_join(temp2)

temp4 <- temp3|>
  mutate(biomass = a * (LEN^b))|>
  select(YEAR, MONTH, DAY, REGION_DESCRIPTION, SCIENTIFIC_NAME, biomass)|>
  pivot_wider(names_from = SCIENTIFIC_NAME, 
              values_from = biomass,
              values_fn = ~ mean(.x, na.rm = TRUE),
              values_fill = 0)|>
  mutate(" " = " ",
       ISLAND = factor(REGION_DESCRIPTION, levels = c("Puerto Rico", 
                                                      "St. Croix - U.S. Virgin Islands" ,
                                                      "St. Thomas and St. John - U.S. Virgin Islands"),
                                           labels = c("PUERTO RICO", 
                                                      "ST. CROIX",
                                                      "ST. THOMAS/ST. JOHN")))|>
  select(-REGION_DESCRIPTION)|>
  relocate(YEAR:DAY, .after = ISLAND)|>
  mutate(label = str_c(ISLAND, YEAR, MONTH, DAY, sep = "-"))|>
  relocate(label, .before = `Apogon binotatus`)

write.csv(temp4, file = "fish_PRIMER_ncrmp.csv", na = "0", row.names = F)


temp5 <- temp3|>
  filter(SCIENTIFIC_NAME == "Apogon binotatus")

# Biomasa con base de datos generada manualmente (Fish_Biomass12-03-24.xlsx)

fish_biomass <- read_excel("Fish_Biomass12-03-24.xlsx")|> # datos de biomasa por talla
  mutate(SCIENTIFIC_NAME = str_c(Genera, Species, sep = " "))|>
  relocate(SCIENTIFIC_NAME, .before = `Biomass (<5)`)

