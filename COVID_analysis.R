library(sf)
library(RUMBA)
library(ggplot2)
library(tidyverse)
library(readxl)
library(writexl)
library(leaflet)
library(RColorBrewer)


# Open the database
COVID <- read_excel("COVID.xlsx")

# Georeference addresses using the RUMBA package (vulnerable locations)
COVIDVul <- mutate_USIG_geocode_barrios_vulnerables(COVID, "direccion")

# Remove the 'direccion' and 'barrio vulnerable' columns to avoid duplication later
COVIDVul <- COVIDVul[,-10,]
COVIDVul <- COVIDVul[,-9,]

# Save the rows that were successfully georeferenced
COVIDVul_ok <- COVIDVul %>% filter(!is.na(COVIDVul$lon))

# Save the rows that were NOT georeferenced correctly
COVID_mal <- COVIDVul %>% filter(is.na(COVIDVul$lon))

# Georeference addresses using the RUMBA package (Non-vulnerable locations) using the ones that were not georeferenced correctly
COVIDNoVul <- mutate_USIG_geocode(COVID_mal, "direccion")

# Delete duplicate or unnecessary columns
COVIDNoVul <- COVIDNoVul[,-11,]
COVIDNoVul <- COVIDNoVul[,-10,]
COVIDNoVul <- COVIDNoVul[,-9,]

# Save the rows that were georeferenced correctly
COVIDNoVul_ok <- COVIDNoVul %>% filter(!is.na(COVIDNoVul$lon))

# Save the rows that couldn't be georeferenced with either of the two RUMBA codes
COVID_SinGeoref <- COVIDNoVul %>% filter(is.na(COVIDNoVul$lon))

# Download the previous object to manually correct loading errors
write_xlsx(x = COVID_SinGeoref, path = 'COVID_SinGeoref.xlsx')

# Open the manually corrected database
COVID_corregida <- read_excel("COVID_SinGeoref_manual.xlsx")

# Remove the 'lon.1' and 'lat.1' columns before running RUMBA again to avoid duplication
COVID_corregida <- COVID_corregida[,-10,]
COVID_corregida <- COVID_corregida[,-9,]

# Georeference the non-vulnerable addresses again with RUMBA
COVIDNoVul_corregida <- mutate_USIG_geocode(COVID_corregida, "direccion")

# Remove the "address_normalised" column
COVIDNoVul_corregida <- COVIDNoVul_corregida[,-9,]

# Save the rows that were georeferenced correctly
COVIDNoVul_corregida_ok <- COVIDNoVul_corregida %>% filter(!is.na(COVIDNoVul_corregida$lon))

# Georeference the vulnerable addresses again with RUMBA
COVIDVul_corregida <- mutate_USIG_geocode_barrios_vulnerables(COVID_corregida, "direccion")

# Remove duplicate/unnecessary columns for later filtering
COVIDVul_corregida <- COVIDVul_corregida[,-10,]
COVIDVul_corregida <- COVIDVul_corregida[,-9,]

# Save the rows that were georeferenced correctly
COVIDVul_corregida_ok <- COVIDVul_corregida %>% filter(!is.na(COVIDVul_corregida$lon))

# Change the column names 'lat.1' and 'lon.1' to 'lat' and 'lon' to avoid errors when merging the datasets
names(COVIDNoVul_ok)[9] = "lon"
names(COVIDNoVul_ok)[10] = "lat"

# Merge the four georeferenced datasets into a single dataframe for analysis
COVID_Final <- full_join(COVIDVul_ok, COVIDNoVul_ok)
COVID_Final2 <- full_join(COVIDVul_corregida_ok, COVIDNoVul_corregida_ok)
COVID_TOTAL <- full_join(COVID_Final, COVID_Final2)

# Delete all intermediate dataframes to reach "COVID_TOTAL"
rm(COVID_corregida, COVID_Final, COVID_Final2, COVID_mal,
   COVID_SinGeoref, COVIDNoVul_corregida, COVIDNoVul_corregida_ok,
   COVIDNoVul_ok, COVIDVul, COVIDVul_corregida, COVIDVul_corregida_ok,
   COVIDVul_ok, COVIDNoVul)

# Convert to an 'shape file' object
COVID_TOTAL <- st_as_sf(COVID_TOTAL,
                        coords=c("lon","lat"),
                        crs=4326)

# Open the map files to be used for analysis

# Open the GEOJSON file with CABA neighborhoods' polygons
CABA_Barrios <- st_read('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/barrios/barrios.geojson')

# Open the ShapeFile with vulnerable neighborhoods' polygons
BarVul <- st_read("barrios-populares", stringsAsFactors = FALSE)

# Remove vulnerable neighborhoods that do not belong to CABA
BarVul <- filter(BarVul, provincia == "CIUDAD AUTONOMA DE BUENOS AIRES")

# Manipulate the data to create a ggplot with cases per neighborhood

# See which points of the dataset are inside CABA
COVID_CABA <- st_join(CABA_Barrios, COVID_TOTAL, join=st_intersects)

# Group the data by neighborhood
COVID_CABA_Group <- COVID_CABA %>% group_by(barrio) %>% 
  summarise(Casos = table(barrio))

# Plot the number of cases per CABA neighborhood
ggplot() + 
  geom_sf(data = CABA_Barrios) +
  geom_sf(data = COVID_CABA_Group, aes(fill= Casos)) +
  geom_sf(data = BarVul, fill = "gray") +
  scale_fill_viridis_c() +
  theme(legend.position = "none") +
  theme_minimal() +
  coord_sf(datum = NA) +
  labs(fill = "",
       title = "Number of Cases per Neighborhood", 
       subtitle = "City of Buenos Aires")

# Plot the mapping of cases as points using leaflet

# Merge the datasets of neighborhoods and cases to determine which ones fall within CABA and filter out the ones that don't
COVID_leaflet <- st_join(COVID_TOTAL, CABA_Barrios, join=st_intersects)
COVID_leaflet <- COVID_leaflet %>% filter(!is.na(COVID_leaflet$barrio))

# Create a palette to color the cases by neighborhood
# (the previous file had neighborhoods georeferenced, here we need the points)
COVID_leaflet <- st_join(COVID_TOTAL, CABA_Barrios, join=st_intersects)
COVID_leaflet <- COVID_leaflet %>% filter(!is.na(COVID_leaflet$barrio))

# Create a palette to color the cases by neighborhood
display.brewer.all()
brewer.pal(n = 12, name = "Set3")

pal <- colorFactor(c("#4DAF4A","blue","red","#8DD3C7","#FFFFB3","#BEBADA","#FB8072","#80B1D3","#FDB462","#B3DE69","#FCCDE5","#D9D9D9","#BC80BD","#CCEBC5","#FFED6F"),
                   domain = c("1","2","3","4","5","6","7","8","9","10","11","12","13", "14","15"))        

leaflet(COVID_leaflet) %>%
  addProviderTiles("CartoDB.DarkMatter", group = "Night") %>%
  addProviderTiles("CartoDB.Positron", group = "Day") %>%
  addPolygons(data = BarVul, 
              fill = TRUE,
              stroke = FALSE,
              color = "orange",
              group = "Vulnerable Neighborhoods",
              popup= ~c(nombreBarr),
              popupOptions = popupOptions(closeOnClick = TRUE),
              fillOpacity = 2) %>%
  addCircleMarkers(radius = 2.8,
                   color = ~pal(comuna),
                   stroke = FALSE, 
                   group = "Cases",
                   fillOpacity = 1) %>%
  addLayersControl(
    baseGroups = c("Night", "Day"),
    overlayGroups = c("Vulnerable Neighborhoods", "Cases"),
    options = layersControlOptions(collapsed = FALSE))

# Merge the cases with the census tracts for analysis in GeoDa

# Open the GEOJSON file with CABA census tracts
RadioCens <- st_read('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/informacion-censal-por-radio/caba_radios_censales.geojson')

# Open the GEOJSON file with CABA communes
CABA_Comunas <- st_read('http://cdn.buenosaires.gob.ar/datosabiertos/datasets/comunas/CABA_comunas.geojson')

# Merge CABA dataset with the cases
COVID_Comunas <- st_join(COVID_TOTAL, CABA_Comunas, join=st_intersects)

# Filter only the cases that fall within the CABA polygons
COVID_Comunas <- COVID_Comunas %>% filter(!is.na(COVID_Comunas$COMUNAS))

# Merge the census tracts dataset with the cases in CABA
COVID_Radios <- st_join(RadioCens, COVID_Comunas, join=st_intersects)

# Create an object to group the number of cases per census tract
COVID_Radios_Group <- COVID_Radios %>% group_by(RADIO_ID) %>% 
  summarise(Casos = table(RADIO_ID))

# Re-merge the grouped file with the census tracts dataset
COVID_Radios <- st_join(COVID_Radios_Group, RadioCens, join=st_equals)

# Delete the duplicated column RADIO_ID.y
COVID_Radios <- select(COVID_Radios, -RADIO_ID.y)

# Create a column for inhabitants per household
COVID_Radios <- mutate(COVID_Radios, Hab_Hogar = POBLACION/HOGARES)

# Create a column for population density
COVID_Radios <- mutate(COVID_Radios, Densidad = POBLACION/AREA_KM2)

# Change the column name RADIO_ID.x to RADIO_ID
names(COVID_Radios)[1] = "RADIO_ID"

# Download the file in GEOJSON format for use in GeoDA
write_sf(COVID_Radios, 'COVID_Radios.geojson')