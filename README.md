# Sensors catalogue
This includes solution to collect info about sensors (excel file), transform info to SensorML and to TTL (following SOSA and SSN ontologies, by R scripts), and translate them to HTML landing pages by XSLT. System type, componente type, and instance are considered.

The implementation has been done following Physical Sample Curation recommendations of ESIP, SOSA ontology main version https://www.w3.org/TR/vocab-ssn/ (19-10-2017) and the new draft https://w3c.github.io/sdw-sosa-ssn/ssn/ (09-02-2024 - https://github.com/w3c/sdw-sosa-ssn?tab=readme-ov-file)

The proposed work is compliant also with [PIDINST schema](https://docs.pidinst.org/en/latest/white-paper/metadata-schema.html) and, trougth this, with DataCite Metadata Schema 4.4 (mapping is here https://github.com/rdawg-pidinst/schema/blob/master/schema-datacite.rst).
An example of SensorML use PIDINST schema is here: https://linkedsystems.uk/system/instance/TOOL0022_2490/current/

```
library(magrittr)
source("info.R")
# info.R is not provided trough this repo because contains the password of services
source("manufacturers/check_man_exist.R")
source("manufacturers/new_manufacturer.R")
source("sensors2SensorML/sensors_catalogue.R")
source("SensorML2TTL/sensorML_type_rdf.R")
```

# The workflow of this app is:
1. fill the `sensors.xlsx` spreadsheet

2. check if the information about manufacturer is in [triplestore fuseki](http://fuseki1.get-it.it/dataset.html?tab=query&ds=/manufacturers);
if it is not presents:
2.1. execute the `new_manufacturer_ui()` function for create single manufacturer record
in a triple store by web app form;
or
2.2.1. fill the `new_manufacturer.xlsx` file;
2.2.2. execute the `new_manufacturer()` functions for create RDF (both ttl and xml)
version of the record(s) included in the manufacturer excel file;

```
new_manufacturer(
  excel_path = "new_manufacturers.xlsx",
  orcid_creator = "http://orcid.org/0000-0002-7997-219X"
)
```
2.2.3 update GitHub repository `https://github.com/oggioniale/RDF-FOAF-Manufacturer-list/tree/master`
```
path_github <- "/Users/alessandrooggioni/Sites/GitHub/"
repo <- paste0(path_github, "RDF-FOAF-Manufacturer-list/")
```
copy files rdf created to the folder of github repo "RDF-FOAF-Manufacturer-list" ----
```
files <- list.files(path = "./manufacturers", pattern = "\\.rdf$")
file.copy(from = paste0("./manufacturers/", files), to = repo)
# remotes::install_github("ropensci/git2r")
git2r::status(repo = repo)
setwd(repo)
git2r::add(repo = repo, path = list.files(pattern = files))
git2r::commit(repo = repo, message = "new manufacturers")
setwd("../sensors_catalogue/")
```
TODO missing the push!
```
# git2r::push(...)
```
2.2.4. open the terminal and execute git push

3. use `sensors_catalogue()` function for obtain SensorML XML of system and, eventually,
of components for all the sensors described in the excel file;
```
sensors_catalogue(excel_path = "./sensors.xlsx")
```

4. use `sensorML_type_rdf()` function for obtain ttl of system and, eventually, of
components starting from XML file (the output of `sensors_catalogue()` function);
```
sensorML_type_rdf(
  files_path = "./sensorML_files_system_4ce8484c-b9e1-11ee-98e3-daf69f6cfb8a/"
)
```
5. use `sensorML_instance_rdf()` function for obtain ttl version of sensor instance
shared in SOS system.
this function is in the TODO list 

The production flow is illustrated in the figure:

![Screenshot 2024-05-16 at 14 28 46](https://github.com/oggioniale/sensors_catalogue/assets/1393893/1990707f-34d4-4e1a-9fad-b94f9192bd14)


