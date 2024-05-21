# Sensors catalogue
This project involves developing a solution to collect information and metadata about sensors from an Excel file, transform it into SensorML and TTL (following the SOSA and SSN ontologies using R functions), and then translate them into HTML landing pages using XSLT. The system considers type, component type, and instance.

The implementation adheres to the SOSA ontology main version as specified in [SOSA/SSN ontology documentation from 2017](https://www.w3.org/TR/vocab-ssn/) and the [new draft version](https://w3c.github.io/sdw-sosa-ssn/ssn/) from February 2024.

The proposed work is also compliant with the [PIDINST schema](https://docs.pidinst.org/en/latest/white-paper/metadata-schema.html), and through this compliance, it aligns with the DataCite Metadata Schema 4.4 ([mapping available here](https://github.com/rdawg-pidinst/schema/blob/master/schema-datacite.rst))).

An example of using SensorML with the PIDINST schema can be found [here](https://linkedsystems.uk/system/instance/TOOL0022_2490/current/).

```
library(magrittr)
source("info.R")
# info.R is not provided trough this repo because contains the password of services
source("check_man_exist.R")
source("sensors2SensorML/sensors_catalogue.R")
source("SensorML2TTL/sensorML_type_rdf.R")
```

# The two steps of this are:
1. Fill the `sensors_template.xlsx` spreadsheet, including the manufacturer sheet. If the desired manufacturer is not present in the list, add the required information filling the sheet `new_manufacturer`.

2. Use the `sensors_catalogue()` function to obtain the SensorML XML and TTL for the system and, if applicable, for the components of all the sensors described in the `sensors_template.xlsx` spreadsheet;
```
sensors_catalogue(excel_path = "./sensors.xlsx")
```

The output files for each sensors described in the `sensors_template.xlsx` spreadsheet are stored in a folder named using the convention <sensorML_files_system_UUID>. Each folder contains the following:

- An XML file for the system in the `system` folder
- XML files for each system component, if any, in the `components` folder
- TTL files for both the system and components, if any, in the `system_components_files_ttl` folder
- A copy of the system datasheet, if provided, in the `datasheet` folder
- A copy of the system image, if provided, in the `image` folder

The complete process is illustrated in the figure:

<img width="880" alt="Screenshot 2024-05-17 at 12 43 38" src="https://github.com/oggioniale/sensor_catalogue/assets/1393893/d75e1698-6a94-4b01-8177-eab01956175d">

