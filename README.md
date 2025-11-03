# Sensors Catalogue package

This project involves developing a solution to collect information and metadata about sensors from an Excel file, transform it into SensorML and TTL (following the SOSA and SSN ontologies using R functions), and then translate them into HTML landing pages using XSLT. The system considers **type**, **component type**, and **instance**.

The implementation adheres to the SOSA ontology main version as specified in the [SOSA/SSN ontology documentation (2017)](https://www.w3.org/TR/vocab-ssn/) and the [new draft version](https://w3c.github.io/sdw-sosa-ssn/ssn/) from October 2025.

The proposed work is also compliant with the [PIDINST schema](https://docs.pidinst.org/en/latest/white-paper/metadata-schema.html), and through this compliance, it aligns with the [DataCite Metadata Schema 4.4](https://github.com/rdawg-pidinst/schema/blob/master/schema-datacite.rst).

An example of using SensorML with the PIDINST schema can be found [here](https://linkedsystems.uk/system/instance/TOOL0022_2490/current/).

## R Workflow

### Two main steps of the workflow

#### 1. Prepare the input Excel file  
Fill the `sensors_template.xlsx` spreadsheet, including the *Manufacturer* sheet.  
If the desired manufacturer is not present in the list of manufacturers already registered in the Fuseki triple store, the workflow automatically triggers the `sensors_new_manufacturer()` function.  

This function generates a new RDF Turtle (`.ttl`) file containing the metadata description of the missing manufacturer, following the [FOAF ontology](http://xmlns.com/foaf/spec/).  
At this point, the execution of the script **stops** and the generated `.ttl` file must be **manually uploaded** to the Fuseki triple store (in the *manufacturers* dataset).  
Once the new manufacturer record has been successfully loaded, the `sensors_catalogue()` function can be executed again to continue the processing.

⚠️ **Manufacturer check:**  
Before generating SensorML files, the script verifies that all manufacturers listed in your Excel file exist in the Fuseki triple store.
If any are missing, `sensors_new_manufacturer()` automatically generates RDF Turtle files describing them.  
Upload these TTL files to the *manufacturers* dataset in Fuseki, then rerun `sensors_catalogue()` to continue the workflow.

The goal of this collection is to provide an RDF-based catalogue of manufacturers using the Friends-Of-A-Friend (FOAF) ontology, allowing their integration into SensorML metadata and FAIR data workflows.

#### 2. Run the catalogue transformation  
Use the `sensors_catalogue()` function to generate the SensorML XML files for each system and component described in the Excel file.  
After this step, use the `sensorML_type_rdf()` function to generate the corresponding TTL version, derived from the XML files, for each system and component.

Example workflow:

```r
sensors_catalogue(excel_path = "./sensors.xlsx")
sensorML_type_rdf(files_path = "sensorML_files_system_4ce8484c-b9e1-11ee-98e3-daf69f6cfb8a/")
```

## Output structure

For each sensor described in the sensors_template.xlsx file, a dedicated folder is created following the convention:

`sensorML_files_system_<UUID>/`

Each folder contains:

- system/ → the SensorML XML file describing the system
- components/ → one XML file per component, if any
- system_components_files_ttl/ → TTL files derived from SensorML for both system and components
- datasheet/ → a local copy of the datasheet, if provided
- image/ → a local copy of the image, if provided

The complete process is illustrated in the figure:

<img width="880" alt="Screenshot 2024-05-17 at 12 43 38" src="https://github.com/oggioniale/sensor_catalogue/assets/1393893/d75e1698-6a94-4b01-8177-eab01956175d">

## Function Overview

This project includes two core R functions that together implement the complete FAIR workflow for sensor metadata transformation — from Excel metadata to SensorML and RDF/Turtle serialization.

| Step | Function | Input | Output | Description |
|------|-----------|--------|---------|-------------|
| **1** | `sensors_catalogue()` | Excel file (`SensorInfo`, `new_manufacturer`) | System directories and UUIDs | Main controller: reads spreadsheet, validates manufacturers, orchestrates SensorML generation |
| **2** | `sensorML_type_rdf()` | Path to a system folder (`sensorML_files_system_<UUID>/`) | RDF/Turtle (.ttl) files | Converts SensorML XML to RDF following SOSA/SSN and PIDINST ontologies and prepares data for Fuseki ingestion |

---

### `sensors_catalogue()`

**Purpose:**  
Main entry point of the workflow. It reads the Excel file, cleans and groups data, checks manufacturer presence in the Fuseki triple store, and triggers the generation of SensorML XML and TTL files.

**Main operations:**
- Reads the `"SensorInfo"` sheet and filters valid sensor rows.
- Groups entries by sensor name (system + components).
- Checks and registers new manufacturers (`new_manufacturer` sheet).
- For each system:
  - Validates manufacturer existence with `check_man_exist()`;
  - Generates UUIDs for all sensors and components;
  - Produces the SensorML files for systems and components.

**Output:**  
Creates one folder per system containing XML, TTL, and ancillary files.

---

### `sensorML_type_rdf()`

**Purpose:**  
Transforms SensorML XML files (both system and components) into RDF/Turtle format according to the SOSA/SSN and PROV ontologies, aligning with the PIDINST and DataCite metadata model.  
It is the final step that enables the integration of sensor metadata into a triple store such as Apache Jena Fuseki.

**Main operations:**
- Reads the SensorML XML structure from the `system/` and `components/` folders.
- Extracts key metadata (identifier, description, observed properties, capabilities).
- Maps SensorML elements to RDF triples following SOSA, SSN, FOAF, PROV, and Schema.org vocabularies.
- Creates one or more TTL files in the `system_components_files_ttl/` folder.
- Optionally prepares data for upload to a Fuseki triple store.

**Output:**  
Generates RDF/Turtle files describing systems and components, stored under:

`sensorML_files_system_/system_components_files_ttl/`

and optionally ready for loading into:

`http://fuseki1.get-it.it/`

## Integration with RDF and Fuseki

The SensorML XML files produced by this workflow can be transformed into RDF/Turtle using the sensorML_type_rdf() function (SPARQL-Generate based).
This step enriches the metadata following SOSA/SSN ontologies and prepares it for publication in a Fuseki triple store, where triples can be queried and linked to other FAIR resources.

Example usage:

`sensorML_type_rdf(files_path = "sensorML_files_system_<UUID>/")`

## Dependencies

- Core R packages: dplyr, tidyr, readxl, xml2, stringr, uuid, httr2, tools
- Triple store: Apache Jena Fuseki (e.g., http://fuseki1.get-it.it/)
- Controlled vocabularies: NERC Vocabulary Server (collections P06, P10)
- Ontologies: SOSA, SSN, FOAF, PROV-O, DCAT, Schema.org
