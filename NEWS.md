# litteR 0.8.1 (2020-07-03)

* added checks for write access and non permissible path names
* improved some error and warning messages
* having duplicated spatial_code/date is no longer an error but
  a warning. This makes spatial aggregation more convenient.
* in type file: renamed WAXPOL group to WAX



# litteR 0.8.0 (2020-06-11)

* simplified version with breaking changes
* added outlier analysis
* added separate litter group analysis section plus figures
* added log-file and more quality control
* dropped baseline analysis and power analysis
* updated manual
* simplified settings files
* simplified input files



# litteR 0.7.0 (2019-12-09)

* added new litter types to OSPAR-groups file
* from now on the line ending of the settings file (YAML) is CR/LF to facilitate 
  MS-Windows users on poor text editors
* changed 'litter_types' to 'litter_types_groups' in settings file
  both are currently valid for backward compatibility.
* refinement of some warning and error messages
* minor improvements of the documentation



# litteR 0.6.6 (2019-11-25)

* added wax and 'other polutants' to 'ospar-groups.csv'
* updated documentation
* better error message in case no data are available due to date selection


# litteR 0.6.5 (2019-10-22)

* added a section to the manual on how to load (attach) litteR for users less 
  familiar with R.


# litteR 0.6.4 (2019-10-16)

* minor textual corrections to please CRAN


# litteR 0.6.3 (2019-10-15)

* Added installation guide (vignette)


# litteR 0.6.2 (2019-10-03)

* Added 'Faeces: In_bags [121]' (SUP) to ospar-groups.csv


# litteR 0.6.1 (2019-10-01)

* Various textual inprovements in tutorial


# litteR 0.6.0 (2019-09-27)

* Assessment analysis has been removed.
* Various textual inprovements


# litteR 0.5.0 (2019-09-16)

* GUI-improvements: now only the data file is needed. The settings file is 
  now always named 'settings.yaml' and the name of the group-file should
  be given in the settings file.
* added check on column delimiter in CSV-input files (needs to be a comma)
* now also ISO 8601:2004 dates (YYYY-mm-dd) are allowed in the
  OSPAR data format
* added additional error messages in case type names are missing in
  the group file and vice versa
* upgraded to tidyr 1.0.0


# litteR 0.4.2 (2019-09-06)

* improved error messages in case litter types and/or groups can't be found
* minor additions to the package vignette
* added Theil-Sen intercept tot output file with summary statistics.
* added start up messages


# litteR 0.4.1 (2019-07-26)

* initial release on CRAN
