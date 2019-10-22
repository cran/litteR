# litteR 0.6.5 (2019-10-22)

* added a section to the manual on how to load (attach) litteR for users less familiar with R.


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
