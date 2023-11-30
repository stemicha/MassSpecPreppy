
## Version 1.1.1

- add additional error messages for the digest module, when the provided sample table have missing values in volume or protein concentration column

## Version 1.1.0

-   BCA assay: addition of take3 BCA assay for low sample volumes
    -   using max. 50µl and min. 25µl as total dilution volume
    -   allowing dilution up to 50 fold
    -   sample volume for dilution is calculated in balance (e.g. 5 µl in 25µl total volume = 5 fold dilution / 2 µl in 50 µl total volume = 25 fold dilution)
-   BCA assay: adding hard coded standard curve in the analysis for a better comparison of the measurement
-   BCA assay: show error when sample volume left after dilution is to low; also hide download bar in that case
-   BCA assay: adding the sample volume and buffer volumes to the html file for a more comprehensive documentation
-   BCA assay: adding pipetting quality to the table for a better anticipation of the pipetting accuracy and precision
-   digest: loading Evotip now include touch tip command during the distributions to avoid droplet carryover during washing step

## Version 1.0.0

-   initial release
-   described in: Reder, A., Hentschker, C., Steil, L., Gesell Salazar, M., Hammer, E., Dhople, V. M., Sura, T., Lissner, U., Wolfgramm, H., Dittmar, D., Harms, M., Surmann, K., Völker, U., & Michalik, S. (2023). MassSpecPreppy---An end-to-end solution for automated protein concentration determination and flexible sample digestion for proteomics applications. Proteomics, 00, e2300294. https://doi.org/10.1002/pmic.202300294
