#' Socio-professional breakdown of the population aged 15+ in Paris, 2011
#'
#' A dataset containing the number of inhabitants by socio-professional category in 2011 in Paris, by IRIS (the French census tract).
#' @details
#' - `C11_POP15P`: _nombre de personnes de 15 ans ou plus_
#' - `C11_POP15P_CS1`: _Agriculteurs exploitants_
#' - `C11_POP15P_CS2`: _Artisans, Commerçants, Chefs d'entreprise_
#' - `C11_POP15P_CS3`: _Cadres et Professions intellectuelles supérieures_
#' - `C11_POP15P_CS4`: _Professions intermédiaires_
#' - `C11_POP15P_CS5`: _Employés_
#' - `C11_POP15P_CS6`: _Ouvriers_
#' - `C11_POP15P_CS7`: _Retraités_
#' - `C11_POP15P_CS8`: _Autres sans activité professionnelle_
#' @md
#' @source INSEE: \url{https://www.insee.fr/fr/statistiques/2028584}
"RP_2011_CS8_Paris"

#' Paris IRIS shapefile, 2013
#'
#' A shapefile of the IRIS (French census tract) of Paris in 2013.
#'
#' @source IGN: \url{https://geoservices.ign.fr/contoursiris}
"ParisIris"

#' Paris polling stations shapefile, 2012
#'
#' A shapefile of the polling stations in Paris in 2012.
#'
#' @source Ville de Paris: \url{https://opendata.paris.fr/explore/dataset/zones-de-rattachement-des-bureaux-de-vote-en-2012/table/}
"ParisPollingStations2012"

#' Voter addresses in the 20th arrondissement of Paris
#'
#' All voter addresses located in the 20th arrondissement of Paris, according to
#' the Répertoire électoral unique (REU). We first extracted polling stations
#' from \code{table-bv-reu.parquet}, and then extracted addresses located within
#' them from \code{table-adresses-reu.parquet}. Addresses were converted into
#' spatial points of class \code{sf}. The weight variable is \code{nb_adresses},
#' although that variable does not count voters per se: it measures the number
#' of voter addresses at that particular location, which is probably the best
#' publicly available proxy at that level of precision.
#'
#' @source INSEE: \url{https://www.data.gouv.fr/datasets/bureaux-de-vote-et-adresses-de-leurs-electeurs}
"Paris20eAddresses"
