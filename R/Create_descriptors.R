#' Create rdkit descriptors
#'
#'Based on a .xlsx file of specific structure and a subset of rdkit descriptors, this function 
#' returns the values of the specific descriptors for all requested molecules
#'
#' @param file A .xlsx file, with two columns: a 'Name' column and a 'SMILES' column, containing
#' the molecule names and SMILES representations, respectively
#' @param descriptors A vector containing the desired descriptors. This argument is optional. If not
#' provided, then the function returns the complete list of generated descriptors
#' @return  A vector of rcdk descriptors
#' @details The user can generate rcdk descriptors using the
#'  function \code{create.descriptors()}. Additionally, a subset of the complete rcdk descriptor
#'  list can be returned leveraging the argument 'descriptors'. Note that in order for this function to
#'  be excecuted, the following libraries need to be installed: rJava, rlang, Rcpp
#'  openxlsx, rsvg, rcdk, ChemmineR, zlibbioc, BiocGenerics and ChemmineOB
#'  
#' @examples
#'  \dontrun{
#'  #tree.model <- create.descriptors("SMILES.xlsx", descriptors = c("VP.7", "MDEC.34","VPC.4","fragC"))
#' }
#'
#' @export
create.descriptors <- function(file_name,  descriptors =NULL){
  
  # Define a vector containing the names of the required libraries
  required_libraries <- c("rJava", "rlang", "Rcpp", "openxlsx", "rsvg", 
                          "rcdk", "ChemmineR", "zlibbioc", "BiocGenerics", "ChemmineOB")
  
  # Check for the existence of each required library
  missing_libraries <- required_libraries[!sapply(required_libraries, function(lib) requireNamespace(lib, quietly = TRUE))]
  
  # If any libraries are missing, stop execution and raise an error
  if (length(missing_libraries) > 0) {
    stop(paste("The following libraries need to be installed:", paste(missing_libraries, collapse = ", ")))
  } 
  
  # Read SMILES data from an Excel file named 'SMILES_demo.xlsx' and extract the 'SMILES' column
  mol <- openxlsx::read.xlsx(file_name, sheet = "SMILES") ##PFOA smiles
  
  # Convert SMILES strings to SDF format
  mol.sdf <- ChemmineR::smiles2sdf(mol[, "SMILES"])
  
  # Set compound IDs in SDF data from the 'Name' column
  ChemmineR::cid(mol.sdf) <- mol[, "Name"]
  
  # Ensure compound IDs are unique
  ChemmineR::cid(mol.sdf) <- ChemmineR::makeUnique(ChemmineR::cid(mol.sdf))
  
  # Write SDF data to a file named 'mol.sdf'
  ChemmineR::write.SDF(mol.sdf, "mol.sdf", cid = TRUE)
  
  # Read SDF data from the file 'mol.sdf'
  extra_mol.sdf <- ChemmineR::read.SDFset("mol.sdf")
  
  # Ensure compound IDs in the additional SDF data are unique
  ChemmineR::cid(extra_mol.sdf) <- ChemmineR::makeUnique(ChemmineR::cid(extra_mol.sdf))
  
  # Regenerate coordinates for the molecules
  extra_molset2 <- ChemmineR::regenerateCoords(extra_mol.sdf)
  
  # Generate 3D coordinates for the molecules
  sdf3D <- ChemmineR::generate3DCoords(extra_molset2)
  
  # Write the 3D coordinates to a new SDF file named 'extra_molset3.sdf'
  ChemmineR::write.SDF(sdf3D, "extra_molset3.sdf", cid = TRUE)
  
  # Load molecules from the SDF file 'extra_molset3.sdf'
  MOL <- rcdk::load.molecules("extra_molset3.sdf")
  
  # Get a list of all available descriptors
  allDescr <- unique(unlist(sapply(rcdk::get.desc.categories(), rcdk::get.desc.names)))
  
  # Evaluate the descriptors for the loaded molecules, dividing them into groups
  MOL.descrsa <- rcdk::eval.desc(MOL, allDescr[1:10])
  MOL.descrsb <- rcdk::eval.desc(MOL, allDescr[11:20])
  MOL.descrsc <- rcdk::eval.desc(MOL, allDescr[21:30])
  MOL.descrsd <- rcdk::eval.desc(MOL, allDescr[31:35])
  MOL.descrse <- rcdk::eval.desc(MOL, allDescr[36])
  MOL.descrse1 <- rcdk::eval.desc(MOL, allDescr[37])
  MOL.descrse2 <- rcdk::eval.desc(MOL, allDescr[38])
  MOL.descrse3 <- rcdk::eval.desc(MOL, allDescr[39:40])
  MOL.descrsf <- rcdk::eval.desc(MOL, allDescr[41:51])
  
  # Combine the evaluated descriptors into a single matrix
  MOL.descrs <- cbind(MOL.descrsa, MOL.descrsb, MOL.descrsc, MOL.descrsd, MOL.descrse,
                      MOL.descrse1, MOL.descrse2, MOL.descrse3, MOL.descrsf)
  
  # Select the desired computational descriptors 
  computational_descriptors <- MOL.descrs[, descriptors]
  
  return(computational_descriptors)
  
}
