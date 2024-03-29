#' Create rdkit descriptors
#'
#'Based on a .xlsx file of specific structure and a subset of rdkit descriptors, this function 
#' returns the values of the specific descriptors for all requested molecules
#'
#' @param file A .xlsx file, with two columns: a 'Name' column and a 'SMILES' column, containing
#' the molecule names and SMILES representations, respectively
#' @param descriptors A vector containing the desired descriptors. This argument is optional. If not
#' provided, then the function returns the complete list of generated descriptors
#' @details The user can upload on Jaqpot a model that has been trained using the
#'  function \code{tree()} of package 'tree'. Apart from the model object, the user is requested
#'  to provide further information (i.e. Jaqpot API key or credentials, model title and short
#'  description etc.) via prompt messages. If the upload process is successful,
#'  the user is given a unique model id key.
#'
#' @examples
#'  \dontrun{
#'  #tree.model <- tree(y~x, data=df)
#'  #deploy.tree(tree.model)
#' }
#'
#' @export
deploy.tree <- function(file,  descriptors =NULL){
  mol <- openxlsx::read.xlsx('SMILES_demo.xlsx',sheet = "SMILES") ##PFOA smiles
  mol.sdf<- ChemmineR::smiles2sdf(mol[,"SMILES"])
  ChemmineR::cid(mol.sdf) <- mol[,"Name"]
  ChemmineR::cid(mol.sdf) <- ChemmineR::makeUnique(ChemmineR::cid(mol.sdf))
  ChemmineR::write.SDF(mol.sdf, "mol.sdf",cid=TRUE)
  extra_mol.sdf<- ChemmineR::read.SDFset("mol.sdf")
  ChemmineR::cid(extra_mol.sdf) <- ChemmineR::makeUnique(ChemmineR::cid(extra_mol.sdf))
  extra_molset2 = ChemmineR::regenerateCoords(extra_mol.sdf)
  sdf3D = ChemmineR::generate3DCoords(extra_molset2)
  ChemmineR::write.SDF(sdf3D, "extra_molset3.sdf",cid=TRUE)
  
  MOL<-rcdk::load.molecules("extra_molset3.sdf")
  allDescr <- unique(unlist(sapply(rcdk::get.desc.categories(), rcdk::get.desc.names)))
  MOL.descrsa <- rcdk::eval.desc(MOL, allDescr[1:10])
  MOL.descrsb <- rcdk::eval.desc(MOL, allDescr[11:20])
  MOL.descrsc <- rcdk::eval.desc(MOL, allDescr[21:30])
  MOL.descrsd <- rcdk::eval.desc(MOL, allDescr[31:35])
  MOL.descrse <- rcdk::eval.desc(MOL, allDescr[36])
  MOL.descrse1 <- rcdk::eval.desc(MOL, allDescr[37])
  MOL.descrse2 <- rcdk::eval.desc(MOL, allDescr[38])
  MOL.descrse3 <- rcdk::eval.desc(MOL, allDescr[39:40])
  MOL.descrsf <- rcdk::eval.desc(MOL, allDescr[41:51])
  MOL.descrs<-cbind(MOL.descrsa,MOL.descrsb,MOL.descrsc,MOL.descrsd,MOL.descrse,
                    MOL.descrse1, MOL.descrse2,MOL.descrse3,MOL.descrsf)
  computational_descriptors <- MOL.descrs[, descriptors]
  return(computational_descriptors)
  
}
