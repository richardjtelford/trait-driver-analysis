## download data

##TODO - 
# trigger on osf change TRAIT COMMUNITY CLIMATE

#function to download from osf and return file path
download <- function(node, remote_path = ".", file, path = "data"){
  get_file(node = node, remote_path = remote_path, file = file, path = path)
  file.path(path, file)
  } 


download_plan <- drake_plan(
  
  #community
  community_download = target(
    download(node = "f3knq", remote_path = "Community", file = "transplant.sqlite"),
    format = "file"), 
  
  #import trait data
  traits_leaf_download = target(
    download(node = "f3knq", remote_path = "Traits", file = "PFTC1.2_China_2015_2016_LeafTraits.csv"),
    format = "file"),
  
  traits_chemical_download = target(
    download(node = "f3knq", remote_path = "Traits", file = "PFTC1.2_China_2015_2016_ChemicalTraits.csv"), 
    format = "file"),

  #import environmental data
  env_download = target(
    download(node = "4hjzu", remote_path = ".", file = "climate_month.Rdata"), 
    format = "file")
)
