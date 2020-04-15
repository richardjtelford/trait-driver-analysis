## download data

##TODO -
# trigger on osf change TRAIT COMMUNITY CLIMATE

download_plan <- drake_plan(
  #community
  community_download = target(
    get_file(
      node = "f3knq",
      remote_path = "Community",
      file = "transplant.sqlite",
      path = "data"
    ),
    format = "file",
    trigger = trigger(
      condition = need_update(
        node = "f3knq",
        remote_path = "Community",
        file = "transplant.sqlite",
        path = "data"
      )
    )
  ),
  
  #import trait data
  traits_leaf_download = target(
    get_file(
      node = "f3knq",
      remote_path = "Traits",
      file = "PFTC1.2_China_2015_2016_LeafTraits.csv",
      path = "data"
    ),
    format = "file",
    trigger = trigger(
      condition = need_update(
        node = "f3knq",
        remote_path = "Traits",
        file = "PFTC1.2_China_2015_2016_LeafTraits.csv",
        path = "data"
      )
    )
  ),
  
  traits_chemical_download = target(
    get_file(
      node = "f3knq",
      remote_path = "Traits",
      file = "PFTC1.2_China_2015_2016_ChemicalTraits.csv",
      path = "data"
    ),
    format = "file",
    trigger = trigger(
      condition = need_update(
        node = "f3knq",
        remote_path = "Traits",
        file = "PFTC1.2_China_2015_2016_ChemicalTraits.csv",
        path = "data"
      )
    )
  ),
  
  #import environmental data
  env_download = target(
    get_file(
      node = "4hjzu",
      remote_path = ".",
      file = "climate_month.Rdata",
      path = "data"
    ),
    format = "file",
    trigger = trigger(
      condition = need_update(
        node = "4hjzu",
        remote_path = ".",
        file = "climate_month.Rdata",
        path = "data"
      )
    )
  )
)
