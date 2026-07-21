# Taxa targets section.

targets_section_taxa <- function() {
  tar_target(
    name = tax_pre,
    command = {
      prepare_taxa(
        input = fea_pre,
        name_filename = par_pre_tax$names$filename,
        extension = par_pre_tax$names$extension,
        colname = par_pre_tax$names$taxon,
        metadata = par_pre_tax$files$metadata$raw,
        org_tax_ott = lib_mer_org_tax_ott,
        output = par_pre_tax$files$metadata$prepared,
        taxon = par_pre_tax$organisms$taxon
      )
    },
    format = "file"
  )
}
