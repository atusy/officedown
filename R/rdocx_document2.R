#' @export
#' @rdname rdocx_document
rdocx_document2 <- function(mapstyles, ...) {

  output_formats <- bookdown::word_document2(...)

  if( missing(mapstyles) )
    mapstyles <- list()

  output_formats$pre_processor = function(metadata, input_file, runtime, knit_meta, files_dir, output_dir){
    md <- readLines(input_file)
    md <- chunk_macro(md)
    md <- block_macro(md)
    writeLines(md, input_file)
  }

  output_formats$post_processor <- function(metadata, input_file, output_file, clean, verbose) {
    x <- officer::read_docx(output_file)
    x <- process_images(x)
    x <- process_links(x)
    x <- process_embedded_docx(x)
    x <- process_chunk_style(x)
    x <- process_sections(x)
    x <- process_par_settings(x)
    x <- change_styles(x, mapstyles = mapstyles)

    print(x, target = output_file)
    output_file
  }

  output_formats
}


