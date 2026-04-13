library(arrow)

files <- list.files("brt_projections_by_rock", 
                    pattern = "future_projections_300res\\.rds$", 
                    full.names = TRUE)

for (f in files) {
  out <- gsub("\\.rds$", ".parquet", f)
  write_parquet(readRDS(f), out)
  cat("Converted:", basename(f), "\n")
}
