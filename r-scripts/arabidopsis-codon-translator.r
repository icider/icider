plant_codon_dict <- list(
  A = "GCT",
  C = "TGC",  # Converted U -> T
  D = "GAT",
  E = "GAG",
  F = "TTC",
  G = "GGA",
  H = "CAC",
  I = "ATT",
  K = "AAG",
  L = "CTC",
  N = "AAC",
  P = "CCA",
  Q = "CAA",
  R = "AGA",
  S = "TCT",
  T = "ACA",
  V = "GTT",
  Y = "TAC",
  M = "ATG",
  W = "TGG"
)

protein_to_dna <- function(protein_seq, codon_dict) {
  protein_seq <- toupper(strsplit(protein_seq, "")[[1]])
  dna_seq <- sapply(protein_seq, function(aa) {
    codon <- codon_dict[[aa]]
    if (is.null(codon)) stop(paste("Unknown amino acid:", aa))
    return(codon)
  })
  return(paste(dna_seq, collapse = ""))
}

# Example usage
protein_seq <- "LEIEAAFLEQENTALETEVAELEQEVQRLENIVSQYETRYGPLGGGK"
dna_seq <- protein_to_dna(protein_seq, plant_codon_dict)
cat("DNA sequence:\n", dna_seq, "\n")