// Grapheme-aware span substring utilities.
//
// This module provides methods for extracting substrings from source text using
// Position and Span (line/column based), handling Unicode grapheme clusters correctly.
//
// This is feature-gated because:
// 1. It requires the `unicode-segmentation` dependency
// 2. Most use cases can use `Span::slice()` with byte offsets directly
// 3. Grapheme iteration is more expensive than byte slicing
//
// Enable with: `features = ["internal-span-substring-test"]`
#[cfg(feature = "internal-span-substring-test")]
pub mod span_substring;
