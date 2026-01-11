// GRCOV_EXCL_START
bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Extensions: usize {
        const UNWRAP_NEWTYPES = 0x1;
        const IMPLICIT_SOME = 0x2;
        const UNWRAP_VARIANT_NEWTYPES = 0x4;
        /// During deserialization, this extension requires that structs' names are stated explicitly.
        const EXPLICIT_STRUCT_NAMES = 0x8;
    }
}
// GRCOV_EXCL_STOP

impl Extensions {
    /// Creates an extension flag from an ident.
    #[must_use]
    pub fn from_ident(ident: &str) -> Option<Extensions> {
        for (name, extension) in Extensions::all().iter_names() {
            if ident == name.to_lowercase() {
                return Some(extension);
            }
        }

        None
    }
}

// GRCOV_EXCL_START
impl Default for Extensions {
    fn default() -> Self {
        Extensions::empty()
    }
}
// GRCOV_EXCL_STOP

#[cfg(test)]
mod tests {
    use super::Extensions;

    #[test]
    fn test_extension_from_ident() {
        assert_eq!(
            Extensions::from_ident("unwrap_newtypes"),
            Some(Extensions::UNWRAP_NEWTYPES)
        );
        assert_eq!(
            Extensions::from_ident("implicit_some"),
            Some(Extensions::IMPLICIT_SOME)
        );
        assert_eq!(
            Extensions::from_ident("unwrap_variant_newtypes"),
            Some(Extensions::UNWRAP_VARIANT_NEWTYPES)
        );
        assert_eq!(
            Extensions::from_ident("explicit_struct_names"),
            Some(Extensions::EXPLICIT_STRUCT_NAMES)
        );
        assert_eq!(Extensions::from_ident("unknown"), None);
    }
}
