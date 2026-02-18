// Patch for c_codegen.rs to add namespace prefix collision avoidance
//
// This module provides the logic for detecting and resolving type name collisions
// by adding module prefixes or numerical suffixes to colliding type names.

use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeKind {
    Struct,
    Enum,
    TaggedUnion,
}

/// Tracks type names and detects/resolves collisions
pub struct TypeNameRegistry {
    /// Map from original Blitz type name to all C names generated for it
    /// e.g., "Assignment" -> [("Assignment", Struct, "ast"), ("Assignment_1", Enum, "parser")]
    type_instances: HashMap<String, Vec<(String, TypeKind, String)>>,
    /// Map from C name back to original name
    c_to_original: HashMap<String, String>,
}

impl TypeNameRegistry {
    pub fn new() -> Self {
        Self {
            type_instances: HashMap::new(),
            c_to_original: HashMap::new(),
        }
    }

    /// Register a type and get its C name.
    /// If a collision is detected (same name, different kind OR same kind but different module),
    /// returns a suffixed/prefixed name.
    /// The `module` parameter is the module name (e.g., "ast", "hir", "parser").
    pub fn register_type(&mut self, original_name: &str, kind: TypeKind, module: &str) -> String {
        let instances = self
            .type_instances
            .entry(original_name.to_string())
            .or_insert_with(Vec::new);

        // Check if we already have this exact type (same name, kind, AND module)
        if let Some((c_name, _, _)) = instances.iter().find(|(_, k, m)| *k == kind && m == module) {
            return c_name.clone();
        }

        // Check if there's a collision
        let c_name = if instances.is_empty() {
            // First instance - use original name
            original_name.to_string()
        } else {
            // Collision detected - prefix with module name for disambiguation
            format!("{}_{}", module, original_name)
        };

        instances.push((c_name.clone(), kind, module.to_string()));
        self.c_to_original
            .insert(c_name.clone(), original_name.to_string());

        c_name
    }

    /// Get the C name for a type reference (used when types reference each other)
    /// If the type hasn't been registered yet, returns the original name.
    /// If there's no collision, returns the sole registration.
    /// If there are collisions, returns the first (default) registration.
    pub fn get_c_name(&self, original_name: &str) -> String {
        if let Some(instances) = self.type_instances.get(original_name) {
            instances
                .first()
                .map(|(name, _, _)| name.clone())
                .unwrap_or_else(|| original_name.to_string())
        } else {
            original_name.to_string()
        }
    }

    /// Get the C name for a type with a specific module.
    /// This is the preferred lookup method when module context is available.
    pub fn get_c_name_for_module(&self, original_name: &str, module: &str) -> Option<String> {
        if let Some(instances) = self.type_instances.get(original_name) {
            instances
                .iter()
                .find(|(_, _, m)| m == module)
                .map(|(name, _, _)| name.clone())
        } else {
            None
        }
    }

    /// Check if a type has collisions
    pub fn has_collision(&self, original_name: &str) -> bool {
        self.type_instances
            .get(original_name)
            .map(|v| v.len() > 1)
            .unwrap_or(false)
    }

    /// Get all instances of a type (for debugging)
    pub fn get_instances(&self, original_name: &str) -> Option<&[(String, TypeKind, String)]> {
        self.type_instances.get(original_name).map(|v| v.as_slice())
    }

    /// Get the C name for a type with a specific kind
    /// This is used when we need to disambiguate between colliding type names
    pub fn get_c_name_for_kind(&self, original_name: &str, kind: TypeKind) -> Option<String> {
        if let Some(instances) = self.type_instances.get(original_name) {
            instances
                .iter()
                .find(|(_, k, _)| *k == kind)
                .map(|(name, _, _)| name.clone())
        } else {
            None
        }
    }

    /// Get the C name for a type with a specific kind and module.
    /// Most precise lookup — used when both kind and module are known.
    pub fn get_c_name_for_kind_and_module(
        &self,
        original_name: &str,
        kind: TypeKind,
        module: &str,
    ) -> Option<String> {
        if let Some(instances) = self.type_instances.get(original_name) {
            instances
                .iter()
                .find(|(_, k, m)| *k == kind && m == module)
                .map(|(name, _, _)| name.clone())
        } else {
            None
        }
    }

    /// Get the C name for a struct type (convenience method for map_type)
    /// Returns None if this type is not registered as a struct
    pub fn get_struct_c_name(&self, original_name: &str) -> Option<String> {
        self.get_c_name_for_kind(original_name, TypeKind::Struct)
    }

    /// Get the C name for a struct type in a specific module
    pub fn get_struct_c_name_for_module(
        &self,
        original_name: &str,
        module: &str,
    ) -> Option<String> {
        self.get_c_name_for_kind_and_module(original_name, TypeKind::Struct, module)
    }

    /// Get the C name for a tagged union type (convenience method for map_type)
    /// Returns None if this type is not registered as a tagged union
    pub fn get_tagged_union_c_name(&self, original_name: &str) -> Option<String> {
        self.get_c_name_for_kind(original_name, TypeKind::TaggedUnion)
    }

    /// Get the C name for a tagged union type in a specific module
    pub fn get_tagged_union_c_name_for_module(
        &self,
        original_name: &str,
        module: &str,
    ) -> Option<String> {
        self.get_c_name_for_kind_and_module(original_name, TypeKind::TaggedUnion, module)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_no_collision() {
        let mut registry = TypeNameRegistry::new();
        let name1 = registry.register_type("Foo", TypeKind::Struct, "ast");
        assert_eq!(name1, "Foo");

        let name2 = registry.register_type("Bar", TypeKind::Enum, "ast");
        assert_eq!(name2, "Bar");
    }

    #[test]
    fn test_collision_different_kinds() {
        let mut registry = TypeNameRegistry::new();

        // First Assignment as struct (in ast module)
        let name1 = registry.register_type("Assignment", TypeKind::Struct, "ast");
        assert_eq!(name1, "Assignment");

        // Second Assignment as enum (in parser module) - should get module prefix
        let name2 = registry.register_type("Assignment", TypeKind::Enum, "parser");
        assert_eq!(name2, "parser_Assignment");

        assert!(registry.has_collision("Assignment"));
    }

    #[test]
    fn test_same_type_registered_twice() {
        let mut registry = TypeNameRegistry::new();

        // Register the same type twice (same module) - should get same name
        let name1 = registry.register_type("Foo", TypeKind::Struct, "ast");
        let name2 = registry.register_type("Foo", TypeKind::Struct, "ast");

        assert_eq!(name1, "Foo");
        assert_eq!(name2, "Foo");
        assert!(!registry.has_collision("Foo"));
    }

    #[test]
    fn test_collision_same_kind_different_module() {
        let mut registry = TypeNameRegistry::new();

        // First Call as struct in ast module
        let name1 = registry.register_type("Call", TypeKind::Struct, "ast");
        assert_eq!(name1, "Call");

        // Second Call as struct in hir module - should get module prefix
        let name2 = registry.register_type("Call", TypeKind::Struct, "hir");
        assert_eq!(name2, "hir_Call");

        assert!(registry.has_collision("Call"));

        // Lookup by module
        assert_eq!(
            registry.get_c_name_for_module("Call", "ast"),
            Some("Call".to_string())
        );
        assert_eq!(
            registry.get_c_name_for_module("Call", "hir"),
            Some("hir_Call".to_string())
        );
    }

    #[test]
    fn test_get_c_name_returns_first() {
        let mut registry = TypeNameRegistry::new();

        registry.register_type("Expression", TypeKind::TaggedUnion, "ast");
        registry.register_type("Expression", TypeKind::Struct, "hir");

        // get_c_name returns the first (default) registration
        assert_eq!(registry.get_c_name("Expression"), "Expression");
    }
}
