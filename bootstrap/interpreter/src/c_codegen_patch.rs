// Patch for c_codegen.rs to add namespace prefix collision avoidance
//
// This module provides the logic for detecting and resolving type name collisions
// by adding numerical suffixes to colliding type names.

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
    /// e.g., "Assignment" -> ["Assignment", "Assignment_1"]
    type_instances: HashMap<String, Vec<(String, TypeKind)>>,
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

    /// Register a type and get its C name
    /// If a collision is detected (same name, different kind), returns a suffixed name
    pub fn register_type(&mut self, original_name: &str, kind: TypeKind) -> String {
        let instances = self
            .type_instances
            .entry(original_name.to_string())
            .or_insert_with(Vec::new);

        // Check if we already have this exact type (same name AND kind)
        if let Some((c_name, _)) = instances.iter().find(|(_, k)| *k == kind) {
            return c_name.clone();
        }

        // Check if there's a collision (same name, different kind)
        let c_name = if instances.is_empty() {
            // First instance - use original name
            original_name.to_string()
        } else {
            // Collision detected - add suffix
            let suffix = instances.len();
            format!("{}_{}", original_name, suffix)
        };

        instances.push((c_name.clone(), kind));
        self.c_to_original
            .insert(c_name.clone(), original_name.to_string());

        c_name
    }

    /// Get the C name for a type reference (used when types reference each other)
    /// If the type hasn't been registered yet, returns the original name
    pub fn get_c_name(&self, original_name: &str) -> String {
        if let Some(instances) = self.type_instances.get(original_name) {
            // If there's only one instance, use it regardless of kind
            // If there are multiple, we have a problem - we need kind info to disambiguate
            // For now, use the first one (this is a limitation)
            instances
                .first()
                .map(|(name, _)| name.clone())
                .unwrap_or_else(|| original_name.to_string())
        } else {
            original_name.to_string()
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
    pub fn get_instances(&self, original_name: &str) -> Option<&[(String, TypeKind)]> {
        self.type_instances.get(original_name).map(|v| v.as_slice())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_no_collision() {
        let mut registry = TypeNameRegistry::new();
        let name1 = registry.register_type("Foo", TypeKind::Struct);
        assert_eq!(name1, "Foo");

        let name2 = registry.register_type("Bar", TypeKind::Enum);
        assert_eq!(name2, "Bar");
    }

    #[test]
    fn test_collision_different_kinds() {
        let mut registry = TypeNameRegistry::new();

        // First Assignment as struct
        let name1 = registry.register_type("Assignment", TypeKind::Struct);
        assert_eq!(name1, "Assignment");

        // Second Assignment as enum - should get suffix
        let name2 = registry.register_type("Assignment", TypeKind::Enum);
        assert_eq!(name2, "Assignment_1");

        assert!(registry.has_collision("Assignment"));
    }

    #[test]
    fn test_same_type_registered_twice() {
        let mut registry = TypeNameRegistry::new();

        // Register the same type twice - should get same name
        let name1 = registry.register_type("Foo", TypeKind::Struct);
        let name2 = registry.register_type("Foo", TypeKind::Struct);

        assert_eq!(name1, "Foo");
        assert_eq!(name2, "Foo");
        assert!(!registry.has_collision("Foo"));
    }
}
