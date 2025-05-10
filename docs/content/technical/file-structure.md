# Medi File Structure and Organization

Medi follows a Rust-inspired approach to code organization, providing clean structure and explicit visibility for healthcare applications.

## File Extensions

Medi source code files use the `.mdi` extension:

```
patient_analysis.mdi
data_pipeline.mdi
clinical_trial.mdi
```

## Module System

### Single File Modules

Each `.mdi` file is a module, containing related functionality. By default, all items (functions, types, etc.) are private to the module unless explicitly marked public:

```mdi
// my_module.mdi
pub fn analyze_vitals(heart_rate: Integer, blood_pressure: Tuple<Integer, Integer>) {
    // This function is accessible from other modules
    // because it's marked with 'pub'
    if (is_tachycardia(heart_rate)) {
        alert("Tachycardia detected");
    }
}

fn is_tachycardia(heart_rate: Integer) -> Boolean {
    // This helper function is private to this module
    // since it lacks the 'pub' keyword
    return heart_rate > 100;
}
```

### Importing Modules

You can import other modules with the `use` keyword:

```mdi
// main.mdi
use vitals;
use patient::records;

pub fn main() {
    let patient = records::load("P-12345");
    vitals::analyze_vitals(patient.heart_rate, patient.blood_pressure);
}
```

### Directory-Based Modules

For larger projects, you can organize code in directories:

```
project/
├── main.mdi         # Main entry point
├── vitals/
│   ├── mod.mdi      # Makes directory a module and re-exports items
│   ├── analysis.mdi # Analysis functions
│   └── alerts.mdi   # Alert management
└── patient/
    ├── mod.mdi
    ├── records.mdi  # Patient record handling
    └── search.mdi   # Patient search functionality
```

The `mod.mdi` file declares and re-exports submodules:

```mdi
// vitals/mod.mdi
pub mod analysis;
pub mod alerts;

// Re-export commonly used functions for convenience
pub use analysis::analyze_vitals;
```

## Visibility and Encapsulation

Medi's visibility system helps enforce good architecture and data privacy:

- `pub`: Item is visible outside the module
- Default (no modifier): Item is private to the module
- `pub(crate)`: Item is visible within the current crate only
- `pub(super)`: Item is visible to the parent module only

This is particularly important for healthcare applications where encapsulation helps protect sensitive data access patterns.

## Best Practices

1. **Group by domain concept**: Organize modules around healthcare domain concepts (patients, vitals, medications) rather than technical layers.

2. **Privacy by default**: Keep implementation details private and only expose necessary APIs.

3. **Re-export for convenience**: Use `mod.mdi` files to re-export common items for easier imports.

4. **Consistent naming**: Use snake_case for filenames and functions, PascalCase for types.

5. **Documentation**: Add documentation comments with `///` before public items to generate automatic documentation.

By following these Rust-inspired practices, Medi code remains maintainable and secure as projects grow in size and complexity.
