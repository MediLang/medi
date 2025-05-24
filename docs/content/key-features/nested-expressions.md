---
title: "Nested Expressions & Medical Operators"
---

# Nested Expressions & Medical Operators

MediLang now supports complex nested expressions and medical-specific operators, making it easier to write and read healthcare-related calculations.

## Key Features

### Nested Expressions

Write complex calculations with proper operator precedence:

```rust
// Calculate BMI with nested expressions
let bmi = weight / (height * height);

// Complex medical calculation with proper operator precedence
let dosage = (weight * 2.5) + (age * 0.5) - (creatinine_clearance * 0.2);
```

### Medical-Specific Operators

#### The `of` Operator

Represents a relationship where one quantity is part of another:

```rust
// 2 tablets of 500mg each
let total_dose = 2 of 500mg;

// 3 vials of 1000 units each
let total_units = 3 of 1000units;
```

#### The `per` Operator

Represents rates and frequencies:

```rust
// Administer 10mg per kg per day
let daily_dose = 10mg per kg per day;

// Infusion rate in mL per hour
let infusion_rate = 1000mL per 8hr;
```

## Operator Precedence

MediLang defines the following operator precedence (from highest to lowest):

1. `of`
2. `*`, `/`, `per`
3. `+`, `-`
4. `<`, `>`, `<=`, `>=`
5. `==`, `!=`
6. `&&`
7. `||`

## Examples

### Medication Dosage Calculation

```rust
// Calculate total daily dose based on weight
let weight_based_dose = 15mg per kg * patient_weight;

// Calculate number of tablets needed
let tablets_needed = (total_dose / (2 of 500mg)).ceil();
```

### IV Infusion Rate

```rust
// Calculate IV infusion rate
let total_volume = 1000mL;
let infusion_time = 8hr;
let drops_per_ml = 20;

let ml_per_hour = total_volume / infusion_time;
let drops_per_minute = (ml_per_hour * drops_per_ml) / 60min;
```

## Best Practices

1. **Use Parentheses for Clarity**: Even when not strictly necessary, using parentheses can make complex expressions more readable.
2. **Break Down Complex Calculations**: For very complex calculations, consider breaking them into multiple steps with descriptive variable names.
3. **Use Units Consistently**: Always include units in your calculations to avoid errors.
4. **Test Edge Cases**: Pay special attention to edge cases like zero or negative values in medical calculations.

## Related Articles

- [Getting Started with MediLang](/getting-started/)
- [Medical Data Science Features](/key-features/medical-data-science/)
- [Full Language Reference](/reference/)
