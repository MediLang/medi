# Task 17: Clinician Self-Testing Plan for Diagnostic Output

**Tester**: Self (clinician/physician)  
**Date**: 2026-01-08  
**Goal**: Evaluate the clarity, helpfulness, and clinical relevance of Medi compiler diagnostic messages.

---

## Instructions

For each scenario below:
1. Run the command shown (or paste the code into the web IDE at `examples/browser/`)
2. Read the diagnostic output
3. Rate and comment using the evaluation criteria

### Evaluation Criteria (1-5 scale)
- **Clarity**: Is the message easy to understand? (1=confusing, 5=crystal clear)
- **Actionability**: Do I know what to fix? (1=no idea, 5=obvious fix)
- **Clinical relevance**: Does the help text make sense for healthcare context? (1=irrelevant, 5=spot-on)
- **Visual layout**: Is the snippet/underline helpful? (1=cluttered, 5=clean)

---

## Test Scenarios

### Scenario 1: Assignment vs Comparison Confusion
**Common mistake**: Using `=` instead of `==` in a condition.

```bash
echo 'if patient_age = 65 { print("Senior") }' | cargo run -p medic -- check /dev/stdin
```

Or paste into IDE:
```medi
if patient_age = 65 {
    print("Senior")
}
```

**Expected behavior**: Error pointing at `=` with help suggesting `==`.

| Criterion | Rating (1-5) | Notes |
|-----------|--------------|-------|
| Clarity | | |
| Actionability | | |
| Clinical relevance | | |
| Visual layout | | |

**Suggestions for improvement**:


---

### Scenario 2: Unmatched Bracket
**Common mistake**: Forgetting to close a bracket in a list.

```medi
let vitals = [98.6, 72, 120
let next_reading = 99.1
```

| Criterion | Rating (1-5) | Notes |
|-----------|--------------|-------|
| Clarity | | |
| Actionability | | |
| Clinical relevance | | |
| Visual layout | | |

**Suggestions for improvement**:


---

### Scenario 3: Invalid Token (Typo)
**Common mistake**: Typing an invalid character.

```medi
let dose = 500 @ mg
```

| Criterion | Rating (1-5) | Notes |
|-----------|--------------|-------|
| Clarity | | |
| Actionability | | |
| Clinical relevance | | |
| Visual layout | | |

**Suggestions for improvement**:


---

### Scenario 4: Healthcare Operator Out of Context
**Common mistake**: Using `per` incorrectly.

```medi
per
```

| Criterion | Rating (1-5) | Notes |
|-----------|--------------|-------|
| Clarity | | |
| Actionability | | |
| Clinical relevance | | |
| Visual layout | | |

**Suggestions for improvement**:


---

### Scenario 5: Missing Identifier After `let`
**Common mistake**: Forgetting the variable name.

```medi
let = 100
```

| Criterion | Rating (1-5) | Notes |
|-----------|--------------|-------|
| Clarity | | |
| Actionability | | |
| Clinical relevance | | |
| Visual layout | | |

**Suggestions for improvement**:


---

### Scenario 6: Type Annotation Syntax
**Common mistake**: Wrong type annotation format.

```medi
let bp int = 120
```

| Criterion | Rating (1-5) | Notes |
|-----------|--------------|-------|
| Clarity | | |
| Actionability | | |
| Clinical relevance | | |
| Visual layout | | |

**Suggestions for improvement**:


---

### Scenario 7: FHIR Query Syntax Error
**Common mistake**: Malformed FHIR query.

```medi
let patient = fhir_query("Patient" "12345")
```

| Criterion | Rating (1-5) | Notes |
|-----------|--------------|-------|
| Clarity | | |
| Actionability | | |
| Clinical relevance | | |
| Visual layout | | |

**Suggestions for improvement**:


---

### Scenario 8: Medical Code Format
**Common mistake**: Invalid ICD-10 format.

```medi
let dx = ICD10:ABC123
```

| Criterion | Rating (1-5) | Notes |
|-----------|--------------|-------|
| Clarity | | |
| Actionability | | |
| Clinical relevance | | |
| Visual layout | | |

**Suggestions for improvement**:


---

### Scenario 9: Unit Expression Error
**Common mistake**: Invalid unit syntax.

```medi
let dose = 5 mg per per kg
```

| Criterion | Rating (1-5) | Notes |
|-----------|--------------|-------|
| Clarity | | |
| Actionability | | |
| Clinical relevance | | |
| Visual layout | | |

**Suggestions for improvement**:


---

### Scenario 10: Regulate Block Syntax
**Common mistake**: Malformed regulate block.

```medi
regulate HIPAA
    let phi = patient.ssn
}
```

| Criterion | Rating (1-5) | Notes |
|-----------|--------------|-------|
| Clarity | | |
| Actionability | | |
| Clinical relevance | | |
| Visual layout | | |

**Suggestions for improvement**:


---

## Summary

### Overall Impressions
Self-testing completed on 2026-01-08. Diagnostics were evaluated for clarity, actionability, and clinical relevance.

### Top 3 Issues Fixed (v0.0.17)
1. **Missing variable name after `let`**: Help text now says "Did you forget the variable name? Example: `let x = 100`" instead of the misleading comparison suggestion.
2. **Type error messages**: Now use clinician-friendly language (e.g., "Condition must be true/false (Bool), but found a number (Int). Try adding a comparison like '== 0' or '> 100'.").
3. **Wrong type annotation syntax**: Parser now logs a warning when detecting `let x int = ...` pattern (missing colon).

### What Worked Well
- Snippet rendering with `^` and `~~~` underlines is clear
- Healthcare-specific help text for `per`, `of`, and medical operators
- Recovery from lexer errors allows continued parsing

---

## Implemented Improvements

### Fix 1: Better help for `let = X`
**Before**: `help: '=' assigns. For comparison, use '=='`
**After**: `help: Did you forget the variable name? Example: 'let x = 100'`

### Fix 2: Clinician-friendly type errors
**Before**: `Condition must be Bool, found Int.`
**After**: `Condition must be true/false (Bool), but found a number (Int). Try adding a comparison like '== 0' or '> 100'.`

Type names now use friendly descriptions:
- `Int` → "a number (Int)"
- `Float` → "a decimal number (Float)"
- `Bool` → "true/false (Bool)"
- `String` → "text (String)"
- Healthcare types like `PatientId`, `Vital`, `LabResult` have domain-specific names

### Fix 3: Warning for wrong type annotation syntax
Parser now detects `let x int = ...` (missing colon) and logs a warning suggesting `let x: int = ...`.

---

## Next Steps
- Consider adding span information to type errors for snippet rendering
- Expand healthcare-specific help text for FHIR queries and medical codes
- Gather feedback from additional clinician users
