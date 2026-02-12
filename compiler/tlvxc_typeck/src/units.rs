use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Dimension {
    Mass,
    Length,
    Time,
    Volume,
    /// Amount of substance
    Substance,
    /// Pressure
    Pressure,
    /// Mass per time (e.g., g/s is base)
    MassPerTime,
    /// Volume per time (e.g., L/s is base)
    VolumePerTime,
    /// Pragmatic composite for mass per volume (e.g., g/L is base)
    MassPerVolume,
    /// Amount of substance per volume (e.g., mol/L is base)
    SubstancePerVolume,
}

#[derive(Debug, Clone, Copy)]
pub struct UnitInfo {
    pub dim: Dimension,
    // Scale to base unit of its dimension (e.g., g for Mass, m for Length, s for Time, L for Volume)
    pub scale_to_base: f64,
}

// -------- Dimension algebra (scaffold) --------
/// Fixed-size vector of exponents for primitive dimensions in the order:
/// [Mass, Length, Time, Volume, Substance, Pressure]
pub type DimVec = [i8; 6];

const IDX_MASS: usize = 0;
const IDX_LENGTH: usize = 1;
const IDX_TIME: usize = 2;
const IDX_VOLUME: usize = 3;
const IDX_SUBSTANCE: usize = 4;
const IDX_PRESSURE: usize = 5;

/// Map a `Dimension` to a `DimVec`. Composites map to combinations.
pub fn dimvec_of(dim: Dimension) -> DimVec {
    let mut v = [0i8; 6];
    match dim {
        Dimension::Mass => v[IDX_MASS] = 1,
        Dimension::Length => v[IDX_LENGTH] = 1,
        Dimension::Time => v[IDX_TIME] = 1,
        Dimension::Volume => v[IDX_VOLUME] = 1,
        Dimension::Substance => v[IDX_SUBSTANCE] = 1,
        Dimension::Pressure => v[IDX_PRESSURE] = 1,
        Dimension::MassPerTime => {
            v[IDX_MASS] = 1;
            v[IDX_TIME] = -1;
        }
        Dimension::VolumePerTime => {
            v[IDX_VOLUME] = 1;
            v[IDX_TIME] = -1;
        }
        Dimension::MassPerVolume => {
            v[IDX_MASS] = 1;
            v[IDX_VOLUME] = -1;
        }
        Dimension::SubstancePerVolume => {
            v[IDX_SUBSTANCE] = 1;
            v[IDX_VOLUME] = -1;
        }
    }
    v
}

/// Element-wise sum of exponent vectors (represents multiplication of quantities)
pub fn combine_dims_mul(a: DimVec, b: DimVec) -> DimVec {
    [
        a[0] + b[0],
        a[1] + b[1],
        a[2] + b[2],
        a[3] + b[3],
        a[4] + b[4],
        a[5] + b[5],
    ]
}

/// Element-wise difference of exponent vectors (represents division of quantities)
pub fn combine_dims_div(a: DimVec, b: DimVec) -> DimVec {
    [
        a[0] - b[0],
        a[1] - b[1],
        a[2] - b[2],
        a[3] - b[3],
        a[4] - b[4],
        a[5] - b[5],
    ]
}

/// Returns true if two dimension vectors represent the same dimension
pub fn dims_equal(a: DimVec, b: DimVec) -> bool {
    a == b
}

fn build_registry() -> HashMap<&'static str, UnitInfo> {
    use Dimension::*;
    let mut m = HashMap::new();

    // Mass base: gram (g)
    m.insert(
        "mg",
        UnitInfo {
            dim: Mass,
            scale_to_base: 1e-3,
        },
    );
    m.insert(
        "ug",
        UnitInfo {
            dim: Mass,
            scale_to_base: 1e-6,
        },
    );
    m.insert(
        "ng",
        UnitInfo {
            dim: Mass,
            scale_to_base: 1e-9,
        },
    );
    m.insert(
        "g",
        UnitInfo {
            dim: Mass,
            scale_to_base: 1.0,
        },
    );
    m.insert(
        "kg",
        UnitInfo {
            dim: Mass,
            scale_to_base: 1e3,
        },
    );
    m.insert(
        "lb",
        UnitInfo {
            dim: Mass,
            scale_to_base: 453.592_37,
        },
    );
    m.insert(
        "oz",
        UnitInfo {
            dim: Mass,
            scale_to_base: 28.349_523_125,
        },
    );

    // Length base: meter (m)
    m.insert(
        "mm",
        UnitInfo {
            dim: Length,
            scale_to_base: 1e-3,
        },
    );
    m.insert(
        "cm",
        UnitInfo {
            dim: Length,
            scale_to_base: 1e-2,
        },
    );
    m.insert(
        "m",
        UnitInfo {
            dim: Length,
            scale_to_base: 1.0,
        },
    );
    m.insert(
        "km",
        UnitInfo {
            dim: Length,
            scale_to_base: 1e3,
        },
    );
    m.insert(
        "in",
        UnitInfo {
            dim: Length,
            scale_to_base: 0.0254,
        },
    );
    m.insert(
        "ft",
        UnitInfo {
            dim: Length,
            scale_to_base: 0.3048,
        },
    );

    // Time base: second (s)
    m.insert(
        "s",
        UnitInfo {
            dim: Time,
            scale_to_base: 1.0,
        },
    );
    m.insert(
        "min",
        UnitInfo {
            dim: Time,
            scale_to_base: 60.0,
        },
    );
    m.insert(
        "h",
        UnitInfo {
            dim: Time,
            scale_to_base: 3600.0,
        },
    );

    // Volume base: liter (L)
    m.insert(
        "mL",
        UnitInfo {
            dim: Volume,
            scale_to_base: 1e-3,
        },
    );
    // Lower-case alias for readability
    m.insert(
        "ml",
        UnitInfo {
            dim: Volume,
            scale_to_base: 1e-3,
        },
    );
    m.insert(
        "L",
        UnitInfo {
            dim: Volume,
            scale_to_base: 1.0,
        },
    );
    // Micro-liter and alias
    m.insert(
        "uL",
        UnitInfo {
            dim: Volume,
            scale_to_base: 1e-6,
        },
    );
    m.insert(
        "ul",
        UnitInfo {
            dim: Volume,
            scale_to_base: 1e-6,
        },
    );
    // Deci-liter and alias
    m.insert(
        "dL",
        UnitInfo {
            dim: Volume,
            scale_to_base: 1e-1,
        },
    );
    m.insert(
        "dl",
        UnitInfo {
            dim: Volume,
            scale_to_base: 1e-1,
        },
    );

    // Substance base: mole (mol)
    m.insert(
        "mol",
        UnitInfo {
            dim: Substance,
            scale_to_base: 1.0,
        },
    );
    m.insert(
        "mmol",
        UnitInfo {
            dim: Substance,
            scale_to_base: 1e-3,
        },
    );
    // Avoid Unicode micro; use 'umol' as ASCII alias
    m.insert(
        "umol",
        UnitInfo {
            dim: Substance,
            scale_to_base: 1e-6,
        },
    );

    // Pressure base: Pascal (Pa)
    m.insert(
        "Pa",
        UnitInfo {
            dim: Pressure,
            scale_to_base: 1.0,
        },
    );
    // kilopascal
    m.insert(
        "kPa",
        UnitInfo {
            dim: Pressure,
            scale_to_base: 1_000.0,
        },
    );
    // 1 mmHg ≈ 133.322 Pa
    m.insert(
        "mmHg",
        UnitInfo {
            dim: Pressure,
            scale_to_base: 133.322,
        },
    );

    // Mass per Volume base: g/L (synthetic base for composites)
    m.insert(
        "g/L",
        UnitInfo {
            dim: MassPerVolume,
            scale_to_base: 1.0,
        },
    );
    // mg/dL = (1e-3 g) / (1e-1 L) = 1e-2 g/L
    m.insert(
        "mg/dL",
        UnitInfo {
            dim: MassPerVolume,
            scale_to_base: 1e-2,
        },
    );
    // mg/mL = (1e-3 g) / (1e-3 L) = 1.0 g/L
    m.insert(
        "mg/mL",
        UnitInfo {
            dim: MassPerVolume,
            scale_to_base: 1.0,
        },
    );
    // mg/L = (1e-3 g) / (1.0 L) = 1e-3 g/L
    m.insert(
        "mg/L",
        UnitInfo {
            dim: MassPerVolume,
            scale_to_base: 1e-3,
        },
    );
    // g/dL = (1 g) / (0.1 L) = 10 g/L
    m.insert(
        "g/dL",
        UnitInfo {
            dim: MassPerVolume,
            scale_to_base: 10.0,
        },
    );

    // Substance per Volume base: mol/L (synthetic base for composites)
    m.insert(
        "mol/L",
        UnitInfo {
            dim: SubstancePerVolume,
            scale_to_base: 1.0,
        },
    );
    m.insert(
        "mmol/L",
        UnitInfo {
            dim: SubstancePerVolume,
            scale_to_base: 1e-3,
        },
    );
    // ASCII alias for µmol/L
    m.insert(
        "umol/L",
        UnitInfo {
            dim: SubstancePerVolume,
            scale_to_base: 1e-6,
        },
    );

    // Mass per Time base: g/s
    m.insert(
        "g/s",
        UnitInfo {
            dim: MassPerTime,
            scale_to_base: 1.0,
        },
    );
    m.insert(
        "mg/s",
        UnitInfo {
            dim: MassPerTime,
            scale_to_base: 1e-3,
        },
    );
    // Per minute variants
    // g/min = g/s * (1/60)
    m.insert(
        "g/min",
        UnitInfo {
            dim: MassPerTime,
            scale_to_base: 1.0 / 60.0,
        },
    );
    m.insert(
        "mg/min",
        UnitInfo {
            dim: MassPerTime,
            scale_to_base: 1e-3 / 60.0,
        },
    );

    // Volume per Time base: L/s
    m.insert(
        "L/s",
        UnitInfo {
            dim: VolumePerTime,
            scale_to_base: 1.0,
        },
    );
    m.insert(
        "mL/s",
        UnitInfo {
            dim: VolumePerTime,
            scale_to_base: 1e-3,
        },
    );
    // Per minute variants
    m.insert(
        "L/min",
        UnitInfo {
            dim: VolumePerTime,
            scale_to_base: 1.0 / 60.0,
        },
    );
    m.insert(
        "mL/min",
        UnitInfo {
            dim: VolumePerTime,
            scale_to_base: 1e-3 / 60.0,
        },
    );

    // Enzyme activity per volume (approximate under substance per volume for now)
    // U/L mapped to SubstancePerVolume pragmatically
    m.insert(
        "U/L",
        UnitInfo {
            dim: SubstancePerVolume,
            scale_to_base: 1.0,
        },
    );

    m
}

static REGISTRY: once_cell::sync::Lazy<HashMap<&'static str, UnitInfo>> =
    once_cell::sync::Lazy::new(build_registry);

pub fn lookup_unit(name: &str) -> Option<UnitInfo> {
    REGISTRY.get(name).copied()
}

/// Computes factor to convert value in `from_unit` to `to_unit`.
/// Returns Ok(factor) if dimensions match and units are known.
/// Returns Err with a message otherwise.
pub fn factor_from_to(from_unit: &str, to_unit: &str) -> Result<f64, String> {
    let from = lookup_unit(from_unit).ok_or_else(|| format!("unknown unit '{from_unit}'"))?;
    let to = lookup_unit(to_unit).ok_or_else(|| format!("unknown unit '{to_unit}'"))?;
    if from.dim != to.dim {
        return Err(format!(
            "incompatible unit dimensions: {:?} -> {:?}",
            from.dim, to.dim
        ));
    }
    // value_in_to = value_in_from * (from.scale_to_base / to.scale_to_base)
    Ok(from.scale_to_base / to.scale_to_base)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dimvec_primitives_and_composites() {
        let m = dimvec_of(Dimension::Mass);
        let v = dimvec_of(Dimension::Volume);
        let mpv = dimvec_of(Dimension::MassPerVolume);
        assert_eq!(m[IDX_MASS], 1);
        assert_eq!(v[IDX_VOLUME], 1);
        assert_eq!(mpv[IDX_MASS], 1);
        assert_eq!(mpv[IDX_VOLUME], -1);
    }

    #[test]
    fn combine_mul_and_div() {
        let m = dimvec_of(Dimension::Mass);
        let v = dimvec_of(Dimension::Volume);
        let mpv = combine_dims_div(m, v); // Mass / Volume
        assert!(dims_equal(mpv, dimvec_of(Dimension::MassPerVolume)));

        let s = dimvec_of(Dimension::Substance);
        let spv = combine_dims_div(s, v); // Substance / Volume
        assert!(dims_equal(spv, dimvec_of(Dimension::SubstancePerVolume)));

        // (Mass/Volume) * Volume -> Mass
        let back_to_mass = combine_dims_mul(mpv, v);
        assert!(dims_equal(back_to_mass, m));
    }
}
