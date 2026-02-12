//! Simple linear int8 quantization utilities

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct QuantParams {
    pub scale: f32,
    pub zero_point: i8,
}

/// Compute symmetric quantization params for data (min/max to range [-127,127])
pub fn calc_symmetric_params(min_val: f32, max_val: f32) -> QuantParams {
    let max_abs = min_val.abs().max(max_val.abs()).max(1e-12);
    // map [-max_abs, max_abs] to [-127,127]
    let scale = max_abs / 127.0;
    QuantParams {
        scale,
        zero_point: 0,
    }
}

pub fn quantize_linear_i8(data: &[f32], params: QuantParams) -> Vec<i8> {
    data.iter()
        .map(|&x| {
            let q = (x / params.scale).round().clamp(-127.0, 127.0);
            q as i8
        })
        .collect()
}

pub fn dequantize_linear_i8(data: &[i8], params: QuantParams) -> Vec<f32> {
    data.iter().map(|&q| (q as f32) * params.scale).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn quant_dequant_roundtrip_reasonable() {
        let data = vec![-1.0, -0.5, 0.0, 0.5, 1.0];
        let params = calc_symmetric_params(-1.0, 1.0);
        let q = quantize_linear_i8(&data, params);
        let x = dequantize_linear_i8(&q, params);
        for (a, b) in data.iter().zip(x.iter()) {
            assert!((a - b).abs() < 0.02);
        }
    }
}
