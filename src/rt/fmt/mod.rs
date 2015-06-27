// Translated from https://github.com/Alxandr/Jurassic.

extern crate test;

use std::{i32, f64, char};
use std::mem::transmute;
use std::fmt::Write;
use self::bigint::BigInt;

mod bigint;

const MANTISSA_EXPLICIT_BITS : u32 = 52;
const MANTISSA_MASK : u64 = 0xFFFFFFFFFFFFF;
const EXPONENT_BIAS : u32 = 1023;
const MANTISSA_IMPLICIT_BIT : u64 = 1 << MANTISSA_EXPLICIT_BITS;

const TENS : [f64; 23] = [
    1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9,
    1e10, 1e11, 1e12, 1e13, 1e14, 1e15, 1e16, 1e17, 1e18, 1e19,
    1e20, 1e21, 1e22
];

#[derive(Copy, Clone, PartialEq)]
pub enum NumberFormatStyle {
	Regular,
	Precision,
	Fixed,
	Exponential
}

pub fn format_number(mut value: f64, radix: u32, mut style: NumberFormatStyle, precision: i32) -> String {
	let mut result = String::with_capacity(18);
	
	if value.is_nan() {
		result.push_str("NaN");
	} else if value == 0.0 {
		match style {
			NumberFormatStyle::Regular => result.push('0'),
			NumberFormatStyle::Precision => {
				result.push_str("0.");
				for _ in 0..(precision - 1) {
					result.push('0');
				}
			}
			NumberFormatStyle::Fixed => {
				if precision == 0 {
					result.push('0');
				} else {
					result.push_str("0.");
					for _ in 0..precision {
						result.push('0');
					}
				}
			}
			NumberFormatStyle::Exponential => {
				if precision <= 0 {
					result.push_str("0e+0");
				} else {
					result.push_str("0.");
					for _ in 0..precision {
						result.push('0');
					}
					result.push_str("e+0");
				}
			}
		};
	} else {
		if value < 0.0 {
			result.push('-');
			value = -value;
		}
		
		if value.is_infinite() {
			result.push_str("Infinity");
		} else {
			let bits = unsafe { transmute::<_, u64>(value) };
			let mut base2_exponent = (bits >> MANTISSA_EXPLICIT_BITS) as i32;
			
			// Extract the mantissa.
			let mut mantissa = bits & MANTISSA_MASK;
			
            // Correct the base-2 exponent.
            if base2_exponent == 0 {
            	// This is a denormalized number.
            	base2_exponent = base2_exponent - EXPONENT_BIAS as i32 - MANTISSA_EXPLICIT_BITS as i32 + 1;
            } else {
            	// This is a normal number.
            	base2_exponent = base2_exponent - EXPONENT_BIAS as i32 - MANTISSA_EXPLICIT_BITS as i32;
            	
            	// Add the implicit bit.
            	mantissa |= MANTISSA_IMPLICIT_BIT;
            }
            
            // Remove any trailing zeros.
            let trailing_zero_bits = count_trailing_zero_bits(mantissa);
            mantissa >>= trailing_zero_bits;
            base2_exponent += trailing_zero_bits as i32;
            
            // Calculate the logarithm of the number.
            
            let mut exponent = if radix == 10 { value.log10() } else { value.log(radix as f64) }.floor() as i32;
            
            if radix == 10 && style == NumberFormatStyle::Regular {
            	// Do we have a small integer?
            	if base2_exponent >= 0 && exponent <= 14 {
            		// Yes.
            		let mut i = exponent;
            		while i >= 0 {
            			let scale_factor = TENS[i as usize];
            			let digit = (value / scale_factor) as u32;
            			result.push(char::from_u32('0' as u32 + digit).unwrap());
            			value -= digit as f64 * scale_factor;
            			i -= 1;
            		}
            		
            		return result;
            	}
            }
            
            // toFixed acts like toString() if the exponent is >= 21.
            if style == NumberFormatStyle::Fixed && exponent >= 21 {
            	style = NumberFormatStyle::Regular;
            }
            
            // Calculate the exponent thresholds.
            let low_exponent_threshold = if radix == 10 && style != NumberFormatStyle::Fixed {
            	-7
            } else if style == NumberFormatStyle::Exponential {
            	-1
            } else {
            	i32::MIN
            };
            
            let high_exponent_threshold = if radix == 10 && style == NumberFormatStyle::Regular {
            	21
            } else if style == NumberFormatStyle::Precision {
            	precision
            } else if style == NumberFormatStyle::Exponential {
            	0
            } else {
            	i32::MAX
            };
            
            // Calculate the number of bits per digit.
            let bits_per_digit = if radix == 10 { 3.322 } else { (radix as f64).log2() };
            
            // Calculate the maximum number of digits to output.
            // We add 7 so that there is enough precision to distinguish halfway numbers.
            let mut max_digits_to_output : i32 = if radix == 10 { 22 } else { (53.0 / bits_per_digit).floor() as i32 + 7 };
            
            // Calculate the number of integral digits, or if negative, the number of zeros after
            // the decimal point.
            let mut integral_digits = exponent + 1;
            
            // toFixed with a low precision causes rounding.
            if style == NumberFormatStyle::Fixed && precision <= -integral_digits {
            	let diff = -integral_digits - (precision - 1);
            	max_digits_to_output += diff;
            	exponent += diff;
            	integral_digits += diff;
            }
            
            // Output any leading zeros.
            
            let mut decimal_point_output = false;
            if integral_digits <= 0 && integral_digits > low_exponent_threshold + 1 {
            	result.push('0');
            	if integral_digits < 0 {
            		result.push('.');
            		decimal_point_output = true;
            		for _ in 0..(-integral_digits) {
            			result.push('0');
            		}
            	}
            }
            
            // We need to calculate the integers "scaledValue" and "divisor" such that:
            // value = scaledValue / divisor * 10 ^ exponent
            // 1 <= scaledValue / divisor < 10
            let mut scaled_value = BigInt::from_u64(mantissa);
            let mut divisor = BigInt::one();
            let mut multiplier = BigInt::one();
            
            if exponent > 0 {
            	// Number is >= 10.
            	divisor = BigInt::multiply(&divisor, &BigInt::pow(radix, exponent as u32));
            } else if exponent < 0 {
            	// Number is < 1.
            	multiplier = BigInt::pow(radix, (-exponent) as u32);
            	scaled_value = BigInt::multiply(&scaled_value, &multiplier);
            }
            
            // Scale the divisor so it is 74 bits ((21 digits + 1 digit for rounding) * approx 3.322 bits per digit).
            let power_of_two_scale_factor =
            	if radix == 10 { 74 } else { (max_digits_to_output as f64 * bits_per_digit).ceil() as i32 } -
            	divisor.bit_count() as i32;
        	divisor = BigInt::left_shift(&divisor, power_of_two_scale_factor);
        	scaled_value = BigInt::left_shift(&scaled_value, power_of_two_scale_factor + base2_exponent as i32);
        	
            // Calculate the error.
            let mut error_delta = BigInt::zero();
            let mut error_power_of_ten = i32::MIN;
            
            match style {
            	NumberFormatStyle::Regular => {
            		error_delta = scale_to_integer(calculate_error(value), &multiplier, power_of_two_scale_factor as i32 - 1);
            	}
            	NumberFormatStyle::Precision => {
            		error_power_of_ten = integral_digits - precision;
            	}
            	NumberFormatStyle::Fixed => {
            		error_power_of_ten = -precision;
            	}
            	NumberFormatStyle::Exponential => {
            		if precision < 0 {
            			error_delta = scale_to_integer(calculate_error(value), &multiplier, power_of_two_scale_factor as i32 - 1);
            		} else {
            			error_power_of_ten = integral_digits - precision - 1;
            		}
            	}
            }
            
            if error_power_of_ten != i32::MIN {
            	error_delta = multiplier;
            	if error_power_of_ten > 0 {
            		error_delta = BigInt::multiply(&error_delta, &BigInt::pow(radix, error_power_of_ten as u32));
            	}
            	error_delta = BigInt::left_shift(&error_delta, power_of_two_scale_factor - 1);
            	if error_power_of_ten < 0 {
            		// We would normally divide by the power of 10 here, but division is extremely
                    // slow so we multiply everything else instead.
                    let error_power_of_ten_multiplier = BigInt::pow(radix, (-error_power_of_ten) as u32);
                    scaled_value = BigInt::multiply(&scaled_value, &error_power_of_ten_multiplier);
                    divisor = BigInt::multiply(&divisor, &error_power_of_ten_multiplier);
                    BigInt::setup_quorum(&mut scaled_value, &mut divisor, &mut error_delta);
            	}
            }
            
            // Shrink the error in the case where ties are resolved towards the value with the 
            // least significant bit set to zero.
            if unsafe { transmute::<_, u64>(value) } & 1 == 1 {
            	error_delta.in_place_decrement();
        	}
            
            // Cache half the divisor.
            let half_divisor = BigInt::right_shift(&divisor, 1);
            
            // Output the digits.
            let mut zero_count = 0;
            let mut digits_output = 0;
            let mut rounded = false;
            let mut scientific_notation = false;
            
            while digits_output < max_digits_to_output && !rounded {
            	// Calculate the next digit.
            	let mut digit = BigInt::quorem(&mut scaled_value, &divisor);
            	
            	if BigInt::compare(&scaled_value, &error_delta) <= 0 && BigInt::compare(&scaled_value, &half_divisor) < 0 {
            		// Round down.
            		rounded = true;
            	} else if BigInt::compare(&BigInt::subtract(&divisor, &scaled_value), &error_delta) <= 0 {
            		// Round up.
            		rounded = true;
            		digit += 1;
            		if digit == radix {
            			digit = 1;
            			exponent += 1;
            			integral_digits += 1;
            		}
            	}
            	
            	if digit > 0 || !decimal_point_output {
            		// Check if the decimal point should be output.
            		if !decimal_point_output && (scientific_notation || digits_output == integral_digits) {
            			result.push('.');
            			decimal_point_output = true;
            		}
            		
            		// Output any pent-up zeros.
            		if zero_count > 0 {
            			for _ in 0..zero_count {
            				result.push('0');
            			}
            			zero_count = 0;
            		}
            		
            		// Output the next digit.
            		if digit < 10 {
            			result.push(char::from_u32('0' as u32 + digit).unwrap());
            		} else {
            			result.push(char::from_u32('a' as u32 + digit - 10).unwrap());
            		}
            	} else {
            		zero_count += 1;
            	}
            	
            	// Check whether the number should be displayed in scientific notation (we cannot
                // determine this up front for large exponents because the number might get rounded
                // up to cross the threshold).
                if digits_output == 0 && (exponent <= low_exponent_threshold || exponent >= high_exponent_threshold) {
                	scientific_notation = true;
                }
                
                scaled_value = BigInt::multiply_add(&scaled_value, radix, 0);
                error_delta = BigInt::multiply_add(&error_delta, radix, 0);
            	
            	digits_output += 1;
            }
            
            // Add any extra zeros on the end, if necessary.
            if !scientific_notation && integral_digits > digits_output {
            	for _ in 0..(integral_digits - digits_output) {
            		result.push('0');
            	}
            	digits_output = integral_digits;
            }
            
            // Most of the styles output redundent zeros.
            let redundent_zero_count = match style {
            	NumberFormatStyle::Precision => zero_count + precision - digits_output,
            	NumberFormatStyle::Fixed => precision - (digits_output - zero_count - integral_digits),
            	NumberFormatStyle::Exponential => precision - (digits_output - zero_count) + 1,
            	NumberFormatStyle::Regular => 0
            };
            
            if redundent_zero_count > 0 {
            	if !decimal_point_output {
            		result.push('.');
            	}
            	for _ in 0..redundent_zero_count {
            		result.push('0');
            	}
            }
            
            if scientific_notation {
            	// Add the exponent on the end.
            	result.push('e');
            	if exponent > 0 {
            		result.push('+');
            	}
            	write!(result, "{}", exponent).ok();
            }
		}
	}
	
	result
}

// Counts the number of trailing zero bits in the given 64-bit value.
fn count_trailing_zero_bits(mut value: u64) -> u32 {
	if value & 7 != 0 {
		return if value & 1 != 0 {
			0
		} else if value & 2 != 0 {
			1 
		} else {
			2
		};
	}
	
	let mut k = if value & 0xFFFFFFFF == 0 {
		value >>= 32;
		32
	} else {
		0
	};
	
	if value & 0xFFFF == 0 {
		k += 16;
		value >>= 16;
	}
	if value & 0xFF == 0 {
		k += 8;
		value >>= 8;
	}
	if value & 0xF == 0 {
		k += 4;
		value >>= 4;
	}
	if value & 0x3 == 0 {
		k += 2;
		value >>= 2;
	}
	if value & 1 == 0 {
		k += 1;
		value >>= 1;
		if value == 0 {
			return 32;
		}
	}
	
	k
}

fn calculate_error(value: f64) -> f64 {
	let bits = unsafe { transmute::<_, u64>(value) };
	
	// Extract the base-2 exponent.
	let base2_exponent = ((bits & 0x7FF0000000000000) >> 52) as u64;
	
	// Handle denormals.
	if base2_exponent == 0 {
		f64::EPSILON
	} else if base2_exponent < 53 {
		// Handle very small numbers.
		unsafe { transmute(1_u64 << (base2_exponent - 1)) }
	} else {
		// Subtract 52 from the exponent to get the error (52 is the number of bits in the mantissa).
		unsafe { transmute((base2_exponent - 52) << 52) }
	}
}

fn scale_to_integer(value: f64, multiplier: &BigInt, shift: i32) -> BigInt {
	let bits = unsafe { transmute::<_, i64>(value) };
	
	// Extract the base-2 exponent.
	let mut base2_exponent = ((bits & 0x7FF0000000000000) >> 52) as i32 - 1023;
	
	// Extract the mantissa.
	let mut mantissa = bits & 0xFFFFFFFFFFFFF;
	if base2_exponent > -1023 {
		mantissa |= 0x10000000000000;
		base2_exponent -= 52;
	} else {
		// Denormals.
		base2_exponent -= 51;
	}
	
	// Extract the sign bit.
	if bits < 0 {
		mantissa = -mantissa;
	}
	
	let result = BigInt::from_i64(mantissa);
	let result = BigInt::multiply(&result, multiplier);
	BigInt::left_shift(&result, shift + base2_exponent)
}

#[cfg(test)]
mod tests {
	use super::*;
	use super::test::Bencher;
	use std::f64;
	
	const MIN_VALUE : f64 = 5e-324_f64;
	const MAX_VALUE : f64 = f64::MAX;
	
	#[test]
	fn tests() {
		test("0e+0", NumberFormatStyle::Exponential, 0.0, None);
		test("0.00e+0", NumberFormatStyle::Exponential, 0.0, Some(2));
		test("7.71234e+1", NumberFormatStyle::Exponential, 77.1234, None);
		test("7.7123e+1", NumberFormatStyle::Exponential, 77.1234, Some(4));
		test("7.71e+1", NumberFormatStyle::Exponential, 77.1234, Some(2));
		test("8e+1", NumberFormatStyle::Exponential, 77.1234, Some(0));
		test("7.71234e+1", NumberFormatStyle::Exponential, 77.1234, None);
		test("8e+1", NumberFormatStyle::Exponential, 77.1234, Some(0));
		test("7.7e+1", NumberFormatStyle::Exponential, 77.0, None);
		test("5e-16", NumberFormatStyle::Exponential, 5e-16, None);
		test("5.000e-16", NumberFormatStyle::Exponential, 5e-16, Some(3));
		test("1e+1", NumberFormatStyle::Exponential, 9.9, Some(0));
		test("1.2345678901234568e+18", NumberFormatStyle::Exponential, 1234567890123456789.0, None);
		test("1.23456789012345676800e+18", NumberFormatStyle::Exponential, 1234567890123456789.0, Some(20));
		test("5e-324", NumberFormatStyle::Exponential, MIN_VALUE, None);
		test("4.94e-324", NumberFormatStyle::Exponential, MIN_VALUE, Some(2));
		test("1.80e+308", NumberFormatStyle::Exponential, MAX_VALUE, Some(2));
		test("Infinity", NumberFormatStyle::Exponential, f64::INFINITY, None);
		test("-Infinity", NumberFormatStyle::Exponential, f64::NEG_INFINITY, None);
		test("NaN", NumberFormatStyle::Exponential, f64::NAN, None);
		
		test("0", NumberFormatStyle::Fixed, 0.0, None);
		test("0.00", NumberFormatStyle::Fixed, 0.0, Some(2));
		test("77", NumberFormatStyle::Fixed, 77.1274, None);
		test("77.1274", NumberFormatStyle::Fixed, 77.1274, Some(4));
		test("77.13", NumberFormatStyle::Fixed, 77.1274, Some(2));
		test("77", NumberFormatStyle::Fixed, 77.1274, Some(0));
		test("77", NumberFormatStyle::Fixed, 77.1234, None);
		test("77.13", NumberFormatStyle::Fixed, 77.1274, Some(2));
		test("77.00", NumberFormatStyle::Fixed, 77.0, Some(2));
		test("0.1", NumberFormatStyle::Fixed, 0.09, Some(1));
		test("0.2", NumberFormatStyle::Fixed, 0.19, Some(1));
		test("0.0", NumberFormatStyle::Fixed, 0.03, Some(1));
		test("-1", NumberFormatStyle::Fixed, (-0.7), None);
		test("1000000000000000", NumberFormatStyle::Fixed, 1e+15, None);
		test("1e+21", NumberFormatStyle::Fixed, 1e21, None);
		test("1e+21", NumberFormatStyle::Fixed, 1e21, Some(15));
		test("1000000000000000.00000000000000000000", NumberFormatStyle::Fixed, 1e+15, Some(20));
		test("0", NumberFormatStyle::Fixed, 1e-15, None);
		test("0.00000000000000100000", NumberFormatStyle::Fixed, 1e-15, Some(20));
		test("1234567890123456768", NumberFormatStyle::Fixed, 1234567890123456789.0, Some(0));
		test("77.12739999999999440661", NumberFormatStyle::Fixed, 77.1274, Some(20));
		test("Infinity", NumberFormatStyle::Fixed, f64::INFINITY, None);
		test("-Infinity", NumberFormatStyle::Fixed, f64::NEG_INFINITY, None);
		test("NaN", NumberFormatStyle::Fixed, f64::NAN, None);
		
		test("8e+1", NumberFormatStyle::Precision, 77.1274, Some(1));
		test("77", NumberFormatStyle::Precision, 77.1274, Some(2));
		test("77.13", NumberFormatStyle::Precision, 77.1274, Some(4));
		test("77.127", NumberFormatStyle::Precision, 77.1274, Some(5));
		test("77.1274", NumberFormatStyle::Precision, 77.1274, Some(6));
		test("77.12740", NumberFormatStyle::Precision, 77.1274, Some(7));
		test("77.1274000000000", NumberFormatStyle::Precision, 77.1274, Some(15));
		test("77.12739999999999", NumberFormatStyle::Precision, 77.1274, Some(16));
		test("77.127399999999994", NumberFormatStyle::Precision, 77.1274, Some(17));
		test("77.1273999999999944", NumberFormatStyle::Precision, 77.1274, Some(18));
		test("77.12739999999999441", NumberFormatStyle::Precision, 77.1274, Some(19));
		test("77.127399999999994407", NumberFormatStyle::Precision, 77.1274, Some(20));
		test("77.1273999999999944066", NumberFormatStyle::Precision, 77.1274, Some(21));
		test("0.0000012", NumberFormatStyle::Precision, 0.00000123, Some(2));
		test("1.2e-7", NumberFormatStyle::Precision, 0.000000123, Some(2));
		test("0.00000123000000000000008198", NumberFormatStyle::Precision, 0.00000123, Some(21));
		test("1.23e+5", NumberFormatStyle::Precision, 123456.0, Some(3));
		test("1.80e+308", NumberFormatStyle::Precision, MAX_VALUE, Some(3));
		test("123456.000000000", NumberFormatStyle::Precision, 123456.0, Some(15));
		test("0.0000", NumberFormatStyle::Precision, 0.0, Some(5));
		test("0.000001", NumberFormatStyle::Precision, 0.000001, Some(1));
		test("0.000001000000000", NumberFormatStyle::Precision, 0.000001, Some(10));
		test("1.000000000e-7", NumberFormatStyle::Precision, 0.0000001, Some(10));
		test("1e-11", NumberFormatStyle::Precision, 0.00000000001, Some(1));
		test("1.0e-11", NumberFormatStyle::Precision, 0.00000000001, Some(2));
		test("55", NumberFormatStyle::Precision, 55.0, Some(2));
		test("6e+1", NumberFormatStyle::Precision, 55.0, Some(1));
		test("-55", NumberFormatStyle::Precision, -55.0, Some(2));
		test("-6e+1", NumberFormatStyle::Precision, -55.0, Some(1));
		test("1e+1", NumberFormatStyle::Precision, 9.59, Some(1));
		test("9.6", NumberFormatStyle::Precision, 9.59, Some(2));
		test("9.9", NumberFormatStyle::Precision, 9.95, Some(2));
		test("10", NumberFormatStyle::Precision, 9.96, Some(2));
		test("-6e+20", NumberFormatStyle::Precision, -555555555555555555555.0, Some(1));
		test("-6e+21", NumberFormatStyle::Precision, -5555555555555555555555.0, Some(1));
		test("18014398509482012.0000", NumberFormatStyle::Precision, 18014398509482012.0, Some(21));
		test("180143985094820134912", NumberFormatStyle::Precision, 180143985094820121234.0, Some(21));
		test("0.100000000000000005551", NumberFormatStyle::Precision, 0.1, Some(21));
		test("4.9e-324", NumberFormatStyle::Precision, MIN_VALUE, Some(2));
		test("4.94065645841246544177e-324", NumberFormatStyle::Precision, MIN_VALUE, Some(21));
		
		test("77.1274", NumberFormatStyle::Precision, 77.1274, None);
		test("77.1274", NumberFormatStyle::Precision, 77.1274, None);
		
		test("NaN", NumberFormatStyle::Precision, f64::NAN, None);
		test("NaN", NumberFormatStyle::Precision, f64::NAN, Some(3));
		test("Infinity", NumberFormatStyle::Precision, f64::INFINITY, None);
		test("Infinity", NumberFormatStyle::Precision, f64::INFINITY, Some(3));
		test("-Infinity", NumberFormatStyle::Precision, f64::NEG_INFINITY, None);
		test("-Infinity", NumberFormatStyle::Precision, f64::NEG_INFINITY, Some(3));
		
		test("12345", NumberFormatStyle::Regular, 12345.0, None);
		test("18014398509481990", NumberFormatStyle::Regular, 18014398509481992.0, None);
		test("18014398509482010", NumberFormatStyle::Regular, 18014398509482008.0, None);
		test("18014398509482012", NumberFormatStyle::Regular, 18014398509482012.0, None);
		test("18014398509481988", NumberFormatStyle::Regular, 18014398509481988.0, None);
		test("1234567890123456800", NumberFormatStyle::Regular, 1234567890123456789.0, None);
		test("2234567890123456800", NumberFormatStyle::Regular, 2234567890123456789.0, None);
		test("3234567890123457000", NumberFormatStyle::Regular, 3234567890123456789.0, None);
		test("4234567890123457000", NumberFormatStyle::Regular, 4234567890123456789.0, None);
		test("72057594037927940", NumberFormatStyle::Regular, 72057594037927944.0, None);
		test("72057594037927950", NumberFormatStyle::Regular, 72057594037927945.0, None);
		test("72057594037927950", NumberFormatStyle::Regular, 72057594037927959.0, None);
		test("9.999999999999998", NumberFormatStyle::Regular, 9.999999999999999, None);
		test("10", NumberFormatStyle::Regular, 9.9999999999999999, None);
		test("100000000000000000000", NumberFormatStyle::Regular, 99999999999999999999.0, None);
		test("999999999999990000000", NumberFormatStyle::Regular, 999999999999990000000.0, None);
		test("1e+21", NumberFormatStyle::Regular, 999999999999999999999.0, None);
		test("100000000000000000000", NumberFormatStyle::Regular, 99999999999999999999.0, None);
		test("-77", NumberFormatStyle::Regular, -77.0, None);
		test("77.1274", NumberFormatStyle::Regular, 77.1274, None);
		test("77.001", NumberFormatStyle::Regular, 77.001, None);
		test("77.12345678901235", NumberFormatStyle::Regular, 77.1234567890123456789, None);
		test("7.123456789012345", NumberFormatStyle::Regular, 7.1234567890123456789, None);
		test("0.000005", NumberFormatStyle::Regular, 5e-6, None);
		test("0.000001", NumberFormatStyle::Regular, 1e-6, None);
		test("5e-7", NumberFormatStyle::Regular, 5e-7, None);
		test("1e-7", NumberFormatStyle::Regular, 1e-7, None);
		test("1000000000000000", NumberFormatStyle::Regular, 1e15, None);
		test("10000000000000000", NumberFormatStyle::Regular, 1e16, None);
		test("100000000000000000", NumberFormatStyle::Regular, 1e17, None);
		test("1000000000000000000", NumberFormatStyle::Regular, 1e18, None);
		test("10000000000000000000", NumberFormatStyle::Regular, 1e19, None);
		test("100000000000000000000", NumberFormatStyle::Regular, 1e20, None);
		test("1e+21", NumberFormatStyle::Regular, 1e21, None);
		test("1e+21", NumberFormatStyle::Regular, 999999999999999999999.0, None);
		test("100111122133144160", NumberFormatStyle::Regular, 100111122133144155.0, None);
		test("Infinity", NumberFormatStyle::Regular, f64::INFINITY, None);
		test("-Infinity", NumberFormatStyle::Regular, f64::NEG_INFINITY, None);
		test("NaN", NumberFormatStyle::Regular, f64::NAN, None);
		test("1.7976931348623157e+308", NumberFormatStyle::Regular, MAX_VALUE, None);
		test("5e-324", NumberFormatStyle::Regular, MIN_VALUE, None);
		
		test_radix("115", NumberFormatStyle::Regular, 77.0, 8);
		test_radix("1001", NumberFormatStyle::Regular, 9.0, 2);
		test_radix("fe", NumberFormatStyle::Regular, 254.0, 16);
		test_radix("-115.4621320712601014", NumberFormatStyle::Regular, -77.598, 8);
		test_radix("0.00142233513615237575", NumberFormatStyle::Regular, 0.003, 8);
		test_radix("27524716460150203300000000000000000", NumberFormatStyle::Regular, 15e30, 8);
		test_radix("0.252525252525252525", NumberFormatStyle::Regular, (1.0/3.0), 8);
	}
	
	fn test(result: &str, mut style: NumberFormatStyle, value: f64, mut precision: Option<i32>) {
		if precision.is_none() && style == NumberFormatStyle::Precision {
			style = NumberFormatStyle::Regular;
		}
		if precision.is_none() && style == NumberFormatStyle::Exponential {
			precision = Some(-1);
		}
		assert_eq!(result, format_number(value, 10, style, precision.unwrap_or(0)));
	}
	
	fn test_radix(result: &str, style: NumberFormatStyle, value: f64, radix: u32) {
		assert_eq!(result, format_number(value, radix, style, 0));
	}
	
	#[bench]
	fn bench_tests(b: &mut Bencher) {
		bench(b, NumberFormatStyle::Exponential, 0.0, None);
		bench(b, NumberFormatStyle::Exponential, 0.0, Some(2));
		bench(b, NumberFormatStyle::Exponential, 77.1234, None);
		bench(b, NumberFormatStyle::Exponential, 77.1234, Some(4));
		bench(b, NumberFormatStyle::Exponential, 77.1234, Some(2));
		bench(b, NumberFormatStyle::Exponential, 77.1234, Some(0));
		bench(b, NumberFormatStyle::Exponential, 77.1234, None);
		bench(b, NumberFormatStyle::Exponential, 77.1234, Some(0));
		bench(b, NumberFormatStyle::Exponential, 77.0, None);
		bench(b, NumberFormatStyle::Exponential, 5e-16, None);
		bench(b, NumberFormatStyle::Exponential, 5e-16, Some(3));
		bench(b, NumberFormatStyle::Exponential, 9.9, Some(0));
		bench(b, NumberFormatStyle::Exponential, 1234567890123456789.0, None);
		bench(b, NumberFormatStyle::Exponential, 1234567890123456789.0, Some(20));
		bench(b, NumberFormatStyle::Exponential, MIN_VALUE, None);
		bench(b, NumberFormatStyle::Exponential, MIN_VALUE, Some(2));
		bench(b, NumberFormatStyle::Exponential, MAX_VALUE, Some(2));
		bench(b, NumberFormatStyle::Exponential, f64::INFINITY, None);
		bench(b, NumberFormatStyle::Exponential, f64::NEG_INFINITY, None);
		bench(b, NumberFormatStyle::Exponential, f64::NAN, None);
		
		bench(b, NumberFormatStyle::Fixed, 0.0, None);
		bench(b, NumberFormatStyle::Fixed, 0.0, Some(2));
		bench(b, NumberFormatStyle::Fixed, 77.1274, None);
		bench(b, NumberFormatStyle::Fixed, 77.1274, Some(4));
		bench(b, NumberFormatStyle::Fixed, 77.1274, Some(2));
		bench(b, NumberFormatStyle::Fixed, 77.1274, Some(0));
		bench(b, NumberFormatStyle::Fixed, 77.1234, None);
		bench(b, NumberFormatStyle::Fixed, 77.1274, Some(2));
		bench(b, NumberFormatStyle::Fixed, 77.0, Some(2));
		bench(b, NumberFormatStyle::Fixed, 0.09, Some(1));
		bench(b, NumberFormatStyle::Fixed, 0.19, Some(1));
		bench(b, NumberFormatStyle::Fixed, 0.03, Some(1));
		bench(b, NumberFormatStyle::Fixed, (-0.7), None);
		bench(b, NumberFormatStyle::Fixed, 1e+15, None);
		bench(b, NumberFormatStyle::Fixed, 1e21, None);
		bench(b, NumberFormatStyle::Fixed, 1e21, Some(15));
		bench(b, NumberFormatStyle::Fixed, 1e+15, Some(20));
		bench(b, NumberFormatStyle::Fixed, 1e-15, None);
		bench(b, NumberFormatStyle::Fixed, 1e-15, Some(20));
		bench(b, NumberFormatStyle::Fixed, 1234567890123456789.0, Some(0));
		bench(b, NumberFormatStyle::Fixed, 77.1274, Some(20));
		bench(b, NumberFormatStyle::Fixed, f64::INFINITY, None);
		bench(b, NumberFormatStyle::Fixed, f64::NEG_INFINITY, None);
		bench(b, NumberFormatStyle::Fixed, f64::NAN, None);
		
		bench(b, NumberFormatStyle::Precision, 77.1274, Some(1));
		bench(b, NumberFormatStyle::Precision, 77.1274, Some(2));
		bench(b, NumberFormatStyle::Precision, 77.1274, Some(4));
		bench(b, NumberFormatStyle::Precision, 77.1274, Some(5));
		bench(b, NumberFormatStyle::Precision, 77.1274, Some(6));
		bench(b, NumberFormatStyle::Precision, 77.1274, Some(7));
		bench(b, NumberFormatStyle::Precision, 77.1274, Some(15));
		bench(b, NumberFormatStyle::Precision, 77.1274, Some(16));
		bench(b, NumberFormatStyle::Precision, 77.1274, Some(17));
		bench(b, NumberFormatStyle::Precision, 77.1274, Some(18));
		bench(b, NumberFormatStyle::Precision, 77.1274, Some(19));
		bench(b, NumberFormatStyle::Precision, 77.1274, Some(20));
		bench(b, NumberFormatStyle::Precision, 77.1274, Some(21));
		bench(b, NumberFormatStyle::Precision, 0.00000123, Some(2));
		bench(b, NumberFormatStyle::Precision, 0.000000123, Some(2));
		bench(b, NumberFormatStyle::Precision, 0.00000123, Some(21));
		bench(b, NumberFormatStyle::Precision, 123456.0, Some(3));
		bench(b, NumberFormatStyle::Precision, MAX_VALUE, Some(3));
		bench(b, NumberFormatStyle::Precision, 123456.0, Some(15));
		bench(b, NumberFormatStyle::Precision, 0.0, Some(5));
		bench(b, NumberFormatStyle::Precision, 0.000001, Some(1));
		bench(b, NumberFormatStyle::Precision, 0.000001, Some(10));
		bench(b, NumberFormatStyle::Precision, 0.0000001, Some(10));
		bench(b, NumberFormatStyle::Precision, 0.00000000001, Some(1));
		bench(b, NumberFormatStyle::Precision, 0.00000000001, Some(2));
		bench(b, NumberFormatStyle::Precision, 55.0, Some(2));
		bench(b, NumberFormatStyle::Precision, 55.0, Some(1));
		bench(b, NumberFormatStyle::Precision, -55.0, Some(2));
		bench(b, NumberFormatStyle::Precision, -55.0, Some(1));
		bench(b, NumberFormatStyle::Precision, 9.59, Some(1));
		bench(b, NumberFormatStyle::Precision, 9.59, Some(2));
		bench(b, NumberFormatStyle::Precision, 9.95, Some(2));
		bench(b, NumberFormatStyle::Precision, 9.96, Some(2));
		bench(b, NumberFormatStyle::Precision, -555555555555555555555.0, Some(1));
		bench(b, NumberFormatStyle::Precision, -5555555555555555555555.0, Some(1));
		bench(b, NumberFormatStyle::Precision, 18014398509482012.0, Some(21));
		bench(b, NumberFormatStyle::Precision, 180143985094820121234.0, Some(21));
		bench(b, NumberFormatStyle::Precision, 0.1, Some(21));
		bench(b, NumberFormatStyle::Precision, MIN_VALUE, Some(2));
		bench(b, NumberFormatStyle::Precision, MIN_VALUE, Some(21));
		
		bench(b, NumberFormatStyle::Precision, 77.1274, None);
		bench(b, NumberFormatStyle::Precision, 77.1274, None);
		
		bench(b, NumberFormatStyle::Precision, f64::NAN, None);
		bench(b, NumberFormatStyle::Precision, f64::NAN, Some(3));
		bench(b, NumberFormatStyle::Precision, f64::INFINITY, None);
		bench(b, NumberFormatStyle::Precision, f64::INFINITY, Some(3));
		bench(b, NumberFormatStyle::Precision, f64::NEG_INFINITY, None);
		bench(b, NumberFormatStyle::Precision, f64::NEG_INFINITY, Some(3));
		
		bench(b, NumberFormatStyle::Regular, 12345.0, None);
		bench(b, NumberFormatStyle::Regular, 18014398509481992.0, None);
		bench(b, NumberFormatStyle::Regular, 18014398509482008.0, None);
		bench(b, NumberFormatStyle::Regular, 18014398509482012.0, None);
		bench(b, NumberFormatStyle::Regular, 18014398509481988.0, None);
		bench(b, NumberFormatStyle::Regular, 1234567890123456789.0, None);
		bench(b, NumberFormatStyle::Regular, 2234567890123456789.0, None);
		bench(b, NumberFormatStyle::Regular, 3234567890123456789.0, None);
		bench(b, NumberFormatStyle::Regular, 4234567890123456789.0, None);
		bench(b, NumberFormatStyle::Regular, 72057594037927944.0, None);
		bench(b, NumberFormatStyle::Regular, 72057594037927945.0, None);
		bench(b, NumberFormatStyle::Regular, 72057594037927959.0, None);
		bench(b, NumberFormatStyle::Regular, 9.999999999999999, None);
		bench(b, NumberFormatStyle::Regular, 9.9999999999999999, None);
		bench(b, NumberFormatStyle::Regular, 99999999999999999999.0, None);
		bench(b, NumberFormatStyle::Regular, 999999999999990000000.0, None);
		bench(b, NumberFormatStyle::Regular, 999999999999999999999.0, None);
		bench(b, NumberFormatStyle::Regular, 99999999999999999999.0, None);
		bench(b, NumberFormatStyle::Regular, -77.0, None);
		bench(b, NumberFormatStyle::Regular, 77.1274, None);
		bench(b, NumberFormatStyle::Regular, 77.001, None);
		bench(b, NumberFormatStyle::Regular, 77.1234567890123456789, None);
		bench(b, NumberFormatStyle::Regular, 7.1234567890123456789, None);
		bench(b, NumberFormatStyle::Regular, 5e-6, None);
		bench(b, NumberFormatStyle::Regular, 1e-6, None);
		bench(b, NumberFormatStyle::Regular, 5e-7, None);
		bench(b, NumberFormatStyle::Regular, 1e-7, None);
		bench(b, NumberFormatStyle::Regular, 1e15, None);
		bench(b, NumberFormatStyle::Regular, 1e16, None);
		bench(b, NumberFormatStyle::Regular, 1e17, None);
		bench(b, NumberFormatStyle::Regular, 1e18, None);
		bench(b, NumberFormatStyle::Regular, 1e19, None);
		bench(b, NumberFormatStyle::Regular, 1e20, None);
		bench(b, NumberFormatStyle::Regular, 1e21, None);
		bench(b, NumberFormatStyle::Regular, 999999999999999999999.0, None);
		bench(b, NumberFormatStyle::Regular, 100111122133144155.0, None);
		bench(b, NumberFormatStyle::Regular, f64::INFINITY, None);
		bench(b, NumberFormatStyle::Regular, f64::NEG_INFINITY, None);
		bench(b, NumberFormatStyle::Regular, f64::NAN, None);
		bench(b, NumberFormatStyle::Regular, MAX_VALUE, None);
		bench(b, NumberFormatStyle::Regular, MIN_VALUE, None);
		
		bench_radix(b, NumberFormatStyle::Regular, 77.0, 8);
		bench_radix(b, NumberFormatStyle::Regular, 9.0, 2);
		bench_radix(b, NumberFormatStyle::Regular, 254.0, 16);
		bench_radix(b, NumberFormatStyle::Regular, -77.598, 8);
		bench_radix(b, NumberFormatStyle::Regular, 0.003, 8);
		bench_radix(b, NumberFormatStyle::Regular, 15e30, 8);
		bench_radix(b, NumberFormatStyle::Regular, (1.0/3.0), 8);
	}

	fn bench(b: &mut Bencher, mut style: NumberFormatStyle, value: f64, mut precision: Option<i32>) {
		if precision.is_none() && style == NumberFormatStyle::Precision {
			style = NumberFormatStyle::Regular;
		}
		if precision.is_none() && style == NumberFormatStyle::Exponential {
			precision = Some(-1);
		}
		b.iter(|| format_number(value, 10, style, precision.unwrap_or(0)));
	}
	
	fn bench_radix(b: &mut Bencher, style: NumberFormatStyle, value: f64, radix: u32) {
		b.iter(|| format_number(value, radix, style, 0));
	}
}
