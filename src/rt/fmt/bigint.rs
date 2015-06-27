// Translated from https://github.com/Alxandr/Jurassic.

use std::cmp::{min, max};
use std::num::Wrapping;

const INTEGERS_POWER_OF_TEN : [u64; 19] = [
    1,
    10,
    100,
    1000,
    10000,
    100000,
    1000000,
    10000000,
    100000000,
    1000000000,
    10000000000,
    100000000000,
    1000000000000,
    10000000000000,
    100000000000000,
    1000000000000000,
    10000000000000000,
    100000000000000000,
    1000000000000000000
];

const WORDS : usize = 39;

/// Represents an arbitrarily large signed integer.
pub struct BigInt {
    bits: [u32; WORDS],
    word_count: u32,
    sign: i32
}

impl Clone for BigInt {
    fn clone(&self) -> BigInt {
        let mut bits = [0; WORDS];
        for i in 0..self.bits.len() {
            bits[i] = self.bits[i];
        }
        BigInt {
            bits: bits,
            word_count: self.word_count,
            sign: self.sign
        }
    }
}

impl BigInt {
    /// Initializes a new instance of the BigInt structure using a 32-bit signed integer
    /// value.
    #[inline]
    pub fn from_i32(value: i32) -> BigInt {
        let mut bits = [0; WORDS];
        bits[0] = value.abs() as u32;
        BigInt {
            bits: bits,
            word_count: 1,
            sign: value.sign()
        }
    }
    
    /// Initializes a new instance of the BigInt structure using a 32-bit unsigned integer
    /// value.
    #[inline]
    pub fn from_u32(value: u32) -> BigInt {
        let mut bits = [0; WORDS];
        bits[0] = value;
        BigInt {
            bits: bits,
            word_count: 1,
            sign: if value == 0 { 0 } else { 1 }
        }
    }
    
    /// Initializes a new instance of the BigInt structure using a 64-bit signed integer
    /// value.
    #[inline]
    pub fn from_i64(value: i64) -> BigInt {
        let sign = value.sign();
        let value = value.abs();
        let mut bits = [0; WORDS];
        bits[0] = value as u32;
        bits[1] = (value >> 32) as u32;
        let word_count = if bits[1] == 0 { 1 } else { 2 };
        
        BigInt {
            bits: bits,
            word_count: word_count,
            sign: sign
        }
    }
    
    /// Initializes a new instance of the BigInt structure using a 64-bit unsigned integer
    /// value.
    #[inline]
    pub fn from_u64(value: u64) -> BigInt {
        let mut bits = [0; WORDS];
        bits[0] = value as u32;
        bits[1] = (value >> 32) as u32;
        let word_count = if bits[1] == 0 { 1 } else { 2 };
        
        BigInt {
            bits: bits,
            word_count: word_count,
            sign: if value == 0 { 0 } else { 1 }
        }
    }
    
    /// Gets a value that represents the number zero (0).
    #[inline]
    pub fn zero() -> BigInt {
        Self::from_i32(0)
    }
    
    /// Gets a value that represents the number one (1).
    #[inline]
    pub fn one() -> BigInt {
        Self::from_i32(1)
    }
    
    #[inline]
    pub fn high_word(&self) -> u32 {
        self.bits[self.word_count as usize - 1]
    }
    
    #[inline]
    pub fn bit_count(&self) -> u32 {
        (32 - count_leading_zero_bits(self.high_word())) + (self.word_count - 1) * 32
    }
    
    /// Adds two BigInt values and returns the result.
    fn add(left: &BigInt, right: &BigInt) -> BigInt {
        // 0 + right = right
        if left.sign == 0 {
            return right.clone();
        }
        
        // left + 0 = left
        if right.sign == 0 {
            return left.clone();
        }

        // If the signs of the two numbers are different, do a subtract instead.
        if left.sign != right.sign {
            return Self::subtract(left, &Self::negate(right.clone()));
        }

        // From here the sign of both numbers is the same.
        let mut output_word_count = max(left.word_count, right.word_count);
        let mut output_bits = [0; WORDS];
        
        let mut borrow = 0;
        let mut i = 0;
        
        while i < min(left.word_count, right.word_count) as usize {
            let temp = left.bits[i] as u64 + right.bits[i] as u64 + borrow;
            borrow = temp >> 32;
            output_bits[i] = temp as u32;
            i += 1;
        }
        
        if left.word_count > right.word_count {
            while i < left.word_count as usize {
                let temp = left.bits[i] as u64 + borrow;
                borrow = temp >> 32;
                output_bits[i] = temp as u32;
                i += 1;
            }
        } else if left.word_count < right.word_count {
            while i < right.word_count as usize {
                let temp = right.bits[i] as u64 + borrow;
                borrow = temp >> 32;
                output_bits[i] = temp as u32;
                i += 1;
            }
        }
        
        if borrow != 0 {
            output_bits[output_word_count as usize] = borrow as u32;
            output_word_count += 1;
        }
        
        BigInt {
            bits: output_bits,
            word_count: output_word_count,
            sign: left.sign
        }
    }
    
    /// Returns the product of two BigInt values.
    pub fn multiply(left: &BigInt, right: &BigInt) -> BigInt {
        // Check for special cases.
        if left.word_count == 1 {
            // 0 * right = 0
            if left.bits[0] == 0 {
                return Self::zero();
            }

            // 1 * right = right
            // -1 * right = -right
            if left.bits[0] == 1 {
                return if left.sign == -1 { Self::negate(right.clone()) } else { right.clone() };
            }
        }
        
        if right.word_count == 1 {
            // left * 0 = 0
            if right.bits[0] == 0 {
                return Self::zero();
            }

            // left * 1 = left
            // left * -1 = -left
            if right.bits[0] == 1 {
                return if right.sign == -1 { Self::negate(left.clone()) } else { left.clone() };
            }
        }
        
        let mut output_word_count = left.word_count + right.word_count - 1;
        let mut output_bits = [0; WORDS];
        
        for i in 0..left.word_count as usize {
            let mut carry = 0;
            for j in 0..right.word_count as usize {
                let temp = (left.bits[i] as u64) * (right.bits[j] as u64) + output_bits[i + j] as u64 + carry;
                carry = temp >> 32;
                output_bits[i + j] = temp as u32;
            }
            if carry != 0 {
                output_word_count = max(output_word_count, i as u32 + right.word_count + 1);
                output_bits[i + right.word_count as usize] = carry as u32;
            }
        }
        
        while output_word_count > 1 && output_bits[output_word_count as usize - 1] == 0 {
            output_word_count -= 1;
        }
        
        BigInt {
            bits: output_bits,
            word_count: output_word_count,
            sign: left.sign * right.sign
        }
    }
    
    /// Subtracts one BigInt value from another and returns the result.
    pub fn subtract<'a>(mut left: &'a BigInt, mut right: &'a BigInt) -> BigInt {
        // 0 - right = -right
        if left.sign == 0 {
            return Self::negate(right.clone());
        }
        
        // left - 0 = left
        if right.sign == 0 {
            return Self::negate(left.clone());
        }

        // If the signs of the two numbers are different, do an add instead.
        if left.sign != right.sign {
            return Self::add(left, &Self::negate(right.clone()));
        }

        // From here the sign of both numbers is the same.
        let mut output_word_count = max(left.word_count, right.word_count);
        let mut output_bits = [0; WORDS];
        let mut output_sign = left.sign;

        // Arrange it so that Abs(a) > Abs(b).
        let mut swap = false;
        if left.word_count < right.word_count {
            swap = true;
        } else if left.word_count == right.word_count {
            for i in (0..left.word_count as usize).rev() {
                if left.bits[i] != right.bits[i] {
                    if left.bits[i] < right.bits[i] {
                        swap = true;
                    }
                    break;
                }
            }
        }
        if swap {
            let temp = left;
            left = right;
            right = temp;
            output_sign = -output_sign;
        }
        
        let mut i = 0;
        let mut borrow = 0;
        
        while i < right.word_count as usize {
            let y = (Wrapping(left.bits[i] as u64) - Wrapping(right.bits[i] as u64) - Wrapping(borrow)).0;
            borrow = (y >> 32) & 1;
            output_bits[i] = y as u32;
            i += 1;
        }
        
        while i < left.word_count as usize {
            let y = left.bits[i] as u64 - borrow;
            borrow = (y >> 32) & 1;
            output_bits[i] = y as u32;
            i += 1;
        }
        
        while output_word_count > 1 && output_bits[output_word_count as usize - 1] == 0 {
            output_word_count -= 1;
        }
        
        BigInt {
            bits: output_bits,
            word_count: output_word_count,
            sign: output_sign
        }
    }
    
    /// Shifts a BigInt value a specified number of bits to the left.
    pub fn left_shift(value: &BigInt, shift: i32) -> BigInt {
        // Shifting by zero bits does nothing.
        if shift == 0 {
            return value.clone();
        }

        // Shifting left by a negative number of bits is the same as shifting right.
        if shift < 0 {
            return Self::right_shift(value, -shift);
        }
        
        let shift = shift as u32;
        let word_shift = shift / 32;
        let bit_shift = shift % 32;
        
        let mut output_word_count = value.word_count + word_shift;
        let mut output_bits = [0; WORDS];
        
        let mut carry = 0;
        for i in 0..value.word_count as usize {
            let word = value.bits[i];
            output_bits[i + word_shift as usize] = (word << bit_shift) | carry;
            carry = if bit_shift == 0 { 0 } else { word >> (32 - bit_shift) };
        }
        if carry != 0 {
            output_bits[output_word_count as usize] = carry;
            output_word_count += 1;
        }
        
        BigInt {
            bits: output_bits,
            word_count: output_word_count,
            sign: value.sign
        }
    }
    
    /// Shifts a BigInt value a specified number of bits to the right.
    pub fn right_shift(value: &BigInt, shift: i32) -> BigInt {
        // Shifting by zero bits does nothing.
        if shift == 0 {
            return value.clone();
        }

        // Shifting right by a negative number of bits is the same as shifting left.
        if shift < 0 {
            return Self::left_shift(value, -shift);
        }
        
        let shift = shift as u32;
        let word_shift = shift / 32;
        let bit_shift = shift % 32;
        
        if word_shift > value.word_count {
            return Self::zero();
        }
        
        let mut output_word_count = value.word_count - word_shift - 1;
        let mut output_bits = [0; WORDS];
        
        let mut carry = 0;
        for i in ((word_shift as usize)..(value.word_count as usize)).rev() {
            let word = value.bits[i];
            output_bits[i - word_shift as usize] =
                (word >> bit_shift) |
                if bit_shift == 0 { 0 } else { carry << (32 - bit_shift) };
            carry = word & (((1 as u32) << bit_shift) - 1);
        }
        if output_bits[output_word_count as usize] != 0 {
            output_word_count += 1;
        }
        
        BigInt {
            bits: output_bits,
            word_count: output_word_count,
            sign: value.sign
        }
    }
    
    /// Multiply by m and add a.
    pub fn multiply_add(b: &BigInt, m: u32, a: u32) -> BigInt {
        assert!(m != 0);
        
        if b.sign == 0 {
            return Self::from_u32(a);
        }
        
        let mut output_word_count = b.word_count;
        let mut output_bits = [0; WORDS];
        
        let mut carry = a as u64;
        for i in 0..b.word_count as usize {
            let temp = b.bits[i] as u64 * m as u64 + carry;
            carry = temp >> 32;
            output_bits[i] = temp as u32;
        }
        if carry != 0 {
            output_bits[output_word_count as usize] = carry as u32;
            output_word_count += 1;
        }
        
        BigInt {
            bits: output_bits,
            word_count: output_word_count,
            sign: 1
        }
    }
    
    /*

    private readonly static int[] powersOfFive = { 5, 25, 125 };

    /// <summary>
    /// Computes b x 5 ^ k.
    /// </summary>
    /// <param name="b"></param>
    /// <param name="k"></param>
    /// <returns></returns>
    public static BigInt MultiplyPow5(BigInt b, int k)
    {
        BigInt p5;

        // Fast route if k <= 3.
        if ((k & 3) != 0)
            b = MultiplyAdd(b, powersOfFive[(k & 3) - 1], 0);
        if ((k >>= 2) == 0)
            return b;

        p5 = new BigInt(625);
        while (true)
        {
            if ((k & 1) == 1)
                b = Multiply(b, p5);
            if ((k >>= 1) == 0)
                break;
            p5 = Multiply(p5, p5);
        }
        return b;
    }
    
    */
    /// Returns <c>-1</c> if a &lt; b, <c>0</c> if they are the same, or <c>1</c> if a &gt; b.
    #[inline]
    pub fn compare(a: &BigInt, b: &BigInt) -> i32 {
        if a.sign != b.sign {
            return if a.sign < b.sign { -1 } else { 1 };
        }
        if a.sign > 0 {
            // Comparison of positive numbers.
            if a.word_count < b.word_count {
                return -1;
            }
            if a.word_count > b.word_count {
                return 1;
            }
            
            for i in (0..a.word_count as usize).rev() {
                if a.bits[i] != b.bits[i] {
                    return if a.bits[i] < b.bits[i] { -1 } else { 1 };
                }
            }
        } else if a.sign < 0 {
            // Comparison of negative numbers.
            if a.word_count < b.word_count {
                return 1;
            }
            if a.word_count > b.word_count {
                return -1;
            }
            for i in (0..a.word_count as usize).rev() {
                if a.bits[i] != b.bits[i] {
                    return if a.bits[i] < b.bits[i] { 1 } else { -1 };
                }
            }
        }
            
        0
    }
    
    /// Negates a specified BigInt value.
    #[inline]
    fn negate(mut value: BigInt) -> BigInt {
        value.sign = -value.sign;
        value
    }
    
    /*
    
    /// <summary>
    /// Modifies the given values so they are suitable for passing to Quorem.
    /// </summary>
    /// <param name="dividend"> The number that will be divided. </param>
    /// <param name="divisor"> The number to divide by. </param>
    public static void SetupQuorum(ref BigInt dividend, ref BigInt divisor)
    {
        var leadingZeroCount = CountLeadingZeroBits(divisor.bits[divisor.wordCount - 1]);
        if (leadingZeroCount < 4 || leadingZeroCount > 28)
        {
            dividend = BigInt.LeftShift(dividend, 8);
            divisor = BigInt.LeftShift(divisor, 8);
        }
    }

    */
    /// Modifies the given values so they are suitable for passing to Quorem.
    #[inline]
    pub fn setup_quorum(divident: &mut BigInt, divisor: &mut BigInt, other: &mut BigInt) {
        let leading_zero_count = count_leading_zero_bits(divisor.bits[divisor.word_count as usize - 1]);
        
        if leading_zero_count < 4 || leading_zero_count > 28 {
            *divident = Self::left_shift(divident, 8);
            *divisor = Self::left_shift(divisor, 8);
            *other = Self::left_shift(other, 8);
        }
    }
    
    /// Calculates the integer result of dividing <paramref name="dividend"/> by
    /// <paramref name="divisor"/> then sets <paramref name="dividend"/> to the remainder.
    pub fn quorem(divident: &mut BigInt, divisor: &BigInt) -> u32 {
        let n = divisor.word_count;
        assert!(divident.word_count <= n);
        
        if divident.word_count < n {
            return 0;
        }
        
        let mut q = divident.bits[divident.word_count as usize - 1] / (divisor.bits[divisor.word_count as usize - 1] + 1 ); // ensure q <= true quotient
        
        if q != 0 {
            let mut borrow = 0;
            let mut carry = 0;
            for i in 0..divisor.word_count as usize {
                let ys = divisor.bits[i] as u64 * q as u64 + carry;
                carry = ys >> 32;
                let y = (Wrapping(divident.bits[i] as u64) - Wrapping(ys & 0xFFFFFFFF) - Wrapping(borrow)).0;
                borrow = (y >> 32) & 1;
                divident.bits[i] = y as u32;
            }
            
            while divident.word_count > 1 && divident.bits[divident.word_count as usize - 1] == 0 {
                divident.word_count -= 1;
            }
        }
        
        if Self::compare(divident, divisor) >= 0 {
            q += 1;
            let mut borrow = 0;
            let mut carry = 0;
            
            for i in 0..divisor.word_count as usize {
                let ys = divisor.bits[i] as u64 + carry;
                carry = ys >> 32;
                let y = (Wrapping(divident.bits[i] as u64) - Wrapping(ys & 0xFFFFFFFF) - Wrapping(borrow)).0;
                borrow = (y >> 32) & 1;
                divident.bits[i] = y as u32;
            }
            
            while divident.word_count > 1 && divident.bits[divident.word_count as usize - 1] == 0 {
                divident.word_count -= 1;
            }
        }
        
        if divident.word_count == 1 && divident.bits[0] == 0 {
            divident.sign = 0;
        }
        
        q
    }
    
    /// Decrements the current value of the BigInt object.
    pub fn in_place_decrement(&mut self) {
        assert!(self.sign >= 0);
        
        let low_word = self.bits[0];
        if low_word > 1 {
            // Fast case: subtract from lowest word.
            self.bits[0] -= 1;
        } else if self.word_count == 1 {
            // value = 0 or 1 - requires sign change.
            self.bits[0] -= 1;
            if low_word == 1 {
                self.sign = 0;
            } else if low_word == 0 {
                self.sign = -self.sign
            }
        } else {
            // Slow case: have to underflow.
            self.bits[0] = (Wrapping(self.bits[0]) - Wrapping(1)).0;
            
            for i in 1..self.word_count as usize {
                let carry = self.bits[i] == 0;
                self.bits[i] = (Wrapping(self.bits[i]) - Wrapping(1)).0;
                if !carry {
                    break;
                }
            }
            if self.bits[self.word_count as usize - 1] == 0 {
                self.word_count -= 1;
            }
        }
    }
    
    /// Equivalent to BigInt.Pow but with integer arguments.
    pub fn pow(radix: u32, exponent: u32) -> BigInt {
        assert!(radix <= 36);
        
        if radix == 10 && (exponent as usize) < INTEGERS_POWER_OF_TEN.len() {
            // Use a table for quick lookup of powers of 10.
            return Self::from_u64(INTEGERS_POWER_OF_TEN[exponent as usize]);
        } else if radix == 2 {
            // Power of two is easy.
            return Self::left_shift(&Self::one(), exponent as i32);
        }
        
        // Special cases.
        match exponent {
            0 => return Self::one(),
            1 => return Self::from_u32(radix),
            2 => return Self::from_u64(radix as u64 * radix as u64),
            3 => return Self::from_u64(radix as u64 * radix as u64 * radix as u64),
            _ => {}
        }

        // Use recursion to calculate the result.
        
        if (exponent & 1) == 1 {
            // Exponent is odd.
            let temp = Self::pow(radix, exponent / 2);
            Self::multiply_add(&Self::multiply(&temp, &temp), radix, 0)
        } else {
            let temp = Self::pow(radix, exponent / 2);
            Self::multiply(&temp, &temp)
        }
    }

/*
    /// <summary>
    /// Gets the absolute value of a BigInt object.
    /// </summary>
    /// <param name="value"> A number. </param>
    /// <returns> The absolute value of <paramref name="value"/> </returns>
    public static BigInt Abs(BigInt b)
    {
        return new BigInt(b.bits, b.wordCount, Math.Abs(b.sign));
    }

    /// <summary>
    /// Returns the logarithm of a specified number in a specified base.
    /// </summary>
    /// <param name="value"> A number whose logarithm is to be found. </param>
    /// <param name="baseValue"> The base of the logarithm. </param>
    /// <returns> The base <paramref name="baseValue"/> logarithm of <paramref name="value"/>. </returns>
    public static double Log(BigInt value, double baseValue)
    {
        if (baseValue <= 1.0 || double.IsPositiveInfinity(baseValue) || double.IsNaN(baseValue))
            throw new ArgumentOutOfRangeException("baseValue", "Unsupported logarithmic base.");
        if (value.sign < 0)
            return double.NaN;
        if (value.sign == 0)
            return double.NegativeInfinity;
        if (value.wordCount == 1)
            return Math.Log((double)value.bits[0], baseValue);

        double d = 0.0;
        double residual = 0.5;
        int bitsInLastWord = 32 - CountLeadingZeroBits(value.bits[value.wordCount - 1]);
        int bitCount = ((value.wordCount - 1) * 32) + bitsInLastWord;
        uint highBit = ((uint)1) << (bitsInLastWord - 1);
        for (int i = value.wordCount - 1; i >= 0; i--)
        {
            while (highBit != 0)
            {
                if ((value.bits[i] & highBit) != 0)
                    d += residual;
                residual *= 0.5;
                highBit = highBit >> 1;
            }
            highBit = 0x80000000;
        }
        return ((Math.Log(d) + (0.69314718055994529 * bitCount)) / Math.Log(baseValue));

    }

    /// <summary>
    /// Returns a value that indicates whether the current instance and a specified BigInt
    /// object have the same value.
    /// </summary>
    /// <param name="obj"> The object to compare. </param>
    /// <returns> <c>true</c> if this BigInt object and <paramref name="obj"/> have the
    /// same value; otherwise, <c>false</c>. </returns>
    public override bool Equals(object obj)
    {
        if ((obj is BigInt) == false)
            return false;
        if (this.wordCount != ((BigInt)obj).wordCount)
            return false;
        return Compare(this, (BigInt)obj) == 0;
    }

    /// <summary>
    /// Returns the hash code for the current BigInt object.
    /// </summary>
    /// <returns> A 32-bit signed integer hash code. </returns>
    public override int GetHashCode()
    {
        uint result = 0;
        for (int i = 0; i < this.wordCount; i++)
            result ^= this.bits[i];
        return (int)result;
    }

    /// <summary>
    /// Converts the string representation of a number to its BigInt equivalent.
    /// </summary>
    /// <param name="str"> A string that contains the number to convert. </param>
    /// <returns> A value that is equivalent to the number specified in the
    /// <paramref name="value"/> parameter. </returns>
    public static BigInt Parse(string str)
    {
        BigInt result = BigInt.Zero;
        bool negative = false;
        int i = 0;
        if (str[0] == '-')
        {
            negative = true;
            i = 1;
        }
        else if (str[0] == '+')
            i = 1;
        for (; i < str.Length; i++)
        {
            char c = str[i];
            if (c < '0' || c > '9')
                throw new FormatException("Invalid character in number.");
            result = MultiplyAdd(result, 10, c - '0');
        }
        if (result.wordCount != 1 || result.bits[0] != 0)
            result.sign = negative == true ? -1 : 1;
        return result;
    }

    /// <summary>
    /// Converts the numeric value of the current BigInt object to its equivalent string
    /// representation.
    /// </summary>
    /// <returns> The string representation of the current BigInt value. </returns>
    public override string ToString()
    {
        if (BigInt.Equals(this, Zero))
            return "0";

        var result = new System.Text.StringBuilder();
        var value = this;
        if (value.Sign < 0)
        {
            result.Append('-');
            value.sign = 1;
        }

        // Overestimate of Floor(Log10(value))
        int log10 = (int)Math.Floor(Log(value, 10)) + 1;

        var divisor = Pow(10, log10);
        
        // Adjust the values so that Quorum works.
        SetupQuorum(ref value, ref divisor);

        // Check for overestimate of log10.
        if (BigInt.Compare(divisor, value) > 0)
        {
            value = BigInt.MultiplyAdd(value, 10, 0);
            log10--;
        }

        for (int i = 0; i <= log10; i ++)
        {
            // value = value / divisor
            int digit = Quorem(ref value, divisor);

            // Append the digit.
            result.Append((char)(digit + '0'));

            // value = value * 10;
            value = BigInt.MultiplyAdd(value, 10, 0);
        }
        return result.ToString();
    }

    /// <summary>
    /// Returns a new instance BigInt structure from a 64-bit double precision floating
    /// point value.
    /// </summary>
    /// <param name="value"> A 64-bit double precision floating point value. </param>
    /// <returns> The corresponding BigInt value. </returns>
    public static BigInt FromDouble(double value)
    {
        long bits = BitConverter.DoubleToInt64Bits(value);

        // Extract the base-2 exponent.
        var base2Exponent = (int)((bits & 0x7FF0000000000000) >> 52) - 1023;

        // Extract the mantissa.
        long mantissa = bits & 0xFFFFFFFFFFFFF;
        if (base2Exponent > -1023)
        {
            mantissa |= 0x10000000000000;
            base2Exponent -= 52;
        }
        else
        {
            // Denormals.
            base2Exponent -= 51;
        }

        // Extract the sign bit.
        if (bits < 0)
            mantissa = -mantissa;

        return BigInt.LeftShift(new BigInt(mantissa), base2Exponent);
    }

    /// <summary>
    /// Returns a new instance BigInt structure from a 64-bit double precision floating
    /// point value.
    /// </summary>
    /// <param name="value"> A 64-bit double precision floating point value. </param>
    /// <returns> The corresponding BigInt value. </returns>
    public double ToDouble()
    {
        // Special case: zero.
        if (this.wordCount == 1 && this.bits[0] == 0)
            return 0.0;

        // Get the number of bits in the BigInt.
        var bitCount = this.BitCount;

        // The top 53 bits can be packed into the double (the top-most bit is implied).
        var temp = BigInt.RightShift(this, bitCount - 53);
        ulong doubleBits = (((ulong)temp.bits[1] << 32) | temp.bits[0]) & 0xFFFFFFFFFFFFF;

        // Base-2 exponent is however much we shifted, plus 52 (because the decimal point is
        // effectively at the 52nd bit), plus 1023 (the bias).
        int biasedExponent = bitCount - 53 + 52 + 1023;

        // The biased exponent must be between 0 and 2047, since there are 11 bits available,
        // otherwise bad things happen.
        if (biasedExponent >= 2048)
            return double.PositiveInfinity;

        // Move the exponent to the right position.
        doubleBits |= (ulong)(biasedExponent) << 52;

        // Handle the sign bit.
        if (this.sign == -1)
            doubleBits |= (ulong)1 << 63;

        // Convert the bit representation to a double.
        return BitConverter.Int64BitsToDouble((long)doubleBits);
    }
*/
}

/// Returns the number of leading zero bits in the given 32-bit integer.
#[inline]
fn count_leading_zero_bits(mut value: u32) -> u32 {
    let mut k = 0;
    
    if (value & 0xFFFF0000) == 0 {
        k = 16;
        value <<= 16;
    }
    if (value & 0xFF000000) == 0 {
        k += 8;
        value <<= 8;
    }
    if (value & 0xF0000000) == 0 {
        k += 4;
        value <<= 4;
    }
    if (value & 0xC0000000) == 0 {
        k += 2;
        value <<= 2;
    }
    if (value & 0x80000000) == 0 {
        k += 1;
        if (value & 0x40000000) == 0 {
            k = 32;
        }
    }
    
    k
}

trait ToSign {
    #[inline]
    fn sign(&self) -> i32;
}

impl ToSign for i32 {
    #[inline]
    fn sign(&self) -> i32 {
        if *self < 0 {
            -1
        } else if *self == 0 {
            0
        } else {
            1
        }
    }
}

impl ToSign for i64 {
    #[inline]
    fn sign(&self) -> i32 {
        if *self < 0 {
            -1
        } else if *self == 0 {
            0
        } else {
            1
        }
    }
}
