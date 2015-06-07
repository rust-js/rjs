const UNI_MAX_BMP : u32 = 0xFFFF;
const UNI_SUR_HIGH_START : u32 = 0xD800;
const UNI_SUR_LOW_START : u32 = 0xDC00;
const UNI_SUR_LOW_END : u32 = 0xDFFF;
const UNI_REPLACEMENT_CHAR : u32 = 0x0000FFFD;
const UNI_MAX_LEGAL_UTF32 : u32 = 0x0010FFFF;

const HALF_BASE : u32 = 0x0010000;
const HALF_MASK : u32 = 0x3FF;
const HALF_SHIFT : u32 = 10;

/*===--- ConvertUTF.c - Universal Character Names conversions ---------------===
 *
 *                     The LLVM Compiler Infrastructure
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See LICENSE.TXT for details.
 *
 *===------------------------------------------------------------------------=*/
/*
 * Copyright 2001-2004 Unicode, Inc.
 * 
 * Disclaimer
 * 
 * This source code is provided as is by Unicode, Inc. No claims are
 * made as to fitness for any particular purpose. No warranties of any
 * kind are expressed or implied. The recipient agrees to determine
 * applicability of information provided. If this file has been
 * purchased on magnetic or optical media from Unicode, Inc., the
 * sole remedy for any claim will be exchange of defective media
 * within 90 days of receipt.
 * 
 * Limitations on Rights to Redistribute This Code
 * 
 * Unicode, Inc. hereby grants the right to freely use the information
 * supplied in this file in the creation of products supporting the
 * Unicode Standard, and to make copies of this file in any form
 * for internal or external distribution as long as this notice
 * remains attached.
 */

pub fn utf32_to_utf16(source: &[u32], strict: bool) -> Vec<u16> {
	let mut i = 0;
	let mut target : Vec<u16> = Vec::new();
	
	while i < source.len() {
		let ch = source[i];
		i += 1;
		
		if ch <= UNI_MAX_BMP { // Target is a character <= 0xFFFF
			// UTF-16 surrogate values are illegal in UTF-32; 0xffff or 0xfffe are both reserved values
			
			if ch >= UNI_SUR_HIGH_START && ch <= UNI_SUR_LOW_END {
				if strict {
					panic!("source illegal");
				} else {
					target.push(UNI_REPLACEMENT_CHAR as u16);
				}
			} else {
				target.push(ch as u16);
			}
        } else if ch > UNI_MAX_LEGAL_UTF32 {
        	if strict {
        		panic!("source illegal");
        	} else {
        		target.push(UNI_REPLACEMENT_CHAR as u16);
        	}
        } else {
            // target is a character in range 0xFFFF - 0x10FFFF.
            let ch = ch - HALF_BASE;
            target.push(((ch >> HALF_SHIFT) + UNI_SUR_HIGH_START) as u16);
            target.push(((ch & HALF_MASK) + UNI_SUR_LOW_START) as u16);
        }
    }
	
	target
}
