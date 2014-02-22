// Copyright 2013 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

enum E { V16(u16), V32(u32) }
static C: (E, u16, u16) = (V16(0xDEAD), 0x600D, 0xBAD);

pub fn main() {
    let (_, n, _) = C;
    fail_unless!(n != 0xBAD);
    assert_eq!(n, 0x600D);
}
