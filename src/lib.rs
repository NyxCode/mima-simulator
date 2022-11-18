#![allow(clippy::upper_case_acronyms)]
#![recursion_limit = "1024"]

mod builder;

use Instruction::*;
pub use builder::*;

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum Instruction<A> {
    LDC(A),
    LDV(A),
    STV(A),
    ADD(A),
    AND(A),
    OR(A),
    XOR(A),
    EQL(A),
    JMP(A),
    JMN(A),
    LDIV(A),
    STIV(A),
    HALT,
    NOT,
    RAR,
    TRAP,
}

impl<A> Instruction<A> {
    fn opcode(self) -> u32 {
        match self {
            LDC(_) => 0x0 << 20,
            LDV(_) => 0x1 << 20,
            STV(_) => 0x2 << 20,
            ADD(_) => 0x3 << 20,
            AND(_) => 0x4 << 20,
            OR(_) => 0x5 << 20,
            XOR(_) => 0x6 << 20,
            EQL(_) => 0x7 << 20,
            JMP(_) => 0x8 << 20,
            JMN(_) => 0x9 << 20,
            LDIV(_) => 0xA << 20,
            STIV(_) => 0xB << 20,
            HALT => 0xF0 << 16,
            NOT => 0xF1 << 16,
            RAR => 0xF2 << 16,
            TRAP => 0xF3 << 16,
        }
    }
}

impl Instruction<u32> {
    fn from_u32(v: u32) -> Self {
        assert_eq!(v & 0xFF000000, 0, "found something outside of 24 LSBs");
        let opcode = v >> 20;
        let operand = v & 0x000FFFFF;
        match opcode {
            0x0 => LDC(operand),
            0x1 => LDV(operand),
            0x2 => STV(operand),
            0x3 => ADD(operand),
            0x4 => AND(operand),
            0x5 => OR(operand),
            0x6 => XOR(operand),
            0x7 => EQL(operand),
            0x8 => JMP(operand),
            0x9 => JMN(operand),
            0xA => LDIV(operand),
            0xB => STIV(operand),
            0xF => {
                let opcode = v >> 16;
                match opcode {
                    0xF0 => HALT,
                    0xF1 => NOT,
                    0xF2 => RAR,
                    0xF3 => TRAP,
                    _ => panic!(),
                }
            }
            _ => panic!(),
        }
    }
}

#[derive(Debug)]
pub struct Mima {
    pub iar: usize,
    pub akku: u32,
    pub memory: Vec<u32>,
    pub halted: bool,
}

impl Mima {
    /// Runs the MIMA until a HALT is reached
    pub fn run(&mut self) {
        while let Some(trap) = self.step() {
            if trap {
                println!("reached trap");
            }
        }
    }

    /// Executes one instruction.
    /// `None` is returned if the machine has reached a HALT.
    /// `Some(true)` is returned if a TRAP instruction was reached,
    /// `Some(false)` otherwise.
    pub fn step(&mut self) -> Option<bool> {
        if self.halted {
            return None;
        }
        let instruction = self.memory[self.iar];
        self.iar += 1;
        let instruction = Instruction::<u32>::from_u32(instruction);

        match instruction {
            LDC(a) => {
                self.akku = a;
            }
            LDV(a) => {
                self.akku = self.memory[a as usize];
            }
            STV(a) => {
                self.memory[a as usize] = self.akku;
            }
            ADD(a) => {
                let a_val = self.memory[a as usize];
                self.akku = add_1c::<24>(self.akku, a_val);
            }
            AND(a) => {
                let a_val = self.memory[a as usize];
                self.akku &= a_val;
            }
            OR(a) => {
                let a_val = self.memory[a as usize];
                self.akku |= a_val;
            }
            XOR(a) => {
                let a_val = self.memory[a as usize];
                self.akku ^= a_val;
            }
            EQL(a) => {
                let a_val = self.memory[a as usize];
                self.akku = if self.akku == a_val { !1 } else { 0 }
            }
            JMP(a) => {
                self.iar = a as usize;
            }
            JMN(a) => {
                let sign_mask = 1 << (5 * 4 - 1);
                if self.akku & sign_mask != 0 {
                    // sign bit is set
                    if self.akku != 0xFFFFFF {
                        // we made sure it's not -0
                        self.iar = a as usize;
                    }
                }
            }
            HALT => {
                self.halted = true;
                return None;
            }
            NOT => {
                self.akku = !self.akku;
            }
            RAR => {
                unimplemented!()
            }
            LDIV(a) => {
                let addr = self.memory[a as usize] as usize;
                self.akku = self.memory[addr];
            }
            STIV(a) => {
                let addr = self.memory[a as usize] as usize;
                self.memory[addr] = self.akku;
            }
            TRAP => {
                return Some(true);
            }
        };

        self.akku &= 0xFFFFFF;
        if self.akku == 0xFFFFFF {
            // we sneakily convert negative zero back to zero
            self.akku = 0;
        }

        Some(false)
    }
}

fn add_1c<const BITS: usize>(mut a: u32, mut b: u32) -> u32 {
    let carry_mask = 1 << BITS;
    let valid_mask = (1 << BITS) - 1;

    a &= valid_mask;
    b &= valid_mask;

    let mut result = a + b;
    let carry = result & carry_mask;
    result &= valid_mask;
    if carry != 0 {
        result += 1;
        result &= valid_mask;
    }

    if result == valid_mask {
        // the result is all 1's, so it's the negative 0
        // let's just sneakily turn that back into a normal 0.
        0
    } else {
        result & valid_mask
    }
}

#[cfg(test)]
mod tests {
    use super::add_1c;

    #[test]
    fn ones_complement() {
        assert_eq!(0, add_1c::<24>(!1, 1));
        assert_eq!(1, add_1c::<24>(!3, 4));
        assert_eq!(!3 & 0xFFFFFF, add_1c::<24>(!4, 1));
        assert_eq!(0, add_1c::<24>(!0, !0));
        assert_eq!(0, add_1c::<24>(0, 0));
        assert_eq!(0x800000, add_1c::<24>(0x7FFFFF, 1));
        assert_eq!(!1 & 0xFFFFFF, add_1c::<24>(3, !4));
        assert_eq!(0, add_1c::<24>(3, !3));
    }
}
