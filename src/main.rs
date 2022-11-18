#![feature(is_sorted)]
#![allow(clippy::upper_case_acronyms)]

use std::fmt;
use Instruction::*;

fn main() {
    let mut m = mima! {
        START = 0x010;
        0x010:  LDC 1;
                NOT;
                STV 0x00;
                LDC 2;
                ADD 0x00;
                STV 0x00;
                JMN 0x100;
                HALT;
        0x100:  TRAP;
                HALT;
    };
    m.run();
    println!("0x{:X}", m.memory[0x00]);
}

#[macro_export]
macro_rules! mima {
    (@
        $m:expr;
        $vars:expr;
        $(;)*
    ) => {
        $m
    };
    (@
        $m:expr;
        $vars:expr;
        $a:literal : let $i:ident = $v:literal;
        $($t:tt)*
    ) => {{
        let mut m = $m;
        let mut vars = $vars;
        m.memory[$a] = $v;
        vars.insert(stringify!($i), $a);

        mima!(@ m;vars; $($t)*)
    }};
    (@
        $m:expr;
        $vars:expr;
        $($a:literal :)? $op:ident;
        $($t:tt)*
    ) => {
        mima!(@ $m$(.addr($a))?.instruction(Instruction::$op) ; $vars ; $($t)*)
    };
    (@
        $m:expr;
        $vars:expr;
        $($a:literal :)? $op:ident $v:literal;
        $($t:tt)*
    ) => {
        mima!(@ $m$(.addr($a))?.instruction(Instruction::$op($v)) ; $vars ; $($t)*)
    };
    (@
        $m:expr;
        $vars:expr;
        $($a:literal :)? $op:ident $v:ident;
        $($t:tt)*
    ) => {{
        let vars = $vars;
        let operand_name = stringify!($v);
        let operand = *vars.get(operand_name).expect(&format!("variable {operand_name:?} not defined"));
        mima!(@ $m$(.addr($a))?.instruction(Instruction::$op(operand)) ; vars ; $($t)*)
    }};
    (START = $l:literal; $($t:tt)*) => {
        mima!(@
            Builder::new($l);
            std::collections::HashMap::<&'static str, u32>::new();
            $($t)*
        ).build()
    };
}

#[derive(Copy, Clone)]
enum Instruction {
    LDC(u32),
    LDV(u32),
    STV(u32),
    ADD(u32),
    AND(u32),
    OR(u32),
    XOR(u32),
    EQL(u32),
    JMP(u32),
    JMN(u32),
    LDIV(u32),
    STIV(u32),
    HALT,
    NOT,
    RAR,
    TRAP,
}

impl fmt::Debug for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LDC(x) => write!(f, "LDC {x:#05X}"),
            LDV(x) => write!(f, "LDV {x:#05X}"),
            STV(x) => write!(f, "STV {x:#05X}"),
            ADD(x) => write!(f, "ADD {x:#05X}"),
            AND(x) => write!(f, "AND {x:#05X}"),
            OR(x) => write!(f, "OR {x:#05X}"),
            XOR(x) => write!(f, "XOR {x:#05X}"),
            EQL(x) => write!(f, "EQL {x:#05X}"),
            JMP(x) => write!(f, "JMP {x:#05X}"),
            JMN(x) => write!(f, "JMN {x:#05X}"),
            LDIV(x) => write!(f, "LDIV {x:#05X}"),
            STIV(x) => write!(f, "STIV {x:#05X}"),
            HALT => write!(f, "HALT"),
            NOT => write!(f, "NOT"),
            RAR => write!(f, "RAR"),
            TRAP => write!(f, "TRAP"),
        }
    }
}

impl Instruction {
    fn from_repr(v: u32) -> Self {
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

    fn operand(self) -> u32 {
        let operand = match self {
            LDC(x) => x,
            LDV(x) => x,
            STV(x) => x,
            ADD(x) => x,
            AND(x) => x,
            OR(x) => x,
            XOR(x) => x,
            EQL(x) => x,
            JMP(x) => x,
            JMN(x) => x,
            LDIV(x) => x,
            STIV(x) => x,
            HALT => 0,
            NOT => 0,
            RAR => 0,
            TRAP => 0,
        };
        operand & 0x000FFFFF
    }

    fn repr(self) -> u32 {
        self.opcode() | self.operand()
    }
}

struct Builder {
    addr: usize,
    memory: Vec<u32>,
    start: usize,
}

impl Builder {
    fn new(start: usize) -> Self {
        Builder {
            addr: 0,
            memory: vec![0; 1024],
            start,
        }
    }

    fn addr(mut self, addr: usize) -> Self {
        self.addr = addr;
        self
    }

    fn instruction(mut self, i: Instruction) -> Self {
        assert_eq!(self.memory[self.addr], 0, "writing in occupied memory");
        let addr = self.addr;
        println!("0x{addr:>04x}: {i:?}");
        self.addr += 1;
        self.memory[addr] = i.repr();
        self
    }

    fn build(self) -> Mima {
        Mima {
            akku: 0,
            memory: self.memory,
            halted: false,
            iar: self.start,
        }
    }
}

struct Mima {
    iar: usize,
    akku: u32,
    memory: Vec<u32>,
    halted: bool,
}

impl Mima {
    fn run(&mut self) {
        while let Some(trap) = self.step() {
            if trap {
                println!("reached trap");
            }
        }
    }

    fn step(&mut self) -> Option<bool> {
        if self.halted {
            return None;
        }
        let instruction = self.memory[self.iar];
        self.iar += 1;

        let instruction = Instruction::from_repr(instruction);
        //println!("{:?}", instruction);

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
                self.akku = add_1c::<24>(self.akku, self.memory[a as usize]);
            }
            AND(a) => {
                self.akku &= self.memory[a as usize];
            }
            OR(a) => {
                self.akku |= self.memory[a as usize];
            }
            XOR(a) => {
                self.akku ^= self.memory[a as usize];
            }
            EQL(a) => {
                self.akku = if self.akku == self.memory[a as usize] {
                    !1
                } else {
                    0
                }
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
