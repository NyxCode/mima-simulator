#![feature(is_sorted)]
#![allow(clippy::upper_case_acronyms)]

use std::fmt;
use Instruction::*;

fn main() {
    let mut m = mima! {
        START = 0x100;
        0x080: let i = 0;
        0x081: let j = 0;
        0x082: let len = 0;
        0x083: let min_idx = 0;
        0x084: let tmp1 = 0;

        // calculate length
        0x100:  LDC 1;
                ADD 0x040;
                STV len;
        // outer: abort condition
        //      i == len - 1
        0x103:  LDV i;
                EQL len;
                JMN 0x124; // -> end

        // min_idx = i
        0x106:  LDV i;
                STV min_idx;

        // j = i + 1
        0x108:  LDC 1;
                ADD i;
                STV j;

            // inner: abort condition
            //      j == len
            0x10B:  LDV j;
                    EQL len;
                    JMN 0x11A; // -> break inner loop
            // arr[j] >= arr[min_idx]
            // arr[min_idx] - arr[j] - 1 < 0
            0x10E:  LDIV j;
                    NOT;
                    STV tmp1;   // tmp1 = -arr[j]
                    LDIV min_idx;
                    ADD tmp1;
                    JMN 0x116; // -> skip assignment of min_idx
            // assign min_idx
            0x114:  LDV j;
                    STV min_idx;
            // inner: increment
            0x116:  LDC 1;
                    ADD j;
                    STV j;
            // inner: loop
            0x119:  JMP 0x10B; // -> inner abort condition

        // swap arr[i] with arr[min_idx]
        0x11A:  LDIV min_idx;
                STV tmp1;
                LDIV i;
                STIV min_idx;
                LDV tmp1;
                STIV i;
        // outer: increment
        0x120:  LDC 1;
                ADD i;
                STV i;
        // outer: loop
        0x123:  JMP 0x103; // -> outer abort condition

        0x124:  HALT;
    };

    m.memory[0x00] = 5;
    m.memory[0x01] = 3;
    m.memory[0x02] = 4;
    m.memory[0x40] = 0x02;

    let mut i = 1;
    while let Some(_) = m.step() {
        println!(
            "{i:>03} || {:#05X} | {:#05X} | {:#05X} | {:#05X} | {:#05X} | {:#05X}",
            m.akku, m.memory[0x80], m.memory[0x81], m.memory[0x82], m.memory[0x83], m.memory[0x84],
        );
        i += 1;
    }
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
        while self.step().is_some() {}
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
                self.akku = self.akku.wrapping_add(self.memory[a as usize]);
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
                    if self.akku != 0xFFFFF {
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
        }

        Some(false)
    }
}
