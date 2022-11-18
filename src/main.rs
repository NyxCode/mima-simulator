#![feature(is_sorted)]
#![allow(clippy::upper_case_acronyms)]
#![recursion_limit = "1024"]

mod builder;

use std::collections::HashMap;
use std::fmt;
use Instruction::*;

fn main() {
    let mut m = mima! {
               * = 0x80;
              i: static = 0;
              j: static = 0;
            len: static = 0;
        min_idx: static = 0;
            tmp: static = 0;

        * = 0x100;
        START:  LDC 1;
                ADD 0x040;
                STV len;
        outer:  LDV i;
                EQL len;
                JMN end;
                LDV i;
                STV min_idx;
                LDC 1;
                ADD i;
                STV j;
        inner:  LDV j;
                EQL len;
                JMN swap;
                LDIV j;
                NOT;
                STV tmp;
                LDIV min_idx;
                ADD tmp;
                JMN inc_inner;
                LDV j;
                STV min_idx;
        inc_inner:  LDC 1;
                    ADD j;
                    STV j;
                    JMP inner;
        swap:   LDIV min_idx;
                STV tmp;
                LDIV i;
                STIV min_idx;
                LDV tmp;
                STIV i;
                LDC 1;
                ADD i;
                STV i;
                JMP outer;
        end: HALT;
    };

    m.memory[0x00] = 5;
    m.memory[0x01] = 12;
    m.memory[0x02] = 2;
    m.memory[0x03] = 6;
    m.memory[0x40] = 0x03;

    m.run();

    println!("{}", m.memory[0x00]);
    println!("{}", m.memory[0x01]);
    println!("{}", m.memory[0x02]);
    println!("{}", m.memory[0x03]);
}

type Labels = HashMap<&'static str, u32>;

/// A "dynamic" value used for constructing a MIMA.
/// It may contain a 24bit number, or an instruction, which may contain a label.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum DynValue {
    Num(u32),
    Instruction(Instruction<DynAddr>),
}

impl DynValue {
    fn to_u32(self, labels: &Labels) -> u32 {
        match self {
            DynValue::Num(n) => n,
            DynValue::Instruction(i) => i.to_u32(labels),
        }
    }
}

/// A "dynamic" address used for constructing a MIMA.
/// It may point to a specific location in memory, or to a label in the ASM.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum DynAddr {
    Fixed(u32),
    Label(&'static str),
}

impl DynAddr {
    /// Converts the address to a fixed address, pointing to a specific location in memory.
    fn to_u32(self, labels: &Labels) -> u32 {
        match self {
            DynAddr::Fixed(a) => a,
            DynAddr::Label(l) => *labels
                .get(l)
                .unwrap_or_else(|| panic!("Label {l:?} is not defined")),
        }
    }
}

#[macro_export]
macro_rules! _mima {
    // variable
    (@ $m:expr;
       static = $v:literal; $($t:tt)*
    ) => {
       _mima!(@ $m.var($v) ; $($t)*)
    };
    // offset
    (@ $m:expr;
       * = $v:literal; $($t:tt)*
    ) => {
       _mima!(@ $m.addr(DynAddr::Fixed($v)) ; $($t)*)
    };
    // parse literal address
    (@ $m:expr;
       $a:literal : $($t:tt)*
    ) => {
        _mima!(
            @ $m.addr(DynAddr::Fixed($a)) ;
            $($t)*
        )
    };
    // parse label address
    (@ $m:expr;
       $a:ident : $($t:tt)*
    ) => {
        _mima!(
            @ $m.addr(DynAddr::Label(stringify!($a))) ;
            $($t)*
        )
    };
    (@ $m:expr;
       $(;)*
    ) => {
        $m
    };
    // instruction without args
    (@ $m:expr;
       $op:ident; $($t:tt)*
    ) => {
        _mima!(@ $m.instruction(Instruction::$op) ; $($t)*)
    };
    // instruction with literal arg
    (@ $m:expr;
       $op:ident $v:literal; $($t:tt)*
    ) => {{
        let operand = DynAddr::Fixed($v);
        _mima!(@ $m.instruction(Instruction::$op(operand)) ; $($t)*)
    }};
    // instruction with label arg
    (@ $m:expr;
       $op:ident $v:ident; $($t:tt)*
    ) => {{
        let operand = DynAddr::Label(stringify!($v));
        _mima!(@ $m.instruction(Instruction::$op(operand)) ; $($t)*)
    }};

}

#[macro_export]
macro_rules! mima {
    ($($t:tt)*) => {
        _mima!(@
            Builder::new();
            $($t)*
        ).build()
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum Instruction<A> {
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

impl Instruction<DynAddr> {
    fn to_fixed(self, labels: &Labels) -> Instruction<u32> {
        match self {
            LDC(x) => LDC(x.to_u32(labels)),
            LDV(x) => LDV(x.to_u32(labels)),
            STV(x) => STV(x.to_u32(labels)),
            ADD(x) => ADD(x.to_u32(labels)),
            AND(x) => AND(x.to_u32(labels)),
            OR(x) => OR(x.to_u32(labels)),
            XOR(x) => XOR(x.to_u32(labels)),
            EQL(x) => EQL(x.to_u32(labels)),
            JMP(x) => JMP(x.to_u32(labels)),
            JMN(x) => JMN(x.to_u32(labels)),
            LDIV(x) => LDIV(x.to_u32(labels)),
            STIV(x) => STIV(x.to_u32(labels)),
            HALT => HALT,
            NOT => NOT,
            RAR => RAR,
            TRAP => TRAP,
        }
    }

    fn operand(self, labels: &Labels) -> u32 {
        let operand = match self {
            LDC(x) | LDV(x) | STV(x) | ADD(x) | AND(x) | OR(x) | XOR(x) | EQL(x) | JMP(x)
            | JMN(x) | LDIV(x) | STIV(x) => x.to_u32(labels),
            HALT => 0,
            NOT => 0,
            RAR => 0,
            TRAP => 0,
        };
        operand & 0x000FFFFF
    }

    fn to_u32(self, labels: &Labels) -> u32 {
        self.opcode() | self.operand(labels)
    }
}

impl fmt::Debug for Instruction<DynAddr> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LDC(x) => write!(f, "LDC {x:?}"),
            LDV(x) => write!(f, "LDV {x:?}"),
            STV(x) => write!(f, "STV {x:?}"),
            ADD(x) => write!(f, "ADD {x:?}"),
            AND(x) => write!(f, "AND {x:?}"),
            OR(x) => write!(f, "OR {x:?}"),
            XOR(x) => write!(f, "XOR {x:?}"),
            EQL(x) => write!(f, "EQL {x:?}"),
            JMP(x) => write!(f, "JMP {x:?}"),
            JMN(x) => write!(f, "JMN {x:?}"),
            LDIV(x) => write!(f, "LDIV {x:?}"),
            STIV(x) => write!(f, "STIV {x:?}"),
            HALT => write!(f, "HALT"),
            NOT => write!(f, "NOT"),
            RAR => write!(f, "RAR"),
            TRAP => write!(f, "TRAP"),
        }
    }
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

struct Builder {
    addr: usize,
    memory: Vec<DynValue>,
    labels: HashMap<&'static str, u32>,
}

impl Builder {
    fn new() -> Self {
        Builder {
            addr: 0,
            memory: vec![DynValue::Num(0); 1024],
            labels: HashMap::new(),
        }
    }

    fn var(mut self, value: u32) -> Self {
        let addr = self.addr;
        self.addr += 1;
        self.memory[addr] = DynValue::Num(value);
        self
    }

    fn addr(mut self, addr: DynAddr) -> Self {
        let addr = match addr {
            DynAddr::Fixed(addr) => addr as usize,
            DynAddr::Label(label) => {
                let addr = self.addr;
                self.labels.insert(label, addr as u32);
                self.addr += 1; // TODO: needed?
                addr
            }
        };
        self.addr = addr;
        self
    }

    fn instruction(mut self, i: Instruction<DynAddr>) -> Self {
        assert_eq!(
            self.memory[self.addr],
            DynValue::Num(0),
            "writing in occupied memory"
        );
        let addr = self.addr;
        println!("0x{addr:>04x}: {i:?}");
        self.addr += 1;
        self.memory[addr] = DynValue::Instruction(i);
        self
    }

    fn build(self) -> Mima {
        let start = *self.labels.get("START").expect("Label 'START' required");
        let memory = self.memory.iter().map(|v| v.to_u32(&self.labels)).collect();

        Mima {
            akku: 0,
            memory,
            halted: false,
            iar: start as usize,
        }
    }
}

#[derive(Debug)]
struct Mima {
    iar: usize,
    akku: u32,
    memory: Vec<u32>,
    halted: bool,
}

impl Mima {
    /// Runs the MIMA until a HALT is reached
    fn run(&mut self) {
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
    fn step(&mut self) -> Option<bool> {
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
