#![allow(clippy::upper_case_acronyms)]

use std::time::Duration;
use Instruction::*;

fn main() {
    let mut m = mima! {
        START = 0x0FD;
        0x080: let i = 0;
        0x081: let j = 0;
        0x082: let jMin = 0;
        0x083: let tmp1 = 0;
        0x084: let tmp2 = 0;
        0x085: let length = 0;

        // calculate length
        0x0FD:  LDC 1;
                ADD 0x040;
                STV length;

        // begin outer loop
        0x100:  LDV i;
                EQL length;
                JMN 0x124;
        
        0x103:  LDV i;
                STV jMin;
        
        0x105:  LDC 1;
                ADD i;
                STV j;
        
        // begin inner loop
        0x108:  LDV j;
                EQL length;
                JMN 0x117;

                LDIV j;
                NOT;
                STV tmp1;
                LDIV jMin;
                ADD tmp1;
                JMN 0x113;
        0x111:  LDV j;
                STV jMin;
        0x113:  LDC 1;
                ADD j;
                STV j;
                JMP 0x108;
        0x117:  LDV jMin;
                EQL i;
                JMN 0x103;
        0x11A:  LDIV i;
                STV tmp2;
                LDIV jMin;
                STIV i;
                LDV tmp2;
                STIV jMin;
        0x120:  LDC 1;
                ADD i;
                STV i;
                JMP 0x100;
        0x124:  HALT;
    };

    m.memory[0x00] = 7;
    m.memory[0x01] = 2;
    m.memory[0x02] = 4;
    m.memory[0x40] = 0x02;
    
    while let Some(()) = m.step() {
        std::thread::sleep(Duration::from_millis(300));
        println!("i,j = {:x}, {:x}", m.memory[0x80], m.memory[0x81]);
    }

    println!("length: {:x}", m.memory[0x85]);
    println!("{:x}", m.memory[0]);
    println!("{:x}", m.memory[1]);
    println!("{:x}", m.memory[2]);
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

#[derive(Copy, Clone, Debug)]
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

    fn step(&mut self) -> Option<()> {
        if self.halted {
            return None;
        }
        let instruction = self.memory[self.iar];
        self.iar += 1;

        let instruction = Instruction::from_repr(instruction);

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
                        println!(
                            "jumping to {a:x} = {:?}",
                            Instruction::from_repr(self.memory[self.iar])
                        );
                    } else {
                        println!("oh boi..");
                    }
                } else {
                    println!("JMN, but sign bit not set: {:x}", self.akku);
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
        }

        Some(())
    }
}
