use crate::Instruction::*;
use crate::{Instruction, Mima};
use std::collections::HashMap;
use std::fmt;

pub struct Builder {
    addr: usize,
    memory: Vec<DynValue>,
    labels: HashMap<&'static str, u32>,
}

impl Default for Builder {
    fn default() -> Self {
        Builder {
            addr: 0,
            memory: vec![DynValue::Num(0); 1024],
            labels: HashMap::new(),
        }
    }
}

impl Builder {
    pub fn var(mut self, value: u32) -> Self {
        let addr = self.addr;
        self.addr += 1;
        self.memory[addr] = DynValue::Num(value);
        self
    }

    pub fn addr(mut self, addr: DynAddr) -> Self {
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

    pub fn instruction(mut self, i: Instruction<DynAddr>) -> Self {
        assert_eq!(
            self.memory[self.addr],
            DynValue::Num(0),
            "writing in occupied memory"
        );
        let addr = self.addr;
        self.addr += 1;
        self.memory[addr] = DynValue::Instruction(i);
        self
    }

    pub fn build(self) -> Mima {
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

type Labels = HashMap<&'static str, u32>;

/// A "dynamic" value used for constructing a MIMA.
/// It may contain a 24bit number, or an instruction, which may contain a label.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum DynValue {
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
pub enum DynAddr {
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

impl Instruction<DynAddr> {
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
    ($($t:tt)*) => {{
        use $crate::{Builder, DynAddr, Instruction, _mima};
        _mima!(@
            Builder::default();
            $($t)*
        ).build()
    }}
}
