use std::fmt::{Debug, Formatter, Result};

macro_rules! concat_u8 {
    ($high:expr, $low:expr) => {
        ($high as u16) << 8 | ($low as u16)
    }
}

macro_rules! split_u16 {
    ($value:expr) => {
        (($value as u16 >> 8) as u8, ($value as u16 & 0xFF) as u8)
    }
}

macro_rules! m_time {
    ($cpu:expr, $value:expr) => {
        {
            $cpu.registers.m += $value;
            $cpu.registers.t += $value << 2;
        }
    }
}

macro_rules! reg_set {
    ($cpu:expr, $($target:tt=$value:expr),+) => {
        $(
            $cpu.registers.$target = $value;
        )+
    };
    ($cpu:expr, {$($target:tt : $value:expr),+}) => {
        $(
            $cpu.registers.$target = $value;
        )+
    }
}

macro_rules! mem_set {
    ($cpu:expr, $($target:tt=$value:expr),+) => {
        $(
            $cpu.mmu.write_u8($target, $value);
        )+
    };
    ($cpu:expr, {$($target:tt : $value:expr),+}) => {
        $(
            $cpu.mmu.write_u8($target, $value);
        )+
    }
}

macro_rules! read {
    ($cpu:expr, u16) => {
        {
            let lower = $cpu.mmu.read_u8($cpu.registers.pc);
            $cpu.registers.pc += 1;
            let upper = $cpu.mmu.read_u8($cpu.registers.pc);
            $cpu.registers.pc += 1;
            m_time!($cpu, 2);
            concat_u8!(upper, lower)
        }
    };
    ($cpu:expr, u8) => {
        {
            let val = $cpu.mmu.read_u8($cpu.registers.pc);
            m_time!($cpu, 1);
            $cpu.registers.pc += 1;
            val
        }
    };
    ($cpu:expr, ($high:tt, $low:tt)) => {
        {
            $cpu.mmu.read_u8(read!($cpu, $high, $low))
        }
    };
    ($cpu:expr, $high:tt, $low:tt) => {
        {
            concat_u8!($cpu.registers.$high, $cpu.registers.$low)
        }
    };
    ($cpu:expr, $r:tt) => {
        $cpu.registers.$r
    };
}

macro_rules! write {
    ($cpu:expr, u16, $value:expr) => {
        {
            let (upper, lower) = split_u16!($value);
            let address = read!($cpu, u16);
            $cpu.mmu.write_u8(address, lower);
            $cpu.mmu.write_u8(address + 1, upper);
            m_time!($cpu, 2);
        }
    };
    ($cpu:expr, ($high:tt, $low:tt), $value:expr) => {
        {
            $cpu.mmu.write_u8(read!($cpu, $high, $low), $value);
            m_time!($cpu, 1);
        }
    };
    ($cpu:expr, $high:tt, $low:tt, $value:expr) => {
        {
            let (upper, lower) = split_u16!($value);
            $cpu.registers.$high = upper;
            $cpu.registers.$low = lower;
        }
    };
    ($cpu:expr, $r:tt, $value:expr) => {
        {
            $cpu.registers.$r = $value;
        }
    };
}

macro_rules! nop {
    () => {
        |cpu: &mut Z80<Execute>| {
        }
    }
}

macro_rules! ld {
    ([$($target:tt)+], [$($source:tt)+]) => {
        |cpu: &mut Z80<Execute>| {
            let val = read!(cpu, $($source)+);
            write!(cpu, $($target)+, val);
        }
    }
}

macro_rules! inc {
    ($target:tt) => {
        |cpu: &mut Z80<Execute>| {
            let current = read!(cpu, $target);
            let new = current.wrapping_add(1);
            cpu.registers.f &= 0b0001_0000;
            let z = if new == 0 { 0b1000_0000 } else { 0b0000_0000 };
            let h = (((current ^ 1) ^ new) & 0b0001_0000) << 1;
            cpu.registers.f |= z | h;
            write!(cpu, $target, new);
        }
    };
    ($high:tt, $low:tt) => {
        |cpu: &mut Z80<Execute>| {
            let val = read!(cpu, $high, $low);
            write!(cpu, $high, $low, val.wrapping_add(1));
            m_time!(cpu, 1);
        }
    };
    ($($target:tt)+) => {
        |cpu: &mut Z80<Execute>| {
            let val = read!(cpu, $($target)+);
            write!(cpu, $($target)+, val.wrapping_add(1));
        }
    }
}

macro_rules! dec {
    ($target:tt) => {
        |cpu: &mut Z80<Execute>| {
            let current = read!(cpu, $target);
            let new = current.wrapping_sub(1);
            cpu.registers.f &= 0b0001_0000;
            let z = if new == 0 { 0b1000_0000 } else { 0b0000_0000 };
            let h = (((current ^ 1) ^ new) & 0b0001_0000) << 1;
            const N : u8 = 0b0100_0000;
            cpu.registers.f |= z | h | N;
            write!(cpu, $target, new);
        }
    }
}

macro_rules! rlca {
    () => {
        |cpu: &mut Z80<Execute>| {
            let val = read!(cpu, a);
            let c = (val & 0b1000_0000) >> 7;
            cpu.registers.f = c << 4;
            write!(cpu, a, (val << 1) | c);
        }
    }
}

macro_rules! rla {
    () => {
        |cpu: &mut Z80<Execute>| {
            let val = read!(cpu, a);
            let c = (cpu.registers.f & 0b0001_0000);
            cpu.registers.f = (val & 0b1000_0000) >> 3;
            write!(cpu, a, (val << 1) | (c >> 4));
        }
    }
}

macro_rules! add {
    ([$($target:tt)+], [$($source:tt)+]) => {
        |cpu: &mut Z80<Execute>| {
            let src = u32::from(read!(cpu, $($source)+));
            let dst = u32::from(read!(cpu, $($target)+));
            let result = src + dst;
            let h = (((src ^ dst) ^ result) & 0x1000) >> 7;
            let c = result >> 12;
            cpu.registers.f &= 0b1000_0000;
            cpu.registers.f |= ((h | c) as u8) & 0b0011_0000;
            write!(cpu, $($target)+, result as u16);
            m_time!(cpu, 1);
        }
    }
}

#[derive(Debug, PartialEq)]
struct Z80<S: State> {
    registers: Registers,
    mmu: Mmu,
    s: S
}

#[derive(Debug, PartialEq)]
struct Registers {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    h: u8,
    l: u8,
    f: u8,
    pc: u16,
    sp: u16,
    m: u64,
    t: u64,
}

impl Registers {
    fn new() -> Registers {
        Registers {
            a: 0,
            b: 0,
            c: 0,
            d: 0,
            e: 0,
            h: 0,
            l: 0,
            f: 0,
            pc: 0,
            sp: 0,
            m: 0,
            t: 0,
        }
    }
}

trait State {}
#[derive(Debug, PartialEq)]
struct Fetch {}
#[derive(Debug, PartialEq)]
struct Decode {
    opcode: u8,
}
#[derive(Debug, PartialEq)]
struct Execute {
    instruction: Instruction,
}

impl State for Fetch {}
impl State for Decode {}
impl State for Execute {}

impl<S> Z80<S>
where
    S: State,
{
    fn new() -> Z80<Fetch> {
        Z80 {
            s: Fetch {},
            mmu: Mmu::new(),
            registers: Registers::new(),
        }
    }
}

impl Z80<Fetch> {
    fn fetch(self) -> Z80<Decode> {
        Z80 {
            s: Decode {
                opcode: self.mmu.read_u8(self.registers.pc),
            },
            registers: Registers {
                m: self.registers.m + 1,
                t: self.registers.t + 4,
                pc: self.registers.pc + 1,
                ..self.registers
            },
            mmu: self.mmu
        }
    }
}

impl Z80<Decode> {
    fn decode(self) -> Z80<Execute> {
        let function = match self.s.opcode {
            0x00 => nop!(),
            0x01 => ld!([b, c], [u16]),
            0x02 => ld!([(b, c)], [a]),
            0x03 => inc!(b, c),
            0x04 => inc!(b),
            0x05 => dec!(b),
            0x06 => ld!([b], [u8]),
            0x07 => rlca!(),
            0x08 => ld!([u16], [sp]),
            0x09 => add!([h, l], [b, c]),
            0x17 => rla!(),
            _ => panic!("Uh, oh")
        };

        Z80 {
            s: Execute {
                instruction: Instruction::new(self.s.opcode, function),
            },
            registers: self.registers,
            mmu: self.mmu
        }
    }
}

impl Z80<Execute> {
    fn execute(&mut self) {
        (self.s.instruction.function)(self)
    }
}

struct Instruction {
    opcode: u8,
    function: fn(&mut Z80<Execute>)
}

impl Debug for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.debug_struct("Instruction")
            .field("opcode", &self.opcode)
            .finish()
    }
}

impl PartialEq for Instruction {
    fn eq(&self, other: &Self) -> bool {
        self.opcode == other.opcode
    }

    fn ne(&self, other: &Self) -> bool {
        !self.eq(other)
    }
}

impl Instruction {
    fn new(opcode: u8, function: fn(&mut Z80<Execute>)) -> Instruction {
        Instruction {
            opcode,
            function,
        }
    }
}

#[derive(Debug, PartialEq)]
struct Mmu {
    bank: [u8; Mmu::SIZE],
}

impl Mmu {
    const SIZE: usize = u16::MAX as usize;

    fn new() -> Mmu {
        Mmu {
            bank: [0; Mmu::SIZE],
        }
    }

    fn read_u8(&self, address: u16) -> u8 {
        self.bank[address as usize]
    }

    fn write_u8(&mut self, address: u16, value: u8) {
        self.bank[address as usize] = value
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::read_dir;
    macro_rules! reg_eq {
        ($cpu:expr, $($target:tt=$value:expr),+) => {
            $(
                assert_eq!($cpu.registers.$target, $value);
            )+
        };
        ($cpu:expr, {$($target:tt : $value:expr),+}) => {
            $(
                assert_eq!($cpu.registers.$target, $value);
            )+
        }
    }

    macro_rules! mem_eq {
        ($cpu:expr, {$(($high:expr, $low:expr) : $value:expr),+}) => {
            $(
                assert_eq!($cpu.mmu.read_u8(concat_u8!($high, $low)), $value);
            )+
        }
    }

    macro_rules! assert_instruction {
        (
            opcode : $opcode:tt
            $macro:tt : $body:tt
            setup : {$($setupMacro:tt : $setupBody:tt),*}
            assert : {$($assertMacro:tt : $assertBody:tt),*}
        ) => {
            {
                let mut fetcherA = Z80::<Fetch>::new();
                let mut fetcherB = Z80::<Fetch>::new();
                let function = $macro!$body;
                $(
                    $setupMacro!(fetcherA, $setupBody);
                    $setupMacro!(fetcherB, $setupBody);
                )*
                fetcherA.mmu.write_u8(fetcherA.registers.pc, $opcode);
                fetcherB.mmu.write_u8(fetcherA.registers.pc, $opcode);
                let mut cpuA = fetcherA.fetch().decode();
                let mut cpuB = fetcherB.fetch().decode();
                assert_eq!(cpuA.s.instruction.opcode, $opcode);
                (function)(&mut cpuA);
                $(
                    $assertMacro!(cpuA, $assertBody);
                )*
                (cpuB.s.instruction.function)(&mut cpuB);
                $(
                    $assertMacro!(cpuB, $assertBody);
                )*
            };
        }
    }

    fn cpu_decode(opcode: u8) -> Z80<Execute> {
        Z80 {
            s: Decode { opcode },
            mmu: Mmu::new(),
            registers: Registers::new()
        }.decode()
    }

    fn cpu_execute() -> Z80<Execute> {
        Z80 {
            s: Decode { opcode: 0x00 },
            mmu: Mmu::new(),
            registers: Registers::new()
        }.decode()
    }

    #[test]
    fn registers_new() {
        let registers = Registers::new();
        assert_eq!(
            registers,
            Registers {
                a: 0,
                b: 0,
                c: 0,
                d: 0,
                e: 0,
                h: 0,
                l: 0,
                f: 0,
                pc: 0,
                sp: 0,
                m: 0,
                t: 0
            }
        )
    }

    #[test]
    fn cpu_pipeline() {
        let mmu = Mmu::new();
        let fetcher = Z80::<Fetch>::new().fetch().decode().execute();
    }

    #[test]
    fn reg_set_macro() {
        let mut cpu = cpu_execute();
        reg_set!(cpu, a=22, b=44);
        reg_set!(cpu, c=1);
        assert_eq!(22, cpu.registers.a);
        assert_eq!(44, cpu.registers.b);
        assert_eq!(1, cpu.registers.c);
    }

    #[test]
    fn m_time_macro() {
        let mut cpu = cpu_execute();
        let assert_m = |c: &mut Z80<Execute>, m| {
            let expected_m = c.registers.m + m;
            let expected_t = c.registers.t + (m << 2);
            m_time!(c, m);
            assert_eq!(expected_m, c.registers.m);
            assert_eq!(expected_t, c.registers.t);
        };
        assert_m(&mut cpu, 1);
    }

    #[test]
    fn concat_u8_macro() {
        let h = 2;
        let l = 1;
        let expected = ((h as u16) << 8 | l as u16);
        assert_eq!(expected, concat_u8!(h, l));
    }

    #[test]
    fn read_macro() {
        let mut cpu = cpu_execute();
        reg_set!(cpu, a=100, h=2, l=1);
        let address = concat_u8!(cpu.registers.h, cpu.registers.l);
        let value = 24 as u8;
        cpu.mmu.write_u8(address, value);
        cpu.mmu.write_u8(0, (address & 0xFF) as u8);
        cpu.mmu.write_u8(1, (address >> 8) as u8);

        // 8 bit register read
        assert_eq!(100, read!(cpu, a));
        // 16 bit register read
        assert_eq!(address, read!(cpu, h, l));
        // 16 bit indirect read from memory
        assert_eq!(value, read!(cpu, (h, l)));
        // 16 bit immediate mode
        assert_eq!(address, read!(cpu, u16));
        assert_eq!(2, cpu.registers.pc);
    }

    #[test]
    fn write_macro() {
        let mut cpu = cpu_execute();
        reg_set!(cpu, h=2, l=1);
        let expected_u16 =  concat_u8!(cpu.registers.h, cpu.registers.l);
        let expected_u8 = 10;
        // 8 bit register write
        write!(cpu, a, expected_u8);
        assert_eq!(expected_u8, cpu.registers.a);
        // 16 bit register write
        write!(cpu, b, c, expected_u16);
        assert_eq!(expected_u16, concat_u8!(cpu.registers.b, cpu.registers.c));
        // 8 bit indirect memory write
        write!(cpu, (h, l), expected_u8);
        assert_eq!(expected_u8, cpu.mmu.read_u8(concat_u8!(cpu.registers.h, cpu.registers.l)));
    }

    #[test]
    fn ld_macro() {
        let upper = 2;
        let lower = 1;
        let mut cpu = cpu_execute();
        let expected_u16 = concat_u8!(upper, lower);
        cpu.mmu.write_u8(0, lower);
        cpu.mmu.write_u8(1, upper);

        // LD BC, u16
        ld!([b, c], [u16])(&mut cpu);
        reg_eq!(cpu, b=upper, c=lower);

        // LD BC, HL
        reg_set!(cpu, b=0, c=0, h=upper, l=lower);
        ld!([b, c], [h, l])(&mut cpu);
        reg_eq!(cpu, b=upper, c=lower, h=upper, l=lower);
    }

    #[test]
    fn nop() {
        assert_instruction!(
            opcode : 0x00
            nop : [ ]
            setup : { }
            assert : {
                reg_eq : {
                    m : 1,
                    t : 4,
                    f : 0
                }
            }
        )
    }

    #[test]
    fn ld_bc_u16() {
        assert_instruction! {
            opcode : 0x01
            ld : [ [b, c], [u16] ]
            setup : {
                mem_set : {
                    1 : 2,
                    2 : 1
                }
            }
            assert : {
                reg_eq : {
                    b : 1,
                    c : 2,
                    m : 3,
                    t : 12,
                    f : 0,
                    pc : 3
                }
            }
        }
    }

    #[test]
    fn ld_bc_a_indirect() {
        assert_instruction! {
            opcode : 0x02
            ld : [ [(b, c)], [a] ]
            setup : {
                reg_set : {
                    a : 22,
                    b: 1,
                    c: 1
                }
            }
            assert : {
                reg_eq : {
                    a : 22,
                    b : 1,
                    c : 1,
                    m : 2,
                    t : 8,
                    f : 0
                },
                mem_eq : {
                    (1, 1) : 22
                }
            }
        }
    }

    #[test]
    fn inc_bc() {
        assert_instruction! {
            opcode : 0x03
            inc : [ b, c ]
            setup : {
                reg_set : {
                    b : 1,
                    c : 2
                }
            }
            assert : {
                reg_eq : {
                    b : 1,
                    c : 3,
                    m : 2,
                    t : 8,
                    f : 0
                }
            }
        }
    }

    #[test]
    fn inc_b() {
        assert_instruction! {
            opcode : 0x04
            inc : [ b ]
            setup : { }
            assert : {
                reg_eq : {
                    m : 1,
                    t : 4,
                    b : 1,
                    f : 0
                }
            }
        }
    }

    #[test]
    fn inc_b_sets_half_carry_flag() {
        assert_instruction! {
            opcode : 0x04
            inc : [ b ]
            setup : {
                reg_set : {
                    b : 0b0000_1111
                }
            }
            assert : {
                reg_eq : {
                    m : 1,
                    t : 4,
                    b : 0b0001_0000,
                    f : 0b0010_0000
                }
            }
        }
    }

    #[test]
    fn inc_b_sets_zero_flag() {
        assert_instruction! {
            opcode : 0x04
            inc : [ b ]
            setup : {
                reg_set : {
                    b : 0b1111_1111
                }
            }
            assert : {
                reg_eq : {
                    m : 1,
                    t : 4,
                    b : 0,
                    f : 0b1010_0000
                }
            }
        }
    }

    #[test]
    fn inc_b_clears_negative_flag() {
        assert_instruction! {
            opcode : 0x04
            inc : [ b ]
            setup : {
                reg_set : {
                    b : 0,
                    f : 0b1110_0000
                }
            }
            assert : {
                reg_eq : {
                    m : 1,
                    t : 4,
                    b : 1,
                    f : 0
                }
            }
        }
    }

    #[test]
    fn dec_b() {
        assert_instruction! {
            opcode : 0x05
            dec : [ b ]
            setup : {
                reg_set : {
                    b : 2
                }
            }
            assert : {
                reg_eq : {
                    m : 1,
                    t : 4,
                    b : 1,
                    f : 0b0100_0000
                }
            }
        }
    }

    #[test]
    fn dec_b_sets_zero_flag() {
        assert_instruction! {
            opcode : 0x05
            dec : [ b ]
            setup : {
                reg_set : {
                    b : 1
                }
            }
            assert : {
                reg_eq : {
                    m : 1,
                    t : 4,
                    b : 0,
                    f : 0b1100_0000
                }
            }
        }
    }

    #[test]
    fn dec_b_sets_half_carry_flag() {
        assert_instruction! {
            opcode : 0x05
            dec : [ b ]
            setup : {
                reg_set : {
                    b : 0b0001_0000
                }
            }
            assert : {
                reg_eq : {
                    m : 1,
                    t : 4,
                    b : 0b0000_1111,
                    f : 0b0110_0000
                }
            }
        }
    }

    #[test]
    fn dec_b_sets_neg_flag() {
        assert_instruction! {
            opcode : 0x05
            dec : [ b ]
            setup : {
                reg_set : {
                    b : 2,
                    f : 0b1110_0000
                }
            }
            assert : {
                reg_eq : {
                    m : 1,
                    t : 4,
                    b : 1,
                    f : 0b0100_0000
                }
            }
        }
    }

    #[test]
    fn ld_b_u8() {
        assert_instruction! {
            opcode : 0x06
            ld : [ [b], [u8] ]
            setup : {
                mem_set : {
                    1 : 3
                }
            }
            assert : {
                reg_eq : {
                    m : 2,
                    t : 8,
                    b : 3,
                    pc : 2
                }
            }
        }
    }

    #[test]
    fn rlca() {
        assert_instruction! {
            opcode : 0x07
            rlca : [ ]
            setup : {
                reg_set : {
                    a : 0x95,
                    f : 0b0001_0000
                }
            }
            assert : {
                reg_eq : {
                    m : 1,
                    t : 4,
                    a : 0x2B,
                    f : 0b0001_0000
                }
            }
        }
    }

    #[test]
    fn ld_u16_sp_indirect() {
        assert_instruction! {
            opcode : 0x08
            ld : [ [u16], [sp] ]
            setup : {
                reg_set : {
                    sp : 0b1000_1000_0100_0100
                },
                mem_set : {
                    1 : 0b0001_0001,
                    2 : 0b0010_0010
                }
            }
            assert : {
                reg_eq : {
                    m : 5,
                    t : 20,
                    pc : 3
                },
                mem_eq : {
                    (0b0010_0010, 0b0001_0001) : 0b0100_0100,
                    (0b0010_0010, 0b0001_0010) : 0b1000_1000
                }
            }
        }
    }

    #[test]
    fn add_hl_bc() {
        assert_instruction! {
            opcode : 0x09
            add : [ [h, l], [b, c] ]
            setup : {
                reg_set : {
                    b : 0b0000_0001,
                    c : 0b0000_0010,
                    h : 0b0000_0100,
                    l : 0b0000_1000,
                    f : 0b1100_0000
                }
            }
            assert : {
                reg_eq : {
                    m : 2,
                    t : 8,
                    b : 0b0000_0001,
                    c : 0b0000_0010,
                    h : 0b0000_0101,
                    l : 0b0000_1010,
                    f : 0b1000_0000
                }
            }
        }
    }

    #[test]
    fn add_hl_bc_half_carry() {
        assert_instruction! {
            opcode : 0x09
            add : [ [h, l], [b, c] ]
            setup : {
                reg_set : {
                    b : 0b0000_1000,
                    h : 0b0000_1000
                }
            }
            assert : {
                reg_eq : {
                    m : 2,
                    t : 8,
                    h : 0b0001_0000,
                    f : 0b0010_0000
                }
            }
        }
    }

    #[test]
    fn add_hl_bc_carry() {
        assert_instruction! {
            opcode : 0x09
            add : [ [h, l], [b, c] ]
            setup : {
                reg_set : {
                    b : 0b1000_1010,
                    h : 0b1000_0101
                }
            }
            assert : {
                reg_eq : {
                    m : 2,
                    t : 8,
                    h : 0b0000_1111,
                    f : 0b0001_0000
                }
            }
        }
    }

    #[test]
    fn rla() {
        assert_instruction! {
            opcode : 0x17
            rla : [ ]
            setup : {
                reg_set : {
                    a : 0x85
                }
            }
            assert : {
                reg_eq : {
                    m : 1,
                    t : 4,
                    a : 0x0A,
                    f : 0b0001_0000
                }
            }
        }
    }
}
