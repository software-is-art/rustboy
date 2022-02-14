use std::fmt::{Debug, Formatter, Result};
use paste::paste;

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
    ($cpu:expr, {$(($high:expr, $low:expr) : $value:expr),+}) => {
        $(
            $cpu.mmu.write_u8(concat_u8!($high, $low), $value);
        )+
    };
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

macro_rules! flags {
    (z) => {
        0b1000_0000
    };
    (n) => {
        0b0100_0000
    };
    (h) => {
        0b0010_0000
    };
    (c) => {
        0b0001_0000
    };
    ($($flag:tt)+) => {
        $(
            flags!($flag) |
        )+ 0x0000_0000;
    };
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
            m_time!($cpu, 1);
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
    ($cpu:expr, (u16), $value:expr) => {
        {
            let (upper, lower) = split_u16!($value);
            let address = read!($cpu, u16);
            $cpu.mmu.write_u8(address, lower);
            $cpu.mmu.write_u8(address + 1, upper);
            m_time!($cpu, 2);
        }
    };
    ($cpu:expr, ($high:tt, $low:tt)+, $value:expr) => {
        {
            $cpu.mmu.write_u8(read!($cpu, $high, $low) + 1, $value);
            m_time!($cpu, 1);
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
    ($high:tt, $low:tt) => {
        |cpu: &mut Z80<Execute>| {
            let val = read!(cpu, $high, $low);
            write!(cpu, $high, $low, val.wrapping_sub(1));
        }
    };
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

macro_rules! rrca {
    () => {
        |cpu: &mut Z80<Execute>| {
            let val = read!(cpu, a);
            let c = (val & 0b0000_0001) << 7;
            cpu.registers.f = c >> 3;
            write!(cpu, a, (val >> 1) | c);
        }
    }
}

macro_rules! rra {
    () => {
        |cpu: &mut Z80<Execute>| {
            let val = read!(cpu, a);
            let c = cpu.registers.f << 3;
            cpu.registers.f = (val & 0b0000_0001) << 4;
            write!(cpu, a, (val >> 1) | c);
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

macro_rules! stop {
    () => {
        |cpu: &mut Z80<Execute>| {
            cpu.registers.pc += 1;
        }
    }
}

macro_rules! jr {
    (i8) => {
        |cpu: &mut Z80<Execute>| {
            let offset = i16::from(cpu.mmu.read_u8(cpu.registers.pc) as i8);
            cpu.registers.pc = ((1i16 + offset) + (cpu.registers.pc as i16)) as u16;
            m_time!(cpu, 2);
        }
    };
    ([$($flags:tt)+], [i8]) => {
        |cpu: &mut Z80<Execute>| {
            let offset = i16::from(cpu.mmu.read_u8(cpu.registers.pc) as i8);
            cpu.registers.pc += 1u16;
            m_time!(cpu, 1);
            let mut mask: u8 = flags!($($flags)+);
            if ((cpu.registers.f & mask) == mask) {
                m_time!(cpu, 1);
                cpu.registers.pc = (offset + cpu.registers.pc as i16) as u16;
            }
        }
    };
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
            0x08 => ld!([(u16)], [sp]),
            0x09 => add!([h, l], [b, c]),
            0x0A => ld!([a], [(b, c)]),
            0x0B => dec!(b, c),
            0x0C => inc!(c),
            0x0D => dec!(c),
            0x0E => ld!([c], [u8]),
            0x0F => rrca!(),
            0x10 => stop!(),
            0x11 => ld!([d, e], [u16]),
            0x12 => ld!([(d, e)], [a]),
            0x13 => inc!(d, e),
            0x14 => inc!(d),
            0x15 => dec!(d),
            0x16 => ld!([d], [u8]),
            0x17 => rla!(),
            0x18 => jr!(i8),
            0x19 => add!([h, l], [d, e]),
            0x1A => ld!([a], [(d, e)]),
            0x1B => dec!(d, e),
            0x1C => inc!(e),
            0x1D => dec!(e),
            0x1E => ld!([e], [u8]),
            0x1F => rra!(),
            0x20 => jr!([n z], [i8]),
            0x21 => ld!([h, l], [u16]),
            0x22 => ld!([(h, l)+], [a]),
            0x23 => inc!(h, l),
            0x24 => inc!(h),
            0x25 => dec!(h),
            0x26 => ld!([h], [u8]),
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
            }
        };
    }

    macro_rules! assert_inc_x8 {
        ($operand:tt, $opcode:expr) => {
            paste! {
                #[test]
                fn [<inc_ $operand>]() {
                    assert_instruction! {
                        opcode : $opcode
                        inc : [ $operand ]
                        setup : {
                            reg_set : {
                                f : 0b1111_0000
                            }
                        }
                        assert : {
                            reg_eq : {
                                m : 1,
                                t : 4,
                                $operand : 1,
                                f : 0b0001_0000,
                                pc : 1
                            }
                        }
                    }
                }

                #[test]
                fn [<inc_ $operand _sets_half_carry_flag>]() {
                    assert_instruction! {
                        opcode : $opcode
                        inc : [ $operand ]
                        setup : {
                            reg_set : {
                                $operand : 0b0000_1111
                            }
                        }
                        assert : {
                            reg_eq : {
                                m : 1,
                                t : 4,
                                $operand : 0b0001_0000,
                                f : 0b0010_0000,
                                pc : 1
                            }
                        }
                    }
                }

                #[test]
                fn [<inc_ $operand _sets_zero_flag>]() {
                    assert_instruction! {
                        opcode : $opcode
                        inc : [ $operand ]
                        setup : {
                            reg_set : {
                                $operand : 0b1111_1111
                            }
                        }
                        assert : {
                            reg_eq : {
                                m : 1,
                                t : 4,
                                $operand : 0,
                                f : 0b1010_0000,
                                pc : 1
                            }
                        }
                    }
                }

                #[test]
                fn [<inc_ $operand _clears_negative_flag>]() {
                    assert_instruction! {
                        opcode : $opcode
                        inc : [ $operand ]
                        setup : {
                            reg_set : {
                                $operand : 0,
                                f : 0b1110_0000
                            }
                        }
                        assert : {
                            reg_eq : {
                                m : 1,
                                t : 4,
                                $operand : 1,
                                f : 0,
                                pc : 1
                            }
                        }
                    }
                }
            }
        };
    }

    macro_rules! assert_dec_x8 {
        ($operand:tt, $opcode:expr) => {
            paste! {
                #[test]
                fn [<dec_ $operand>]() {
                    assert_instruction! {
                        opcode : $opcode
                        dec : [ $operand ]
                        setup : {
                            reg_set : {
                                $operand : 2
                            }
                        }
                        assert : {
                            reg_eq : {
                                m : 1,
                                t : 4,
                                $operand : 1,
                                f : 0b0100_0000,
                                pc : 1
                            }
                        }
                    }
                }

                #[test]
                fn [<dec_ $operand _sets_zero_flag>]() {
                    assert_instruction! {
                        opcode : $opcode
                        dec : [ $operand ]
                        setup : {
                            reg_set : {
                                $operand : 1
                            }
                        }
                        assert : {
                            reg_eq : {
                                m : 1,
                                t : 4,
                                $operand : 0,
                                f : 0b1100_0000,
                                pc : 1
                            }
                        }
                    }
                }

                #[test]
                fn [<dec_ $operand _sets_half_carry_flag>]() {
                    assert_instruction! {
                        opcode : $opcode
                        dec : [ $operand ]
                        setup : {
                            reg_set : {
                                $operand : 0b0001_0000
                            }
                        }
                        assert : {
                            reg_eq : {
                                m : 1,
                                t : 4,
                                $operand : 0b0000_1111,
                                f : 0b0110_0000,
                                pc : 1
                            }
                        }
                    }
                }

                #[test]
                fn [<dec_ $operand _sets_neg_flag>]() {
                    assert_instruction! {
                        opcode : $opcode
                        dec : [ $operand ]
                        setup : {
                            reg_set : {
                                $operand : 2,
                                f : 0b1110_0000
                            }
                        }
                        assert : {
                            reg_eq : {
                                m : 1,
                                t : 4,
                                $operand : 1,
                                f : 0b0100_0000,
                                pc : 1
                            }
                        }
                    }
                }
            }
        };
    }

    macro_rules! assert_ld_u8 {
        ($operand:tt, $opcode:expr) => {
            paste! {
                #[test]
                fn [<ld_ $operand _u8>]() {
                    assert_instruction! {
                        opcode : $opcode
                        ld : [ [$operand], [u8] ]
                        setup : {
                            mem_set : {
                                1 : 3
                            }
                        }
                        assert : {
                            reg_eq : {
                                m : 2,
                                t : 8,
                                $operand : 3,
                                pc : 2
                            }
                        }
                    }
                }
            }
        };
    }

    macro_rules! assert_ld_u16_x16 {
        ($high:tt, $low:tt, $opcode:expr) => {
            paste! {
                #[test]
                fn [<ld_ $high $low _u16>]() {
                    assert_instruction! {
                        opcode : $opcode
                        ld : [ [$high, $low], [u16] ]
                        setup : {
                            mem_set : {
                                1 : 2,
                                2 : 1
                            }
                        }
                        assert : {
                            reg_eq : {
                                $high : 1,
                                $low : 2,
                                m : 3,
                                t : 12,
                                f : 0,
                                pc : 3
                            }
                        }
                    }
                }
            }
        };
    }

    macro_rules! assert_ld_indirect_target_x8 {
        ([$high:tt, $low:tt], [$src:tt], $opcode:expr) => {
            paste! {
                #[test]
                fn [<ld_ $high $low _ $src _indirect>]() {
                    assert_instruction! {
                        opcode : $opcode
                        ld : [ [($high, $low)], [$src] ]
                        setup : {
                            reg_set : {
                                $src : 22,
                                $high: 1,
                                $low: 1
                            }
                        }
                        assert : {
                            reg_eq : {
                                $src : 22,
                                $high : 1,
                                $low : 1,
                                m : 2,
                                t : 8,
                                f : 0,
                                pc : 1
                            },
                            mem_eq : {
                                (1, 1) : 22
                            }
                        }
                    }
                }
            }
        };
    }

    macro_rules! assert_ld_indirect_target_x8_with_offset {
        ([$high:tt, $low:tt]+, [$src:tt], $opcode:expr) => {
            assert_ld_indirect_target_x8_with_offset!([$high, $low], +, 1, [$src], $opcode);
        };
        ([$high:tt, $low:tt], $offset_token:tt, $offset:expr, [$src:tt], $opcode:expr) => {
            paste! {
                #[test]
                fn [<ld_ $high $low _ $src _indirect>]() {
                    assert_instruction! {
                        opcode : $opcode
                        ld : [ [($high, $low)$offset_token], [$src] ]
                        setup : {
                            reg_set : {
                                $src : 22,
                                $high: 1,
                                $low: 1
                            }
                        }
                        assert : {
                            reg_eq : {
                                $src : 22,
                                $high : 1,
                                $low : 1,
                                m : 2,
                                t : 8,
                                f : 0,
                                pc : 1
                            },
                            mem_eq : {
                                (1, 1 + $offset) : 22
                            }
                        }
                    }
                }
            }
        };
    }

    macro_rules! assert_inc_x16 {
        ($high:tt, $low:tt, $opcode:expr) => {
            paste! {
                #[test]
                fn [<inc_ $high $low>]() {
                    assert_instruction! {
                        opcode : $opcode
                        inc : [ $high, $low ]
                        setup : {
                            reg_set : {
                                $high : 1,
                                $low : 2
                            }
                        }
                        assert : {
                            reg_eq : {
                                $high : 1,
                                $low : 3,
                                m : 2,
                                t : 8,
                                f : 0,
                                pc : 1
                            }
                        }
                    }
                }
            }
        };
    }

    macro_rules! assert_add_x16 {
        ([$tgt_high:tt, $tgt_low:tt], [$src_high:tt, $src_low:tt], $opcode:expr) => {
            paste! {
                #[test]
                fn [<add_ $tgt_high $tgt_low _ $src_high $src_low>]() {
                    assert_instruction! {
                        opcode : $opcode
                        add : [ [$tgt_high, $tgt_low], [$src_high, $src_low] ]
                        setup : {
                            reg_set : {
                                $src_high : 0b0000_0001,
                                $src_low : 0b0000_0010,
                                $tgt_high : 0b0000_0100,
                                $tgt_low : 0b0000_1000,
                                f : 0b1100_0000
                            }
                        }
                        assert : {
                            reg_eq : {
                                m : 2,
                                t : 8,
                                $src_high : 0b0000_0001,
                                $src_low : 0b0000_0010,
                                $tgt_high : 0b0000_0101,
                                $tgt_low : 0b0000_1010,
                                f : 0b1000_0000,
                                pc : 1
                            }
                        }
                    }
                }

                #[test]
                fn [<add_ $tgt_high $tgt_low _ $src_high $src_low _half_carry>]() {
                    assert_instruction! {
                        opcode : $opcode
                        add : [ [$tgt_high, $tgt_low], [$src_high, $src_low] ]
                        setup : {
                            reg_set : {
                                $src_high : 0b0000_1000,
                                $tgt_high : 0b0000_1000
                            }
                        }
                        assert : {
                            reg_eq : {
                                m : 2,
                                t : 8,
                                $tgt_high : 0b0001_0000,
                                f : 0b0010_0000,
                                pc : 1
                            }
                        }
                    }
                }

                #[test]
                fn [<add_ $tgt_high $tgt_low _ $src_high $src_low _carry>]() {
                    assert_instruction! {
                        opcode : $opcode
                        add : [ [$tgt_high, $tgt_low], [$src_high, $src_low] ]
                        setup : {
                            reg_set : {
                                $src_high : 0b1000_1010,
                                $tgt_high : 0b1000_0101
                            }
                        }
                        assert : {
                            reg_eq : {
                                m : 2,
                                t : 8,
                                $tgt_high : 0b0000_1111,
                                f : 0b0001_0000,
                                pc : 1
                            }
                        }
                    }
                }
            }
        }
    }

    macro_rules! assert_ld_indirect_src_x8 {
        ($target:tt, [$src_high:tt, $src_low:tt], $opcode:expr) => {
            paste! {
                #[test]
                fn [<ld_ $target _ $src_high $src_low _indirect>]() {
                    assert_instruction! {
                        opcode : $opcode
                        ld : [[$target], [($src_high, $src_low)]]
                        setup : {
                            reg_set : {
                                $src_high : 4,
                                $src_low : 5
                            },
                            mem_set : {
                                (4, 5) : 10
                            }
                        }
                        assert : {
                            reg_eq : {
                                m : 2,
                                t : 8,
                                $target : 10,
                                pc : 1
                            }
                        }
                    }
                }
            }
        }
    }

    macro_rules! assert_dec_x16 {
        ([$src_high:tt, $src_low:tt], $opcode:expr) => {
            paste! {
                #[test]
                fn [<dec_ $src_high $src_low>]() {
                    assert_instruction! {
                        opcode : $opcode
                        dec : [ $src_high, $src_low ]
                        setup : {
                            reg_set : {
                                $src_high : 0,
                                $src_low : 0,
                                f : 0b1111_0000
                            }
                        }
                        assert : {
                            reg_eq : {
                                $src_high : 0xFF,
                                $src_low : 0xFF,
                                f : 0b1111_0000,
                                pc : 1
                            }
                        }
                    }
                }
            }
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
    fn flags_macro() {
        assert_eq!(0b1000_0000, flags!(z));
        assert_eq!(0b0100_0000, flags!(n));
        assert_eq!(0b0010_0000, flags!(h));
        assert_eq!(0b0001_0000, flags!(c));
        assert_eq!(flags!(z) | flags!(n) | flags!(h) | flags!(c), flags!(z n h c));
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
                    f : 0,
                    pc : 1
                }
            }
        )
    }

assert_ld_u16_x16!(b, c, 0x01);
assert_ld_u16_x16!(d, e, 0x11);
assert_ld_u16_x16!(h, l, 0x21);

assert_ld_indirect_target_x8!([b, c], [a], 0x02);
assert_ld_indirect_target_x8!([d, e], [a], 0x12);

assert_ld_indirect_target_x8_with_offset!([h, l]+, [a], 0x22);


assert_inc_x16!(b, c, 0x03);
assert_inc_x16!(d, e, 0x13);
assert_inc_x16!(h, l, 0x23);

assert_inc_x8!(b, 0x04);
assert_inc_x8!(c, 0x0C);
assert_inc_x8!(d, 0x14);
assert_inc_x8!(e, 0x1C);
assert_inc_x8!(h, 0x24);

assert_dec_x8!(b, 0x05);
assert_dec_x8!(c, 0x0D);
assert_dec_x8!(d, 0x15);
assert_dec_x8!(e, 0x1D);
assert_dec_x8!(h, 0x25);

assert_ld_u8!(b, 0x06);
assert_ld_u8!(c, 0x0E);
assert_ld_u8!(d, 0x16);
assert_ld_u8!(e, 0x1E);
assert_ld_u8!(h, 0x26);

assert_add_x16!([h, l], [b, c], 0x09);
assert_add_x16!([h, l], [d, e], 0x19);

assert_ld_indirect_src_x8!(a, [b, c], 0x0A);
assert_ld_indirect_src_x8!(a, [d, e], 0x1A);

assert_dec_x16!([b, c], 0x0B);
assert_dec_x16!([d, e], 0x1B);

    #[test]
    fn rlca() {
        assert_instruction! {
            opcode : 0x07
            rlca : [ ]
            setup : {
                reg_set : {
                    a : 0b1001_0101,
                    f : 0b0001_0000
                }
            }
            assert : {
                reg_eq : {
                    m : 1,
                    t : 4,
                    a : 0b0010_1011,
                    f : 0b0001_0000,
                    pc : 1
                }
            }
        }
    }

    #[test]
    fn ld_u16_sp_indirect() {
        assert_instruction! {
            opcode : 0x08
            ld : [ [(u16)], [sp] ]
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
    fn rrca() {
        assert_instruction! {
            opcode : 0x0F
            rrca : [ ]
            setup : {
                reg_set : {
                    a : 0b1001_0101,
                    f : 0b0000_0000
                }
            }
            assert : {
                reg_eq : {
                    m : 1,
                    t : 4,
                    a : 0b1100_1010,
                    f : 0b0001_0000,
                    pc : 1
                }
            }
        }
    }

    #[test]
    fn rra() {
        assert_instruction! {
            opcode : 0x1F
            rra : [ ]
            setup : {
                reg_set : {
                    a : 0b1001_0100,
                    f : 0b0001_0000
                }
            }
            assert : {
                reg_eq : {
                    m : 1,
                    t : 4,
                    a : 0b1100_1010,
                    f : 0b0000_0000,
                    pc : 1
                }
            }
        }
    }

    #[test]
    fn stop() {
        assert_instruction! {
            opcode : 0x10
            stop : [ ]
            setup : {
                mem_set : {
                    (0, 1) : 0
                }
            }
            assert : {
                reg_eq : {
                    m : 1,
                    t : 4,
                    pc : 2
                },
                mem_eq : {
                    (0, 1) : 0
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
                    f : 0b0001_0000,
                    pc : 1
                }
            }
        }
    }

    #[test]
    fn jr_i8_jump_forwards() {
        assert_instruction! {
            opcode : 0x18
            jr : [ i8 ]
            setup : {
                mem_set : {
                    1 : 25
                }
            }
            assert : {
                reg_eq : {
                    m : 3,
                    t : 12,
                    pc : 27
                }
            }
        }
    }

    #[test]
    fn jr_i8_jump_backwards() {
        assert_instruction! {
            opcode : 0x18
            jr : [ i8 ]
            setup : {
                mem_set : {
                    1 : (-25i8) as u8
                }
            }
            assert : {
                reg_eq : {
                    m : 3,
                    t : 12,
                    pc :  65513
                }
            }
        }
    }

    #[test]
    fn jr_nz_i8_forwards() {
        assert_instruction! {
            opcode : 0x20
            jr : [ [n z], [i8] ]
            setup : {
                mem_set : {
                    1 : 25
                },
                reg_set : {
                    f : flags!(n) | flags!(z)
                }
            }
            assert : {
                reg_eq : {
                    m : 3,
                    t : 12,
                    pc : 27
                }
            }
        }
    }

    #[test]
    fn jr_nz_i8_backwards() {
        assert_instruction! {
            opcode : 0x20
            jr : [ [n z], [i8] ]
            setup : {
                mem_set : {
                    1 : (-25i8) as u8
                },
                reg_set : {
                    f : flags!(n z)
                }
            }
            assert : {
                reg_eq : {
                    m : 3,
                    t : 12,
                    pc : 65513
                }
            }
        }
    }

    #[test]
    fn jr_nz_i8_missing_z_no_branch() {
        assert_instruction! {
            opcode : 0x20
            jr : [ [n z], [i8] ]
            setup : {
                mem_set : {
                    1 : 25
                },
                reg_set : {
                    f : flags!(n)
                }
            }
            assert : {
                reg_eq : {
                    m : 2,
                    t : 8,
                    pc : 2
                }
            }
        }
    }

    #[test]
    fn jr_nz_i8_missing_n_no_branch() {
        assert_instruction! {
            opcode : 0x20
            jr : [ [n z], [i8] ]
            setup : {
                mem_set : {
                    1 : 25
                },
                reg_set : {
                    f : flags!(z)
                }
            }
            assert : {
                reg_eq : {
                    m : 2,
                    t : 8,
                    pc : 2
                }
            }
        }
    }
}
