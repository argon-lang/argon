#[derive(Debug, Copy, Clone)]
pub struct Flags(u8);

#[derive(Debug, Copy, Clone)]
pub struct FlagsUnpacked {
    pub mode: Mode,
    pub type_index_kind: TypeIndexKind,
}

#[derive(Debug, Copy, Clone)]
#[repr(u8)]
pub enum Mode {
    Concrete = 0x00,
    Erased = 0x01,
    Token = 0x02,
}

#[derive(Debug, Copy, Clone)]
#[repr(u8)]
pub enum TypeIndexKind {
    Local = 0x00,
    Type = 0x04,
    BigType = 0x08,
    Builtin = 0x0C,
}

const MODE_MASK: u8 = 0x03;
const TYPE_INDEX_KIND_MASK: u8 = 0x0C;

impl Flags {
    pub fn pack(unpacked: FlagsUnpacked) -> Flags {
        let mut flags = 0;
        flags |= unpacked.mode as u8;
        flags |= unpacked.type_index_kind as u8;
        Flags(flags)
    }

    pub fn unpack(self) -> FlagsUnpacked {
        let mode = self.get_mode();
        let type_index_kind = self.get_type_index_kind();
        FlagsUnpacked {
            mode,
            type_index_kind,
        }
    }

    pub fn get_mode(&self) -> Mode {
        unsafe { std::mem::transmute::<u8, Mode>(self.0 & MODE_MASK) }
    }

    pub fn get_type_index_kind(&self) -> TypeIndexKind {
        unsafe { std::mem::transmute::<u8, TypeIndexKind>(self.0 & TYPE_INDEX_KIND_MASK) }
    }

    pub fn set_mode(&mut self, mode: Mode) {
        self.0 &= !MODE_MASK;
        self.0 |= mode as u8;
    }

    pub fn set_type_index_kind(&mut self, type_index_kind: TypeIndexKind) {
        self.0 &= !TYPE_INDEX_KIND_MASK;
        self.0 |= type_index_kind as u8;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_mode_eq(actual: Mode, expected: Mode) {
        assert_eq!(actual as u8, expected as u8);
    }

    fn assert_type_index_kind_eq(actual: TypeIndexKind, expected: TypeIndexKind) {
        assert_eq!(actual as u8, expected as u8);
    }

    #[test]
    fn pack_encodes_mode_and_type_index_kind() {
        let cases = [
            (Mode::Concrete, TypeIndexKind::Local, 0x00),
            (Mode::Concrete, TypeIndexKind::Type, 0x04),
            (Mode::Concrete, TypeIndexKind::BigType, 0x08),
            (Mode::Erased, TypeIndexKind::Local, 0x01),
            (Mode::Erased, TypeIndexKind::Type, 0x05),
            (Mode::Erased, TypeIndexKind::BigType, 0x09),
            (Mode::Token, TypeIndexKind::Local, 0x02),
            (Mode::Token, TypeIndexKind::Type, 0x06),
            (Mode::Token, TypeIndexKind::BigType, 0x0a),
        ];

        for (mode, type_index_kind, expected) in cases {
            let flags = Flags::pack(FlagsUnpacked {
                mode,
                type_index_kind,
            });

            assert_eq!(flags.0, expected);
        }
    }

    #[test]
    fn unpack_decodes_mode_and_type_index_kind() {
        let cases = [
            (Mode::Concrete, TypeIndexKind::Local),
            (Mode::Concrete, TypeIndexKind::Type),
            (Mode::Concrete, TypeIndexKind::BigType),
            (Mode::Erased, TypeIndexKind::Local),
            (Mode::Erased, TypeIndexKind::Type),
            (Mode::Erased, TypeIndexKind::BigType),
            (Mode::Token, TypeIndexKind::Local),
            (Mode::Token, TypeIndexKind::Type),
            (Mode::Token, TypeIndexKind::BigType),
        ];

        for (mode, type_index_kind) in cases {
            let unpacked = Flags::pack(FlagsUnpacked {
                mode,
                type_index_kind,
            })
            .unpack();

            assert_mode_eq(unpacked.mode, mode);
            assert_type_index_kind_eq(unpacked.type_index_kind, type_index_kind);
        }
    }

    #[test]
    fn set_mode_updates_only_mode_bits() {
        let mut flags = Flags::pack(FlagsUnpacked {
            mode: Mode::Concrete,
            type_index_kind: TypeIndexKind::BigType,
        });

        flags.set_mode(Mode::Token);

        assert_mode_eq(flags.get_mode(), Mode::Token);
        assert_type_index_kind_eq(flags.get_type_index_kind(), TypeIndexKind::BigType);
        assert_eq!(flags.0, 0x0a);
    }

    #[test]
    fn set_type_index_kind_updates_only_type_index_kind_bits() {
        let mut flags = Flags::pack(FlagsUnpacked {
            mode: Mode::Erased,
            type_index_kind: TypeIndexKind::Local,
        });

        flags.set_type_index_kind(TypeIndexKind::Type);

        assert_mode_eq(flags.get_mode(), Mode::Erased);
        assert_type_index_kind_eq(flags.get_type_index_kind(), TypeIndexKind::Type);
        assert_eq!(flags.0, 0x05);

        flags.set_type_index_kind(TypeIndexKind::BigType);

        assert_mode_eq(flags.get_mode(), Mode::Erased);
        assert_type_index_kind_eq(flags.get_type_index_kind(), TypeIndexKind::BigType);
        assert_eq!(flags.0, 0x09);
    }
}
