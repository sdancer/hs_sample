

-- hapstone last available update is for version 3.0.5_rc2

-- test if capstone & hapstone are working correctly
input = [0xB8, 0x01, 0x00, 0x00, 0x00, 0xB8, 0x02, 0x00, 0x00, 0x00]

inst1_ops  = [CsX86Op {value = Reg X86RegEax, size = 4, access = 0, avxBcast = X86AvxBcastInvalid, avxZeroOpmask = False},
CsX86Op {value = Imm 1, size = 4, access = 0, avxBcast = X86AvxBcastInvalid, avxZeroOpmask = False}]],
inst2_ops = [[CsX86Op {value = Reg X86RegEax, size = 4, access = 0, avxBcast = X86AvxBcastInvalid, avxZeroOpmask = False},
CsX86Op {value = Imm 2, size = 4, access = 0, avxBcast = X86AvxBcastInvalid, avxZeroOpmask = False}]
