package Argon::Core::PerlExterns;
use 5.020;
use strict;
use warnings;

sub int_to_s { "$_[0]" }
sub i8_to_s { "$_[0]" }
sub u8_to_s { "$_[0]" }
sub i16_to_s { "$_[0]" }
sub u16_to_s { "$_[0]" }
sub i32_to_s { "$_[0]" }
sub u32_to_s { "$_[0]" }
sub i64_to_s { "$_[0]" }
sub u64_to_s { "$_[0]" }
sub string_starts_with { index($_[0], $_[1]) == 0 }
sub string_ends_with {
    my ($value, $suffix) = @_;
    return 1 if $suffix eq '';
    return length($suffix) <= length($value) && substr($value, -length($suffix)) eq $suffix;
}
sub string_contains { index($_[0], $_[1]) >= 0 }
sub puts { print $_[0], "\n"; [] }

sub argon_externs {
    return {
        functions => {
            int_to_s => \&int_to_s,
            i8_to_s => \&i8_to_s,
            u8_to_s => \&u8_to_s,
            i16_to_s => \&i16_to_s,
            u16_to_s => \&u16_to_s,
            i32_to_s => \&i32_to_s,
            u32_to_s => \&u32_to_s,
            i64_to_s => \&i64_to_s,
            u64_to_s => \&u64_to_s,
            string_starts_with => \&string_starts_with,
            string_ends_with => \&string_ends_with,
            string_contains => \&string_contains,
            puts => \&puts,
        },
        methods => {},
        static_methods => {},
    };
}

1;
