package Argon::TestExterns::PerlExterns;
use 5.020;
use strict;
use warnings;

sub test_extern_function { 'function result' }
sub test_extern_method { 'method result' }
sub test_extern_static_method { 'static method result' }

sub argon_externs {
    return {
        functions => { test_extern_function => \&test_extern_function },
        methods => { test_extern_method => \&test_extern_method },
        static_methods => { test_extern_static_method => \&test_extern_static_method },
    };
}

1;
