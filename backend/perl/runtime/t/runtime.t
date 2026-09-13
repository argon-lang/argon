use 5.020;
use strict;
use warnings;
use Test::More;
use Scalar::Util qw(refaddr);
use lib 'lib';
use Argon::Runtime ();

my $bool = Argon::Runtime::canonical_token('bool');
is refaddr($bool), refaddr(Argon::Runtime::canonical_token('bool')), 'tokens canonicalize';
is Argon::Runtime::normalize_integer(255, 8, 1, 0), -1, 'signed integers wrap';
is Argon::Runtime::normalize_integer(-1, 8, 0, 0), 255, 'unsigned integers wrap';
is ref(Argon::Runtime::normalize_integer(1, 64, 1, 1)), 'Math::BigInt', 'wide integers stay big';

my $array = Argon::Runtime::array_create(2);
Argon::Runtime::array_set($array, 1, 9);
is Argon::Runtime::array_get($array, 1), 9, 'checked array access';
ok !eval { Argon::Runtime::array_get($array, -1); 1 }, 'negative index rejected';

my $box = Argon::Runtime::box($array);
is refaddr($box->value), refaddr($array), 'box preserves payload identity';
my $reference = Argon::Runtime::reference($array);
is refaddr($reference->get), refaddr($array), 'reference preserves payload identity';

my $descriptor = Argon::Runtime::descriptor('R', 'record', [$bool], {
    value_class => 'Argon::Runtime::Value', fields => ['x'], methods => {}, static => {}, variants => {},
});
is refaddr($descriptor), refaddr(Argon::Runtime::descriptor('R', 'record', [$bool], {})), 'descriptors canonicalize';
my $value = $descriptor->construct({ x => 12 });
is $value->field(0), 12, 'record construction';

my $base_method = sub { 'base' };
my $variant_method = sub { 'variant' };
my $enum = Argon::Runtime::descriptor('E', 'enum', [], {
    value_class => 'Argon::Runtime::Value',
    methods => { display => $base_method },
    variants => { V => ['Argon::Runtime::Value', [], { display => $variant_method }] },
});
my $variant_value = $enum->variant('V')->construct({});
is $enum->dispatch($variant_value, $enum->method('display')), 'variant',
    'variant method overrides enum method';

done_testing;
