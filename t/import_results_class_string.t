use Mojo::Base -strict;
use Test::More;

BEGIN {
    plan skip_all => 'Mojo::mysql not installed' unless eval { require Mojo::mysql; 1 };
}

use Mojo::DB::Results::Role::MoreMethods results_class => 'Mojo::mysql::Results';

can_ok('Mojo::mysql::Results', qw(get get_by_name c c_by_name collections flatten));

done_testing;
