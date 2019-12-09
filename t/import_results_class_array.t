use Mojo::Base -strict;
use Test::More;
use Mojo::DB::Results::Role::MoreMethods results_class => ['Mojo::mysql::Results', 'Mojo::Pg::Results'];

can_ok('Mojo::mysql::Results', qw(get get_by_name c c_by_name collections flatten));
can_ok('Mojo::Pg::Results', qw(get get_by_name c c_by_name collections flatten));

done_testing;
