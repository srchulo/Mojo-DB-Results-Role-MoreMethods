use Mojo::Base -strict;
use Test::More;
use Mojo::DB::Results::Role::MoreMethods -mysql;

can_ok('Mojo::mysql::Results', qw(get get_by_name c c_by_name collections flatten));

done_testing;
