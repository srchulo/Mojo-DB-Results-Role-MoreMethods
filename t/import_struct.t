use Mojo::Base -strict;
use Test::More;
use Mojo::DB::Results::Role::MoreMethods -struct;

can_ok('Mojo::DB::Results::Role::MoreMethods', qw(struct structs));

done_testing;
