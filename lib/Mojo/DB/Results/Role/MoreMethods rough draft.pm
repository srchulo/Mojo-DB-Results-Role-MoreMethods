package Mojo::DB::Results::Role::MoreMethods;
use Mojo::Base -role;
use Mojo::Collection;
use Mojo::Util ();

our $VERSION = '0.01';

requires qw(array arrays columns hash hashes);

sub import {
    my $class = shift;

    my (@mysql_indexes) = grep { ($_[$_] // '') eq '-mysql' } 0..$#_;
    Carp::croak '-mysql flag provided more than once' if @mysql_indexes > 1;

    my $mysql_flag;
    if (@mysql_indexes) {
        splice @_, $mysql_indexes[0], 1;
        $mysql_flag = 1;
    }

    my (@pg_indexes) = grep { ($_[$_] // '') eq '-Pg' } 0..$#_;
    Carp::croak '-Pg flag provided more than once' if @pg_indexes > 1;

    my $pg_flag;
    if (@pg_indexes) {
        splice @_, $pg_indexes[0], 1;
        $pg_flag = 1;
    }

    my (@struct_indexes) = grep { ($_[$_] // '') eq '-struct' } 0..$#_;
    Carp::croak '-struct flag provided more than once' if @struct_indexes > 1;

    my $struct_flag;
    if (@struct_indexes) {
        splice @_, $struct_indexes[0], 1;
        $struct_flag = 1;
    }

    my %options = @_;
    return unless %options or $mysql_flag or $pg_flag or $struct_flag;

    my $results_class;
    if (exists $options{results_class}) {
        $results_class = delete $options{results_class};
        Carp::croak 'results_class must be a defined and non-empty value' unless defined $results_class and $results_class ne '';

        my $ref = ref $results_class;
        Carp::croak 'results_class must be a string or an arrayref' unless $ref eq '' or $ref eq 'ARRAY';

        if ($ref and $ref eq 'ARRAY') {
            Carp::croak 'results_class array cannot be empty' unless @$results_class;
            Carp::croak 'results_class array entries must be non-empty strings'
                unless scalar(grep { defined and not ref $_ and $_ ne '' } @$results_class) == @$results_class
        } else {
            $results_class = [$results_class];
        }
    }

    Carp::croak 'unknown options provided to import: ' . Mojo::Util::dumper \%options if %options;

    if ($mysql_flag or $pg_flag) {
        $results_class ||= [];
        if ($mysql_flag) {
            if (grep { $_ eq 'Mojo::mysql::Results' } @$results_class) {
                Carp::croak 'cannot provide -mysql flag and provide Mojo::mysql::Results in result_class';
            }

            push @$results_class, 'Mojo::mysql::Results';
        }

        if ($pg_flag) {
            if (grep { $_ eq 'Mojo::Pg::Results' } @$results_class) {
                Carp::croak 'cannot provide -Pg flag and provide Mojo::Pg::Results in result_class';
            }

            push @$results_class, 'Mojo::Pg::Results';
        }
    }

    if ($results_class) {
        require Role::Tiny;

        my %seen;
        for my $class (@$results_class) {
            Carp::croak "$class provided more than once to result_class" if $seen{$class}++;

            do { eval "require $class"; 1 } or Carp::croak "Failed to require $class";
            Role::Tiny->apply_roles_to_package($class, 'Mojo::DB::Results::Role::MoreMethods');
        }
    }

    if ($struct_flag) {
        with 'Mojo::DB::Results::Role::Struct';
    }
}

sub get {
    my $self = shift;

    my $options = ref $_[0] eq 'HASH' ? shift : {};
    my $die = delete $options->{die};
    my $one = delete $options->{one};
    my @indexes = @_;

    if ($one) {
        Carp::confess 'no rows returned' if $self->rows == 0;
        Carp::confess 'multiple rows returned' if $self->rows > 1;
    }

    my $wantarray = wantarray;
    if (not defined $wantarray) {
        Carp::cluck 'get or get variant called without using return value';

        if ($die or $one) {
            Carp::croak 'no results' unless $self->array;
        } else {
            $self->array;
            return;
        }
    } elsif ($wantarray) {
        my $array = $self->array;
        unless ($array) {
            Carp::confess 'no results' if $die or $one;
            return;
        }

        if (@indexes) {
            $self->_assert_indexes(@indexes);

            return @$array[@indexes];
        } else {
            return @$array;
        }
    } else {
        Carp::confess 'multiple indexes passed for single requested get value' if @indexes > 1;

        my $index = $indexes[0] || 0;
        my $array = $self->array;
        unless ($array) {
            Carp::confess 'no results' if $die or $one;
            return;
        }

        $self->_assert_indexes($index);

        return $array->[$index];
    }
}

sub get_by_name {
    my $self = shift;

    my $options = ref $_[0] eq 'HASH' ? shift : {};
    my @names = @_;
    Carp::croak 'names required' unless @names;

    return $self->get($options, $self->_find_column_indexes(@names));
}

sub c {
    return shift->get(@_) if not defined wantarray;

    my @values = shift->get(@_);
    return @values ? Mojo::Collection->new(@values) : undef;
}

sub c_by_name {
    return shift->get_by_name(@_) if not defined wantarray;

    my @values = shift->get_by_name(@_);
    return @values ? Mojo::Collection->new(@values) : undef;
}

sub collections { Mojo::Collection->new(map { Mojo::Collection->new(@$_) } @{ shift->arrays }) }

sub flatten { shift->arrays->flatten }

sub hashify {
    my $self = shift;
    my ($collection, $get_keys, $get_value) = $self->_parse_hash_options({}, @_);

    return $collection->with_roles('+Transform')->hashify($get_keys, $get_value);
}

sub hashify_collect {
    my ($collection, $get_keys, $get_value, $flatten) = shift->_parse_hash_options({flatten_allowed => 1}, @_);

    return $collection->with_roles('+Transform')->hashify_collect({flatten => $flatten}, $get_keys, $get_value);
}

sub collect_by {
    my ($collection, $get_keys, $get_value, $flatten) = shift->_parse_hash_options({flatten_allowed => 1}, @_);

    return $collection->with_roles('+Transform')->collect_by({flatten => $flatten}, $get_keys, $get_value);
}

sub _parse_hash_options {
    my $self = shift;
    my $private_options = shift;
    my $options = ref $_[0] eq 'HASH' ? shift : undef;

    my $key                       = _parse_hash_key(shift);
    my ($value, $value_is_column) = @_ ? _parse_hash_value(@_) : ();
    if (ref $key ne 'CODE' and ref $value ne 'CODE' and $value_is_column and defined $options and not exists $options->{array}) {
        Carp::carp 'useless options provided. array will be used';
    }

    # if user will not access the rows and the value is a column, default rows to arrays for speed
    if ($value_is_column and $key_ref ne 'CODE') {
        $options //= {array => 1};
    } else {
        $options //= {hash => 1};
    }

    my $option = _validate_hash_options($private_options, $options);
    my $collection = ($option eq 'array' or $option eq 'flatten') ? $self->arrays
                   : $option eq 'c'                               ? $self->collections
                   : $option eq 'hash'                            ? $self->hashes
                   : $self->structs
                   ;
    my $get_keys   = ref $key eq 'CODE'   ? $key : $self->_create_get_keys_sub($option, $key);
    my $get_value  = ref $value eq 'CODE' ? $value
                   : $value_is_column     ? $self->_create_column_value_getter($option, $value)
                   : sub { $_ }
                   ;

    return $collection, $get_keys, $get_value, $option eq 'flatten';
}

sub _parse_hash_key {
    my ($key) = @_;

    my $key_ref = ref $key;
    if ($key_ref) {
        Carp::confess qq{key must be an arrayref, a sub or a non-empty string, but had ref '$key_ref'}
            unless $key_ref eq 'ARRAY' or $key_ref eq 'CODE';
    } else {
        Carp::confess 'key was undefined or an empty string' unless defined $key and $key ne '';
        $key = [$key];
    }

    return $key;
}

sub _parse_hash_value {
    my ($value, $value_is_column);

    if (@_ == 1) {
        $value = shift;

        my $value_ref = ref $value;
        if ($value_ref) {
            Carp::confess qq{value must be a sub or non-empty string, but was '$value_ref'} unless $value_ref eq 'CODE';
        } elsif (not defined $value or $value eq '') {
            Carp::confess 'value was undefined or an empty string'
        } else {
            $value_is_column = 1;
        }
    } elsif (@_ > 1) {
        Carp::confess 'too many arguments provided (more than one value)';
    }

    return $value, $value_is_column;
}

sub _validate_hash_options {
    my ($private_options, $options) = @_;

    Carp::confess 'exactly one key/value pair required for options' unless keys %$options == 1;

    my @valid_options = qw(array c hash struct);
    push @valid_options, 'flatten' if $private_options->{flatten_allowed};

    my ($option) = keys %$options;
    Carp::confess "option must be one of: @{[ join ', ', @valid_options ]}"
        unless grep { $option eq $_ } @valid_options;

    return $option;
}

sub _create_get_keys_sub {
    my ($self, $option, $key) = @_;

    if ($option eq 'array' or $option eq 'flatten' or $option eq 'c') {
        my @key_indexes = $self->_find_column_indexes(@$key);
        return sub { @{$_}[@key_indexes] };
    } elsif ($option eq 'hash') {
        # assert columns exist
        $self->_find_column_indexes(@$key);

        return sub { @{$_}{@$key} };
    } else {
        # assert columns exist
        $self->_find_column_indexes(@$key);

        return sub {
            map { $_[0]->${\$_} } @$key
        };
    }
}

sub _create_column_value_getter {
    my ($self, $option, $value) = @_;

    if ($option eq 'array' or $option eq 'flatten' or $option eq 'c') {
        my $column_index = $self->_find_column_indexes($value);
        return sub { $_->[$column_index] };
    } elsif ($option eq 'hash') {
        # assert that column exists
        $self->_find_column_indexes($value);

        return sub { $_->{$value} };
    } else {
        # assert that column exists
        $self->_find_column_indexes($value);

        return sub { $_->${\$value} };
    }
}

sub get_or_die { shift->get({die => 1}, @_) }

sub get_by_name_or_die { shift->get_by_name({die => 1}, @_) }

sub c_or_die {
    return shift->c({die => 1}, @_);
}

sub c_by_name_or_die {
    return shift->c_by_name({die => 1}, @_);
}

sub struct_or_die {
    if (not defined wantarray) {
        Carp::cluck 'struct called without using return value';
    }

    Carp::croak 'no results' unless my $struct = shift->struct;
    return $struct;
}

sub one { shift->get({one => 1}, @_) }

sub one_by_name { shift->get_by_name({one => 1}, @_) }

sub one_c { Mojo::Collection->new(shift->one(@_)) }

sub one_c_by_name { Mojo::Collection->new(shift->one_by_name(@_)) }

sub one_struct {
    my $self = shift;

    Carp::croak 'no rows returned' if $self->rows == 0;
    Carp::croak 'multiple rows returned' if $self->rows > 1;

    return $self->struct;
}

sub one_array {
    my $self = shift;

    Carp::croak 'no rows returned' if $self->rows == 0;
    Carp::croak 'multiple rows returned' if $self->rows > 1;

    return $self->array;
}

sub one_hash {
    my $self = shift;

    Carp::croak 'no rows returned' if $self->rows == 0;
    Carp::croak 'multiple rows returned' if $self->rows > 1;

    return $self->hash;
}

sub _find_column_indexes {
    my $columns = shift->columns;

    return map { _find_column_index($columns, $_) } @_;
}

sub _find_column_index {
    my ($columns, $column) = @_;

    my @indexes = grep { $columns->[$_] eq $column } 0..$#$columns;
    Carp::confess "could not find column '$column' in returned columns" unless @indexes;
    Carp::confess "more than one column named '$column' in returned columns" if @indexes > 1;

    return $indexes[0];
}

sub _assert_indexes {
    my ($self, @indexes) = @_;

    my $num_columns = @{ $self->columns };
    Carp::croak 'cannot index into a size zero results array' if $num_columns == 0;

    for my $index (@indexes) {
        Carp::croak "index out of valid range -$num_columns to @{[ $num_columns - 1 ]}"
            unless $index >= -$num_columns and $index < $num_columns;
    }
}

1;
__END__

=encoding utf-8

=head1 NAME

Mojo::DB::Results::Role::MoreMethods - More methods for DB Results like Mojo::Pg::Results and Mojo::mysql::Results

=head1 STATUS

=for html <a href="https://travis-ci.org/srchulo/Mojo-DB-Results-Role-MoreMethods"><img src="https://travis-ci.org/srchulo/Mojo-DB-Results-Role-MoreMethods.svg?branch=master"></a>

=head1 SYNOPSIS

  use Mojo::DB::Results::Role::MoreMethods results_class => 'Mojo::Pg::Results';

  my $results = $db->select(people => ['name', 'age', 'favorite_food'] => {id => 123});
  my $name    = $results->get;
  my $name    = $results->get(0);
  my $name    = $results->get(-3);
  my ($name)  = $results->get;

  my ($name, $age, $favorite_food)  = $results->get;
  my ($name, $age, $favorite_food)  = $results->get(0..2);
  my ($name, $favorite_food)        = $results->get(0, 2);
  my ($name, $favorite_food)        = $results->get(-3, -1);

  my $name   = $results->get_by_name('name');
  my ($name) = $results->get_by_name('name');
  my ($name, $favorite_food) = $results->get_by_name('name', 'favorite_food');

  while (my ($name, $favorite_food) = $results->get('name', 'favorite_food')) {
      say qq{$name's favorite food is $favorite_food};
  }

  # get the next row as a Mojo::Collection
  my $results   = $db->select(people => ['first_name', 'middle_name', 'last_name']);
  my $full_name = $results->c->join(' ');

  # or get collection values by name
  my $first_and_last_name = $results->c_by_name('first_name', 'last_name')->join(' ');

  # get all rows as collections in a Mojo::Collection
  my $full_names = $results->collections->map(sub { $_->join(' ') });

  # assert that exactly one row is returned where expected (not 0, not more than one)
  my $results = $db->select(people => ['name', 'age', 'favorite_food'] => {id => 123});
  my $name    = $results->one;
  my ($name, $age, $favorite_food) = $results->one;
  my ($name, $favorite_food)       = $results->one_by_name('name', 'favorite_food');

  # Flatten results into one Mojo::Collection with names of all people who like Pizza
  my $results = $db->select(people => ['name'] => {favorite_food => 'Pizza'});
  my $names   = $results->flatten;
  say 'Pizza lovers:';
  say for $names->each;

  # access results by a key
  my $results = $db->select(people => '*');
  my $results_by_name = $results->by_key('name');

  # $alice_row is a hash
  my $alice_row = $results_by_name->{Alice};

  # access by multiple keys with a multilevel hash
  my $results_by_full_name = $results->by_key(['first_name', 'last_name']);

  # $alice_smith_row is a hash
  my $alice_smith_row = $results_by_full_name->{Alice}{Smith};

  # collect results by a key in a Mojo::Collection
  my $results = $db->select(people => '*');
  my $collections_by_name = $results->collect_by('name');

  # $alice_collection is a Mojo::Collection of all rows with the name 'Alice' as hashes
  my $alice_collection = $collections_by_name->{Alice};

  # access by multiple keys with a multilevel hash
  my $collections_by_full_name = $results->collect_by(['first_name', 'last_name']);

  # $alice_smith_row is a hash
  my $alice_smith_collection = $collections_by_full_name->{Alice}{Smith};

=head1 DESCRIPTION

L<Mojo::DB::Results::Role::MoreMethods> is a role that that provides additional methods for results classes
like L<Mojo::Pg::Results> or L<Mojo::mysql::Results>.

L<Mojo::DB::Results::Role::MoreMethods> requires a results class that has at least these methods:

=over 4

=item *

array

=item *

arrays

=item *

columns

=item *

hash

=item *

hashes

=back

=head1 HOW TO APPLY ROLE

=head2 results_class

  use Mojo::DB::Results::Role::MoreMethods results_class => 'Mojo::Pg::Results';

  # or multiple

  use Mojo::DB::Results::Role::MoreMethods results_class => ['Mojo::Pg::Results', 'Mojo::mysql::Results'];

L</results_class> allows you to apply L<Mojo::DB::Results::Role::MoreMethods> to one results class by providing the results class name,
or to multiple by providing an arrayref of results class names.

=head2 -mysql

  use Mojo::DB::Results::Role::MoreMethods -mysql;

  # shortcut for

  use Mojo::DB::Results::Role::MoreMethods results_class => 'Mojo::mysql::Results';

L<-mysql> is a shortcut for applying L<Mojo::DB::Results::Role::MoreMethods> to L<Mojo::mysql::Results>.

This can be used with L</-Pg>.

=head2 -Pg

  use Mojo::DB::Results::Role::MoreMethods -Pg;

  # shortcut for

  use Mojo::DB::Results::Role::MoreMethods results_class => 'Mojo::Pg::Results';

L<-Pg> is a shortcut for applying L<Mojo::DB::Results::Role::MoreMethods> to L<Mojo::Pg::Results>.

This can be used with L</-mysql>.

=head2 with_roles

  # apply Mojo::Pg::Results::Role::MoreMethods
  my $pg      = Mojo::Pg->new(...);
  my $results = $pg->db->select(people => ['name'] => {id => 123})->with_roles('+MoreMethods');
  my $name    = $results->get;

  # apply Mojo::mysql::Results::Role::MoreMethods
  my $mysql   = Mojo::mysql->new(...);
  my $results = $mysql->db->select(people => ['name'] => {id => 123})->with_roles('+MoreMethods');
  my $name    = $results->get;

  # apply using any results class
  my $pg      = Mojo::Pg->new(...);
  my $results = $pg->db->select(people => ['name'] => {id => 123})->with_roles('Mojo::DB::Results::Role::MoreMethods');
  my $name    = $results->get;

You may use L<Mojo::Base/with_roles> to apply L<Mojo::DB::Results::Role::MoreMethods> to your results classes.

These roles are also available to take advantage of C<with_role>'s shorthand C<+> notation when using L<Mojo::Pg::Results>
or L<Mojo::mysql::Results>:

=over 4

=item *

L<Mojo::Pg::Results::Role::MoreMethods>

=item *

L<Mojo::mysql::Results::Role::MoreMethods>

=back

These two roles are essentially just aliases for L<Mojo::DB::Results::Role::MoreMethods>. They are just empty roles with only this line:

  with 'Mojo::DB::Results::Role::MoreMethods';

=head2 Mojo::DB::Role::ResultsRoles

  # example from Mojo::DB::Role::ResultsRoles

  use Mojo::Pg;
  my $pg = Mojo::Pg->new(...)->with_roles('Mojo::DB::Role::ResultsRoles');
  push @{$pg->results_roles}, 'Mojo::DB::Results::Role::MoreMethods';
  my $results = $pg->db->query(...);
  # $results does Mojo::DB::Results::Role::MoreMethods

L<Mojo::DB::Role::ResultsRoles> allows roles to be applied to the results objects returned by database APIs like L<Mojo::Pg> or
L<Mojo::mysql>. See its documentation for more information.

You may take advantage of C<with_role>'s shorthand C<+> notation when using L<Mojo::Pg>
or L<Mojo::mysql> objects:

  # short hand with_roles syntax supported for Mojo::Pg and Mojo::mysql objects
  push @{$pg->results_roles}, '+MoreMethods';

=head1 -struct

  # applies Mojo::DB::Results::Role::Struct
  use Mojo::DB::Results::Role::MoreMethods -struct;

L<-struct> applies L<Mojo::DB::Results::Role::Struct> to your L</"results class"> for you and makes
L</struct> and L</structs> available. This also makes the struct option available in methods like
L</by_key> and L</collect_by>, or methods like L</struct_or_die> and L</one_struct>.
You may also load L<Mojo::DB::Results::Role::Struct> yourself and use those options and methods.

=head1 METHODS

=head2 get

Be sure to call C<finish>, such as L<Mojo::Pg::Results/"finish"> or L<Mojo::mysql::Results/"finish">,
if you are not fetching all of the possible rows.

=head3 SCALAR CONTEXT

  my $results = $db->select(people => ['name', 'age', 'favorite_food'] => {id => 123});

  # return the first column
  my $name = $results->get;

  # same as above but specifying index
  my $name = $results->get(0);

  # negative indexes may be used
  my $name = $results->get(-3);

  # any column may be gotten with an index
  my $age = $results->get(1);

When L</get> is called in scalar context with no index, it will return the first column requested in your query.
If an index is specified, the value corresponding to the column at that index in the query will be used instead.
A negative index may be used just like indexing into Perl arrays.

=head4 WHILE LOOPS

  # THIS IS WRONG DO NOT DO THIS.
  while (my $name = $results->get) {
    # broken loop...
  }

Because L</get> in scalar context may return C<undef>, an empty string or a C<0> as values for a column, it
cannot be reliably used in while loops. Because of this, L</get> may be most useful for queries
where you expect one row to be returned. If this is the case, considering using L</one> instead.

If you would like to use while loops with L</get>, consider using a while loop in list context:

  while (my ($name) = $results->get) {
    say $name;
  }

=head3 LIST CONTEXT

  my $results = $db->select(people => ['name', 'age', 'favorite_food'] => {id => 123});

  # return the first column
  my ($name) = $results->get;

  # same as above but specifying index
  my ($name) = $results->get(0);

  # multiple indexes may be used
  my ($name, $favorite_food) = $results->get(0, 2);

  # negative indexes may be used
  my ($name, $favorite_food) = $results->get(-3, -1);

  # get all column values
  my ($name, $age, $favorite_food) = $results->get;
  my @person = $results->get;

  # iterate
  while (my ($name, $age, $favorite_food) = $results->get) {
    say qq{$name is $age years old and their favorite food is $favorite_food};
  }

When L</get> is called in list context with no index, it will return all values for the row as a list.
Individual column values may be requested by providing indexes. Negative indexes may also be used just like
indexing into Perl arrays.

=head3 OPTIONS

You may provide options to L</get> by providing an options hashref as the first
argument.

=head4 die

  # dies if no next row exists
  my $name = $results->get({die => 1});
  my $name = $results->get({die => 1}, 0);

Dies unless there is a next row to be retrieved.
See L</get_or_die> for this same behavior without needing to provide the die option.

The L</die> option does nothing if L</one> is provided, as L</one> is a superset of the functionality of L</die>.

=head4 one

  # dies unless exactly one row was returned in the results
  my $name = $results->get({one => 1});
  my $name = $results->get({one => 1}, 0);

Dies unless exactly one row was returned in the results.
See L</one> for this same behavior without needing to provide the one option.

=head2 get_by_name

Be sure to call C<finish>, such as L<Mojo::Pg::Results/"finish"> or L<Mojo::mysql::Results/"finish">,
if you are not fetching all of the possible rows.

=head3 SCALAR CONTEXT

  my $results = $db->select(people => ['name', 'age', 'favorite_food'] => {id => 123});

  # return the name column
  my $name = $results->get_by_name('name');

L</get_by_name> called in scalar context returns the individual value for the column corresponding
to the provided name.

=head4 WHILE LOOPS

  # THIS IS WRONG DO NOT DO THIS.
  while (my $name = $results->get_by_name('name')) {
    # broken loop...
  }

Because L</get_by_name> in scalar context may return C<undef>, an empty string or a C<0> as values for a column, it
cannot be reliably used in while loops. Because of this, L</get_by_name> may be most useful for queries
where you expect one row to be returned. If this is the case, considering using L</one_by_name> instead.

If you would like to use while loops with L</get_by_name>, consider using a while loop in list context:

  while (my ($name) = $results->get_by_name('name')) {
    say $name;
  }

=head3 LIST CONTEXT

  my $results = $db->select(people => ['name', 'age', 'favorite_food'] => {id => 123});

  # return the name column
  my ($name) = $results->get_by_name('name');

  # multiple names may be used
  my ($name, $favorite_food) = $results->get('name', 'favorite_food');

  # get all column values
  my ($name, $age, $favorite_food) = $results->get_by_name('name', 'age', 'favorite_food');

  # iterate
  while (my ($name, $age, $favorite_food) = $results->get_by_name('name', 'age', 'favorite_food')) {
    say qq{$name is $age years old and their favorite food is $favorite_food};
  }

L</get_by_name> returns the list of values corresponding to the list of column names provided.

=head3 OPTIONS

You may provide options to L</get_by_name> by providing an options hashref as the first
argument.

=head4 die

  # dies if no next row exists
  my $name = $results->get_by_name({die => 1});
  my $name = $results->get_by_name({die => 1}, 0);

Dies unless there is a next row to be retrieved.
See L</get_by_name_or_die> for this same behavior without needing to provide the die option.

The L<die/die1> option does nothing if L<one|/one1> is provided, as L<one|/one1> is a superset of the functionality of L<die|/die1>.

=head4 one

  # dies unless exactly one row was returned in the results
  my $name = $results->get({one => 1});
  my $name = $results->get({one => 1}, 0);

Dies unless exactly one row was returned in the results.
See L</one_by_name> for this same behavior without needing to provide the one option.

=head2 c

Be sure to call C<finish>, such as L<Mojo::Pg::Results/"finish"> or L<Mojo::mysql::Results/"finish">,
if you are not fetching all of the possible rows.

  my $results   = $db->select(people => ['first_name', 'middle_name', 'last_name']);
  my $full_name = $results->c->join(' ');

  # iterate
  while (my $c = $results->c) {
    my $full_name = $c->join(' ');
    say "Full name is $full_name";
  }

L</c> returns the next row as a L<Mojo::Collection>. If there is no next row available, C<undef> is returned.

You may provide indexes to get just those values in the L<Mojo::Collection>, just as you can do with L</get>:

  my $results   = $db->select(people => ['first_name', 'middle_name', 'last_name']);
  my $full_name = $results->c(0, 2)->join(' ');

  # prints "$first_name $last_name"
  say $full_name;

=head3 OPTIONS

You may provide options to L</c> by providing an options hashref as the first
argument.

=head4 die

  # dies if no next row exists
  my $person = $results->c({die => 1});

Dies unless there is a next row to be retrieved.
See L</c_or_die> for this same behavior without needing to provide the die option.

The L<die|/die2> option does nothing if L<one|/one2> is provided, as L<one|/one2> is a superset of the functionality of L<die|/die2>.

=head4 one

  # dies unless exactly one row was returned in the results
  my $person = $results->c({one => 1});

Dies unless exactly one row was returned in the results.
See L</one_c> for this same behavior without needing to provide the one option.

=head2 c_by_name

Be sure to call C<finish>, such as L<Mojo::Pg::Results/"finish"> or L<Mojo::mysql::Results/"finish">,
if you are not fetching all of the possible rows.

  my $results   = $db->select(people => ['first_name', 'middle_name', 'last_name']);
  my $full_name = $results->c_by_name('first_name', 'middle_name', 'last_name')->join(' ');

  # iterate
  while (my $c = $results->c_by_name('first_name', 'middle_name', 'last_name')) {
    my $full_name = $c->join(' ');
    say "Full name is $full_name";
  }

L</c_by_name> returns the values corresponding to the provided columns for the next row as a L<Mojo::Collection>.
If there is no next row available, C<undef> is returned.

=head3 OPTIONS

You may provide options to L</c_by_name> by providing an options hashref as the first
argument.

=head4 die

  # dies if no next row exists
  my $person = $results->c_by_name({die => 1}, 'first_name', 'middle_name', 'last_name');

Dies unless there is a next row to be retrieved.
See L</c_by_name_or_die> for this same behavior without needing to provide the die option.

The L<die|/die3> option does nothing if L<one|/one3> is provided, as L<one|/one3> is a superset of the functionality of L<die|/die3>.

=head4 one

  # dies unless exactly one row was returned in the results
  my $person = $results->c({one => 1}, 'first_name', 'middle_name', 'last_name');

Dies unless exactly one row was returned in the results.
See L</one_c_by_name> for this same behavior without needing to provide the one option.

=head2 collections

  my $results    = $db->select(people => ['first_name', 'middle_name', 'last_name']);
  my $full_names = $results->collections->map(sub { $_->join(' ') });

L</collections> returns a L<Mojo::Collection> of L<Mojo::Collection>s. Each inner L<Mojo::Collection>
corresponds to one array returned by the results.

This is similar to L<Mojo::Pg::Results/"arrays"> or L<Mojo::mysql::Results/"arrays">, but each arrayref
is a L<Mojo::Collection> instead.

=head2 flatten

  # Mojo::Collection with names of all people who like Pizza
  my $results = $db->select(people => ['name'] => {favorite_food => 'Pizza'});

  my $names   = $results->flatten; # equivalent to $results->arrays->flatten

  say 'Pizza lovers:';
  say for $names->each;

L</flatten> returns a L<Mojo::Collection> with all result arrays flattened to return a
L<Mojo::Collection> with all elements. This is equivalent to calling L<Mojo::Collection/"flatten"> on
the C<arrays> method.

=head2 struct

  my $struct = $results->struct;

Fetch next row from the statement handle with the result object's array method, and return it as a struct.

This method is composed from L<Mojo::DB::Results::Role::Struct> and the L<-struct> flag must be provided to use this method.

=head2 structs

  my $collection = $results->structs;

Fetch all rows from the statement handle with the result object's C<arrays> method, and return them as a L<Mojo::Collection> object containing structs.

This method is composed from L<Mojo::DB::Results::Role::Struct> and the L<-struct> flag must be provided to use this method.

=head2 by_key

  # access results by a key
  my $results = $db->select(people => '*');
  my $results_by_name = $results->by_key('name');

  # $alice_row is a hash
  my $alice_row = $results_by_name->{Alice};

  # access by multiple keys with a multilevel hash
  my $results_by_full_name = $results->by_key(['first_name', 'last_name']);

  # $alice_smith_row is a hash
  my $alice_smith_row = $results_by_full_name->{Alice}{Smith};

  # store the value as a struct instead of a hash
  my $results_by_name = $results->by_key({struct => 1}, 'name');
  my $alice_struct = $results_by_name->{Alice};

  say 'Alice is ' . $alice_struct->age . ' years old';

L</by_key> allows you to store a single row or value (usually a column value) behind a key or multiple keys in a hash.

=head3 OPTIONS

=head4 array

  my $results = $db->select(people => '*');
  my $results_by_name = $results->by_key({array => 1}, 'name');

  my $alice_array = $results_by_name->{Alice};

L</array> allows you to store the value as an array instead of the default L</hash>.
This also means the value provided to the L</KEY> L</SUB> or L</VALUE> L</SUB>, if used, will be an array.

=head4 c

  my $results = $db->select(people => '*');
  my $results_by_name = $results->by_key({c => 1}, 'name');

  my $alice_collection = $results_by_name->{Alice};

L</c> allows you to store the value as a L<Mojo::Collection> instead of the default L</hash>.
This also means the value provided to the L</KEY> L</SUB> or L</VALUE> L</SUB>, if used, will be a L<Mojo::Collection>.

=head4 hash

  my $results = $db->select(people => '*');
  my $results_by_name = $results->by_key({hash => 1}, 'name'); # default

  my $alice_hash = $results_by_name->{Alice};

L</hash> allows you to store the value as a hash. This is the default and is the same as providing no option hash:

  my $results_by_name = $results->by_key('name');

This also means the value provided to the L</KEY> L</SUB> or L</VALUE> L</SUB>, if used, will be a hash.

=head4 struct

  my $results = $db->select(people => '*');
  my $results_by_name = $results->by_key({struct => 1}, 'name');

  my $alice_struct = $results_by_name->{Alice};

L<struct|/struct2> allows you to store the value as a readonly struct provided by L<Mojo::DB::Results::Role::Struct> instead of the default L</hash>.
This also means the value provided to the L</KEY> L</SUB> or L</VALUE> L</SUB>, if used, will be a readonly struct.

L<Mojo::DB::Results::Role::Struct> must be composed for this option to be available. See L</-struct> or L<Mojo::Base/"with_roles">.

=head3 KEY

=head4 SINGLE KEY

  my $results_by_name = $results->by_key('name');
  my $alice_row = $results_by_name->{Alice};

A single key may be used to access values. This key should be the name of a returned column.

=head4 MULTIPLE KEYS

  my $results_by_full_name = $results->by_key(['first_name', 'last_name']);
  my $alice_smith_row = $results_by_full_name->{Alice}{Smith};

Multiple keys may be used to access values. Multiple keys should be provided as an arrayref of returned columns.

=head4 SUB

  # single key
  my $results_by_name = $results->by_key(sub { $_->{name} });
  my $alice_row = $results_by_name->{Alice};

  # multiple keys
  my $results_by_full_name = $results->by_key(sub { @{ $_ }{qw(first_name last_name)} });
  my $alice_smith_row = $results_by_full_name->{Alice}{Smith};

Providing a subroutine for the key allows you to create the key (or keys) with the returned row.
The row is available either as C<$_> or as the first argument to the subroutine. The type of row
that is passed to the subroutine depends on any L</OPTIONS> value that is passed (default is L</hash>).

If the subroutine returns one key, the hash will be a L</"SINGLE KEY"> hash. If multiple keys are returned
as a list, the hash with be a L</"MULTIPLE KEYS"> hash.

=head3 VALUE

=head4 DEFAULT

  # values are hashes
  my $results_by_name = $results->by_key('name');
  my $alice_hash = $results_by_name->{Alice};

  # values are still hashes
  my $results_by_name = $results->by_key({hash => 1}, 'name');
  my $alice_hash = $results_by_name->{Alice};

  # values are arrays
  my $results_by_name = $results->by_key({array => 1}, 'name');
  my $alice_array = $results_by_name->{Alice};

  # values are Mojo::Collection's
  my $results_by_name = $results->by_key({c => 1}, 'name');
  my $alice_collection = $results_by_name->{Alice};

  # values are readonly structs
  my $results_by_name = $results->by_key({struct => 1}, 'name');
  my $alice_struct = $results_by_name->{Alice};

If no value argument is provided, the default is to use the row as the value according to the type
specified in L</OPTIONS>. The default is to use the row as a hash if no options value is provided.

=head4 COLUMN

  # value will be age
  my $results_by_name = $results->by_key('name', 'age');
  my $alice_age = $results_by_name->{Alice};

The value can be provided as a column returned in the results and will be used as the
final value in the hash.

=head4 SUB

  # value will be the age squared
  my $results_by_name = $results->by_key('name', sub { $_->{age} * $_->{age} });
  my $alice_age_squared = $results_by_name->{Alice};

Providing a subroutine for the value allows you to create the value with the returned row.
The row is available either as C<$_> or as the first argument to the subroutine. The type of row
that is passed to the subroutine depends on any L</OPTIONS> value that is passed (default is L</hash>).

=head2 collect_by

  # group results by a key
  my $results = $db->select(people => '*');
  my $collections_by_name = $results->collect_by('name');

  # $alice_collection is a Mojo::Collection with all rows with the name Alice as hashes
  my $alice_collection = $collections_by_name->{Alice};

  # group by multiple keys with a multilevel hash
  my $collections_by_full_name = $results->collect_by(['first_name', 'last_name']);

  # $alice_smith_collection is a Mojo::Collection with all rows with
  # the first name Alice and last name Smith as hashes
  my $alice_smith_collection = $results_by_full_name->{Alice}{Smith};

  # group the values as structs instead of hashes
  my $collections_by_name = $results->collect_by({struct => 1}, 'name');
  my $alice_collection = $results_by_name->{Alice};

  $alice_collection->each(sub {
    say 'Alice is ' . $_->age . ' years old';
  });

L</collect_by> allows you to group rows behind a key or multiple keys in a hash.

=head3 OPTIONS

=head4 array

  my $results = $db->select(people => '*');
  my $collections_by_name = $results->by_key({array => 1}, 'name');

  my $alice_collection = $collections_by_name->{Alice};
  my $alice_array = $alice_collection->first;

L</array1> allows you to group rows as arrays instead of the default L</hash>.
This also means the value provided to the L</KEY1> L</SUB1> or L</VALUE1> L</SUB1>, if used, will be an array.

=head4 flatten

  my $results = $db->select(people => ['name', 'age']);
  my $age_collections_by_name = $results->collect_by({flatten => 1}, 'name', 'age');

  my $alice_ages_collection = $age_collections_by_name->{Alice};
  my $age_sum = $alice_ages_collection->reduce(sub { $a + $b }, 0);

  say "Collective age of Alices is $age_sum years old";

L</flatten> flattens all values for a key into the same L<Mojo::Collection>.
This also means the value provided to the L</KEY1> L</SUB1> or L</VALUE1> L</SUB1>, if used, will be an array.

If no L</VALUE1> is specified, the returned row as an array with have all values added to the L<Mojo::Collection>.

Any value returned by a L</VALUE1> L</SUB1> should be an arrayref and all of its values will be
added to the L<Mojo::Collection>

=head4 c

  my $results = $db->select(people => '*');
  my $collections_by_name = $results->collect_by({c => 1}, 'name');

  my $alice_collections = $collections_by_name->{Alice};
  $alice_collections->each(sub {
    say 'Random column value is ' . $_->shuffle->first;
  });

L</c1> allows you to group rows as L<Mojo::Collection>s instead of the default L</hash>.
This also means the value provided to the L</KEY1> L</SUB1> or L</VALUE1> L</SUB1>, if used, will be a L<Mojo::Collection>.

=head4 hash

  my $results = $db->select(people => '*');
  my $collections_by_name = $results->collect_by({hash => 1}, 'name'); # default

  my $alice_collection = $collections_by_name->{Alice};

L</hash1> allows you groupe the rows as hashes. This is the default and is the same as providing no option hash:

  my $collections_by_name = $results->collect_by('name');

This also means the value provided to the L</KEY1> L</SUB1> or L</VALUE1> L</SUB1>, if used, will be a hash.

=head4 struct

  my $results = $db->select(people => '*');
  my $collections_by_name = $results->collect_by({struct => 1}, 'name');

  my $alice_collection = $results_by_name->{Alice};
  say q{First Alice's age is } . $alice_collection->first->age;

L<struct|/struct3> allows you to group the rows as readonly structs provided by L<Mojo::DB::Results::Role::Struct> instead of the default L</hash>.
This also means the value provided to the L</KEY1> L</SUB1> or L</VALUE1> L</SUB1>, if used, will be a readonly struct.

L<Mojo::DB::Results::Role::Struct> must be composed for this option to be available. See L</-struct> or L<Mojo::Base/"with_roles">.

=head3 KEY

=head4 SINGLE KEY

  my $collections_by_name = $results->collect_by('name');
  my $alice_collection = $collections_by_name->{Alice};

A single key may be used to access collections. This key should be the name of a returned column.

=head4 MULTIPLE KEYS

  my $collections_by_full_name = $results->collect_by(['first_name', 'last_name']);
  my $alice_smith_collection = $collections_by_full_name->{Alice}{Smith};

Multiple keys may be used to access collections. Multiple keys should be provided as an arrayref of returned columns.

=head4 SUB

  # single key
  my $collections_by_name = $results->collect_by(sub { $_->{name} });
  my $alice_collection = $collections_by_name->{Alice};

  # multiple keys
  my $collections_by_full_name = $results->collect_by(sub { @{ $_ }{qw(first_name last_name)} });
  my $alice_smith_collection = $collection_by_full_name->{Alice}{Smith};

Providing a subroutine for the key allows you to create the key (or keys) with the returned row.
The row is available either as C<$_> or as the first argument to the subroutine. The type of row
that is passed to the subroutine depends on any L</OPTIONS1> value that is passed (default is L</hash1>).

If the subroutine returns one key, the hash will be a L</"SINGLE KEY1"> hash. If multiple keys are returned
as a list, the hash with be a L</"MULTIPLE KEYS1"> hash.

=head3 VALUE

=head4 DEFAULT

  # collections contain hashes
  my $collections_by_name = $results->collect_by('name');
  my $alice_collection_of_hashes = $collection_by_name->{Alice};

  # collections still contain hashes
  my $collections_by_name = $results->collect_by({hash => 1}, 'name');
  my $alice_collection_of_hashes = $collection_by_name->{Alice};

  # collections contain arrays
  my $collections_by_name = $results->collect_by({array => 1}, 'name');
  my $alice_collection_of_arrays = $collections_by_name->{Alice};

  # collections contain Mojo::Collection's
  my $collections_by_name = $results->collect_by({c => 1}, 'name');
  my $alice_collection_of_collections = $collections_by_name->{Alice};

  # collections contain readonly structs
  my $collections_by_name = $results->collect_by({struct => 1}, 'name');
  my $alice_collection_of_structs = $collections_by_name->{Alice};

If no value argument is provided, the default is to collect the rows as the value according to the type
specified in L</OPTIONS1>. The default is to collect the rows as hashes if no options value is provided.

=head4 COLUMN

  # age will be collected
  my $collections_by_name = $results->collect_by('name', 'age');
  my $alice_collection_of_ages = $collections_by_name->{Alice};

The value can be provided as a column returned in the results, and this column value for
each row will be collected into the L<Mojo::Collection>.

=head4 SUB

  # collected value will be the age squared
  my $collections_by_name = $results->collect_by('name', sub { $_->{age} * $_->{age} });
  my $alice_collection_of_ages_squared = $collections_by_name->{Alice};

Providing a subroutine for the value allows you to create the collected values with the returned row.
The row is available either as C<$_> or as the first argument to the subroutine. The type of row
that is passed to the subroutine depends on any L</OPTIONS1> value that is passed (default is L</hash1>).

=head2 DIE METHODS

L</"DIE METHODS"> are equivalent to the L</get>, L</get_by_name>, L</c>, and L</c_by_name> methods above, however
the C<die> options for these methods is passed for you and the method will die if there is no next row to be
retrieved.

Additionally, L</struct_or_die> is provided.

=head3 get_or_die

  my $results = $db->select(people => ['name', 'age', 'favorite_food'] => {id => 123});

  # all of these die if there is no next row to be retrieved

  # return the first column
  my $name = $results->get_or_die;

  # same as above but specifying index
  my $name = $results->get_or_die(0);

  # negative indexes may be used
  my $name = $results->get_or_die(-3);

  # any column may be gotten with an index
  my $age = $results->get_or_die(1);

Same as L</get>, but dies if there is no next row to be retrieved.

=head3 get_by_name_or_die

  my $results = $db->select(people => ['name', 'age', 'favorite_food'] => {id => 123});

  # die if there is no next row to be retrieved
  my $name = $results->get_by_name_or_die('name');

Same as L</get_by_name>, but dies if there is no next row to be retrieved.

=head3 c_or_die

  my $results = $db->select(people => ['first_name', 'middle_name', 'last_name']);

  # die if there is no next row to be retrieved
  my $full_name = $results->c_or_die->join(' ');

Same as L</c>, but dies if there is no next row to be retrieved.

=head3 c_by_name_or_die

  my $results = $db->select(people => ['first_name', 'middle_name', 'last_name']);

  # die if there is no next row to be retrieved
  my $full_name = $results->c_by_name_or_die('first_name', 'middle_name', 'last_name')->join(' ');

Same as L</c_by_name>, but dies if there is no next row to be retrieved.

=head3 struct_or_die

  my $results = $db->select(people => '*' => {id => 123});

  # die if there is no next row to be retrieved
  my $person_struct = $results->struct_or_die;

L</struct_or_die> is the same as L</struct>, but dies unless exactly one row was returned.

L<Mojo::DB::Results::Role::Struct> must be composed for this option to be available. See L</-struct> or L<Mojo::Base/"with_roles">.

=head2 ONE METHODS

L</"ONE METHODS"> are equivalent to the L</get>, L</get_by_name>, L</c>, L</c_by_name>, and L</struct> methods above, however
the C<one> options for these methods is passed for you and the method will die unless exactly one row was returned.

Additionally, L</one_array> and L</one_hash> are provided.

=head3 one

  my $results = $db->select(people => ['name', 'age', 'favorite_food'] => {id => 123});

  # all of these die unless exactly one row was returned

  # return the first column
  my $name = $results->one;

  # same as above but specifying index
  my $name = $results->one(0);

  # negative indexes may be used
  my $name = $results->one(-3);

  # any column may be gotten with an index
  my $age = $results->one(1);

Same as L</get>, but dies unless exactly one row was returned.

=head3 one_by_name

  my $results = $db->select(people => ['name', 'age', 'favorite_food'] => {id => 123});

  # die unless exactly one row was returned
  my $name = $results->one_by_name('name');

Same as L</get_by_name>, but dies unless exactly one row was returned.

=head3 one_c

  my $results = $db->select(people => ['first_name', 'middle_name', 'last_name'] => {id => 123});

  # die unless exactly one row was returned
  my $full_name = $results->one_c->join(' ');

Same as L</c>, but dies unless exactly one row was returned.

=head3 one_c_by_name

  my $results = $db->select(people => ['first_name', 'middle_name', 'last_name'] => {id => 123});

  # die unless exactly one row was returned
  my $full_name = $results->one_c_by_name('first_name', 'middle_name', 'last_name')->join(' ');

Same as L</c_by_name>, but dies unless exactly one row was returned

=head3 one_struct

  my $results = $db->select(people => '*' => {id => 123});

  # die unless exactly one row was returned
  my $person_struct = $results->one_struct;

L</one_struct> is the same as L</struct>, but dies unless exactly one row was returned.

L<Mojo::DB::Results::Role::Struct> must be composed for this option to be available. See L</-struct> or L<Mojo::Base/"with_roles">.

=head3 one_array

  my $results = $db->select(people => ['first_name', 'middle_name', 'last_name'] => {id => 123});

  # die unless exactly one row was returned
  my $full_name = join ' ', @{ $results->one_array('first_name', 'middle_name', 'last_name') };

L</one_array> is similar to L<Mojo::Pg::Results/array> or L<Mojo::mysql::Results/array>, but dies
unless exactly one row was returned.

=head3 one_hash

  my $results = $db->select(people => '*' => {id => 123});

  # die unless exactly one row was returned
  my $person = $results->one_hash;

L</one_hash> is similar to L<Mojo::Pg::Results/hash> or L<Mojo::mysql::Results/hash>, but dies
unless exactly one row was returned.

=head1 AUTHOR

Adam Hopkins E<lt>srchulo@cpan.orgE<gt>

=head1 COPYRIGHT

Copyright 2019- Adam Hopkins

=head1 LICENSE

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=head1 SEE ALSO

=over 4

=item *

L<Mojo::Pg::Results>

=item *

L<Mojo::mysql::Results>

=item *

L<Mojo::DB::Role::ResultsRoles>

=item *

L<Mojo::DB::Results::Role::Struct>

=item *

L<Role::Tiny>

=item *

L<Mojo::Base>

=back

=cut
