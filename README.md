# NAME

Mojo::DB::Results::Role::MoreMethods - More methods for DB Results, like Mojo::Pg::Results and Mojo::mysql::Results

# STATUS

<div>
    <a href="https://travis-ci.org/srchulo/Mojo-DB-Results-Role-MoreMethods"><img src="https://travis-ci.org/srchulo/Mojo-DB-Results-Role-MoreMethods.svg?branch=master"></a> <a href='https://coveralls.io/github/srchulo/Mojo-DB-Results-Role-MoreMethods?branch=master'><img src='https://coveralls.io/repos/github/srchulo/Mojo-DB-Results-Role-MoreMethods/badge.svg?branch=master' alt='Coverage Status' /></a>
</div>

# SYNOPSIS

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
    my $results_by_name = $results->hashify('name');

    # $alice_row is a hash
    my $alice_row = $results_by_name->{Alice};

    # access results by multiple keys with a multilevel hash
    my $results_by_full_name = $results->hashify(['first_name', 'last_name']);

    # $alice_smith_row is a hash
    my $alice_smith_row = $results_by_full_name->{Alice}{Smith};

    # collect results by a key in a Mojo::Collection behind a hash
    my $results = $db->select(people => '*');
    my $collections_by_name = $results->hashify_collect('name');

    # $alice_collection is a Mojo::Collection of all rows with the name 'Alice' as hashes
    my $alice_collection = $collections_by_name->{Alice};

    # collect results by multiple keys in a Mojo::Collection behind a multilevel hash
    my $collections_by_full_name = $results->hashify_collect(['first_name', 'last_name']);

    # $alice_smith_row is a hash
    my $alice_smith_collection = $collections_by_full_name->{Alice}{Smith};

    # create a Mojo::Collection of Mojo::Collection's, where all results that share the same key
    # are grouped in the same inner Mojo::Collection
    my $results = $db->select(people => '*');
    my $name_collections = $results->collect_by('name');

    for my $name_collection ($name_collections->each) {

      say 'Ages for ' . $name_collection->[0]{name};
      for my $row ($name_collection->each) {
        say "$row->{name} is $row->{age} years old";
      }
    }

# DESCRIPTION

[Mojo::DB::Results::Role::MoreMethods](https://metacpan.org/pod/Mojo::DB::Results::Role::MoreMethods) is a role that that provides additional methods for results classes
like [Mojo::Pg::Results](https://metacpan.org/pod/Mojo::Pg::Results) or [Mojo::mysql::Results](https://metacpan.org/pod/Mojo::mysql::Results).

[Mojo::DB::Results::Role::MoreMethods](https://metacpan.org/pod/Mojo::DB::Results::Role::MoreMethods) requires a results class that has at least these methods:

- array
- arrays
- columns
- hash
- hashes

# HOW TO APPLY ROLE

## results\_class

    use Mojo::DB::Results::Role::MoreMethods results_class => 'Mojo::Pg::Results';

    # or multiple

    use Mojo::DB::Results::Role::MoreMethods results_class => ['Mojo::Pg::Results', 'Mojo::mysql::Results'];

["results\_class"](#results_class) allows you to apply [Mojo::DB::Results::Role::MoreMethods](https://metacpan.org/pod/Mojo::DB::Results::Role::MoreMethods) to one results class package by providing the results class name,
or to multiple by providing an arrayref of results class names.

## -mysql

    use Mojo::DB::Results::Role::MoreMethods -mysql;

    # shortcut for

    use Mojo::DB::Results::Role::MoreMethods results_class => 'Mojo::mysql::Results';

[-mysql](https://metacpan.org/pod/-mysql) is a shortcut for applying [Mojo::DB::Results::Role::MoreMethods](https://metacpan.org/pod/Mojo::DB::Results::Role::MoreMethods) to [Mojo::mysql::Results](https://metacpan.org/pod/Mojo::mysql::Results).

This can be used with ["-Pg"](#pg).

## -Pg

    use Mojo::DB::Results::Role::MoreMethods -Pg;

    # shortcut for

    use Mojo::DB::Results::Role::MoreMethods results_class => 'Mojo::Pg::Results';

[-Pg](https://metacpan.org/pod/-Pg) is a shortcut for applying [Mojo::DB::Results::Role::MoreMethods](https://metacpan.org/pod/Mojo::DB::Results::Role::MoreMethods) to [Mojo::Pg::Results](https://metacpan.org/pod/Mojo::Pg::Results).

This can be used with ["-mysql"](#mysql).

## with\_roles

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

You may use ["with\_roles" in Mojo::Base](https://metacpan.org/pod/Mojo::Base#with_roles) to apply [Mojo::DB::Results::Role::MoreMethods](https://metacpan.org/pod/Mojo::DB::Results::Role::MoreMethods) to your results classes.

These roles are also available to take advantage of `with_role`'s shorthand `+` notation when using [Mojo::Pg::Results](https://metacpan.org/pod/Mojo::Pg::Results)
or [Mojo::mysql::Results](https://metacpan.org/pod/Mojo::mysql::Results):

- [Mojo::Pg::Results::Role::MoreMethods](https://metacpan.org/pod/Mojo::Pg::Results::Role::MoreMethods)
- [Mojo::mysql::Results::Role::MoreMethods](https://metacpan.org/pod/Mojo::mysql::Results::Role::MoreMethods)

These two roles are essentially just aliases for [Mojo::DB::Results::Role::MoreMethods](https://metacpan.org/pod/Mojo::DB::Results::Role::MoreMethods). They are just empty roles with only this line:

    with 'Mojo::DB::Results::Role::MoreMethods';

## Mojo::DB::Role::ResultsRoles

    # example from Mojo::DB::Role::ResultsRoles

    use Mojo::Pg;
    my $pg = Mojo::Pg->new(...)->with_roles('Mojo::DB::Role::ResultsRoles');
    push @{$pg->results_roles}, 'Mojo::DB::Results::Role::MoreMethods';
    my $results = $pg->db->query(...);
    # $results does Mojo::DB::Results::Role::MoreMethods

[Mojo::DB::Role::ResultsRoles](https://metacpan.org/pod/Mojo::DB::Role::ResultsRoles) allows roles to be applied to the results objects returned by database APIs like [Mojo::Pg](https://metacpan.org/pod/Mojo::Pg) or
[Mojo::mysql](https://metacpan.org/pod/Mojo::mysql). See its documentation for more information.

You may take advantage of `with_role`'s shorthand `+` notation when using [Mojo::Pg](https://metacpan.org/pod/Mojo::Pg)
or [Mojo::mysql](https://metacpan.org/pod/Mojo::mysql) objects:

    # short hand with_roles syntax supported for Mojo::Pg and Mojo::mysql objects
    push @{$pg->results_roles}, '+MoreMethods';

# METHODS

## get

Be sure to call `finish`, such as ["finish" in Mojo::Pg::Results](https://metacpan.org/pod/Mojo::Pg::Results#finish) or ["finish" in Mojo::mysql::Results](https://metacpan.org/pod/Mojo::mysql::Results#finish),
if you are not fetching all of the possible rows.

["get"](#get) will fetch the next row from `sth`.

### SCALAR CONTEXT

    my $results = $db->select(people => ['name', 'age', 'favorite_food'] => {id => 123});

    # return the first column
    my $name = $results->get;

    # same as above but specifying index
    my $name = $results->get(0);

    # negative indexes may be used
    my $name = $results->get(-3);

    # any column may be gotten with an index
    my $age = $results->get(1);

When ["get"](#get) is called in scalar context with no index, it will fetch the next row from `sth` and return the first column requested in your query.
If an index is specified, the value corresponding to the column at that index in the query will be used instead.
A negative index may be used just like indexing into Perl arrays.

#### WHILE LOOPS

    # THIS IS WRONG DO NOT DO THIS.
    while (my $name = $results->get) {
      # broken loop...
    }

Because ["get"](#get) in scalar context may return `undef`, an empty string or a `0` as values for a column, it
cannot be reliably used in while loops (unless used in ["LIST CONTEXT"](#list-context)).
If you expect one row to be returned, considering using ["one"](#one) instead.

If you would like to use while loops with ["get"](#get), consider using a while loop in ["LIST CONTEXT"](#list-context):

    while (my ($name) = $results->get) {
      say $name;
    }

### LIST CONTEXT

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

When ["get"](#get) is called in list context with no index, it will fetch the next row from `sth` and return all values for the row as a list.
Individual column values may be requested by providing indexes. Negative indexes may also be used just like
indexing into Perl arrays.

### OPTIONS

You may provide options to ["get"](#get) by providing an options hashref as the first
argument.

#### die

    # dies if no next row exists
    my $name = $results->get({die => 1});
    my $name = $results->get({die => 1}, 0);

Dies unless there is a next row to be retrieved.
See ["get\_or\_die"](#get_or_die) for this same behavior without needing to provide the die option.

The ["die"](#die) option does nothing if ["one"](#one) is provided, as ["one"](#one) is a superset of the functionality of ["die"](#die).

#### one

    # dies unless exactly one row was returned in the results
    my $name = $results->get({one => 1});
    my $name = $results->get({one => 1}, 0);

Dies unless exactly one row was returned in the results.
See ["one"](#one) for this same behavior without needing to provide the one option.

## get\_by\_name

Be sure to call `finish`, such as ["finish" in Mojo::Pg::Results](https://metacpan.org/pod/Mojo::Pg::Results#finish) or ["finish" in Mojo::mysql::Results](https://metacpan.org/pod/Mojo::mysql::Results#finish),
if you are not fetching all of the possible rows.

["get\_by\_name"](#get_by_name) will fetch the next row from `sth`.

### SCALAR CONTEXT

    my $results = $db->select(people => ['name', 'age', 'favorite_food'] => {id => 123});

    # return the name column
    my $name = $results->get_by_name('name');

["get\_by\_name"](#get_by_name) called in scalar context will fetch the next row from `sth` and returns the individual value for the column corresponding
to the provided name.

#### WHILE LOOPS

    # THIS IS WRONG DO NOT DO THIS.
    while (my $name = $results->get_by_name('name')) {
      # broken loop...
    }

Because ["get\_by\_name"](#get_by_name) in scalar context may return `undef`, an empty string or a `0` as values for a column, it
cannot be reliably used in while loops (unless used in ["LIST CONTEXT"](#list-context1)).
If you expect one row to be returned, considering using ["one\_by\_name"](#one_by_name) instead.

If you would like to use while loops with ["get\_by\_name"](#get_by_name), consider using a while loop in ["LIST CONTEXT"](#list-context1):

    while (my ($name) = $results->get_by_name('name')) {
      say $name;
    }

### LIST CONTEXT

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

["get\_by\_name"](#get_by_name) fetches the next row from `sth` and returns the list of values corresponding to the list of column names provided.

### OPTIONS

You may provide options to ["get\_by\_name"](#get_by_name) by providing an options hashref as the first
argument.

#### die

    # dies if no next row exists
    my $name = $results->get_by_name({die => 1});
    my $name = $results->get_by_name({die => 1}, 0);

Dies unless there is a next row to be retrieved.
See ["get\_by\_name\_or\_die"](#get_by_name_or_die) for this same behavior without needing to provide the die option.

The ["die"](#die1) option does nothing if ["one"](#one1) is provided, as ["one"](#one1) is a superset of the functionality of ["die"](#die1).

#### one

    # dies unless exactly one row was returned in the results
    my $name = $results->get({one => 1});
    my $name = $results->get({one => 1}, 0);

Dies unless exactly one row was returned in the results.
See ["one\_by\_name"](#one_by_name) for this same behavior without needing to provide the one option.

## c

Be sure to call `finish`, such as ["finish" in Mojo::Pg::Results](https://metacpan.org/pod/Mojo::Pg::Results#finish) or ["finish" in Mojo::mysql::Results](https://metacpan.org/pod/Mojo::mysql::Results#finish),
if you are not fetching all of the possible rows.

["c"](#c) will fetch the next row from `sth`.

    my $results   = $db->select(people => ['first_name', 'middle_name', 'last_name']);
    my $full_name = $results->c->join(' ');

    # iterate
    while (my $c = $results->c) {
      my $full_name = $c->join(' ');
      say "Full name is $full_name";
    }

["c"](#c) fetches the next row from `sth` and returns the row as a [Mojo::Collection](https://metacpan.org/pod/Mojo::Collection). If there is no next row available, `undef` is returned.

You may provide indexes to get just those values in the [Mojo::Collection](https://metacpan.org/pod/Mojo::Collection), just as you can do with ["get"](#get):

    my $results   = $db->select(people => ['first_name', 'middle_name', 'last_name']);
    my $full_name = $results->c(0, 2)->join(' ');

    # prints "$first_name $last_name"
    say $full_name;

### OPTIONS

You may provide options to ["c"](#c) by providing an options hashref as the first
argument.

#### die

    # dies if no next row exists
    my $person = $results->c({die => 1});
    my $person = $results->c({die => 1}, 0, 2);

Dies unless there is a next row to be retrieved.
See ["c\_or\_die"](#c_or_die) for this same behavior without needing to provide the die option.

The ["die"](#die2) option does nothing if ["one"](#one2) is provided, as ["one"](#one2) is a superset of the functionality of ["die"](#die2).

#### one

    # dies unless exactly one row was returned in the results
    my $person = $results->c({one => 1});
    my $person = $results->c({one => 1}, 0, 2);

Dies unless exactly one row was returned in the results.
See ["one\_c"](#one_c) for this same behavior without needing to provide the one option.

## c\_by\_name

Be sure to call `finish`, such as ["finish" in Mojo::Pg::Results](https://metacpan.org/pod/Mojo::Pg::Results#finish) or ["finish" in Mojo::mysql::Results](https://metacpan.org/pod/Mojo::mysql::Results#finish),
if you are not fetching all of the possible rows.

["c\_by\_name"](#c_by_name) will fetch the next row from `sth`.

    my $results   = $db->select(people => ['first_name', 'middle_name', 'last_name']);
    my $full_name = $results->c_by_name('first_name', 'middle_name', 'last_name')->join(' ');

    # iterate
    while (my $c = $results->c_by_name('first_name', 'middle_name', 'last_name')) {
      my $full_name = $c->join(' ');
      say "Full name is $full_name";
    }

["c\_by\_name"](#c_by_name) fetches the next row from `sth` and returns the values corresponding to the provided columns for the next
row as a [Mojo::Collection](https://metacpan.org/pod/Mojo::Collection). If there is no next row available, `undef` is returned.

### OPTIONS

You may provide options to ["c\_by\_name"](#c_by_name) by providing an options hashref as the first
argument.

#### die

    # dies if no next row exists
    my $person = $results->c_by_name({die => 1}, 'first_name', 'middle_name', 'last_name');

Dies unless there is a next row to be retrieved.
See ["c\_by\_name\_or\_die"](#c_by_name_or_die) for this same behavior without needing to provide the die option.

The ["die"](#die3) option does nothing if ["one"](#one3) is provided, as ["one"](#one3) is a superset of the functionality of ["die"](#die3).

#### one

    # dies unless exactly one row was returned in the results
    my $person = $results->c({one => 1}, 'first_name', 'middle_name', 'last_name');

Dies unless exactly one row was returned in the results.
See ["one\_c\_by\_name"](#one_c_by_name) for this same behavior without needing to provide the one option.

## collections

    my $results    = $db->select(people => ['first_name', 'middle_name', 'last_name']);
    my $full_names = $results->collections->map(sub { $_->join(' ') });

["collections"](#collections) returns a [Mojo::Collection](https://metacpan.org/pod/Mojo::Collection) of [Mojo::Collection](https://metacpan.org/pod/Mojo::Collection)s. Each inner [Mojo::Collection](https://metacpan.org/pod/Mojo::Collection)
corresponds to one array returned by the results.

This is similar to ["arrays" in Mojo::Pg::Results](https://metacpan.org/pod/Mojo::Pg::Results#arrays) or ["arrays" in Mojo::mysql::Results](https://metacpan.org/pod/Mojo::mysql::Results#arrays), but each arrayref
is a [Mojo::Collection](https://metacpan.org/pod/Mojo::Collection) instead.

## flatten

    # Mojo::Collection with names of all people who like Pizza
    my $results = $db->select(people => ['name'] => {favorite_food => 'Pizza'});
    my $names   = $results->flatten; # equivalent to $results->arrays->flatten

    say 'Pizza lovers:';
    say for $names->each;

["flatten"](#flatten) returns a [Mojo::Collection](https://metacpan.org/pod/Mojo::Collection) with all result arrays flattened to return a
[Mojo::Collection](https://metacpan.org/pod/Mojo::Collection) with all elements. This is equivalent to calling ["flatten" in Mojo::Collection](https://metacpan.org/pod/Mojo::Collection#flatten) on
the `arrays` method.

## struct

    my $struct = $results->struct;

Fetch next row from the statement handle with the result object's array method, and return it as a struct.

This method is composed from [Mojo::DB::Results::Role::Struct](https://metacpan.org/pod/Mojo::DB::Results::Role::Struct).

## structs

    my $collection = $results->structs;

Fetch all rows from the statement handle with the result object's `arrays` method, and return them as a [Mojo::Collection](https://metacpan.org/pod/Mojo::Collection) object containing structs.

This method is composed from [Mojo::DB::Results::Role::Struct](https://metacpan.org/pod/Mojo::DB::Results::Role::Struct).

## TRANSFORM METHODS

["TRANSFORM METHODS"](#transform-methods) is a group of methods that build on top of [Mojo::Collection::Role::Transform](https://metacpan.org/pod/Mojo::Collection::Role::Transform) that allow
you to transform your results in meaningful and convenient ways. These are:

- ["hashify"](#hashify)
- ["hashify\_collect"](#hashify_collect)
- ["collect\_by"](#collect_by)

## hashify

    # access results by a key
    my $results         = $db->select(people => '*');
    my $results_by_name = $results->hashify('name');

    # $alice_row is a hash
    my $alice_row = $results_by_name->{Alice};

    # access by multiple keys with a multilevel hash
    my $results_by_full_name = $results->hashify(['first_name', 'last_name']);

    # $alice_smith_row is a hash
    my $alice_smith_row = $results_by_full_name->{Alice}{Smith};

    # store the value as a struct instead of a hash
    my $results_by_name = $results->hashify({struct => 1}, 'name');
    my $alice_struct = $results_by_name->{Alice};

    say 'Alice is ' . $alice_struct->age . ' years old';

["hashify"](#hashify) transforms your results into a hash that stores single rows or values (usually a column value) behind a key or
multiple keys (usually column values).

["hashify"](#hashify) builds on ["hashify" in Mojo::Collection::Role::Transform](https://metacpan.org/pod/Mojo::Collection::Role::Transform#hashify) and adds useful functionality specific to DB results.

### OPTIONS

#### array

    my $results         = $db->select(people => '*');
    my $results_by_name = $results->hashify({array => 1}, 'name');

    my $alice_array = $results_by_name->{Alice};

["array"](#array) allows you to store the value as an array instead of the default ["hash"](#hash).
This also means the value provided to the ["KEY"](#key) ["SUB"](#sub) or the ["VALUE"](#value) ["SUB"](#sub1), if used, will be an array.

#### c

    my $results         = $db->select(people => '*');
    my $results_by_name = $results->hashify({c => 1}, 'name');

    my $alice_collection = $results_by_name->{Alice};

["c"](#c1) allows you to store the value as a [Mojo::Collection](https://metacpan.org/pod/Mojo::Collection) instead of the default ["hash"](#hash).
This also means the value provided to the ["KEY"](#key) ["SUB"](#sub) or the ["VALUE"](#value) ["SUB"](#sub1), if used, will be a [Mojo::Collection](https://metacpan.org/pod/Mojo::Collection).

#### hash

    my $results         = $db->select(people => '*');
    my $results_by_name = $results->hashify({hash => 1}, 'name'); # default

    my $alice_hash = $results_by_name->{Alice};

["hash"](#hash) allows you to store the value as a hash. This is the default and is the same as providing no option hash:

    my $results_by_name = $results->hashify('name');

This also means the value provided to the ["KEY"](#key) ["SUB"](#sub) or the ["VALUE"](#value) ["SUB"](#sub1), if used, will be a hash.

#### struct

    my $results         = $db->select(people => '*');
    my $results_by_name = $results->hashify({struct => 1}, 'name');

    my $alice_struct = $results_by_name->{Alice};

["struct"](#struct1) allows you to store the value as a readonly struct provided by [Mojo::DB::Results::Role::Struct](https://metacpan.org/pod/Mojo::DB::Results::Role::Struct) instead of the default ["hash"](#hash).
This also means the value provided to the ["KEY"](#key) ["SUB"](#sub) or the ["VALUE"](#value) ["SUB"](#sub1), if used, will be a readonly struct.

### KEY

#### SINGLE KEY

    my $results_by_name = $results->hashify('name');
    my $alice_row       = $results_by_name->{Alice};

A single key may be used to access values. This key should be the name of a returned column.

#### MULTIPLE KEYS

    my $results_by_full_name = $results->hashify(['first_name', 'last_name']);
    my $alice_smith_row      = $results_by_full_name->{Alice}{Smith};

Multiple keys may be used to access values. Multiple keys should be provided as an arrayref of names of returned columns.

#### SUB

    # single key
    my $results_by_name = $results->hashify(sub { $_->{name} });
    my $alice_row       = $results_by_name->{Alice};

    # multiple keys
    my $results_by_full_name = $results->hashify(sub { @{ $_ }{qw(first_name last_name)} });
    my $alice_smith_row      = $results_by_full_name->{Alice}{Smith};

Providing a subroutine for the key allows you to create the key (or keys) with the returned row.
The row is available either as `$_` or as the first argument to the subroutine. The type of the row
(["array"](#array), ["c"](#c1), ["hash"](#hash), ["struct"](#struct1)) that is passed to the subroutine depends on any
["OPTIONS"](#options4) value that is passed (default is ["hash"](#hash)).

If the subroutine returns one key, the hash will be a ["SINGLE KEY"](#single-key) hash. If multiple keys are returned
as a list, the hash with be a ["MULTIPLE KEYS"](#multiple-keys) hash.

### VALUE

#### DEFAULT

    # values are hashes
    my $results_by_name = $results->hashify('name');
    my $alice_hash      = $results_by_name->{Alice};

    # values are still hashes
    my $results_by_name = $results->hashify({hash => 1}, 'name');
    my $alice_hash      = $results_by_name->{Alice};

    # values are arrays
    my $results_by_name = $results->hashify({array => 1}, 'name');
    my $alice_array     = $results_by_name->{Alice};

    # values are Mojo::Collection's
    my $results_by_name  = $results->hashify({c => 1}, 'name');
    my $alice_collection = $results_by_name->{Alice};

    # values are readonly structs
    my $results_by_name = $results->hashify({struct => 1}, 'name');
    my $alice_struct    = $results_by_name->{Alice};

If no value argument is provided, the default is to use the row as the value according to the type
specified in ["OPTIONS"](#options4) (["array"](#array), ["c"](#c1), ["hash"](#hash), ["struct"](#struct1)). The default is ["hash"](#hash).

#### COLUMN

    # value will be age
    my $results_by_name = $results->hashify('name', 'age');
    my $alice_age       = $results_by_name->{Alice};

The value can be provided as a column returned in the results and will be used as the
final value in the hash.

#### SUB

    # value will be the age squared
    my $results_by_name   = $results->hashify('name', sub { $_->{age} * $_->{age} });
    my $alice_age_squared = $results_by_name->{Alice};

Providing a subroutine for the value allows you to create the value with the returned row.
The row is available either as `$_` or as the first argument to the subroutine. The type of the row
(["array"](#array), ["c"](#c1), ["hash"](#hash), ["struct"](#struct1)) that is passed to the subroutine depends on any
["OPTIONS"](#options4) value that is passed (default is ["hash"](#hash)).

## hashify\_collect

    # group results by a key in a hash
    my $results             = $db->select(people => '*');
    my $collections_by_name = $results->hashify_collect('name');

    # $alice_collection is a Mojo::Collection with all rows with the name Alice as hashes
    my $alice_collection = $collections_by_name->{Alice};

    # group by multiple keys with a multilevel hash
    my $collections_by_full_name = $results->hashify_collect(['first_name', 'last_name']);

    # $alice_smith_collection is a Mojo::Collection with all rows with
    # the first name Alice and last name Smith as hashes
    my $alice_smith_collection = $collections_by_full_name->{Alice}{Smith};

    # group the values as structs instead of hashes
    my $structs_by_name = $results->hashify_collect({struct => 1}, 'name');
    my $alice_structs   = $structs_by_name->{Alice};

    $alice_structs->each(sub {
      say 'Alice is ' . $_->age . ' years old';
    });

    # collect a single column value
    my $ages_by_name = $results->hashify_collect('name', 'age');

    # contains all ages in one Mojo::Collection for all rows with the name Alice
    my $alice_ages = $ages_by_name->{Alice};

    # flatten grouped results
    my $results               = $db->select(people => '*');
    my $column_values_by_name = $results->hashify_collect({flatten => 1}, 'name');

    # contains all column values in one Mojo::Collection for all rows with the name Alice
    my $alice_all_column_values = $column_values_by_name->{Alice};

["hashify\_collect"](#hashify_collect) allows you to group rows behind a key or multiple keys in a hash.

["hashify\_collect"](#hashify_collect) builds on ["hashify\_collect" in Mojo::Collection::Role::Transform](https://metacpan.org/pod/Mojo::Collection::Role::Transform#hashify_collect) and adds useful functionality specific to DB results.

### OPTIONS

#### array

    my $results             = $db->select(people => '*');
    my $collections_by_name = $results->hashify_collect({array => 1}, 'name');

    my $alice_collection = $collections_by_name->{Alice};
    my $alice_array      = $alice_collection->first;

["array"](#array1) allows you to group rows as arrays instead of the default ["hash"](#hash1).
This also means the value provided to the ["KEY"](#key1) ["SUB"](#sub2) or the ["VALUE"](#value1) ["SUB"](#sub3), if used, will be an array.

#### flatten

    my $results = $db->select(people => ['name', 'age']);

    # trivial example returning arrayref to demonstrate flatten
    my $age_collections_by_name = $results->hashify_collect({flatten => 1}, 'name', sub { [$_->{age}] });

    my $alice_ages_collection = $age_collections_by_name->{Alice};
    my $age_sum               = $alice_ages_collection->reduce(sub { $a + $b }, 0);

    say "Collective age of Alices is $age_sum years old";

["flatten"](#flatten1) flattens all values for a key into the same [Mojo::Collection](https://metacpan.org/pod/Mojo::Collection).

["flatten"](#flatten1) may be combined with the other type options to specify the type of the rows that will be passed to the
["KEY"](#key1) ["SUB"](#sub2) or the ["VALUE"](#value1) ["SUB"](#sub3).

If no ["VALUE"](#value1) is specified, all returned column values for a row will be returned in the order they were requested
and flattened into the resulting [Mojo::Collection](https://metacpan.org/pod/Mojo::Collection). This works regardless of any type option that is specified:

    my $results = $db->select(people => ['name', 'age']);

    # both contain name and age flattened into the collections
    my $collections_by_name = $results->hashify_collect({hash => 1, flatten => 1}, sub { $_->{name} });
    my $collections_by_name = $results->hashify_collect({struct => 1, flatten => 1}, sub { $_->name });

Any value returned by a ["VALUE"](#value1) ["SUB"](#sub3) should be an arrayref or list of values and all values will be
added to the [Mojo::Collection](https://metacpan.org/pod/Mojo::Collection):

    my $collections_by_name = $results->hashify_collect({flatten => 1}, 'name', sub { $_->{age} }); # flatten not needed in this specific case because it's a list of 1
    my $collections_by_name = $results->hashify_collect({flatten => 1}, 'name', sub { [$_->{age}] });

#### c

    my $results             = $db->select(people => '*');
    my $collections_by_name = $results->hashify_collect({c => 1}, 'name');

    my $alice_collections = $collections_by_name->{Alice};
    $alice_collections->each(sub {
      say 'Random column value is ' . $_->shuffle->first;
    });

["c"](#c2) allows you to group rows as [Mojo::Collection](https://metacpan.org/pod/Mojo::Collection)s instead of the default ["hash"](#hash1).
This also means the value provided to the ["KEY"](#key1) ["SUB"](#sub2) or the ["VALUE"](#value1) ["SUB"](#sub3), if used, will be a [Mojo::Collection](https://metacpan.org/pod/Mojo::Collection).

#### hash

    my $results             = $db->select(people => '*');
    my $collections_by_name = $results->hashify_collect({hash => 1}, 'name'); # default

    my $alice_collection = $collections_by_name->{Alice};

["hash"](#hash1) allows you to group the rows as hashes. This is the default and is the same as providing no option hash:

    my $collections_by_name = $results->hashify_collect('name');

This also means the value provided to the ["KEY"](#key1) ["SUB"](#sub2) or the ["VALUE"](#value1) ["SUB"](#sub3), if used, will be a hash.

#### struct

    my $results             = $db->select(people => '*');
    my $collections_by_name = $results->hashify_collect({struct => 1}, 'name');

    my $alice_collection = $collections_by_name->{Alice};
    say q{First Alice's age is } . $alice_collection->first->age;

["struct"](#struct2) allows you to group the rows as readonly structs provided by [Mojo::DB::Results::Role::Struct](https://metacpan.org/pod/Mojo::DB::Results::Role::Struct) instead of the default ["hash"](#hash1).
This also means the value provided to the ["KEY"](#key1) ["SUB"](#sub2) or the ["VALUE"](#value1) ["SUB"](#sub3), if used, will be a readonly struct.

### KEY

#### SINGLE KEY

    my $collections_by_name = $results->hashify_collect('name');
    my $alice_collection    = $collections_by_name->{Alice};

A single key may be used to access collections. This key should be the name of a returned column.

#### MULTIPLE KEYS

    my $collections_by_full_name = $results->hashify_collect(['first_name', 'last_name']);
    my $alice_smith_collection   = $collections_by_full_name->{Alice}{Smith};

Multiple keys may be used to access collections. Multiple keys should be provided as an arrayref of names of returned columns.

#### SUB

    # single key
    my $collections_by_name = $results->hashify_collect(sub { $_->{name} });
    my $alice_collection    = $collections_by_name->{Alice};

    # multiple keys
    my $collections_by_full_name = $results->hashify_collect(sub { @{ $_ }{qw(first_name last_name)} });
    my $alice_smith_collection   = $collections_by_full_name->{Alice}{Smith};

Providing a subroutine for the key allows you to create the key (or keys) with the returned row.
The row is available either as `$_` or as the first argument to the subroutine. The type of the row
(["array"](#array1), ["c"](#c2), ["hash"](#hash1), ["struct"](#struct2))
that is passed to the subroutine depends on any ["OPTIONS"](#options5) value that is passed (default is ["hash"](#hash1)).

If the subroutine returns one key, the hash will be a ["SINGLE KEY"](#single-key1) hash. If multiple keys are returned
as a list, the hash with be a ["MULTIPLE KEYS"](#multiple-keys1) hash.

### VALUE

#### DEFAULT

    # collections contain hashes
    my $collections_by_name        = $results->hashify_collect('name');
    my $alice_collection_of_hashes = $collections_by_name->{Alice};

    # collections still contain hashes
    my $collections_by_name        = $results->hashify_collect({hash => 1}, 'name');
    my $alice_collection_of_hashes = $collections_by_name->{Alice};

    # collections contain arrays
    my $collections_by_name        = $results->hashify_collect({array => 1}, 'name');
    my $alice_collection_of_arrays = $collections_by_name->{Alice};

    # collections contain Mojo::Collection's
    my $collections_by_name             = $results->hashify_collect({c => 1}, 'name');
    my $alice_collection_of_collections = $collections_by_name->{Alice};

    # collections contain readonly structs
    my $collections_by_name         = $results->hashify_collect({struct => 1}, 'name');
    my $alice_collection_of_structs = $collections_by_name->{Alice};

If no value argument is provided, the default is to collect the rows as the value according to the type
specified in ["OPTIONS"](#options5) (["array"](#array1), ["c"](#c2), ["hash"](#hash1), ["struct"](#struct2)).
The default is ["hash"](#hash1).

#### COLUMN

    # age will be collected
    my $collections_by_name      = $results->hashify_collect('name', 'age');
    my $alice_collection_of_ages = $collections_by_name->{Alice};

The value can be provided as a column returned in the results, and this column value for
each row will be collected into the corresponding [Mojo::Collection](https://metacpan.org/pod/Mojo::Collection) based on the key(s).

#### SUB

    # collected value will be the age squared
    my $collections_by_name              = $results->hashify_collect('name', sub { $_->{age} * $_->{age} });
    my $alice_collection_of_ages_squared = $collections_by_name->{Alice};

Providing a subroutine for the value allows you to create the collected values for each returned row.
The row is available either as `$_` or as the first argument to the subroutine. The type of the row
(["array"](#array1), ["c"](#c2), ["hash"](#hash1), ["struct"](#struct2))
that is passed to the subroutine depends on any ["OPTIONS"](#options5) value that is passed (default is ["hash"](#hash1)).

You may return a single value, or a list of values to be collected:

    my $collections_by_name = $results->hashify_collect('name', sub { $_->{age}, $_->{favorite_food} });

## collect\_by

    # group results by a key in Mojo::Collection's inside of a Mojo::Collection
    my $results             = $db->select(people => '*');
    my $collections_by_name = $results->collect_by('name');
    say 'First collection contains rows with name', $collections_by_name->first->first->{name};

    # group results by multiple keys
    my $collections_by_full_name = $results->collect_by(['first_name', 'last_name']);
    my $first_collection = $collections_by_name->first;
    say
      'First collection contains rows with first name',
      $first_collection->first->{first_name},
      ' and last name ',
      $first_collection->first->{last_name};

    # group the values as structs instead of hashes
    my $structs_by_name = $results->collect_by({struct => 1}, 'name');

    $structs_by_name->first->each(sub {
      say $_->name, ' is ' . $_->age . ' years old';
    });

    # collect a single column value
    my $ages_by_name = $results->collect_by('name', 'age');
    say 'First collection contains ages for name', $ages_by_name->first->first->{name};

    # flatten grouped results
    my $results = $db->select(people => '*');
    # each inner Mojo::Collection is flattened
    my $column_values_by_name = $results->collect_by({flatten => 1}, 'name');

["collect\_by"](#collect_by) allows you to group rows/values that share the same key or multiple keys in [Mojo::Collection](https://metacpan.org/pod/Mojo::Collection)s inside of a [Mojo::Collection](https://metacpan.org/pod/Mojo::Collection).

["collect\_by"](#collect_by) builds on ["collect\_by" in Mojo::Collection::Role::Transform](https://metacpan.org/pod/Mojo::Collection::Role::Transform#collect_by) and adds useful functionality specific to DB results.

### OPTIONS

#### array

    my $results             = $db->select(people => '*');
    my $collections_by_name = $results->collect_by({array => 1}, 'name');

["array"](#array2) allows you to group rows as arrays instead of the default ["hash"](#hash2).
This also means the value provided to the ["KEY"](#key2) ["SUB"](#sub4) or the ["VALUE"](#value2) ["SUB"](#sub5), if used, will be an array.

#### flatten

    my $results = $db->select(people => ['name', 'age']);

    # trivial example returning arrayref to demonstrate flatten
    my $age_collections_by_name = $results->collect_by({flatten => 1}, 'name', sub { [$_->{age}] });

["flatten"](#flatten2) flattens all values for each inner [Mojo::Collection](https://metacpan.org/pod/Mojo::Collection).

["flatten"](#flatten2) may be combined with the other type options to specify the type of the rows that will be passed to the
["KEY"](#key2) ["SUB"](#sub4) or the ["VALUE"](#value2) ["SUB"](#sub5).

If no ["VALUE"](#value2) is specified, all returned column values for a row will be returned in the order they were requested
and flattened into the resulting [Mojo::Collection](https://metacpan.org/pod/Mojo::Collection). This works regardless of any type option that is specified:

    my $results = $db->select(people => ['name', 'age']);

    # both contain name and age flattened into the inner collections
    my $collections_by_name = $results->collect_by({hash => 1, flatten => 1}, sub { $_->{name} });
    my $collections_by_name = $results->collect_by({struct => 1, flatten => 1}, sub { $_->name });

Any value returned by a ["VALUE"](#value2) ["SUB"](#sub5) should be an arrayref or list of values and all values will be
added to the inner [Mojo::Collection](https://metacpan.org/pod/Mojo::Collection)s:

    my $collections_by_name = $results->collect_by({flatten => 1}, 'name', sub { $_->{age} }); # flatten not needed in this specific case because it's a list of 1
    my $collections_by_name = $results->collect_by({flatten => 1}, 'name', sub { [$_->{age}] });

#### c

    my $results             = $db->select(people => '*');
    my $collections_by_name = $results->collect_by({c => 1}, 'name');

    $collections_by_name->first->first->each(sub {
      say 'Random column value is ' . $_->shuffle->first;
    });

["c"](#c3) allows you to group rows as [Mojo::Collection](https://metacpan.org/pod/Mojo::Collection)s instead of the default ["hash"](#hash2).
This also means the value provided to the ["KEY"](#key2) ["SUB"](#sub4) or the ["VALUE"](#value2) ["SUB"](#sub5), if used, will be a [Mojo::Collection](https://metacpan.org/pod/Mojo::Collection).

#### hash

    my $results             = $db->select(people => '*');
    my $collections_by_name = $results->collect_by({hash => 1}, 'name'); # default

["hash"](#hash2) allows you to group the rows as hashes. This is the default and is the same as providing no option hash:

    my $collections_by_name = $results->collect_by('name');

This also means the value provided to the ["KEY"](#key2) ["SUB"](#sub4) or the ["VALUE"](#value2) ["SUB"](#sub5), if used, will be a hash.

#### struct

    my $results             = $db->select(people => '*');
    my $collections_by_name = $results->collect_by({struct => 1}, 'name');

    my $first_collection = $collections_by_name->first;
    say $first_collection->first->name, ' is ', $first_collection->first->age, ' years old';

["struct"](#struct3) allows you to group the rows as readonly structs provided by [Mojo::DB::Results::Role::Struct](https://metacpan.org/pod/Mojo::DB::Results::Role::Struct) instead of the default ["hash"](#hash2).
This also means the value provided to the ["KEY"](#key2) ["SUB"](#sub4) or the ["VALUE"](#value2) ["SUB"](#sub5), if used, will be a readonly struct.

### KEY

#### SINGLE KEY

    my $collections_by_name = $results->collect_by('name');

A single key may be used to group rows/values into inner [Mojo::Collection](https://metacpan.org/pod/Mojo::Collection)s. This key should be the name of a returned column.

#### MULTIPLE KEYS

    my $collections_by_full_name = $results->collect_by(['first_name', 'last_name']);

Multiple keys may be used to group rows/values into inner [Mojo::Collection](https://metacpan.org/pod/Mojo::Collection)s. Multiple keys should be provided as an arrayref of names of returned columns.

#### SUB

    # single key
    my $collections_by_name = $results->collect_by(sub { $_->{name} });

    # multiple keys
    my $collections_by_full_name = $results->collect_by(sub { @{ $_ }{qw(first_name last_name)} });

Providing a subroutine for the key allows you to create the key (or keys) with the returned row.
The row is available either as `$_` or as the first argument to the subroutine. The type of the row
(["array"](#array2), ["c"](#c3), ["hash"](#hash2), ["struct"](#struct3))
that is passed to the subroutine depends on any ["OPTIONS"](#options6) value that is passed (default is ["hash"](#hash2)).

If the subroutine returns one key, the hash will be a ["SINGLE KEY"](#single-key2) hash. If multiple keys are returned
as a list, the hash with be a ["MULTIPLE KEYS"](#multiple-keys2) hash.

### VALUE

#### DEFAULT

    # collections contain hashes
    my $collections_by_name = $results->collect_by('name');

    # collections still contain hashes
    my $collections_by_name = $results->collect_by({hash => 1}, 'name');

    # collections contain arrays
    my $collections_by_name = $results->collect_by({array => 1}, 'name');

    # collections contain Mojo::Collection's
    my $collections_by_name = $results->collect_by({c => 1}, 'name');

    # collections contain readonly structs
    my $collections_by_name = $results->collect_by({struct => 1}, 'name');

If no value argument is provided, the default is to collect the rows as the value according to the type
specified in ["OPTIONS"](#options6) (["array"](#array2), ["c"](#c3), ["hash"](#hash2), ["struct"](#struct3)).
The default is ["hash"](#hash2).

#### COLUMN

    # age will be collected
    my $collections_by_name = $results->collect_by('name', 'age');

The value can be provided as a column returned in the results, and this column value for
each row will be collected into the corresponding inner [Mojo::Collection](https://metacpan.org/pod/Mojo::Collection) based on the key(s).

#### SUB

    # collected value will be the age squared
    my $collections_by_name = $results->collect_by('name', sub { $_->{age} * $_->{age} });

Providing a subroutine for the value allows you to create the collected values for each returned row.
The row is available either as `$_` or as the first argument to the subroutine. The type of the row
(["array"](#array2), ["c"](#c3), ["hash"](#hash2), ["struct"](#struct3))
that is passed to the subroutine depends on any ["OPTIONS"](#options6) value that is passed (default is ["hash"](#hash2)).

You may return a single value, or a list of values to be collected:

    my $collections_by_name = $results->collect_by('name', sub { $_->{age}, $_->{favorite_food} });

## DIE METHODS

["DIE METHODS"](#die-methods) are equivalent to the ["get"](#get), ["get\_by\_name"](#get_by_name), ["c"](#c), and ["c\_by\_name"](#c_by_name) methods above, however,
the `die` option for these methods is passed as `true` for you and the method will die if there is no next row to be
retrieved.

Additionally, ["struct\_or\_die"](#struct_or_die), ["array\_or\_die"](#array_or_die), and ["hash\_or\_die"](#hash_or_die) are provided.

### get\_or\_die

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

Same as ["get"](#get), but dies if there is no next row to be retrieved.

### get\_by\_name\_or\_die

    my $results = $db->select(people => ['name', 'age', 'favorite_food'] => {id => 123});

    # dies if there is no next row to be retrieved
    my $name = $results->get_by_name_or_die('name');

Same as ["get\_by\_name"](#get_by_name), but dies if there is no next row to be retrieved.

### c\_or\_die

    my $results = $db->select(people => ['first_name', 'middle_name', 'last_name']);

    # dies if there is no next row to be retrieved
    my $full_name = $results->c_or_die->join(' ');

Same as ["c"](#c), but dies if there is no next row to be retrieved.

### c\_by\_name\_or\_die

    my $results = $db->select(people => ['first_name', 'middle_name', 'last_name']);

    # dies if there is no next row to be retrieved
    my $full_name = $results->c_by_name_or_die('first_name', 'middle_name', 'last_name')->join(' ');

Same as ["c\_by\_name"](#c_by_name), but dies if there is no next row to be retrieved.

### struct\_or\_die

    my $results = $db->select(people => '*' => {id => 123});

    # dies if there is no next row to be retrieved
    my $person_struct = $results->struct_or_die;

["struct\_or\_die"](#struct_or_die) is the same as ["struct"](#struct), but dies if there is no next row to be retrieved.

### array\_or\_die

    my $results = $db->select(people => ['first_name', 'middle_name', 'last_name'] => {id => 123});

    # dies if there is no next row to be retrieved
    my $full_name = join ' ', @{ $results->array_or_die };

["array\_or\_die"](#array_or_die) is similar to ["array" in Mojo::Pg::Results](https://metacpan.org/pod/Mojo::Pg::Results#array) or ["array" in Mojo::mysql::Results](https://metacpan.org/pod/Mojo::mysql::Results#array), but dies
if there is no next row to be retrieved.

### hash\_or\_die

    my $results = $db->select(people => '*' => {id => 123});

    # dies if there is no next row to be retrieved
    my $person = $results->hash_or_die;

["hash\_or\_die"](#hash_or_die) is similar to ["hash" in Mojo::Pg::Results](https://metacpan.org/pod/Mojo::Pg::Results#hash) or ["hash" in Mojo::mysql::Results](https://metacpan.org/pod/Mojo::mysql::Results#hash), but dies
if there is no next row to be retrieved.

## ONE METHODS

["ONE METHODS"](#one-methods) are equivalent to the ["get"](#get), ["get\_by\_name"](#get_by_name), ["c"](#c), ["c\_by\_name"](#c_by_name), and ["struct"](#struct) methods above, however,
the `one` option for these methods is passed as `true` for you and the method will die unless exactly one row was returned.

Additionally, ["one\_struct"](#one_struct), ["one\_array"](#one_array) and ["one\_hash"](#one_hash) are provided.

### one

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

Same as ["get"](#get), but dies unless exactly one row was returned.

### one\_by\_name

    my $results = $db->select(people => ['name', 'age', 'favorite_food'] => {id => 123});

    # dies unless exactly one row was returned
    my $name = $results->one_by_name('name');

Same as ["get\_by\_name"](#get_by_name), but dies unless exactly one row was returned.

### one\_c

    my $results = $db->select(people => ['first_name', 'middle_name', 'last_name'] => {id => 123});

    # dies unless exactly one row was returned
    my $full_name = $results->one_c->join(' ');

Same as ["c"](#c), but dies unless exactly one row was returned.

### one\_c\_by\_name

    my $results = $db->select(people => ['first_name', 'middle_name', 'last_name'] => {id => 123});

    # dies unless exactly one row was returned
    my $full_name = $results->one_c_by_name('first_name', 'middle_name', 'last_name')->join(' ');

Same as ["c\_by\_name"](#c_by_name), but dies unless exactly one row was returned

### one\_struct

    my $results = $db->select(people => '*' => {id => 123});

    # dies unless exactly one row was returned
    my $person_struct = $results->one_struct;

["one\_struct"](#one_struct) is the same as ["struct"](#struct), but dies unless exactly one row was returned.

### one\_array

    my $results = $db->select(people => ['first_name', 'middle_name', 'last_name'] => {id => 123});

    # dies unless exactly one row was returned
    my $full_name = join ' ', @{ $results->one_array };

["one\_array"](#one_array) is similar to ["array" in Mojo::Pg::Results](https://metacpan.org/pod/Mojo::Pg::Results#array) or ["array" in Mojo::mysql::Results](https://metacpan.org/pod/Mojo::mysql::Results#array), but dies
unless exactly one row was returned.

### one\_hash

    my $results = $db->select(people => '*' => {id => 123});

    # dies unless exactly one row was returned
    my $person = $results->one_hash;

["one\_hash"](#one_hash) is similar to ["hash" in Mojo::Pg::Results](https://metacpan.org/pod/Mojo::Pg::Results#hash) or ["hash" in Mojo::mysql::Results](https://metacpan.org/pod/Mojo::mysql::Results#hash), but dies
unless exactly one row was returned.

# AUTHOR

Adam Hopkins <srchulo@cpan.org>

# COPYRIGHT

Copyright 2019- Adam Hopkins

# LICENSE

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

# SEE ALSO

- [Mojo::Pg::Results](https://metacpan.org/pod/Mojo::Pg::Results)
- [Mojo::mysql::Results](https://metacpan.org/pod/Mojo::mysql::Results)
- [Mojo::DB::Role::ResultsRoles](https://metacpan.org/pod/Mojo::DB::Role::ResultsRoles)
- [Mojo::DB::Results::Role::Struct](https://metacpan.org/pod/Mojo::DB::Results::Role::Struct)
- [Role::Tiny](https://metacpan.org/pod/Role::Tiny)
- [Mojo::Base](https://metacpan.org/pod/Mojo::Base)
