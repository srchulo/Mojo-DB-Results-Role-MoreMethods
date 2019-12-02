requires 'perl', '5.010001';

requires 'Mojolicious';
requires 'Role::Tiny', '2.000001';

on test => sub {
    requires 'Test::More', '0.96';
    requires 'Test::Exception';
    requires 'Test::Warn';
    requires 'Test::Pod';
};