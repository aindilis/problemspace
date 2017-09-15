package PS::Operations;

use Data::Dumper;

use Class::MethodMaker
  new_with_init => 'new',
  get_set       => [ qw /  / ];

sub init {
  my ($self,%args) = (shift,@_);
}

sub Execute {
  my $self = shift;
}

sub New {

}

1;
