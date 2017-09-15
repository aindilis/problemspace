package PS;

use BOSS::Config;

use Data::Dumper;

use Class::MethodMaker
  new_with_init => 'new',
  get_set       => [ qw / Config / ];

sub init {
  my ($self,%args) = (shift,@_);
  $specification = "
	-s <loc>		Start location
	-e <loc>		Finish location
	-t <time>		Start time
	-T <time>		End time
	-d <files>...		Data files
	-u [<host> <port>]	Run as a UniLang agent
	-p			Schedule Report
	-r			Travel Times Report

";
  $self->Config(BOSS::Config->new
		(Spec => $specification,
		 ConfFile => ""));
  my $conf = $self->Config->CLIConfig;
}

sub Execute {
  my ($self,%args) = (shift,@_);
}

1;

