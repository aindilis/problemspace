#!/usr/bin/perl -w

use BOSS::Config;
use PerlLib::SwissArmyKnife;

$specification = q(
	-i <file>		Input file to create matrixdata file from
	-o <file>		Output file to print matrixdata file to

	-w <width>		Width to make the file
	-h <height>		Height to make the file
);

my $config =
  BOSS::Config->new
  (Spec => $specification);
my $conf = $config->CLIConfig;
# $UNIVERSAL::systemdir = "/var/lib/myfrdcsa/codebases/minor/system";

my $c;
if (exists $conf->{'-i'}) {
  $c = read_file($conf->{'-i'});
}
my $width = $conf->{'-w'};

my @lines = split /\n/, $c;
my @randomized = RandomizeList(List => @lines);

my @output;
my $i = 0;
foreach my $line (@randomized) {
  chomp $line;
  $line =~ s/^\s*//sg;
  $line =~ s/\s*$//sg;
  next unless $line =~ /./;
  my $length = length($line);
  if ($length < $width) {
    my $spacing = $width - $length;
    my $pre = rand($spacing);
    my $post = $spacing - $pre;
    push @output, (' 'x$pre).$line.(' 'x$post);
  }
  ++$i;
  last if $i >= $conf->{'-h'};
}

my $output = join("\n",@output);
WriteFile
  (
   File => $conf->{'-o'},
   Contents => $output,
  );
