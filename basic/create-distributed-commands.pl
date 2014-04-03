#!/usr/bin/perl -w
use strict;

my @machines   = ('ds02'..'ds08');
my @outputs    = map { open my $fh, '>', "cmds-$_.sh"; $fh } @machines;
print $_ "#!/bin/bash\nsource ~/.bash_profile\nexport LD_LIBRARY_PATH=\$HOME/lib\n" foreach (@outputs);
chmod 0755, $_ foreach (@outputs); # -rwxr-xr-x

my @commands = ();
my $command = '';
open my $make, '-|', 'make -n picotables-slow 2>&-';
while (<$make>) {
  if (/^make picotables/../^time make/ and !/^(ghc|make picotables|time make)/) {
    # This is easy, so place it on the machine least likely to be overloaded.
    my $out = $outputs[-1];
    print $out $_;
  }
  $command .= $_ if /^export FILENAME/../^\tmv/;
  if (/^\tmv/) {
    chomp $command;
    $command .= "; \\\n\tscp \$FILENAME antals\@eniac.seas.upenn.edu:~/html/picotables/\n";
    push @commands, $command;
    $command = '';
  }
}
close $make;

my %prop_commands = ();
foreach (@commands) {
  if (/exp-\w+-(\w+)/) {
    $prop_commands{$1} ||= [];
    push @{$prop_commands{$1}}, $_;
  }
}

my $cur_output = 0;
foreach my $cmd (map { @{$prop_commands{$_}} } qw(EENI LLNI SSNI)) {
  my $fh = $outputs[$cur_output++];
  $cur_output %= @outputs;
  print $fh $cmd;
}

foreach my $i (0..$#machines) {
  my $machine = $machines[$i];
  my $out     = $outputs[$i];
  print $out "mail -s 'Finished running commands on $machine <EOM>' antals\@seas.upenn.edu < /dev/null\n";
  close $out;
}

open my $script, '>', 'distribute-commands.sh';
chmod 0755, $script; # -rwxr-xr-x
print $script "#!/bin/bash\n";
print $script "svn commit -m '[minor] Beginning distributed table generation'\n";
print $script "ssh bcpierce\@ds01.seas.upenn.edu 'source ~/.bash_profile; cd ~/safeqc; svn up; cd tmu; make'\n";
foreach my $machine (@machines) {
  print $script "ssh -f bcpierce\@$machine.seas.upenn.edu 'cd ~/safeqc/tmu; ./cmds-$machine.sh'\n"
}
close $script;
