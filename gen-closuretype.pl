#!/usr/bin/env perl
use feature 'say', 'switch';
use warnings;
use strict;

# -- Auxiliary functions -----------

# Do any version-specific preprocessing
sub strip_per_version {
    my $hcver = shift;
    my @data  = @_;
    my @res;

    given ($hcver) {
        when(/7\.[0-4]{1}\.*/) { 
            @res = grep(/#define/, @data);
            @res = grep(!/#define RTS_STORAGE_CLOSURETYPES_H/, @res);
        }
        default { die "strip version: GHC version unsupported!" }
    }

    #
    # WARNING WARNING WARNING
    # 
    # @res at this point should contain only lines which begin with '#define'
    # which define all the necessary closures types for that version of GHC.
    # Weird things will probably happen otherwise
    # 

    my @finalresults; # yes these variables are named terribly...
    foreach (@res) {
	my ($prefix, $nm, $val) = split;
	$finalresults[$val]     = $nm;
    }
    return @finalresults;
}

sub output_hs {
    my $ver     = shift;
    my @verdots = split(/./, $ver);
    
    my $modname;
    given($ver) {
        when(/7\.([0-4]{1})\.*/) { $modname = "V70$1"; } 
        default { die "output_hs: GHC version unsupported!"; } 
    }
    my $modprefix = <<END;
module GHC.Vacuum.ClosureType.$modname ( ClosureType(..) ) where

END

    my $datadecl = "data ClosureType\n  ";
    my $instancetop = "instance Enum ClosureType where";
    my $toenum = "";
    my $fromenum = "";

    my $i = 0;
    foreach (@_) {
	my $prefix = ($i == 0) ? "=": "|";
	$datadecl .= "$prefix $_[$i]\n  ";

	$fromenum .= "  fromEnum $_[$i]\t= $i\n";
	$toenum   .= "  toEnum $i\t= $_[$i]\n";

	$i++;
    }

    $datadecl .= "deriving (Eq, Ord, Show, Read)\n";
    $toenum   .= "  toEnum n\t= error (\"toEnum: ClosureType: invalid ClosureType: \" ++ show n)\n";

    # And now output all the things!
    say $modprefix;
    say $datadecl;
    say $instancetop;
    say $fromenum;
    say $toenum;
}

# -- Main --------------------------

# Find GHC binary, version, and ClosureTypes.h
my $hc;
if(defined($ARGV[0])) {
    $hc = $ARGV[0]; chomp($hc);
}
else {
    $hc = "ghc";
}

my $hcver    = `$hc --numeric-version`; chomp($hcver);
my $hclibdir = `$hc --print-libdir`; chomp($hclibdir);
my $closuretypesf = $hclibdir . "/include/rts/storage/ClosureTypes.h";

die "ERR: couldn't locate ClosureTypes.h!" unless (-e $closuretypesf);
#say "Located $closuretypesf (version $hcver)";

# Open file, filter it, and print out the results
open(CLOSURETYPES, $closuretypesf) or die "Couldn't open ClosureTypes.h!";
my @closuretypesd = <CLOSURETYPES>;
close(CLOSURETYPES);

my @results = strip_per_version($hcver, @closuretypesd);

output_hs($hcver, @results);
