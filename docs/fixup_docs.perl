#!/usr/bin/perl

use strict;
use File::Basename;
use Cwd 'abs_path';

sub escape_html($) {
    my $s = $_[0];
    $s =~ s/&/&amp;/g;
    $s =~ s/</&lt;/g;
    $s =~ s/>/&gt;/g;
    $s =~ s/\"/&#34;/g;
    $s =~ s/\'/&#37;/g;
    return $s;
}

my $dbg = 1;

sub glossaryTextToKey($) {
    my $t = $_[0];
    $t =~ s/["']//g;
    $t =~ s/^\s*|\s*$//g;
    $t =~ s/e?s$//;
    $t =~ s/\s+/_/g;
    $t = "\L$t";
    return $t;
}

my $dir = dirname(abs_path($0));
print STDERR "Working in $dir\n";

my %glossaryItems;
open(GLOSSARY, "<$dir/glossary.md") or die $!;
while (<GLOSSARY>) {
    if (m/^##(?:\s*<a name="([^\"]+)"><\/a>)?\s*(.*?)\s*$/) {
        my $text = $2;
        my $oldKey = $1;
        my $key = glossaryTextToKey($text);
        if ($oldKey ne "" && $oldKey ne $key) {
            print STDERR "glossary.md:#.: ERROR: Key changed $oldKey != $key\n";
        }
        if (exists($glossaryItems{$key})) {
            print STDERR "glossary.md:$.: ERROR: Duplicate glossary item $text => $key\n";
        } else {
            #print STDERR "glossary.md:$.: Found glossary item $text\n";
            $glossaryItems{$key} = 1;
        }
    }
}
close(GLOSSARY);

my %grammarItems;
open (GRAMMAR, "<$dir/grammar.md") or die $!;
while (<GRAMMAR>) {
    if (m/^\*(\w+)\* :=/) {
        my $name = $1;
        if (exists($grammarItems{$name})) {
            print STDERR "grammar.md:$.: ERROR: Duplicate grammar item $name\n";
        } else {
            $grammarItems{$name} = 1;
        }
    }
}
close(GRAMMAR);

sub linkToGrammarItem($$) {
    my $loc = $_[0];
    my $name = $_[1];
    if (exists($grammarItems{$name})) {
        return "[*$name*](grammar.md#$name)";
    } else {
        print STDERR "$loc: ERROR: No such grammar item $name\n";
    }
    return "*$name*";
}

sub linkToGlossaryItem($$) {
    my $loc = $_[0];
    my $text = $_[1];
    my $key = glossaryTextToKey($text);
    if (exists($glossaryItems{$key})) {
        return "[$text](glossary.md#$key)";
    } else {
        print STDERR "$loc: ERROR: No such glossary item $key\n";
    }
    return "[$text](glossary.md)";
}

sub addGlossaryAnchor($$) {
    my $loc = $_[0];
    my $text = $_[1];
    my $key = glossaryTextToKey($text);
    return "## <a name=\"$key\"><\/a> $text\n";
}

my %fixedFiles = ();
opendir(DIR, $dir) or die $!;
while (my $basename = readdir(DIR)) {
    next unless $basename =~ /\.md$/;

    my $file = "$dir/$basename";

    print STDERR "Processing $file\n";

    open (IN, "<$file") or die "$!";
    my $original = "";
    my $fixed = "";
    while (<IN>) {
        $original .= $_;

        # Replace links to glossary items with links to the right anchor.
        s/\[(.*?)\]\(glossary.md\)/linkToGlossaryItem("$basename:$.", $1)/ge;

        if ($basename eq "glossary.md") {
            # Add anchors where necessary.
            s/^##(?!\s*<a name)\s*(.*?)\s*$/addGlossaryAnchor("$basename:$.", $1)/ge;
        } elsif ($basename eq "grammar.md") {
            # Replace non-terminals on the right of a production with links to the definition.
            s/(?<!^)(?<![\[\*])\*(\w+)\*/linkToGrammarItem("$basename:$.", $1)/ge;

            if (m/^\*(\w+)\* :=/) {
                my $name = $1;
                if (exists($grammarItems{$name})) {
                    my $tail = substr $fixed, -20;
                    $tail =~ s/[\r\n]/ /g;
                    print "tail of fixed : `$tail`\n";
                    if ($fixed !~ /<\/a>\s*$/) {
                        $fixed .= "<a name=\"$name\"></a>\n";
                    }
                }
            }
        }

        $fixed .= $_;
    }
    if ($fixed ne $original) {
        $fixedFiles{$file} = $fixed;
    }
}
closedir(DIR);

foreach my $file (keys(%fixedFiles)) {
    print "$file changed\n";
}

my $commit = $ARGV[1] eq "-c";

foreach my $file (keys(%fixedFiles)) {
    if ($commit) {
        rename "$file", "$file.bak" or die "$!";
        open (OUT, ">$file") or die "$!";
    } else {
        open (OUT, ">$file.tentative") or die "$!";
    }
    print OUT $fixedFiles{$file};
    close (OUT) or die "$!";
}
unless ($commit) {
    print STDERR "Not committing changes\n";
}
