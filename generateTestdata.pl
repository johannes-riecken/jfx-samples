#!/usr/bin/perl -w
use v5.30;
use Data::Dumper;

my $i = 0;
while (<>) {
    if (/^\s*+let got = translate ("public class (\w++).*")$/) {
        s/\$/\\\$/g;
        s/\@/\\\@/g;
        my $base_name = $2;

        open my $f, '>', "testdata/$base_name.java";
        say {$f} eval $1;
        warn $@ if $@;
        close $f;
    }

    if (/^\s*+got `shouldBe` Right (".*")$/) {
        s/\$/\\\$/g;
        s/\@/\\\@/g;
        open my $f, '>', sprintf 'testdata/out%02d.hs', $i;
        say {$f} eval $1;
        warn $@ if $@;
        say {$f} "main :: IO ()\nmain = pure ()";
        close $f;
        $i++;
    }
}
