#!/usr/bin/perl
# Bob Crosier  3/8/95
# Bob Crosier  3/22/95 (modified from webclean.pl)
# Bob Crosier  6/02/95 required the RunningDisplay subroutine

# This Perl script MAKES CHANGES TO FILES; BE CAREFUL

# Pass this Perl script a bunch of filenames on STDIN 
#   (e.g. by piping it the output of 'find' or 'ls')
# This script will make changes within the files, but when done, the files
# will have the same owner/permissions.
#
#This Perl script removes external links and replaces them with the URL.
#------------------------------------------------------------------------------

# This require has a number of useful support subroutines.
require "support_subs.pl";

$program = "shuttlechange";

if ($ARGV[0] ne '') {
    $diff_dump = "diff_dump";
    $diff_tmp = "DIFFTMP";
    if (-e $diff_dump) {
        unlink $diff_dump;
    }
}

if ($ARGV[0] ne 'x') {
    $write_flag = 1;
}

$greeting = <<"Greeting";


		 THIS PERL SCRIPT CAN CHANGE FILES !
This script reads in filenames on STDIN (so pass it output from 'find' or 'ls')
Any comments from this run will go into file $program.comments.
Any warnings or error messages will go into file $program.warn.
I will always ignore the file "heasarc_results.html", which is the MOMspider
result file, a huge ~ 16MB file, and not relevant to these changes.
Also, heasarc_results_small.html will be ignored.
I will use STDOUT for a running display of files processed...

Greeting


print $greeting;

open(COMMENTS, ">$program.comments") || warn "Cannot open file $program.comments: $!";
open(STDERR, ">$program.warn") || warn "Cannot open file $program.warn: $!";

$num_files = 0;
while ($full_filename = <STDIN>) {
    chop($full_filename);

    &RunningDisplay( $full_filename );

# I catch if the current filename is that of a zero length or unwriteable (to me)
# file, and if not, read the entire file into memory (Perl: no limits) before
# I make any changes to the text, then write it all back out to the same
# filename, preserving owner and permissions.

    $current_file = "";
    if (-z $full_filename) {
       warn "File $full_filename has 0 size ! \n"; }
    elsif (! -w $full_filename ) {
       warn "File $full_filename is not writeable to me ! \n"; }
    elsif ( $basename eq "heasarc_results.html" ) {
        warn "File $full_filename is being deliberately skipped. \n"; }
    elsif ( $basename eq "heasarc_results_small.html" ) {
        warn "File $full_filename is being deliberately skipped. \n"; }
    else {

       open(HTMLIN, $full_filename) || warn "Cannot open file $full_filename for input: $!";
  
#      Read the entire file into variable $current_file

       $string_found_flag = 0;
       while ($line = <HTMLIN>) {
          $current_file .= $line; 
       }

# Following are example search and replace statements.  This is where you have
# to put in your own.  It must end with $string_found_flag = 1.  The search
# must use "isg" if you want it to be a global search (g) that ignores case (i)
# and looks at more than one line at a time (s).



# to remove shuttle_left and shuttle_right from files


       if ($current_file =~ /Inheritance diagram for(.*?)usemap/isg) {
           $current_file =~ s/Inheritance diagram for(.*?)usemap/Inheritance diagram for$1alt="Inheritance diagram" usemap/isg;
           $string_found_flag = 1;
       }


#      Write any changed text back into the exact same filename
       if ($string_found_flag) {
         if ($write_flag) {
             open(HTMLOUT, ">$full_filename") || warn "Cannot open file $full_filename for output: $!";
             print HTMLOUT $current_file;
             close(HTMLOUT);
             if ($diff_dump ne '') {
               system("cp $full_filename $diff_tmp");
             }
         }
         else {
             open(DD,">$diff_tmp")||warn "Could not write $diff_tmp!";
             print DD $current_file;
             close(DD);
         }
         if ($diff_dump ne '') {
             system("echo Comparing $diff_tmp to $full_filename >> $diff_dump");
             system("diff $diff_tmp $full_filename >> $diff_dump");
             unlink $diff_tmp;
         }
       }
   }
    close(HTMLIN);


}
print "\nDone \n";
close(COMMENTS);

# end of shuttlechange.pl
