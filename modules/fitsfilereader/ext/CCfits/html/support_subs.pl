# This is support_subs.pl.  It will be # 'require'd in the web scripts, so it 
# must return 1 at the very end.

sub RunningDisplay{

# $full_filename is passed in.
    local($full_filename) = @_;

#   Get the basename for the running display of files processed
    if ( $full_filename =~ m#/# ) {
       $full_filename =~ m#/([^/]+)$#;
       $basename = $1;
    }
    else {
	$basename = $full_filename;
    }
    $num_files++;

# I want a running display of form:
# num_field  mesg_field  name_field    e.g.
# 12378 files processed so far... some_random_file.html 
    $num_files_string = sprintf ("%u", $num_files);
    $num_field = length $num_files_string;
    $mesg = " files processed so far... ";
    $mesg_field = length $mesg;
    $name_field = 79 - ($num_field + $mesg_field);
    $name = "";
    $name = sprintf ("%-${name_field}.${name_field}s", $basename);
# if $| is set to nonzero, forces a flush after every print on the currently
# selected output filehandle.
    $| = 1;
    printf "\r%u%s%s", $num_files, $mesg, $name;
}

# To require this file, it must return a value of 1 after loading:
1;



