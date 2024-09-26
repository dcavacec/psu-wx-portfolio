#!/usr/bin/perl  
# Makes the script executable 

use strict; # ensures proper variable declarations, etc.
use warnings;	# allows warnings to be issued

# Create date list-------------------------------
use Date::Simple::D8 (':all');					 
	my $start = Date::Simple::D8->new('19790101');	 
	my $end = Date::Simple::D8->new('20090718');	 
	my @dates;
	while ( $start < $end ) {						
		push @dates, $start;						
		#chomp(@dates);								
		$start = $start->next;						
	}		
	
#-------------------------------------------------
# Open file for writing-------------------------------------------
open OUT, '>', "c:/perl/scripts/496/hlai.txt" or die "open: $!\n";
#-----------------------------------------------------------------

# Initiate browsing agent-----------------------------------------
use WWW::Mechanize;
my $url = "http://bub2.meteo.psu.edu/wxstn/wxstn.htm";
my $mech = WWW::Mechanize->new(keep_alive => 1);
$mech->get($url);
	# Start the loop------------------------------------------------
	while (@dates) {
		
		$mech->submit_form(
			form_number => 1,
			fields      => { dtg => $dates[0], }
		);
		# Download the resulting page, text only, and scrape for data-
		my $page = $mech->content(format=>'text');
		
		my @data = ($page =~ /Temperature\s+:\s+(\d\d)/g);
		my ($rain) = $page =~ /Rain or Liquid Equivalent\s+:\s+(\S*)/;
		# Replace 'TRACE' 
		if ($rain eq 'TRACE') {
			$rain = '0.00';
			}
		my ($snow) = $page =~ /Snow and\/or Ice Pellets\s+:\s+(\S*)/;
		# Replace 'TRACE'
		if ($snow eq 'TRACE') {
			$snow = '0.00';
			}
		my ($depth) = $page =~ /Snow Depth\s+:\s+(\S*)/;
		# Replace '(N/A)/TRACE/0'
		if ($depth eq '(N/A)' or $depth eq 'TRACE' or$depth eq '0') {
			$depth = "99";
			}
		my ($hdd) = $page =~ /Degree-Days\s+:\s+(\S*)/;
		my ($maxyrhi) = $page =~ /Average High Temperature\s+:\s+(\S*)/;
			if ($maxyrhi eq '(N/A)') {
			$maxyrhi = "99.99";
			}
		my ($thirtyyrhi) = $page =~ /30-Year Average High Temperature\s+:\s+(\S*)/;
			if ($thirtyyrhi eq '(N/A)') {
			$thirtyyrhi = "99.99";
			}
		my ($tenyrhi) = $page =~ /10-Year Average High Temperature\s+:\s+(\S*)/;
			if ($tenyrhi eq '(N/A)') {
			$tenyrhi = "99.99";
			}
		my ($maxyrlo) = $page =~ /Average Low Temperature\s+:\s+(\S*)/;
			if ($maxyrlo eq '(N/A)') {
			$maxyrlo = "99.99";
			}
		my ($thirtyyrlo) = $page =~ /30-Year Average Low Temperature\s+:\s+(\S*)/;
			if ($thirtyyrlo eq '(N/A)') {
			$thirtyyrlo = "99.99";
			}
		my ($tenyrlo) = $page =~ /10-Year Average Low Temperature\s+:\s+(\S*)/;
			if ($tenyrlo eq '(N/A)') {
			$tenyrlo = "99.99";
			}
		# Format the output for Fortran analysis---------------------
		use Fortran::Format;
		my $fdepth = Fortran::Format->new("I2.1,6X")->write($depth);
			chomp $fdepth;
		my $frain = Fortran::Format->new("F4.2,6X")->write($rain);
			chomp $frain;
		my $fsnow = Fortran::Format->new("F4.2,6X")->write($snow);
			chomp $fsnow;
		my $f = Fortran::Format->new("I2.1,6X")->write($hdd);
			chomp $f;
		#------------------------------------------------------------
		
		# Assign data to the array-------------------------------------------------
		my $hlahdd = ("$dates[0] $data[0] $data[1] $data[2] $maxyrhi $thirtyyrhi $tenyrhi $maxyrlo $thirtyyrlo $tenyrlo $fdepth $frain $fsnow $f\n");
		#--------------------------------------------------------------------------
		
		# Print the array to screen and to file
		print "$hlahdd";
		print OUT "$hlahdd"; 
		#--------------------------------------
		
		# Slow down... then go back a page
		sleep .1;	
		$mech->back();
		#---------------------------------
		
		# remove the date just used
		shift @dates;
		#--------------------------
		
	}	# Exit the loop

close OUT; # Close the written file-------------------------------
