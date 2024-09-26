module my_temps
	type temps
		integer :: dates, hi, lo, av
		real :: hinorms, lonorms
	end type temps
	contains
		subroutine file2array(datafile, array)			
			! declaration block
			implicit none
			character(*) :: datafile
			type(temps), allocatable :: array(:)
			integer :: i, nr_lines, nr_elements
			type(temps) :: dummy
			
			! open the data file
			write (*,*) "Read-in the data from '", datafile, "'"
			open(1,file=datafile,status='old')
			
			! count number of lines
			nr_lines = 0
			do
				read(1,*,end=10) dummy
				nr_lines = nr_lines + 1
			end do
			
			! rewind to beginning of file
			10 rewind(1)
			! # of elements in array = # of lines in file
			nr_elements = nr_lines
			! allocate the array
			allocate (array(nr_elements))
				
			! read array elements from file
			do i = 1, nr_elements
				read(1,*) array(i)
				write(*,'(a)',advance='no') '.'
			end do
			write(*,*) ! (next line)
			close(1)
		end subroutine file2array
		
		subroutine array2screen(array_name, array)
			! declaration block
			implicit none
			integer :: n, i
			type(temps), allocatable :: array(:)
			character(*) :: array_name
			
			! ensure array is allocated
			if (allocated(array)) then
				n = size(array)
				do i = 1, n
					write(*,*) array_name, '[', i, '] = ', array(i)
				end do
				
			! if it isn't, print error msg
			else
				write(*,*) 'Array not allocated!'
			end if
		end subroutine array2screen
		
		subroutine tavgnorms(array, avnorms)
			! declaration block
			implicit none
			type(temps), allocatable :: array(:)
			real, allocatable :: avnorms(:)
			integer i, n
			
			! allocate the array to hold average normal temps
			n = size(array)
			allocate(avnorms(n))
			
			! compute the average normals and store them to 'avnorms'
			do i = 1, n
				avnorms(i) = ((array(i)%hinorms + array(i)%lonorms)/2)
			enddo
		end subroutine tavgnorms
		
		subroutine high_norm_runs(array)
			! declaration block
			implicit none
			type(temps), allocatable :: array(:)
			integer :: i, n, upcount, downcount, maxuprun, maxdownrun
			integer :: upbins(0:50) = (0), downbins(0:50) = 0
			
			! let end-of-loop count = number of years
			n = size(array)
			
			! initiate counters
			upcount = 0
			downcount = 0
			maxuprun = 0
			maxdownrun = 0
			
			! compute the histograms-> see low_norm_runs for more detail
			do i = 1, n
				if(array(i)%hinorms /= 99.99) then
					if(array(i)%hi > array(i)%hinorms) then
						if(downcount > maxdownrun) then
							maxdownrun = downcount
						endif
						downbins(downcount) = downbins(downcount) + 1
						upcount = upcount + 1
						downcount = 0
					else
						if (upcount > maxuprun) then
							maxuprun = upcount
						end if
						upbins(upcount) = upbins(upcount) + 1
						downcount = downcount + 1
						upcount = 0						
					end if
				end if
			end do
			
			! write the histograms to the screen and to output file
			do i = 1, 50
				write(*,*) '[',i,']:', upbins(i), downbins(i)
				write(2,*) upbins(i), downbins(i)
			end do
			
			! write the max run lengths to screen and to output file
			write (*,*) maxuprun, maxdownrun
			write (2,*) maxuprun, maxdownrun
			write (2,*)
			
		end subroutine high_norm_runs
		
		subroutine low_norm_runs(array)
			! declaration block
			implicit none
			type(temps), allocatable :: array(:)
			integer :: i, n, upcount, downcount, maxuprun, maxdownrun
			integer :: upbins(0:50) = (0), downbins(0:50) = 0
			
			! let end-of-loop count = number of years
			n = size(array)
			
			! initiate counters
			upcount = 0
			downcount = 0
			maxuprun = 0
			maxdownrun = 0
			
			! compute the histograms
			do i = 1, n
			! for each day... 
				if(array(i)%lonorms /= 99.99) then
				! if the normal low temperature data is available...
					if(array(i)%lo > array(i)%lonorms) then
					! and if the low temp exceeds the normal...
						if(downcount > maxdownrun) then
						! AND if the current down run is the largest yet encountered...
							maxdownrun = downcount
							! then assign the length of the current run as the largest
						endif
						! whether it's the largest encountered run or not...
						downbins(downcount) = downbins(downcount) + 1
						! increase the count of down runs of this length by 1...
						upcount = upcount + 1
						! and increase the number of consecutive low > norm days by 1
						downcount = 0
						! Also, set the down count back to zero...
					else
					! if the low temp falls short of the normal...
						if (upcount > maxuprun) then
						! AND if the current up run is the largest yet encountered...
							maxuprun = upcount
							! then assign the length of the current run as the largest
						end if
						! whether its the largest encountered run or not...
						upbins(upcount) = upbins(upcount) + 1
						! increase the count of up runs of this length by 1...
						downcount = downcount + 1
						! and increase the number of consecutive low < norm days by 1
						upcount = 0
						! Also, set the up count back to zero.
					end if
				end if
			end do
			
			! write histograms to the screen and to output file
			do i = 1, 50
				write(*,*) '[',i,']:', upbins(i), downbins(i)
				write(2,*) upbins(i), downbins(i)
			end do
			
			! write the max lengths to the screen and to output file
			write (*,*) maxuprun, maxdownrun
			write (2,*) maxuprun, maxdownrun
			write (2,*)
			
		end subroutine low_norm_runs
		
		subroutine avg_norm_runs(array, avnorms)
			! declaration block
			implicit none
			type(temps), allocatable :: array(:)
			integer :: i, n, upcount, downcount, maxuprun, maxdownrun
			integer :: upbins(0:50) = (0), downbins(0:50) = 0
			real, allocatable :: avnorms(:)
			
			!let end-of-loop count = number of years
			n = size(array)
			
			! initiate counters
			upcount = 0
			downcount = 0
			maxuprun = 0
			maxdownrun = 0
			
			! compute the histograms
			do i = 1, n
				if(avnorms(i) /= 99.99) then
					if(array(i)%av > avnorms(i)) then
						if(downcount > maxdownrun) then
							maxdownrun = downcount
						endif
						downbins(downcount) = downbins(downcount) + 1
						upcount = upcount + 1
						downcount = 0
					else
						if (upcount > maxuprun) then
							maxuprun = upcount
						end if
						upbins(upcount) = upbins(upcount) + 1
						downcount = downcount + 1
						upcount = 0
					end if
				end if
			end do
			
			do i = 1, 50
				write(*,*) '[',i,']:', upbins(i), downbins(i)
				write(2,*) upbins(i), downbins(i)
			end do
			
			write (*,*) maxuprun, maxdownrun
			write (2,*) maxuprun, maxdownrun
			write (2,*)
			
		end subroutine avg_norm_runs
		
end module my_temps

program tdata
	! declaration block
	use my_temps
	real, allocatable :: avnorms(:)
	type(temps), allocatable :: temparray(:)
	
	! read data from file to an array
	call file2array('c:\perl\scripts\496\rescraped_trend_space.txt', temparray)
	write(*,*) ! (next line)
	
	! print array to screen
	write(*,*) 'Array of all temps'
	call array2screen('temperature', temparray)
	write(*,*)
	
	! compute Tavg normals
	write(*,*) 'Copmpute the Tavg Normals'
	call tavgnorms(temparray, avnorms)
	write(*,*)
	
	! open file for writing histograms
	open(2,file='c:\g95\bin\rescraped_trend_hist.txt',status='old')
	
	! open file for writing all runs
	open(3,file='c:\g95\bin\all_runs.txt',status='old')
	
	! open file for writing all runs by year
	open(4,file='c:\g95\bin\all_runs_yearly.txt',status='old')
	
	! compute histogram of runs of Thigh above or below the Tnorms
	write(*,*) 'Histogram of Tmax above/below Tnorm'
	call high_norm_runs(temparray)
	write(*,*)	
	
	! compute histogram of runs of Tlow above or below the Tnorms
	write(*,*) 'Histogram of Tlow above/below Tnorm'
	call low_norm_runs(temparray)
	write(*,*)
	
	! compute histogram of runs of Tavg above or below the Tnorms
	write(*,*) 'Histogram of Tavg above/below Tnorm'
	call avg_norm_runs(temparray, avnorms)
	write(*,*)

end program tdata