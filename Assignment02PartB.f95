program VehicleEntrySearch
    implicit none

    character(len=100) :: inputFile, outputFile, line
    integer :: fileUnitInput, fileUnitOutput, io

    
    inputFile = 'logfile.txt'    
    outputFile = 'output.txt'   

   
    open(unit=10, file=inputFile, status='old', action='read', iostat=io)
    if (io /= 0) then
        print *, "Error opening input file"
        stop
    end if

    
    open(unit=11, file=outputFile, status='unknown', action='write', iostat=io)
    if (io /= 0) then
        print *, "Error creating or opening output file"
        stop
    end if

    
    do
        read(10, '(A)', iostat=io) line
        if (io /= 0) then
            print *, "End of file or read error"
            exit
        end if

        
        if (index(line(36:43), ' 15 ') /= 0) then 
            write(11, '(A)') line
        end if
    end do
    
    close(fileUnitInput)
    close(fileUnitOutput)

end program VehicleEntrySearch
