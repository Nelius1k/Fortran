program ExamHalls
    implicit none
    
    character :: name(25)
    integer :: student_id, last_digits
    logical :: check = .true.

    !print*, 'Please enter your full name (maximum 25 letters) using quotations (e.g. "John Smith").'
    !read(*,*) name

    print*, "Please enter your student number."
    read(*,*) student_id

    last_digits = MOD(student_id, 100)

    do while(check)
        
        if (student_id < 215001 .or. student_id > 221999) then
            print*, "Your student number is not valid. Please enter it again."
            print*, "If you are not sure, press control+C on your keyboard and try again lat210er."
            read*, student_id
            last_digits = MOD(student_id, 100)
            cycle
        else if (last_digits >= 00 .and. last_digits <= 33) then
            print*, "Dear student, your exam will be held in Hall-A"
            check = .false.
        else if (last_digits >= 34 .and. last_digits <= 66) then
            print*, "Dear student, your exam will be held in Hall-B"
            check = .false.
        else if (last_digits >= 67 .and. last_digits <= 99) then
            print*, "Dear student, your exam will be held in Hall-C"
            check = .false.
        end if
    end do
           

    
end program ExamHalls


