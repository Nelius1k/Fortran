program WindChill
    implicit none

    real :: Temp, Vel, result

    print*, "Enter Temperature (T) in Farenheit and Wind Speed (V) values"
    read*, Temp, Vel 

    if(Temp >= 91.4 .or. Vel < 4) then
        print*, "Temperature can't be greater than 91.4 or speed can't be less than 4"
        
    else 
        result = ((0.279*sqrt(Vel)) + 0.550 - (0.0203*Vel)) * (Temp - 91.4) + 91.4
        print*, "The Windchill Temperature is ", result
    end if

    

end program WindChill