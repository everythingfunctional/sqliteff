program Hello_world
    use Hello_m, only: sayHello
    use iso_varying_string, only: VARYING_STRING, get, put

    implicit none

    type(VARYING_STRING) :: name

    name = askForName()
    call put(sayHello(name))
contains
    function askForName() result(name_)
        type(VARYING_STRING) :: name_

        print *, "What's your name?"
        call get(name_)
    end function askForName
end program Hello_world
