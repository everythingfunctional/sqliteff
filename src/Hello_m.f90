module Hello_m
    use iso_varying_string, only: VARYING_STRING, operator(//)

    implicit none
    private

    public :: sayHello
contains
    pure function sayHello(name) result(greeting)
        type(VARYING_STRING), intent(in) :: name
        type(VARYING_STRING) :: greeting

        greeting = "Hello, " // name // "!"
    end function sayHello
end module Hello_m
