module hello_test
    use Hello_m, only: sayHello
    use iso_varying_string, only: var_str
    use Vegetables_m, only: Result_t, TestItem_t, assertEquals, describe, it

    implicit none
    private

    public :: test_hello
contains
    function test_hello() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(1)

        individual_tests(1) = it("says hello", checkHello)
        tests = describe("sayHello", individual_tests)
    end function test_hello

    pure function checkHello() result(result_)
        type(Result_t) :: result_

        result_ = assertEquals("Hello, World!", sayHello(var_str("World")))
    end function checkHello
end module hello_test
