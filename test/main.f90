program runTests
    implicit none

    call run()
contains
    subroutine run()
        use sqliteff_test, only: &
            sqliteff_sqliteff => test_sqliteff
        use iso_varying_string
        use Vegetables_m, only: TestItem_t, testThat, runTests

        type(TestItem_t) :: tests
        type(TestItem_t) :: individual_tests(1)

        individual_tests(1) = sqliteff_sqliteff()
        tests = testThat(individual_tests)

        call runTests(tests)
    end subroutine run
end program runTests
