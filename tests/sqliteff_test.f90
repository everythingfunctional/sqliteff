module sqliteff_test
    use iso_varying_string, only: VARYING_STRING
    use sqliteff, only: &
            SqliteDatabase_t, &
            sqliteff_close, &
            sqliteff_exec, &
            sqliteff_open, &
            SQLITE_OK
    use Vegetables_m, only: Result_t, TestItem_t, assertEquals, describe, it

    implicit none
    private

    public :: test_sqliteff
contains
    function test_sqliteff() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(2)

        individual_tests(1) = it( &
                "can open and close a database connection", checkOpenAndClose)
        individual_tests(2) = it( &
                "can execute a statement", checkExec)
        tests = describe("sqliteff", individual_tests)
    end function test_sqliteff

    function checkOpenAndClose() result(result_)
        type(Result_t) :: result_

        type(SqliteDatabase_t) :: connection
        integer :: status

        status = sqliteff_open(":memory:", connection)

        result_ = assertEquals(SQLITE_OK, status, "opened")

        if (result_%passed()) then
            status = sqliteff_close(connection)
            result_ = assertEquals(SQLITE_OK, status, "closed")
        end if
    end function checkOpenAndClose

    function checkExec() result(result_)
        type(Result_t) :: result_

        type(SqliteDatabase_t) :: connection
        type(VARYING_STRING) :: errmsg
        integer :: status

        status = sqliteff_open(":memory:", connection)

        result_ = assertEquals(SQLITE_OK, status, "opened")

        if (result_%passed()) then
            status = sqliteff_exec(connection, "CREATE TABLE example ( identifier INTEGER PRIMARY KEY ASC, dummy TEXT) ;", errmsg)
            result_ = assertEquals(SQLITE_OK, status, errmsg)
            status = sqliteff_close(connection)
        end if
    end function checkExec
end module sqliteff_test
