module sqliteff_test
    use iso_varying_string, only: VARYING_STRING
    use sqliteff, only: &
            SqliteDatabase_t, &
            SqliteStatement_t, &
            sqliteff_bind_double, &
            sqliteff_bind_int, &
            sqliteff_bind_text, &
            sqliteff_clear_bindings, &
            sqliteff_close, &
            sqliteff_column_count, &
            sqliteff_column_double, &
            sqliteff_column_int, &
            sqliteff_column_text, &
            sqliteff_exec, &
            sqliteff_finalize, &
            sqliteff_open, &
            sqliteff_prepare, &
            sqliteff_reset, &
            sqliteff_step, &
            SQLITE_DONE, &
            SQLITE_OK, &
            SQLITE_ROW
    use Vegetables_m, only: Result_t, TestItem_t, assertEquals, describe, it

    implicit none
    private

    public :: test_sqliteff
contains
    function test_sqliteff() result(tests)
        type(TestItem_t) :: tests

        type(TestItem_t) :: individual_tests(8)

        individual_tests(1) = it( &
                "can open and close a database connection", checkOpenAndClose)
        individual_tests(2) = it( &
                "can execute a statement", checkExec)
        individual_tests(3) = it( &
                "can manually prepare, step and finalize a statement", &
                checkManualExec)
        individual_tests(4) = it( &
                "can use bind to insert values", checkInsertWithBind)
        individual_tests(5) = it( &
                "can get data back out", checkExtract)
        individual_tests(6) = it( &
                "can reset a statement", checkReset)
        individual_tests(7) = it( &
                "can clear the bindings from a statement", checkClearBindings)
        individual_tests(8) = it( &
                "can tell how many columns in a result", checkColumnCount)
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

        status = sqliteff_exec( &
                connection, &
                "CREATE TABLE example ( identifier INTEGER PRIMARY KEY ASC, dummy TEXT) ;", &
                errmsg)
        result_ = assertEquals(SQLITE_OK, status, errmsg)
        status = sqliteff_close(connection)
    end function checkExec

    function checkManualExec() result(result_)
        type(Result_t) :: result_

        type(SqliteDatabase_t) :: connection
        type(SqliteStatement_t) :: statement
        type(VARYING_STRING) :: remaining
        integer :: status

        status = sqliteff_open(":memory:", connection)

        status = sqliteff_prepare( &
                connection, &
                "CREATE TABLE example ( identifier INTEGER PRIMARY KEY ASC, dummy TEXT) ;", &
                statement, &
                remaining)
        result_ = assertEquals(SQLITE_OK, status, "prepared")
        if (result_%passed()) then
            status = sqliteff_step(statement)
            result_ = assertEquals(SQLITE_DONE, status, "stepped")
            status = sqliteff_finalize(statement)
            status = sqliteff_close(connection)
        end if
    end function checkManualExec

    function checkInsertWithBind() result(result_)
        type(Result_t) :: result_

        type(SqliteDatabase_t) :: connection
        type(VARYING_STRING) :: errmsg
        type(VARYING_STRING) :: remaining
        type(SqliteStatement_t) :: statement
        integer :: status

        status = sqliteff_open(":memory:", connection)
        status = sqliteff_exec( &
                connection, &
                "CREATE TABLE example (the_integer INTEGER, the_double REAL, the_text TEXT);", &
                errmsg)
        status = sqliteff_prepare( &
                connection, &
                "INSERT INTO example (the_integer, the_double, the_text) VALUES (?, ?, ?);", &
                statement, &
                remaining)
        result_ = assertEquals(SQLITE_OK, status, "prepare")
        if (result_%passed()) then
            status = sqliteff_bind_int(statement, 1, 2)
            result_ = assertEquals(SQLITE_OK, status, "bind_int")
            if (result_%passed()) then
                status = sqliteff_bind_double(statement, 2, 3.0d0)
                result_ = assertEquals(SQLITE_OK, status, "bind_double")
                if (result_%passed()) then
                    status = sqliteff_bind_text(statement, 3, "something")
                    result_ = assertEquals(SQLITE_OK, status, "bind_text")
                    if (result_%passed()) then
                        status = sqliteff_step(statement)
                        result_ = assertEquals(SQLITE_DONE, status, "step")
                        status = sqliteff_finalize(statement)
                        status = sqliteff_close(connection)
                    end if
                end if
            end if
        end if
    end function checkInsertWithBind

    function checkExtract() result(result_)
        type(Result_t) :: result_

        type(SqliteDatabase_t) :: connection
        type(VARYING_STRING) :: errmsg
        type(VARYING_STRING) :: remaining
        type(SqliteStatement_t) :: statement
        integer :: status
        double precision :: the_double
        integer :: the_integer
        type(VARYING_STRING) :: the_text

        status = sqliteff_open(":memory:", connection)
        status = sqliteff_exec( &
                connection, &
                "CREATE TABLE example (the_integer INTEGER, the_double REAL, the_text TEXT);", &
                errmsg)
        status = sqliteff_exec( &
                connection, &
                'INSERT INTO example (the_integer, the_double, the_text) VALUES (2, 3.0, "Hello");', &
                errmsg)
        status = sqliteff_prepare( &
                connection, &
                "SELECT the_integer, the_double, the_text FROM example;", &
                statement, &
                remaining)
        status = sqliteff_step(statement)
        result_ = assertEquals(SQLITE_ROW, status, "step")
        if (result_%passed()) then
            the_integer = sqliteff_column_int(statement, 0)
            the_double = sqliteff_column_double(statement, 1)
            the_text = sqliteff_column_text(statement, 2)
            result_ = &
                    assertEquals(2, the_integer) &
                    .and.assertEquals(3.0d0, the_double) &
                    .and.assertEquals("Hello", the_text)
            status = sqliteff_finalize(statement)
            status = sqliteff_close(connection)
        end if
    end function checkExtract

    function checkReset() result(result_)
        type(Result_t) :: result_

        type(SqliteDatabase_t) :: connection
        type(VARYING_STRING) :: errmsg
        type(VARYING_STRING) :: remaining
        type(SqliteStatement_t) :: statement
        integer :: status

        status = sqliteff_open(":memory:", connection)
        status = sqliteff_exec( &
                connection, &
                "CREATE TABLE example (the_integer INTEGER, the_double REAL, the_text TEXT);", &
                errmsg)
        status = sqliteff_exec( &
                connection, &
                'INSERT INTO example (the_integer, the_double, the_text) VALUES (2, 3.0, "Hello");', &
                errmsg)
        status = sqliteff_prepare( &
                connection, &
                "SELECT the_integer, the_double, the_text FROM example;", &
                statement, &
                remaining)
        status = sqliteff_step(statement)
        result_ = assertEquals(SQLITE_ROW, status, "step")
        if (result_%passed()) then
            status = sqliteff_reset(statement)
            result_ = assertEquals(SQLITE_OK, status, "reset")
            status = sqliteff_finalize(statement)
            status = sqliteff_close(connection)
        end if
    end function checkReset

    function checkClearBindings() result(result_)
        type(Result_t) :: result_

        type(SqliteDatabase_t) :: connection
        type(VARYING_STRING) :: errmsg
        type(VARYING_STRING) :: remaining
        type(SqliteStatement_t) :: statement
        integer :: status

        status = sqliteff_open(":memory:", connection)
        status = sqliteff_exec( &
                connection, &
                "CREATE TABLE example (the_integer INTEGER, the_double REAL, the_text TEXT);", &
                errmsg)
        status = sqliteff_prepare( &
                connection, &
                "INSERT INTO example (the_integer, the_double, the_text) VALUES (?, ?, ?);", &
                statement, &
                remaining)
        result_ = assertEquals(SQLITE_OK, status, "prepare")
        if (result_%passed()) then
            status = sqliteff_bind_int(statement, 1, 2)
            result_ = assertEquals(SQLITE_OK, status, "bind_int")
            if (result_%passed()) then
                status = sqliteff_bind_double(statement, 2, 3.0d0)
                result_ = assertEquals(SQLITE_OK, status, "bind_double")
                if (result_%passed()) then
                    status = sqliteff_bind_text(statement, 3, "something")
                    result_ = assertEquals(SQLITE_OK, status, "bind_text")
                    if (result_%passed()) then
                        status = sqliteff_step(statement)
                        status = sqliteff_clear_bindings(statement)
                        result_ = assertEquals(SQLITE_OK, status, "clear_bindings")
                        status = sqliteff_finalize(statement)
                        status = sqliteff_close(connection)
                    end if
                end if
            end if
        end if
    end function checkClearBindings

    function checkColumnCount() result(result_)
        type(Result_t) :: result_

        type(SqliteDatabase_t) :: connection
        type(VARYING_STRING) :: errmsg
        type(VARYING_STRING) :: remaining
        type(SqliteStatement_t) :: statement
        integer :: status

        status = sqliteff_open(":memory:", connection)
        status = sqliteff_exec( &
                connection, &
                "CREATE TABLE example (the_integer INTEGER, the_double REAL, the_text TEXT);", &
                errmsg)
        status = sqliteff_exec( &
                connection, &
                'INSERT INTO example (the_integer, the_double, the_text) VALUES (2, 3.0, "Hello");', &
                errmsg)
        status = sqliteff_prepare( &
                connection, &
                "SELECT the_integer, the_double, the_text FROM example;", &
                statement, &
                remaining)
        status = sqliteff_step(statement)
        result_ = assertEquals(SQLITE_ROW, status, "step")
        if (result_%passed()) then
            result_ = assertEquals(3, sqliteff_column_count(statement))
            status = sqliteff_finalize(statement)
            status = sqliteff_close(connection)
        end if
    end function checkColumnCount
end module sqliteff_test
