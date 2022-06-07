module sqliteff_test
    use iso_varying_string, only: VARYING_STRING, var_str
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
            sqliteff_column_type, &
            sqliteff_exec, &
            sqliteff_finalize, &
            sqliteff_last_insert_rowid, &
            sqliteff_open, &
            sqliteff_prepare, &
            sqliteff_reset, &
            sqliteff_step, &
            SQLITE_DONE, &
            SQLITE_OK, &
            SQLITE_ROW, &
            SQLITE_INTEGER, &
            SQLITE_FLOAT, &
            SQLITE3_TEXT, &
            SQLITE_BLOB, &
            SQLITE_NULL
    use veggies, only: result_t, test_item_t, assert_equals, describe, it

    implicit none
    private

    public :: test_sqliteff
contains
    function test_sqliteff() result(tests)
        type(test_item_t) :: tests

        type(test_item_t) :: individual_tests(10)

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
        individual_tests(9) = it( &
                "can get the id of the last inserted row", checkLastInsertRowid)
        individual_tests(10) = it( &
                "can check column types", checkColumnType)
        tests = describe("sqliteff", individual_tests)
    end function test_sqliteff

    function checkOpenAndClose() result(result_)
        type(Result_t) :: result_

        type(SqliteDatabase_t) :: connection
        integer :: status

        status = sqliteff_open(var_str(":memory:"), connection)

        result_ = assert_equals(SQLITE_OK, status, "opened")

        if (result_%passed()) then
            status = sqliteff_close(connection)
            result_ = assert_equals(SQLITE_OK, status, "closed")
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
                var_str("CREATE TABLE example (identifier INTEGER PRIMARY KEY ASC, dummy TEXT);"), &
                errmsg)
        result_ = assert_equals(SQLITE_OK, status, errmsg)
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
                var_str("CREATE TABLE example (identifier INTEGER PRIMARY KEY ASC, dummy TEXT);"), &
                statement, &
                remaining)
        result_ = assert_equals(SQLITE_OK, status, "prepared")
        if (result_%passed()) then
            status = sqliteff_step(statement)
            result_ = assert_equals(SQLITE_DONE, status, "stepped")
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
        result_ = assert_equals(SQLITE_OK, status, "prepare")
        if (result_%passed()) then
            status = sqliteff_bind_int(statement, 1, 2)
            result_ = assert_equals(SQLITE_OK, status, "bind_int")
            if (result_%passed()) then
                status = sqliteff_bind_double(statement, 2, 3.0d0)
                result_ = assert_equals(SQLITE_OK, status, "bind_double")
                if (result_%passed()) then
                    status = sqliteff_bind_text(statement, 3, var_str("something"))
                    result_ = assert_equals(SQLITE_OK, status, "bind_text")
                    if (result_%passed()) then
                        status = sqliteff_step(statement)
                        result_ = assert_equals(SQLITE_DONE, status, "step")
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
        result_ = assert_equals(SQLITE_ROW, status, "step")
        if (result_%passed()) then
            the_integer = sqliteff_column_int(statement, 0)
            the_double = sqliteff_column_double(statement, 1)
            the_text = sqliteff_column_text(statement, 2)
            result_ = &
                    assert_equals(2, the_integer) &
                    .and.assert_equals(3.0d0, the_double) &
                    .and.assert_equals("Hello", the_text)
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
        result_ = assert_equals(SQLITE_ROW, status, "step")
        if (result_%passed()) then
            status = sqliteff_reset(statement)
            result_ = assert_equals(SQLITE_OK, status, "reset")
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
        result_ = assert_equals(SQLITE_OK, status, "prepare")
        if (result_%passed()) then
            status = sqliteff_bind_int(statement, 1, 2)
            result_ = assert_equals(SQLITE_OK, status, "bind_int")
            if (result_%passed()) then
                status = sqliteff_bind_double(statement, 2, 3.0d0)
                result_ = assert_equals(SQLITE_OK, status, "bind_double")
                if (result_%passed()) then
                    status = sqliteff_bind_text(statement, 3, "something")
                    result_ = assert_equals(SQLITE_OK, status, "bind_text")
                    if (result_%passed()) then
                        status = sqliteff_step(statement)
                        status = sqliteff_clear_bindings(statement)
                        result_ = assert_equals(SQLITE_OK, status, "clear_bindings")
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
        result_ = assert_equals(SQLITE_ROW, status, "step")
        if (result_%passed()) then
            result_ = assert_equals(3, sqliteff_column_count(statement))
            status = sqliteff_finalize(statement)
            status = sqliteff_close(connection)
        end if
    end function checkColumnCount

    function checkLastInsertRowid() result(result_)
        type(Result_t) :: result_

        type(SqliteDatabase_t) :: connection
        type(VARYING_STRING) :: errmsg
        type(VARYING_STRING) :: remaining
        integer :: query_row_id
        integer :: row_id
        type(SqliteStatement_t) :: statement
        integer :: status

        status = sqliteff_open(":memory:", connection)

        status = sqliteff_exec( &
                connection, &
                "CREATE TABLE example (identifier INTEGER PRIMARY KEY ASC, dummy TEXT);", &
                errmsg)
        status = sqliteff_exec( &
                connection, &
                'INSERT INTO example (dummy) VALUES ("Hello");', &
                errmsg)
        row_id = sqliteff_last_insert_rowid(connection)
        status = sqliteff_prepare( &
                connection, &
                "SELECT identifier FROM example;", &
                statement, &
                remaining)
        status = sqliteff_step(statement)
        query_row_id = sqliteff_column_int(statement, 0)
        result_ = assert_equals(query_row_id, row_id)
        status = sqliteff_finalize(statement)
        status = sqliteff_close(connection)
    end function checkLastInsertRowid

    function checkColumnType() result(result_)
        type(Result_t) :: result_

        type(SqliteDatabase_t) :: connection
        type(VARYING_STRING) :: errmsg
        type(VARYING_STRING) :: remaining
        integer :: column_type_int, &
                   column_type_float, &
                   column_type_text, &
                   column_type_blob, &
                   column_type_null
        type(SqliteStatement_t) :: statement
        integer :: status

        status = sqliteff_open(":memory:", connection)

        status = sqliteff_exec( &
                connection, &
                "CREATE TABLE example (the_integer INTEGER, the_double REAL, the_text TEXT, the_blob BLOB, the_null REAL);", &
                errmsg)
        status = sqliteff_exec( &
                connection, &
                "INSERT INTO example (the_integer, the_double, the_text, the_blob, the_null) " &
                // "VALUES (1, 1.0, 'Hello', X'53514C697465', NULL);", &
                errmsg)

        status = sqliteff_prepare( &
                connection, &
                "SELECT the_integer, the_double, the_text, the_blob, the_null FROM example;", &
                statement, &
                remaining)
        status = sqliteff_step(statement)

        column_type_int = sqliteff_column_type(statement, 0)
        column_type_float = sqliteff_column_type(statement, 1)
        column_type_text = sqliteff_column_type(statement, 2)
        column_type_blob = sqliteff_column_type(statement, 3)
        column_type_null = sqliteff_column_type(statement, 4)

        result_ = &
                assert_equals(column_type_int, SQLITE_INTEGER) &
                .and. assert_equals(column_type_float, SQLITE_FLOAT) &
                .and. assert_equals(column_type_text, SQLITE3_TEXT) &
                .and. assert_equals(column_type_blob, SQLITE_BLOB) &
                .and. assert_equals(column_type_null, SQLITE_NULL)

        status = sqliteff_finalize(statement)
        status = sqliteff_close(connection)
    end function checkColumnType
end module sqliteff_test
