module sqliteff
!   USE csqlite3
    use iso_c_binding, only: c_char, c_int, c_double, c_ptr
    use iso_varying_string, only: VARYING_STRING, assignment(=)

    implicit none
    private

    type, public :: SqliteDatabase_t
        private
        type(c_ptr) :: handle
    end type SqliteDatabase_t

    type, public :: SqliteStatement_t
        private
        type(c_ptr) :: handle
    end type SqliteStatement_t

    integer, parameter, public :: SQLITE_OK = 0
    integer, parameter, public :: SQLITE_ERROR = 1
    integer, parameter, public :: SQLITE_MISUSE = 21
    integer, parameter, public :: SQLITE_ROW = 100
    integer, parameter, public :: SQLITE_DONE = 101

    public :: &
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
            sqliteff_step
contains
    function sqliteff_bind_double(statement, col, val) result(status)
        type(SqliteStatement_t), intent(inout) :: statement
        integer, intent(in) :: col
        double precision, intent(in) :: val
        integer :: status

        interface
            function csqlite3_bind_double( &
                    handle, &
                    col, &
                    val) &
                    result(status) &
                    bind(C, name = "csqlite3_bind_double")
                import c_double, c_int, c_ptr
                type(c_ptr), intent(inout) :: handle
                integer(kind=c_int), value, intent(in) :: col
                real(kind=c_double), value, intent(in) :: val
                integer(kind=c_int) :: status
            end function csqlite3_bind_double
        end interface

        status = csqlite3_bind_double(statement%handle, col, val)
    end function sqliteff_bind_double

    function sqliteff_bind_int(statement, col, val) result(status)
        type(SqliteStatement_t), intent(inout) :: statement
        integer, intent(in) :: col
        integer, intent(in) :: val
        integer :: status

        interface
            function csqlite3_bind_int( &
                    handle, &
                    col, &
                    val) &
                    result(status) &
                    bind(C, name = "csqlite3_bind_int")
                import c_int, c_ptr
                type(c_ptr), intent(inout) :: handle
                integer(kind=c_int), value, intent(in) :: col
                integer(kind=c_int), value, intent(in) :: val
                integer(kind=c_int) :: status
            end function csqlite3_bind_int
        end interface

        status = csqlite3_bind_int(statement%handle, col, val)
    end function sqliteff_bind_int

    function sqliteff_bind_text(statement, col, val) result(status)
        type(SqliteStatement_t), intent(inout) :: statement
        integer, intent(in) :: col
        character(len=*), intent(in) :: val
        integer :: status

        interface
            function csqlite3_bind_text( &
                    handle, &
                    col, &
                    val, &
                    nByte) &
                    result(status) &
                    bind(C, name = "csqlite3_bind_text")
                import c_char, c_int, c_ptr
                type(c_ptr), intent(inout) :: handle
                integer(kind=c_int), value, intent(in) :: col
                character(len=1, kind=c_char), dimension(*), intent(in) :: val
                integer(kind=c_int), value, intent(in) :: nByte
                integer(kind=c_int) :: status
            end function csqlite3_bind_text
        end interface

        status = csqlite3_bind_text(statement%handle, col, fStringToC(val), len(val) + 1)
    end function sqliteff_bind_text

    function sqliteff_clear_bindings(statement) result(status)
        type(SqliteStatement_t), intent(inout) :: statement
        integer :: status

        interface
            function csqlite3_clear_bindings( &
                    handle) &
                    result(status) &
                    bind(C, name = "csqlite3_clear_bindings")
                import c_int, c_ptr
                type(c_ptr), intent(inout) :: handle
                integer(kind=c_int) :: status
            end function csqlite3_clear_bindings
        end interface

        status = csqlite3_clear_bindings(statement%handle)
    end function sqliteff_clear_bindings

    function sqliteff_close(connection) result(status)
        type(SqliteDatabase_t), intent(inout) :: connection
        integer :: status

        interface
            function csqlite3_close( &
                    handle) result(status) bind(C, name = "csqlite3_close")
                import c_int, c_ptr
                type(c_ptr), intent(inout) :: handle
                integer(kind=c_int) :: status
            end function csqlite3_close
        end interface

        status = csqlite3_close(connection%handle)
    end function sqliteff_close

    function sqliteff_column_count(statement) result(num_columns)
        type(SqliteStatement_t), intent(inout) :: statement
        integer :: num_columns

        interface
            function csqlite3_column_count( &
                    handle) &
                    result(num_columns) &
                    bind(C, name = "csqlite3_column_count")
                import c_int, c_ptr
                type(c_ptr), intent(inout) :: handle
                integer(kind=c_int) :: num_columns
            end function csqlite3_column_count
        end interface

        num_columns = csqlite3_column_count(statement%handle)
    end function sqliteff_column_count

    function sqliteff_column_double(statement, col) result(val)
        type(SqliteStatement_t), intent(inout) :: statement
        integer, intent(in) :: col
        double precision :: val

        interface
            function csqlite3_column_double( &
                    handle, &
                    col) &
                    result(val) &
                    bind(C, name = "csqlite3_column_double")
                import c_double, c_int, c_ptr
                type(c_ptr), intent(inout) :: handle
                integer(kind=c_int), value, intent(in) :: col
                real(kind=c_double) :: val
            end function csqlite3_column_double
        end interface

        val = csqlite3_column_double(statement%handle, col)
    end function sqliteff_column_double

    function sqliteff_column_int(statement, col) result(val)
        type(SqliteStatement_t), intent(inout) :: statement
        integer, intent(in) :: col
        integer :: val

        interface
            function csqlite3_column_int( &
                    handle, &
                    col) &
                    result(val) &
                    bind(C, name = "csqlite3_column_int")
                import c_int, c_ptr
                type(c_ptr), intent(inout) :: handle
                integer(kind=c_int), value, intent(in) :: col
                integer(kind=c_int) :: val
            end function csqlite3_column_int
        end interface

        val = csqlite3_column_int(statement%handle, col)
    end function sqliteff_column_int

    function sqliteff_column_text(statement, col) result(val)
        type(SqliteStatement_t), intent(inout) :: statement
        integer, intent(in) :: col
        type(VARYING_STRING) :: val

        interface
            subroutine csqlite3_column_text( &
                    handle, &
                    col, &
                    text, &
                    max_len) &
                    bind(C, name = "csqlite3_column_text")
                import c_char, c_int, c_ptr
                type(c_ptr), intent(inout) :: handle
                integer(kind=c_int), value, intent(in) :: col
                character(len=1, kind=c_char), dimension(*) :: text
                integer(kind=c_int), value, intent(in) :: max_len
            end subroutine csqlite3_column_text
        end interface

        integer, parameter :: MAX_STRING_LENGTH = 1000
        character(len=MAX_STRING_LENGTH, kind=c_char) :: text

        call csqlite3_column_text(statement%handle, col, text, MAX_STRING_LENGTH)
        val = cStringToF(text)
    end function sqliteff_column_text

    function sqliteff_exec(connection, command, errmsg) result(status)
        type(SqliteDatabase_t), intent(inout) :: connection
        character(len=*), intent(in) :: command
        type(VARYING_STRING), intent(out) :: errmsg
        integer :: status

        interface
            function csqlite3_exec( &
                    handle, &
                    command, &
                    errmsg, &
                    max_len) &
                    result(status) &
                    bind(C, name = "csqlite3_exec")
                import c_char, c_int, c_ptr
                type(c_ptr), intent(inout) :: handle
                character(len=1, kind=c_char), dimension(*), intent(in) :: command
                character(len=1, kind=c_char), dimension(*) :: errmsg
                integer(kind=c_int), value, intent(in) :: max_len
                integer(kind=c_int) :: status
            end function csqlite3_exec
        end interface

        integer, parameter :: MAX_MESSAGE_LENGTH = 1000
        character(len=MAX_MESSAGE_LENGTH, kind=c_char) :: message

        status = csqlite3_exec(connection%handle, fStringToC(command), message, MAX_MESSAGE_LENGTH)
        errmsg = cStringToF(message)
    end function sqliteff_exec

    function sqliteff_finalize(statement) result(status)
        type(SqliteStatement_t), intent(inout) :: statement
        integer :: status

        interface
            function csqlite3_finalize( &
                    statement) result(status) bind(C, name = "csqlite3_finalize")
                import c_int, c_ptr
                type(c_ptr), intent(inout) :: statement
                integer(kind=c_int) :: status
            end function csqlite3_finalize
        end interface

        status = csqlite3_finalize(statement%handle)
    end function sqliteff_finalize

    function sqliteff_open(filename, connection) result(status)
        character(len=*), intent(in) :: filename
        type(SqliteDatabase_t), intent(out) :: connection
        integer :: status

        interface
            function csqlite3_open( &
                    filename, &
                    handle) &
                    result(status) &
                    bind(C, name = "csqlite3_open")
                import c_char, c_int, c_ptr
                character(len=1, kind=c_char), dimension(*), intent(in) :: filename
                type(c_ptr), intent(out) :: handle
                integer(kind=c_int) :: status
            end function csqlite3_open
        end interface

        status = csqlite3_open(fStringToC(filename), connection%handle)
    end function sqliteff_open

    function sqliteff_prepare(connection, sql, statement, remaining) result(status)
        type(SqliteDatabase_t), intent(inout) :: connection
        character(len=*), intent(in) :: sql
        type(SqliteStatement_t), intent(out) :: statement
        type(VARYING_STRING), intent(out) :: remaining
        integer :: status

        interface
            function csqlite3_prepare( &
                    db, &
                    zSql, &
                    nByte, &
                    ppStmt, &
                    pzTail, &
                    max_len) &
                    result(status) &
                    bind(C, name = "csqlite3_prepare")
                import c_char, c_int, c_ptr
                type(c_ptr), intent(inout) :: db
                character(len=1, kind=c_char), dimension(*), intent(in) :: zSql
                integer(kind=c_int), value, intent(in) :: nByte
                type(c_ptr), intent(out) :: ppStmt
                character(len=1, kind=c_char), dimension(*) :: pzTail
                integer(kind=c_int), value, intent(in) :: max_len
                integer(kind=c_int) :: status
            end function csqlite3_prepare
        end interface

        integer, parameter :: MAX_REMAINING_LENGTH = 1000
        character(len=MAX_REMAINING_LENGTH, kind=c_char) :: pzTail

        status = csqlite3_prepare(connection%handle, fStringToC(sql), len(sql) + 1, statement%handle, pzTail, MAX_REMAINING_LENGTH)
        remaining = cStringToF(pzTail)
    end function sqliteff_prepare

    function sqliteff_reset(statement) result(status)
        type(SqliteStatement_t), intent(inout) :: statement
        integer :: status

        interface
            function csqlite3_reset( &
                    handle) result(status) bind(C, name = "csqlite3_reset")
                import c_int, c_ptr
                type(c_ptr), intent(inout) :: handle
                integer(kind=c_int) :: status
            end function csqlite3_reset
        end interface

        status = csqlite3_reset(statement%handle)
    end function sqliteff_reset

    function sqliteff_step(statement) result(status)
        type(SqliteStatement_t), intent(inout) :: statement
        integer :: status

        interface
            function csqlite3_step( &
                    statement) result(status) bind(C, name = "csqlite3_step")
                import c_int, c_ptr
                type(c_ptr), intent(inout) :: statement
                integer(kind=c_int) :: status
            end function csqlite3_step
        end interface

        status = csqlite3_step(statement%handle)
    end function sqliteff_step

    pure function fStringToC(f_string) result(c_string)
        character(len=*), intent(in) :: f_string
        character(len=len(f_string) + 1) :: c_string

        c_string = f_string // char(0)
    end function fStringToC

    pure function cStringToF(c_string) result(f_string)
        character(len=*), intent(in) :: c_string
        type(VARYING_STRING) :: f_string

        integer :: terminator_position

        terminator_position = index(c_string, char(0))
        if (terminator_position == 0) then
            f_string = c_string
        else
            f_string = c_string(1:terminator_position - 1)
        end if
    end function cStringToF
end module sqliteff
