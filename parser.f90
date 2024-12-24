
module parser
    implicit none

contains

subroutine parse(input)
    character(len=*), intent(in) :: input  ! Declaración de tipo correcta para input
    integer :: cursor = 1
    character(len=:), allocatable :: lexeme  ! Lexema como variable de longitud dinámica

    ! Inicializar lexeme con valor vacío
    lexeme = ""

    do while (lexeme /= "EOF" .and. lexeme /= "ERROR")
        lexeme = nextsym(input, cursor)  ! Llamada a nextsym
        print *, "Lexema: ", lexeme
    end do
end subroutine parse

function nextsym(input, cursor) result(lexeme)
    character(len=*), intent(in) :: input
    integer, intent(inout) :: cursor
    character(len=:), allocatable :: lexeme

    

    if (cursor > len(input)) then
        allocate(character(len=3) :: lexeme)
        lexeme = "EOF"
        return
    end if

    
    
              if(cursor+1 <= len(input) .and. &
                     ( (input(cursor:cursor) >= "a" .and. input(cursor:cursor) <= "z")  .or. &
                    (input(cursor:cursor) >= "A" .and. input(cursor:cursor) <= "Z"))) then

              lexeme = input(cursor:cursor)
              cursor = cursor + 1

                do while(cursor <= len(input) .and. &
                    ( (input(cursor:cursor) >= "a" .and. input(cursor:cursor) <= "z")  .or. &
                    (input(cursor:cursor) >= "A" .and. input(cursor:cursor) <= "Z")))
                    lexeme = lexeme // input(cursor:cursor)
                    cursor = cursor + 1
                end do
                return
            end if

        

        if (cursor + 1 <= len(input) .and.  (input(cursor:cursor) >= "a" .and. input(cursor:cursor) <= "z") ) then
                lexeme = input(cursor:cursor)
                cursor = cursor + 1
                do while (cursor <= len(input) .and.  (input(cursor:cursor) >= "a" .and. input(cursor:cursor) <= "z")  )
                        lexeme = lexeme // input(cursor:cursor)
                        cursor = cursor +  1
                end do
                return
        end if
        

    if  (input(cursor:cursor) >= "0" .and. input(cursor:cursor) <= "9")  then
        lexeme = input(cursor:cursor)
        cursor = cursor + 1
        return
    end if


        if  (input(cursor:cursor) >= "A" .and. input(cursor:cursor) <= "G")  then
            lexeme = input(cursor:cursor)
            cursor = cursor + 1
            return
        end if
    

            if (cursor + 1 <= len(input) .and.  (input(cursor:cursor) >= "A" .and. input(cursor:cursor) <= "Z")  .and. &
                    (input(cursor + 1:cursor + 1) >= "A" .and. input(cursor + 1:cursor + 1) <= "Z")) then
                    lexeme = input(cursor:cursor)
                    lexeme = lexeme // input(cursor + 1: cursor + 1)
                    cursor = cursor + 2

                do while (cursor <= len(input) .and. input(cursor:cursor) >= "A" .and. input(cursor:cursor) <= "Z" .and. LEN(lexeme) < 2 )
                        lexeme = lexeme // input(cursor:cursor)
                        cursor = cursor + 1
                end do

                if(LEN(lexeme) < 2) then
                    print *, "error lexico en col ", cursor - LEN(lexeme), ', "'//lexeme//'"'
                    allocate(character(len=5) :: lexeme)
                    lexeme = "ERROR"
                    return

                else if(LEN(lexeme) > 2) then
                    print *, "error lexico en col ", cursor - LEN(lexeme), ', "'//lexeme//'" (demasiado largo)"'
                    allocate(character(len=5) :: lexeme)
                    lexeme = "ERROR"
                    return
                end if
                return
            end if
            

                if (cursor + 1 <= len(input) .and.  (input(cursor:cursor) >= "B" .and. input(cursor:cursor) <= "C")  ) then
                lexeme = input(cursor:cursor)
                cursor = cursor + 1
                
                do while (cursor <= len(input) .and. input(cursor:cursor) >= "B" .and. input(cursor:cursor) <= "C" .and. LEN(lexeme) < 3 )
                    lexeme = lexeme // input(cursor:cursor)
                    cursor = cursor + 1
                end do
                return
            end if
            

            if (cursor + 1 <= len(input) .and.  (input(cursor:cursor) >= "0" .and. input(cursor:cursor) <= "2")  ) then
                lexeme = input(cursor:cursor)
                cursor = cursor + 1
                do while (cursor <= len(input) .and. input(cursor:cursor) >= "0" .and. input(cursor:cursor) <= "2")
                    lexeme = lexeme // input(cursor:cursor)
                    cursor = cursor + 1
                end do
                return
            end if
            

        if (cursor + 1 <= len(input) .and.  (input(cursor:cursor) >= "2" .and. input(cursor:cursor) <= "3")  .and. &
                    (input(cursor + 1:cursor + 1) >= "2" .and. input(cursor + 1:cursor + 1) <= "3")) then
                    lexeme = input(cursor:cursor)
                    lexeme = lexeme // input(cursor + 1: cursor + 1)
                    cursor = cursor + 2

                do while (cursor <= len(input) .and. input(cursor:cursor) >= "2" .and. input(cursor:cursor) <= "3")
                        lexeme = lexeme // input(cursor:cursor)
                        cursor = cursor + 1
                end do

                if(LEN(lexeme) < 2) then
                    print *, "error lexico en col ", cursor - LEN(lexeme), ', "'//lexeme//'"'
                    allocate(character(len=5) :: lexeme)
                    lexeme = "ERROR"
                    return
                end if
                return
            end if
            

    print *, "error lexico en col ", cursor, ', "'//input(cursor:cursor)//'"'
    allocate(character(len=5) :: lexeme)
    lexeme = "ERROR"
    cursor = cursor + 1  ! Avanzar el cursor para continuar procesando
end function nextsym

end module parser
