        Subroutine byte_swap_word(x, word_size)
        Implicit None
                                                       ! specify in byte
        INTEGER,         Intent(In)    :: word_size
        INTEGER(Kind=1), Intent(InOut) :: x(word_size)

        INTEGER         :: i_byte, other_byte
        INTEGER(Kind=1) :: t_byte

        Do i_byte = 1, word_size/2

         other_byte = 1 + word_size - i_byte
         t_byte        = x(i_byte)
         x(i_byte)     = x(other_byte)
         x(other_byte) = t_byte
        End Do

        End Subroutine byte_swap_word
