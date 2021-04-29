      Subroutine byte_swap(x, word_size, n_words)
        Implicit None
                                                    ! specify in bytes !
        INTEGER,         Intent(In)    :: word_size
        INTEGER(Kind=1), Intent(InOut) :: x(word_size,*)

        INTEGER,          Intent(In)    :: n_words

        INTEGER :: i_word

        Do i_word = 1, n_words

        Call byte_swap_word(x(1, i_word), word_size)

        End Do

        End Subroutine byte_swap
