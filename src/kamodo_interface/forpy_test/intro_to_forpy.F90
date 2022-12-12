program intro_to_forpy
  use forpy_mod
  implicit none

  integer :: ierror
  type(list) :: my_list

  ierror = forpy_initialize()
  ierror = list_create(my_list)

  ierror = my_list%append(19)
  ierror = my_list%append("Hello world!")
  ierror = my_list%append(3.14d0)
  ierror = print_py(my_list)

  call my_list%destroy
  call forpy_finalize

end program