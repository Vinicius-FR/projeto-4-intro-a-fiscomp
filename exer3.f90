program exer3
implicit none

real(8) :: lambda, dt, t !parâmetros do decaimento
integer :: i, iter, N_0, N

open(1, file = 'decai_in', status = 'old') !lendo os parâmetros a partir de um arquivo genérico

read(1,*) t
read(1,*) N_0
read(1,*) dt
read(1,*) lambda

close(1)

iter = int(t / dt)

open(2, file = 'decai_out', status = 'replace')

N = N_0

write(2,*) 0.0d0, N

do i = 1, iter
    N = N_0 * (1 - lambda * dt) !método da derivada

    N_0 = N

    write(2,*) i * dt, N
end do

close(2)

end program exer3