program exer1
implicit none

real(8) :: lambda, dt !parâmetros do decaimento
integer, dimension(:), allocatable :: matriz_N !matriz que armazena os estados dos átomos
integer :: i, j, iter, N_0, N
real(8), parameter :: t = 10.0d0

print*, 'Insira o número inicial de átomos:'
read(*,*) N_0
print*, 'Insira o intervalo de tempo delta t:'
read(*,*) dt
print*, 'Insira a constante de decaimento:'
read(*,*) lambda

iter = int(t / dt)

allocate(matriz_N(N_0)) !alocando o número de átomos no tamanho da matriz

do j = 1, N_0
    matriz_N(j) = 1 !inicialmente, todos os átomos estão "vivos"
end do

open(1, file = 'decai_out', status = 'replace')

do i = 0, iter

    N = 0

    do j = 1, N_0 !passando por todos os átomos a cada loop de tempo

        if (matriz_N(j) == 1) then

            if (rand() < lambda * dt .and. i /= 0) then !se a condição de decaimento for satisfeita aleatoriamente...
                matriz_N(j) = 0 !átomo j decai
            end if
            
        end if

        N = N + matriz_N(j) !contando quantos átomos ainda não decaíram

    end do

    write(1,*) i * dt, N
end do

close(1)

end program exer1