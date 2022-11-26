program exer2
implicit none

real(8) :: lambda, dt, t_m !parâmetros do decaimento e tempo de vida médio
integer, dimension(:), allocatable :: matriz_N !matriz que armazena os estados dos átomos
real(8), dimension(:), allocatable :: t_vidas !matriz que conta o tempo de vida de cada átomo
integer :: i, j, iter, N_0, N
real(8), parameter :: t = 10.0d0

print*, 'Insira o número inicial de átomos:'
read(*,*) N_0
print*, 'Insira o intervalo de tempo delta t:'
read(*,*) dt
print*, 'Insira a constante de decaimento:'
read(*,*) lambda

iter = int(t / dt)

allocate(matriz_N(N_0), t_vidas(N_0)) !alocando o número de átomos no tamanho das matrizes

do j = 1, N_0
    matriz_N(j) = 1 !inicialmente, todos os átomos estão vivos
    t_vidas(j) = 0.0d0 !e começa a contagem em t=0
end do

do i = 0, iter
    N = 0
    do j = 1, N_0

        if (matriz_N(j) == 1) then !se o átomo ainda estiver "vivo", veremos se vai decair

            if (rand() < lambda * dt .and. i /= 0) then !se a condição de decaimento for satisfeita aleatoriamente...
                matriz_N(j) = 0 !átomo j decai
            end if

        end if

        N = N + matriz_N(j) !contagem dos átomos vivos

        if (matriz_N(j) == 1) then

            t_vidas(j) = t_vidas(j) + dt !se o átomo ainda estiver vivo, acrescentar dt ao t_vida
            
        end if

    end do

end do

close(1)

do j = 1, N_0
    t_m = t_m + t_vidas(j)
end do

t_m = t_m / N_0 !tempo de vida médio

print*, t_m, (1/lambda) !comparação com o resultado teórico extráido da integral fornecida

end program exer2