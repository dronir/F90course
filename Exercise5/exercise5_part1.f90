! Matrix inversion
program Exercise5_part1
    real :: Matrix(3,3)
    Matrix(1,:) = (/10, 0,  1/)
    Matrix(2,:) = (/ 0, 2,  0/)
    Matrix(3,:) = (/ 0, 0, 10/)
    Matrix = inverse(Matrix)
    write(*,*) Matrix
    
contains
    function inverse(Matrix)
        ! Compute inverse by solving A*x_j = e_i for all rows
        real :: Matrix(:,:)
        real :: inverse(size(Matrix,1), size(Matrix,1)) ! Why didn't inverse(size(M)) work?
        integer :: N, row
        
        N = size(Matrix,1)
        do row = 1,N
            inverse(row,:) = rowsolve(Matrix,row)
        end do
    end function
    
    
    function rowsolve(Matrix,row)
        ! Solve A*x = e_i for given A and row with Gaussian elimination
        integer :: i, N, k, pivot, j, row
        real :: Matrix(:,:)
        real :: rowsolve(size(Matrix,1))
        real :: result(size(Matrix,1))
        real :: tempvec(size(Matrix,1))
        real tmp, s, c
        N = size(Matrix,1)
        result = 0.0
        result(row) = 1.0
        
        ! loop over columns
        do i = 1,N
            ! find the largest element in column i
            pivot = i
            s = Matrix(i,i)
            do k = i+1,N
                if (abs(Matrix(k,i)) > s) then 
                    s = abs(Matrix(k,i))
                    pivot = k
                end if
            end do

            ! greatest element on line m; exchange lines i and m
            tempvec = Matrix(i,:)
            Matrix(i,:) = Matrix(pivot,:)
            Matrix(pivot,:) = tempvec
             
            tmp = result(i)
            result(i) = result(pivot)
            result(pivot) = tmp

            ! eliminate variable i
            if (Matrix(i,i) == 0) exit
            do k = i+1,n
                c = Matrix(k,i)/Matrix(i,i)
                do j = i,n  
                    Matrix(k,j) = Matrix(k,j) - c*Matrix(i,j)
                end do
                result(k) = result(k) - c*result(i)
            end do
        end do

        ! back substitution
        do i = n,1,-1
            result(i) = result(i)/Matrix(i,i)
            do k = i-1,1,-1
                result(k) = result(k) - Matrix(k,i)*result(i)
            end do
        end do
        rowsolve = result
    end function
end program