module utils
    contains
        subroutine saveMaze(board, filename)
            IMPLICIT NONE
            LOGICAL, DIMENSION(:,:), INTENT(IN) :: board
            CHARACTER(len=*), INTENT(IN) :: filename
            INTEGER :: ios, i, j, rows, cols

            rows = size(board, 1)
            cols = size(board, 2)

            OPEN(UNIT=10, file=filename, status='unknown', action='write', iostat=ios)

            if (ios /= 0) then
                print *, 'Error opening file: ', ios
                stop
            end if
        
            do i = 1, rows
                do j = 1, cols
                    IF (board(i, j).eqv. .TRUE.) THEN
                        write(10, '(A)', advance='no') '#'
                    ELSE
                        write(10, '(A)', advance='no') ' '
                    END IF
                end do
                write(10, *)
            end do
            close(10)

            print *, 'Maze saved to file: ', trim(filename)
        end subroutine saveMaze
        INTEGER function getRandomInteger(min, max) result(value)
            INTEGER, INTENT(IN) :: min , max
            REAL :: randomVal

            call RANDOM_NUMBER(randomVal) ! 0 < randomVal < 1
            value = nint(randomVal * (max- min)) + min
        end function
end module

module maze_generators
    contains
        subroutine prim_maze_generate(board, rows, cols)
            use utils
            IMPLICIT NONE
            INTEGER, INTENT(IN) :: rows, cols
            LOGICAL, DIMENSION(rows, cols), INTENT(INOUT) :: board
            LOGICAL, DIMENSION(rows, cols) :: visited ! visited walls
            INTEGER, DIMENSION(4, 2) :: directions
            INTEGER, DIMENSION(2, rows*cols) :: walls
            INTEGER :: row, col, nWalls, dr, dc, nextRow, nextCol, i, j, idx

            directions = reshape([ &
            -1,  0, & ! UP
             1,  0, & ! DOWN
             0, -1, & ! LEFT
             0,  1], shape(directions)) ! RIGHT

            ! Initialize viisted
            do i=1, rows
                do j=1, cols
                    visited(i, j) = .False.
                end do
            end do

            ! Pick a starting point
            row = getRandomInteger(2, rows-1) 
            col = getRandomInteger(2, cols-1)
            board(row, col) = .FALSE.
            visited(row, col) = .TRUE.

            !  Walls arrays initialization
            nWalls = 0
            do i=1, 4
                dr = directions(i, 1)
                dc = directions(i, 2)
                nextRow = row + dr 
                nextCol = col + dc

                IF(isOnBoard(nextRow, rows) .and. isOnBoard(nextCol, cols)) THEN
                    nWalls = nWalls + 1
                    walls(:, nWalls) = [nextRow, nextCol]
                END IF
            end do

            ! Main Loop
            do while(nWalls > 0)

                ! Pick a random wall
                idx = getRandomInteger(1, nWalls)
                row = walls(1, idx)
                col = walls(2, idx)

                IF(checkNeighbours(board, row, col, rows, cols, directions)) THEN
                    board(row, col) = .false. ! add to a maze
                    visited(row, col) = .TRUE.
                
                    ! Neighbour cells
                    do i=1, 4
                        dr = directions(i, 1)
                        dc = directions(i, 2)
                        nextRow = row + dr 
                        nextCol = col + dc
                        IF(isOnBoard(nextRow, rows) .and. isOnBoard(nextCol, cols) &
                        .and.(board(nextRow, nextCol) .eqv. .true.) .and. (visited(nextRow, nextCol) .eqv. .FALSE.) ) THEN
                            nWalls = nWalls + 1
                            walls(:, nWalls) = [nextRow, nextCol]
                            visited(row, col) = .TRUE.
                        END IF
                    end do
                END IF

                ! Delete wall from arrays
                walls(:, idx) = walls(:, nWalls)
                nWalls = nWalls - 1 
            end do
        end subroutine prim_maze_generate
        LOGICAL function checkNeighbours(board, row, col, nRows, nCols, directions) result(canBeInMaze)
            IMPLICIT NONE
            LOGICAL, DIMENSION(:, :), INTENT(IN) :: board
            INTEGER, INTENT(IN) :: row, col, nRows, nCols
            INTEGER, DIMENSION(4, 2), INTENT(IN) :: directions
            INTEGER :: i, dr, dc, mazeCells, nextRow, nextCol

            mazeCells = 0
            canBeInMaze = .true.
            do i=1, 4
                dr = directions(i, 1)
                dc = directions(i, 2)
                nextRow = row + dr 
                nextCol = col + dc
                IF(isOnBoard(nextRow, nRows) .and. isOnBoard(nextCol, nCols) &
                .and. board(nextRow, nextCol).eqv. .false.) THEN
                    mazeCells = mazeCells + 1
                    IF(mazeCells > 1) THEN
                        canBeInMaze = .false.
                        exit
                    END IF
                END IF
                
            end do
        end function checkNeighbours
        LOGICAL function isOnBoard(value, maxValue)
            INTEGER, INTENT(IN) :: value, maxValue
            IF(value > 0 .and. value < maxValue+1) THEN
                isOnBoard = .true.
            ELSE
                isOnBoard = .false.
            END IF
        end function isOnBoard
end module maze_generators

PROGRAM projekt
    use maze_generators
    use utils
    IMPLICIT NONE
    INTEGER :: nRows, nCols
    LOGICAL, ALLOCATABLE, DIMENSION(:, :) :: board ! TRUE -> wall
    INTEGER :: i, j, stat
    ! Arguments
    INTEGER :: num_args
    CHARACTER(len=100) :: arg

    num_args = command_argument_count()

    if (num_args /= 2) then
        print *, "2 arguments are needed: [rows_number] [cols_number]"
        stop
    end if

    call get_command_argument(1, arg)
    read(arg, *, iostat=stat) nRows
    if (stat /= 0) then
        print *, "Error converting argument to integer:", trim(arg)
        stop
    end if

    call get_command_argument(2, arg)
    read(arg, *, iostat=stat) nCols
    if (stat /= 0) then
        print *, "Error converting argument to integer:", trim(arg)
        stop
    end if

    allocate(board(nRows, nCols), stat=stat)
    IF (stat /= 0) then
        print *, "Memory allocation error"
        stop
    end IF

    ! Initialize maze
    do i = 1, nRows
        do j = 1, nCols
            board(i, j) = .TRUE.
        end do
    end do

    WRITE(*, '(A21, I3, A9, I3, A9)') "Generating maze with ", nRows, " rows and ", nCols, " columns."

    call prim_maze_generate(board, nRows, nCols)

    call saveMaze(board, "maze.txt")

    deallocate(board)
    stop
END PROGRAM