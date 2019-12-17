! FILE ZBE File type = binary sequential.
! 2018-9-26 VERSION V-1.0
! 该代码用于读取文件ZBE,包含构件信息
! NOTE: PF3D的结果文件建议采用Intel visual Fortran(IVF)进行编程读取，因为其中包含有Form='binary'的格式
! 详细解读见PF3D结果读取文件2 Locating Nodes, Elements, Etc.
program zbe_read
    implicit none
    
    integer*4 :: NO_group                               ! No. of element groups (integer*4)
    integer*4,allocatable :: NO_ele(:)                  ! No. of elements in group (integer*4)
    integer*4,allocatable :: NNOD(:)                    ! No. of nodes per element (NNOD) (integer*4)
    character*40,allocatable :: DES_ele(:)              ! Element group description (character*40)
    integer*4,allocatable :: Node(:,:,:)                  ! Node number for Node    
    integer*4 :: i,j,k,m,num_group
    
    write(*,*)'请输入element group总数'
    read(*,*)num_group
    
    allocate (NO_ele(num_group))
    allocate (NNOD(num_group))
    allocate (DES_ele(num_group))
    allocate (Node(num_group,10000,16))
    
    open(11, file='ZBE',form='binary',access='sequential')   ! 打开ZBE文件，格式为 binary，sequential（顺序读取）
    read(11) NO_group
    do i=1,num_group
        read(11) NO_ele(i)
        read(11) NNOD(i)
        read(11) DES_ele(i)
        do j=1,NO_ele(i)
            do k=1,NNOD(i)
                read(11) Node(i,j,k)
            end do ! for k
        end do ! for j
    end do ! for i
    close(11)
    
    open(22,file='ZBE_out.txt',status='replace')                              ! 打开ZG_out文件，输出record length结果
    do i=1,num_group
        write(22,'(A10,I6)') 'NO_group:',i
        write(22,'(A10,I6)') 'NO_ele:',NO_ele(i)
        write(22,'(A10,I6)') 'NNOD:',NNOD(i)
        write(22,'(A10,A40)') 'DES_ele:',DES_ele(i)
        do j=1,NO_ele(i)
            write(22,'(I6,20I6)')j,Node(i,j,1:NNOD(i))
        end do ! for j
    end do ! for i
    close(22)       
        
    deallocate (NO_ele,NNOD,DES_ele,Node)     ! 解除动态数组
    
end program zbe_read