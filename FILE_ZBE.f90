! FILE ZBE File type = binary sequential.
! 2018-9-26 VERSION V-1.0
! �ô������ڶ�ȡ�ļ�ZBE,����������Ϣ
! NOTE: PF3D�Ľ���ļ��������Intel visual Fortran(IVF)���б�̶�ȡ����Ϊ���а�����Form='binary'�ĸ�ʽ
! ��ϸ�����PF3D�����ȡ�ļ�2 Locating Nodes, Elements, Etc.
program zbe_read
    implicit none
    
    integer*4 :: NO_group                               ! No. of element groups (integer*4)
    integer*4,allocatable :: NO_ele(:)                  ! No. of elements in group (integer*4)
    integer*4,allocatable :: NNOD(:)                    ! No. of nodes per element (NNOD) (integer*4)
    character*40,allocatable :: DES_ele(:)              ! Element group description (character*40)
    integer*4,allocatable :: Node(:,:,:)                  ! Node number for Node    
    integer*4 :: i,j,k,m,num_group
    
    write(*,*)'������element group����'
    read(*,*)num_group
    
    allocate (NO_ele(num_group))
    allocate (NNOD(num_group))
    allocate (DES_ele(num_group))
    allocate (Node(num_group,10000,16))
    
    open(11, file='ZBE',form='binary',access='sequential')   ! ��ZBE�ļ�����ʽΪ binary��sequential��˳���ȡ��
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
    
    open(22,file='ZBE_out.txt',status='replace')                              ! ��ZG_out�ļ������record length���
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
        
    deallocate (NO_ele,NNOD,DES_ele,Node)     ! �����̬����
    
end program zbe_read