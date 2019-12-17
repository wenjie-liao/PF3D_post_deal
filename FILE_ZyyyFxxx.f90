! FiLE ZyyyFxxx File type = binary direct access.
! 2018-9-30 VERSiON V-1.0
! �ô������ڶ�ȡ�ļ�ZyyyFxxx,This is for element end forces only
! NOTE: PF3D�Ľ���ļ��������intel visual Fortran(iVF)���б�̶�ȡ����Ϊ���а�����Form='binary'�ĸ�ʽ
! ��ϸ�����PF3D�����ȡ�ļ�6.5 Frame Element End Forces  
! Record length :
! 44 bytes for a static analysis (11 REAL*4).
! 68 bytes for a dynamic analysis (17 REAL*4).

module ZFdata
    implicit none

    type static_results                 !static results
        REAL*4 :: SBM_i_3               !major plane (3 axis) bending moments at i,j.
        REAL*4 :: SBM_j_3               
        REAL*4 :: SBM_i_2               !minor plane (2 axis) bending moments at i,j.
        REAL*4 :: SBM_j_2
        REAL*4 :: Sshear_i_2            !major plane (2 axis) shears at i,j.
        REAL*4 :: Sshear_j_2
        REAL*4 :: Sshear_i_3            !minor plane (3 axis) shears at i,j.
        REAL*4 :: Sshear_j_3
        REAL*4 :: SAF_i                 !axial forces at i,j.
        REAL*4 :: SAF_j
        REAL*4 :: STM                   !torsional moment.
        REAL*4 :: max_ele_SAF           !�������������������
        REAL*4 :: min_ele_SAF
    end type static_results
    
    type dynamic_results                !dynamic results
        real(kind=4) :: DBM_i_3               !major plane (3 axis) bending moments at i,j.
        real(kind=4) :: DBM_j_3
        real(kind=4):: DBM_i_2               !minor plane (2 axis) bending moments at i,j.
        real(kind=4) :: DBM_j_2
        real(kind=4) :: DAF                   !axial forces at i,j.
        real(kind=4) :: DTM                   !torsional moment.
    end type dynamic_results
    
end module ZFdata

program ele_force_read              !��ȡ��Ԫ���Ľ��
    use ZFdata
    implicit none
    
    real(kind=4),allocatable :: dummy_read(:,:,:)                       !�����������
    real(kind=4),allocatable :: Mi3(:,:)               !���𹤿������
    real(kind=4),allocatable :: Mj3(:,:)
    real(kind=4),allocatable :: Mi2(:,:)
    real(kind=4),allocatable :: Mj2(:,:)
    real(kind=4),allocatable :: AF(:,:)                !���𹤿�������
    real(kind=4),allocatable :: TM(:,:)                !���𹤿���Ť��
    
    real(kind=4),allocatable :: max_ele_Mi3(:)                           !�������������3�����
    real(kind=4),allocatable :: min_ele_Mi3(:)
    real(kind=4),allocatable :: max_ele_Mj3(:)
    real(kind=4),allocatable :: min_ele_Mj3(:)
    real(kind=4),allocatable :: max_ele_Mi2(:)                           !�������������3�����
    real(kind=4),allocatable :: min_ele_Mi2(:)
    real(kind=4),allocatable :: max_ele_Mj2(:)
    real(kind=4),allocatable :: min_ele_Mj2(:)
    real(kind=4),allocatable :: max_ele_DAF(:)                           !�������������������
    real(kind=4),allocatable :: min_ele_DAF(:)
    real(kind=4),allocatable :: max_mid_rot3(:)                           !����в�ת��
    real(kind=4),allocatable :: mid_rot3(:,:)                             !�в�ת��
    
    integer*4 :: i,j,k,count
    integer*4,allocatable :: NELEM(:)                           !��Ԫ����
   
    character*50,allocatable :: ZxF2_raw(:)
    character*50,allocatable :: ZxF2_new(:)
    character*50,allocatable :: ZxF2_maxnew(:)
    character*50 :: no_G,no_A
    
    integer*4 :: read_Egroup
    integer*4 :: NTiM
!************************************************
!�����ļ���Ϣ
!************************************************
    write(*,*)'�ó���������ȡZxxFxx�ļ��ĵ�Ԫ����'
    write(*,*)'������group�ı��'
    read(*,*)read_Egroup
        k=read_Egroup
    write(*,*)'������������'
    read(*,*)no_A
    write(*,*)'��������������㲽��'
    read(*,*)NTiM
    
    allocate (NELEM(100))
    
    write(*,*)'�����뵥Ԫ������'
    read(*,*)NELEM(k)
    
    allocate (ZxF2_raw(100))
    allocate (ZxF2_new(100))
    allocate (ZxF2_maxnew(100))
    
    allocate (dummy_read(NTiM,NELEM(k),11))
    allocate (Mi3(NTiM,NELEM(k)))
    allocate (Mj3(NTiM,NELEM(k)))
    allocate (Mi2(NTiM,NELEM(k)))
    allocate (Mj2(NTiM,NELEM(k)))
    allocate (AF(NTiM,NELEM(k)))
    allocate (TM(NTiM,NELEM(k)))
    allocate (mid_rot3(NTiM,NELEM(k)))
    
    allocate (max_ele_Mi3(NELEM(k)))
    allocate (min_ele_Mi3(NELEM(k)))
    allocate (max_ele_Mj3(NELEM(k)))
    allocate (min_ele_Mj3(NELEM(k)))
    allocate (max_ele_Mi2(NELEM(k)))
    allocate (min_ele_Mi2(NELEM(k)))
    allocate (max_ele_Mj2(NELEM(k)))
    allocate (min_ele_Mj2(NELEM(k)))   
    allocate (max_ele_DAF(NELEM(k)))
    allocate (min_ele_DAF(NELEM(k)))
    allocate (max_mid_rot3(NELEM(k)))
    
    write(no_G,*)k
    ZxF2_raw(k)='Z00'//trim(adjustl(no_G))//'F00'//trim(adjustl(no_A))
    ZxF2_new(k)='new_Z00'//trim(adjustl(no_G))//'F00'//trim(adjustl(no_A))//'.txt'
    ZxF2_maxnew(k)='new_Z00'//trim(adjustl(no_G))//'F00'//trim(adjustl(no_A))//'.txt'
    
!************************************************
!��ȡ�ļ���Ϣ
!************************************************ 
    
    open (11, file=ZxF2_raw(k), Access = 'direct', Form = 'binary', RecL=68 )
    count=0
    do i=1,NTiM
        do j=1,NELEM(k)
            count=count+1
            read (11, rec=count) dummy_read(i,j,1:11),Mi3(i,j),Mj3(i,j),Mi2(i,j),Mj2(i,j),AF(i,j),TM(i,j)
        end do ! for j
    end do! for i
    close (11)
    
    !open (33, file='temp.txt', status='replace')
    !do i=1,NTiM
    !    do j=1,NELEM(k)
    !        write (33,'(17E16.6)') dummy_read(i,j,9), dummy_read(i,j,10)!,Mi3(i,j),Mj3(i,j),Mi2(i,j),Mj2(i,j),AF(i,j),TM(i,j)
    !    end do
    !end do
    !close (33)
    
!************************************************
!��ȡ�����С��أ�������Ť��
!************************************************    
    do j=1,NELEM(k)
        max_ele_DAF(j)=0.0
        min_ele_DAF(j)=0.0
        do i=1,NTiM
            if (Mi3(i,j)>max_ele_Mi3(j)) then
                max_ele_Mi3(j)=Mi3(i,j)
            end if
            if (Mi3(i,j)<min_ele_Mi3(j)) then
                min_ele_Mi3(j)=Mi3(i,j)
            end if
            
            if (Mj3(i,j)>max_ele_Mj3(j)) then
                max_ele_Mj3(j)=Mj3(i,j)
            end if
            if (Mj3(i,j)<min_ele_Mj3(j)) then
                min_ele_Mj3(j)=Mj3(i,j)
            end if
            
            if (Mi2(i,j)>max_ele_Mi2(j)) then
                max_ele_Mi2(j)=Mi2(i,j)
            end if
            if (Mi2(i,j)<min_ele_Mi2(j)) then
                min_ele_Mi2(j)=Mi2(i,j)
            end if
            
            if (Mj2(i,j)>max_ele_Mj2(j)) then
                max_ele_Mj2(j)=Mj2(i,j)
            end if
            if (Mj2(i,j)<min_ele_Mj2(j)) then
                min_ele_Mj2(j)=Mj2(i,j)
            end if
            
            if (abs(dummy_read(i,j,9))>max_ele_DAF(j)) then
                max_ele_DAF(j)=abs(dummy_read(i,j,9))
            end if
            if (AF(i,j)<min_ele_DAF(j)) then
                min_ele_DAF(j)=AF(i,j)
            end if
            if (mid_rot3(i,j)>max_mid_rot3(j)) then
                max_mid_rot3(j)=mid_rot3(i,j)
            end if
            
        end do !for i
    end do !for j

!************************************************
!������ֵ
!************************************************    
    open (22, file=ZxF2_maxnew(k), status='replace')
        write (22,'(A6,A16)') 'NELEM','max_ele_DAF'!,'min_ele_DAF'
        !'max_ele_Mi3','max_ele_Mj3','max_mid_rot3','max_ele_Mj3','min_ele_Mj3',&
        !&'max_ele_Mi2','min_ele_Mi2','max_ele_Mj2','min_ele_Mj2','max_ele_DAF', 'min_ele_DAF'
    do j=1,NELEM(k)
        write (22,'(I6,E16.6)') j,max_ele_DAF(j)!,min_ele_DAF(j)
        !max_ele_Mi3(j),min_ele_Mi3(j),max_ele_Mj3(j),min_ele_Mj3(j),&
        !&max_ele_Mi2(j),min_ele_Mi2(j),max_ele_Mj2(j),min_ele_Mj2(j),max_ele_DAF(j), min_ele_DAF(j)
    end do
    close (22)
    
    deallocate (NELEM,dummy_read,Mi3,Mj3,Mi2,Mj2,AF,TM,&
    &max_ele_Mj3,min_ele_Mj3,max_ele_Mi2,min_ele_Mi2,max_ele_Mj2,min_ele_Mj2,max_ele_DAF,min_ele_DAF,ZxF2_raw,ZxF2_new)
    
end program