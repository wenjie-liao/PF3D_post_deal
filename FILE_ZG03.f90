! FILE ZG03. File type = binary sequential.
! 2018-9-26 VERSION V-1.0
! �ô������ڶ�ȡ�ļ�ZG03
! ���а���ZyyyDxxx��ZyyyFxxx��ZyyyHxxx �ļ��з��������ĳ�����ľ���λ��
! NOTE: PF3D�Ľ���ļ��������Intel visual Fortran(IVF)���б�̶�ȡ����Ϊ���а�����Form='binary'�ĸ�ʽ
! ��ϸ�����PF3D�����ȡ�ļ�7.1��Frame Element �C Detailed Component Results.
program zg03_read
    implicit none
    
    integer(kind=2) :: dummy_read_1(:)          ! �������������������
    integer(kind=2) :: NBC(:)                   ! ����������������ͬ��
    integer(kind=2) :: dummy_read_2(:,:)        ! ���������ĳ������ͣ����
    real(kind=4) :: dummy_read_3(:,:)           ! ���������ĳ��ȣ����
    integer(kind=2) :: dummy_read_4(:,:)        ! �����������ͣ����
    real(kind=4) :: dummy_read_5(:,:)           ! ����������Ϣ1�����
    real(kind=4) :: dummy_read_6(:,:)           ! ����������Ϣ2�����
    integer(kind=2) :: location_static(:,:)     ! �����������λ��
    integer(kind=2) :: location_dynamic(:,:)    ! �����������λ��
!    character(len=64) :: temp
    integer(kind=2) :: i,j,k,m
    integer(kind=2) :: num_set                  ! set������δ֪���ɴ�Perform 3Dģ���ж�ȡ,Ҳ�ɴ�ZG03�ļ��м�����ȡ
    character(len=50) :: no
    character(len=50) :: set
    
    allocatable dummy_read_1,NBC,dummy_read_2,dummy_read_3,dummy_read_4,dummy_read_5,dummy_read_6,location_static,location_dynamic ! ����Ϊ��̬����
    
    allocate (dummy_read_1(10000))              ! ���������С������Perform 3D���ҵ��ܵ�set���������ֱ������Ϊ��set����
    allocate (NBC(10000))
    allocate (dummy_read_2(10000,20))           ! 20�ĵĴ�СΪNBC��С����ΪNBCΪ��ȡ�Ĵ�С�������ݶ�Ϊ20
    allocate (dummy_read_3(10000,20))
    allocate (dummy_read_4(10000,20))
    allocate (dummy_read_5(10000,20))
    allocate (dummy_read_6(10000,20))
    allocate (location_static(10000,32))        ! 32Ϊ��ֵ����������Ĺ���������32�У�������˵���ĵ�
    allocate (location_dynamic(10000,32))
    
    open(11, file='ZG03',form='binary',access='sequential') ! ���ļ�ZG03
    num_set=0
    do i=1,10000                                            ! ��ʼ����ѭ��������set����δ֪�����趨Ϊ10000
        read(11,end=21) dummy_read_1(i)                     ! ��ȡ��i set�Ľ����end=21��˼Ϊ��������ļ�ĩβ������ת�����21�Ĵ�����
        NBC(i)=dummy_read_1(i)                              ! �õ�i set�Ļ�����������
        read(11,end=22) dummy_read_2(i,1:NBC(i))            ! ��ȡi set��Ӧ�������i set�У���NBC(i)����Чֵ���ʶ�ȡ��ΧΪ1��NBC(i)
        read(11,end=23) dummy_read_3(i,1:NBC(i))            ! ͬ��
        read(11,end=24) dummy_read_4(i,1:NBC(i))
        read(11,end=25) dummy_read_5(i,1:NBC(i))
        read(11,end=26) dummy_read_6(i,1:NBC(i))
        read(11,end=27) location_static(i,:)                ! ��ȡ���������еĸ����������Ľ����ʼ�ֽڣ�starting location in record
        read(11,end=28) location_dynamic(i,:)               ! ��ȡ���������еĸ����������Ľ����ʼ�ֽڣ�starting location in record
        num_set=num_set+1                                   ! ��set����
    end do
    close(11)
    
21  write(*,*)'dummy_read_1 finished'                       ! ��������ʾ��һ��Ϊ'dummy_read_1 finished'����������ȷ
22  write(*,*)'dummy_read_2 finished'                       ! ��������ʾ��һ�в�Ϊ'dummy_read_1 finished'ʱ��ע����
23  write(*,*)'dummy_read_3 finished'
24  write(*,*)'dummy_read_4 finished'
25  write(*,*)'dummy_read_5 finished'
26  write(*,*)'dummy_read_6 finished'
27  write(*,*)'location_static finished'
28  write(*,*)'location_dynamic finished'
    pause
    
    open(22,file='ZG03_out.txt',status='replace')           ! ��'ZG03_out.txt'
    write(22,'(A10,I8)')'num_set:',num_set                  ! дset����
    do i=1,num_set
        write(no,*)i        
        set='set'//trim(adjustl(no))
        write(22,'(2A10,I4)')set,'NBC:',NBC(i)                                ! дset���
!        do j=1,NBC(i)
!            write(22,'(2I10,E15.5,I10,2F15.5)')dummy_read_1(i),dummy_read_2(i,j),dummy_read_3(i,j),dummy_read_4(i,j),dummy_read_5(i,j),dummy_read_6(i,j)
!        end do ! for j
        write(22,'(A8,32I6)')'Static',location_static(i,:)              ! д���������������λ��
        write(22,'(A8,32I6)')'Dynamic',location_dynamic(i,:)            ! д���������������λ��
    end do ! for i
    close(22)
    
    deallocate (dummy_read_1,dummy_read_2,dummy_read_3,dummy_read_4,dummy_read_5,dummy_read_6,location_static,location_dynamic)
    
end program