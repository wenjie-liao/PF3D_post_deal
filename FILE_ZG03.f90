! FILE ZG03. File type = binary sequential.
! 2018-9-26 VERSION V-1.0
! 该代码用于读取文件ZG03
! 其中包含ZyyyDxxx，ZyyyFxxx，ZyyyHxxx 文件中分析结果的某项结果的具体位置
! NOTE: PF3D的结果文件建议采用Intel visual Fortran(IVF)进行编程读取，因为其中包含有Form='binary'的格式
! 详细解读见PF3D结果读取文件7.1节Frame Element – Detailed Component Results.
program zg03_read
    implicit none
    
    integer(kind=2) :: dummy_read_1(:)          ! 基本构件的数量，虚读
    integer(kind=2) :: NBC(:)                   ! 基本构件的数量，同上
    integer(kind=2) :: dummy_read_2(:,:)        ! 基本构件的长度类型，虚读
    real(kind=4) :: dummy_read_3(:,:)           ! 基本构件的长度，虚读
    integer(kind=2) :: dummy_read_4(:,:)        ! 基本构件类型，虚读
    real(kind=4) :: dummy_read_5(:,:)           ! 构件补充信息1，虚读
    real(kind=4) :: dummy_read_6(:,:)           ! 构件补充信息2，虚读
    integer(kind=2) :: location_static(:,:)     ! 静力分析结果位置
    integer(kind=2) :: location_dynamic(:,:)    ! 动力分析结果位置
!    character(len=64) :: temp
    integer(kind=2) :: i,j,k,m
    integer(kind=2) :: num_set                  ! set数量，未知，可从Perform 3D模型中读取,也可从ZG03文件中计数读取
    character(len=50) :: no
    character(len=50) :: set
    
    allocatable dummy_read_1,NBC,dummy_read_2,dummy_read_3,dummy_read_4,dummy_read_5,dummy_read_6,location_static,location_dynamic ! 设置为动态数组
    
    allocate (dummy_read_1(10000))              ! 设置数组大小，若在Perform 3D中找到总的set数量，便可直接设置为该set数量
    allocate (NBC(10000))
    allocate (dummy_read_2(10000,20))           ! 20的的大小为NBC大小，因为NBC为读取的大小，所以暂定为20
    allocate (dummy_read_3(10000,20))
    allocate (dummy_read_4(10000,20))
    allocate (dummy_read_5(10000,20))
    allocate (dummy_read_6(10000,20))
    allocate (location_static(10000,32))        ! 32为定值，代表基本的构件类型有32中，详见结果说明文档
    allocate (location_dynamic(10000,32))
    
    open(11, file='ZG03',form='binary',access='sequential') ! 打开文件ZG03
    num_set=0
    do i=1,10000                                            ! 开始读数循环，由于set数量未知，赞设定为10000
        read(11,end=21) dummy_read_1(i)                     ! 读取第i set的结果，end=21意思为如果读到文件末尾，则跳转至编号21的代码行
        NBC(i)=dummy_read_1(i)                              ! 得到i set的基本构件数量
        read(11,end=22) dummy_read_2(i,1:NBC(i))            ! 读取i set对应结果，在i set中，有NBC(i)个有效值，故读取范围为1：NBC(i)
        read(11,end=23) dummy_read_3(i,1:NBC(i))            ! 同上
        read(11,end=24) dummy_read_4(i,1:NBC(i))
        read(11,end=25) dummy_read_5(i,1:NBC(i))
        read(11,end=26) dummy_read_6(i,1:NBC(i))
        read(11,end=27) location_static(i,:)                ! 读取静力分析中的各基本构件的结果起始字节，starting location in record
        read(11,end=28) location_dynamic(i,:)               ! 读取动力分析中的各基本构件的结果起始字节，starting location in record
        num_set=num_set+1                                   ! 对set计数
    end do
    close(11)
    
21  write(*,*)'dummy_read_1 finished'                       ! 输出结果提示第一行为'dummy_read_1 finished'，代表结果正确
22  write(*,*)'dummy_read_2 finished'                       ! 输出结果提示第一行不为'dummy_read_1 finished'时，注意检查
23  write(*,*)'dummy_read_3 finished'
24  write(*,*)'dummy_read_4 finished'
25  write(*,*)'dummy_read_5 finished'
26  write(*,*)'dummy_read_6 finished'
27  write(*,*)'location_static finished'
28  write(*,*)'location_dynamic finished'
    pause
    
    open(22,file='ZG03_out.txt',status='replace')           ! 打开'ZG03_out.txt'
    write(22,'(A10,I8)')'num_set:',num_set                  ! 写set数量
    do i=1,num_set
        write(no,*)i        
        set='set'//trim(adjustl(no))
        write(22,'(2A10,I4)')set,'NBC:',NBC(i)                                ! 写set编号
!        do j=1,NBC(i)
!            write(22,'(2I10,E15.5,I10,2F15.5)')dummy_read_1(i),dummy_read_2(i,j),dummy_read_3(i,j),dummy_read_4(i,j),dummy_read_5(i,j),dummy_read_6(i,j)
!        end do ! for j
        write(22,'(A8,32I6)')'Static',location_static(i,:)              ! 写静力分析结果参数位置
        write(22,'(A8,32I6)')'Dynamic',location_dynamic(i,:)            ! 写动力分析结果参数位置
    end do ! for i
    close(22)
    
    deallocate (dummy_read_1,dummy_read_2,dummy_read_3,dummy_read_4,dummy_read_5,dummy_read_6,location_static,location_dynamic)
    
end program