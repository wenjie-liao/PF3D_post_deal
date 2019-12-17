! FiLE ZyyyDxxx File type = binary direct access.
! 2018-9-30 VERSiON V-1.0
! 该代码用于读取文件ZyyyDxxx,This can be used for drawing the deflected shapes of Frame elements.
! NOTE: PF3D的结果文件建议采用intel visual Fortran(iVF)进行编程读取，因为其中包含有Form='binary'的格式
! 详细解读见PF3D结果读取文件6.6 Frame Element Bending Deformations
! Record length :
!If N = number of basic components in the compound component,
!Record length = 16(N+1) bytes (4(N+1) real*4).   
    
program ele_defor_read
    implicit none
!************************************************
!定义变量
!************************************************
    integer*4 :: i,j,k,m,count
    integer*4,allocatable :: NELEM(:)
    integer*4,allocatable :: Rec_Len(:)
    integer*4,allocatable :: N(:)
    real*4,allocatable :: N_trans_2(:,:,:)          !2轴变形
    real*4,allocatable :: N_trans_3(:,:,:)          !3轴变形
    real*4,allocatable :: N_rotat_2(:,:,:)          !2轴弯曲变形
    real*4,allocatable :: N_rotat_3(:,:,:)          !2轴弯曲变形

    real*4,allocatable :: max_ele_T2(:,:)
    real*4,allocatable :: min_ele_T2(:,:)
    real*4,allocatable :: max_ele_T3(:,:)
    real*4,allocatable :: min_ele_T3(:,:)
    real*4,allocatable :: max_ele_R2(:,:)
    real*4,allocatable :: min_ele_R2(:,:)
    real*4,allocatable :: max_ele_R3(:,:)
    real*4,allocatable :: min_ele_R3(:,:)
    real*4,allocatable :: max_I_R(:)
    real*4,allocatable :: max_J_R(:)
    real*4,allocatable :: mid_R(:,:)
    real*4,allocatable :: max_mid_R(:)
    real*4,allocatable :: temp_maxI(:,:)
    real*4,allocatable :: temp_maxJ(:,:)
    
    character*50,allocatable :: ZxD2_raw(:)
    character*50,allocatable :: ZxD2_new(:)
    character*50 :: no_G,no_A
    
    integer*4,parameter :: N_Egroup=15              !group总数
    integer*4 :: read_Egroup                        !需要读取的那个group的编号
    integer*4 :: NTIM                               !总的存储的计算步数
    integer*4 :: read_NTIM                               !总的存储的计算步数
    
!************************************************
!输入文件信息
!************************************************
    
    write(*,*)'请输入group的编号'
    read(*,*)read_Egroup
        k=read_Egroup
        allocate (NELEM(N_Egroup))
        allocate (Rec_Len(N_Egroup))
        allocate (N(N_Egroup))
        allocate (ZxD2_raw(N_Egroup))
        allocate (ZxD2_new(N_Egroup))
    write(*,*)'请输入分析编号'
    read(*,*)no_A
    write(*,*)'请输入总输出计算步数'
    read(*,*)NTIM
    !NTIM=read_NTIM/5
    write(*,*)'请输入单元总数量'
    read(*,*)NELEM(k)
    write(*,*)'请输入基本单元数量N'
    read(*,*)N(k)
    
!************************************************
!定义数组大小
!************************************************
        
    allocate (N_trans_2(NTIM,NELEM(K),(N(k)+1)))
    allocate (N_trans_3(NTIM,NELEM(K),(N(k)+1)))
    allocate (N_rotat_2(NTIM,NELEM(K),(N(k)+1)))
    allocate (N_rotat_3(NTIM,NELEM(K),(N(k)+1)))
    allocate (max_ele_T2(NELEM(K),(N(k)+1)))
    allocate (min_ele_T2(NELEM(K),(N(k)+1)))
    allocate (max_ele_T3(NELEM(K),(N(k)+1)))
    allocate (min_ele_T3(NELEM(K),(N(k)+1)))
    allocate (max_ele_R2(NELEM(K),(N(k)+1)))
    allocate (min_ele_R2(NELEM(K),(N(k)+1)))
    allocate (max_ele_R3(NELEM(K),(N(k)+1)))
    allocate (min_ele_R3(NELEM(K),(N(k)+1)))
    allocate (mid_R(NTIM,NELEM(K)))
    allocate (temp_maxI(NTIM,NELEM(K)))
    allocate (temp_maxJ(NTIM,NELEM(K)))       
    allocate (max_I_R(NELEM(K)))
    allocate (max_J_R(NELEM(K)))
    allocate (max_mid_R(NELEM(K)))

!************************************************
!读取文件内容
!************************************************
        
        write(no_G,*)k
        ZxD2_raw(k)='Z00'//trim(adjustl(no_G))//'D00'//trim(adjustl(no_A))
        ZxD2_new(k)='new_Z00'//trim(adjustl(no_G))//'D00'//trim(adjustl(no_A))//'.txt'
        do i=1,NTIM
            !if(mod(i,5)==0)then
            open (11, file=ZxD2_raw(k), Access = 'direct', Form = 'binary', RecL=28)
            count=0
            do j=1,NELEM(k)
                count=count+4
                read (11, rec=count-3) N_trans_2(i,j,:)!一个单元N+1个节点的2方向位移
                read (11, rec=count-2) N_trans_3(i,j,:)
                read (11, rec=count-1) N_rotat_2(i,j,:)
                read (11, rec=count)   N_rotat_3(i,j,:)
                !write (*,'(2I6,I8)') i,j,count
            end do ! for j
            close (11)
            !end if
        end do ! for i

!************************************************
!整理数据，求最值
!************************************************
        
        do j=1,NELEM(k)
            max_J_R(j)=0.0
            max_I_R(j)=0.0
            max_mid_R(j)=0.0
            do i=1,NTIM
                do m=1,(N(k)/2)
                    if (abs(N_rotat_3(i,j,m))>max_I_R(j)) then
                        max_I_R(j)=abs(N_rotat_3(i,j,m))
                    end if
                    if (abs(N_rotat_3(i,j,m))>temp_maxI(i,j)) then
                        temp_maxI(i,j)=abs(N_rotat_3(i,j,m))
                    end if
                end do ! for m
                do m=(N(k)/2)+1,(N(k)+1)                       
                    if (abs(N_rotat_3(i,j,m))>max_J_R(j)) then
                        max_J_R(j)=abs(N_rotat_3(i,j,m))
                    end if
                    if (abs(N_rotat_3(i,j,m))>temp_maxJ(i,j)) then
                        temp_maxJ(i,j)=abs(N_rotat_3(i,j,m))
                    end if
                end do ! for m
                mid_R(i,j)=temp_maxI(i,j)-temp_maxJ(i,j)                !近似求解跨中转角
                    if (abs(mid_R(i,j))>max_mid_R(j)) then
                        max_mid_R(j)=abs(mid_R(i,j))
                    end if                    
            end do ! for i
        end do ! for j

!************************************************
!输出转角结果，杆构件I,J端及中部
!************************************************

    open (22, file=ZxD2_new(k), status='replace')
        write (22,'(A6,4A15)') 'NELEM','max_I_R','max_J_R','max_mid_R'
    do j=1,NELEM(k)
        write (22,'(I6,4F15.8)') j,max_I_R(j),max_J_R(j),max_mid_R(j)
    end do ! for j    
    close (22)
    
!************************************************
!释放数组
!************************************************
    
    deallocate (N_trans_2)
    deallocate (N_trans_3)
    deallocate (N_rotat_2)
    deallocate (N_rotat_3)
    deallocate (max_ele_T2)
    deallocate (min_ele_T2)
    deallocate (max_ele_T3)
    deallocate (min_ele_T3)
    deallocate (max_ele_R2)
    deallocate (min_ele_R2)
    deallocate (max_ele_R3)
    deallocate (min_ele_R3)    
    deallocate (ZxD2_raw)
    deallocate (ZxD2_new)
    deallocate (Rec_Len)
    deallocate (max_I_R)
    deallocate (max_J_R)
    deallocate (mid_R)
    deallocate (max_mid_R)
    deallocate (temp_maxI)
    deallocate (temp_maxJ)
    
end program