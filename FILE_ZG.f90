! FILE ZG. File type = binary sequential.
! 2018-9-26 VERSION V-1.0
! 该代码用于读取文件ZG
! 其中包含MXPFL（最大性能等级）
! ZyyyDxxx，ZyyyFxxx，ZyyyHxxx 文件中分析结果的record length
! NOTE: PF3D的结果文件建议采用Intel visual Fortran(IVF)进行编程读取，因为其中包含有Form='binary'的格式
! 详细解读见PF3D结果读取文件7.1节Frame Element – Detailed Component Results.
program zg_read
    implicit none
    
    integer*2 :: MXPFL              ! max. number of performance levels for which capacities have been specified.
    integer*2 :: group_recl(:,:)    ! ZyyyDxxx，ZyyyFxxx，ZyyyHxxx 文件中分析结果的record length
    integer*4 :: num_group          ! 组数
    
    integer*4 :: i,j,k,m
    
    allocatable group_recl          ! 定义group_recl为可变数组
    
    write(*,*)'please input element group number'   ! 输入组数
    read(*,*)num_group                              ! 读取组数
    
    allocate (group_recl(num_group,6))              ! 确定数组大小，其中6代表ZyyyDxxx（静力分析+动力分析），ZyyyFxxx（静力分析+动力分析），ZyyyHxxx（静力分析+动力分析）数量
    
    open(11, file='ZG',form='binary',access='sequential')   ! 打开ZG文件，格式为 binary，sequential（顺序读取）
        read(11) MXPFL                                      ! 读取性能分级
        do i=1,num_group
            read(11) group_recl(i,:)                        ! 读取各单元组record length，若ZyyyDxxx不存在，record length=0
        end do
    close(11)
    
    open(22,file='ZG_out.txt',status='replace')                              ! 打开ZG_out文件，输出record length结果
        write(22,'(A6)') 'MXPFL'
        write(22,'(I6)') MXPFL
        write(22,'(6A6)')'ZD1','ZD2','ZF1','ZF2','ZH1','ZH2'
        do i=1,num_group
            write(22,'(6I6)') group_recl(i,:)
        end do
    close(22)       
        
    deallocate (group_recl)     ! 解除动态数组
    
end program zg_read