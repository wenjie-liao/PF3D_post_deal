! 程序编写时间20170321
! 功能：用于读取地震局下发的地震动原始记录
! Num_GMR  处理地震动的数量：一般为3，分别是东西方向，南北方向与竖向地震动记录，不用改
! Num_ULB  地震动记录开始行：请注意修改
! Num_ULC  地震动记录终止行：请注意修改
! Num_Col  地震动记录列数：一般为8，不用改
! dT       地震动记录间隔：请注意修改
! 请将3条原始地震动记录文件名改为'0.EW.dat''0.SN.dat''0.UD.dat'
! 读取的格式与Num_Col有关，注意修改

program Main

implicit none 

integer(kind=4)                          :: i,j,k,m,n
integer(kind=4),parameter                :: Num_GMR=1                 ! 地震动记录的数量
integer(kind=4),parameter                :: Num_ULB=5                ! useful line begin地震动记录开始行
integer(kind=4)                          :: Num_ULC            ! useful line close地震动记录终止行
integer(kind=4),parameter                :: Num_Col=5                 ! number of colomn 地震动记录列数
integer(kind=4)                          :: Num_Data                  ! 地震动记录点数
real(kind=8),parameter                   :: dT=0.01                  ! 地震动记录间隔

real(kind=8),dimension(Num_GMR,Num_Col,(Num_ULC-Num_ULB+1))  :: Acce  ! 注意如果记录超过20000个需要更改
real(kind=8) :: nmax
character(len=50) :: EQname(Num_GMR)                                  ! 地震动记录文件名称
character(len=50) :: EQname_R(Num_GMR)                                ! 地震动记录结果文件名称

character(len=110)                       :: Buffer  

    EQname(1)='0.RSN.AT2';
    
    EQname_R(1)='1.RSN.txt';
    


!**********************************************************
!读取数据
!**********************************************************
  do i=1,Num_GMR
  real line
  Num_ULC=0
  open(22,file=EQName(i),status='old')
     do while (.true.)
     read(22,*,end=4000) line
          Num_ULC=Num_ULC+1
     end do
     rewind(22) 
     do n=1,Num_ULB-1
        read(22,'(A)') Buffer
     end do
!**********************************************************

     do j=1, Num_ULC-Num_ULB+1       
        read(22,"(5E15.7)") (Acce(i,k,j), k=1, Num_Col)   !注意读数格式与数据列数有关
     end do
   
   close(22)
  end do

Num_Data=(Num_ULC-Num_ULB+1)*Num_Col
!**********************************************************
!数据整理
!**********************************************************
  do i=1,Num_GMR
      nmax=0
      do j=1,Num_ULC-Num_ULB+1
        do k=1,Num_Col
          if nmax<Acce(i,k,j) then
          nmax=Acce(i,k,j)
        end do
      end do
  end do

  do i=1,Num_GMR
    
	m=0
    open(33,file=EQName_R(i),access='append')
    
       write Num_ULC
       do j=1, Num_ULC-Num_ULB+1
         do k=1,Num_Col
             write (33,"(E15.7)") Acce(i,k,j)/nmax
             m=m+1
         end do
       end do
     
	 close(33)
   end do

end program