! FILE ECHO File type = binary sequential.
! 2018-9-26 VERSION V-1.0
! 该代码用于读取文件ECHO
program echo_read
    implicit none
    
    integer*4,allocatable :: dummy_read1(:,:)
    character(len=4),allocatable :: dummy_read2(:)
    integer*4,allocatable :: dummy_read3(:,:)
    
    integer*4,allocatable :: Elem_NO(:)
    real*4,allocatable :: Elem_len(:)
    character(len=50),allocatable :: DES_ele(:)
    character(len=50) :: temp
    real*4,allocatable :: Sec_B(:)
    real*4,allocatable :: Sec_H(:)
    real*4,allocatable :: Sec_W(:)
    real*4,allocatable :: Sec_F(:)
    real*4,allocatable :: Sec_tw(:)
    real*4,allocatable :: Sec_tf(:)
    real*4,allocatable :: Elem_A(:)
    real*4,allocatable :: Elem_I(:)
    real*4,allocatable :: Elem_EA(:)
    character*6,allocatable :: Mat(:)
    character*20,allocatable :: file_name(:)
    integer*4 :: i,j,k,m,n,p,q,r
    integer*4,parameter :: num_group=15
    integer*4,allocatable :: num_ele(:)
    integer*4,allocatable :: start_line(:)
    character(len=4),allocatable :: Mark(:)
    integer*4 :: Group_NO
    character(len=40) :: NO
    real(kind=4) :: E_concrete
    real(kind=4) :: E_steel
    integer*4,allocatable :: lable(:)
    
    allocate (num_ele(num_group))
    allocate (start_line(num_group))
    
    num_ele(1)=3833
    num_ele(2)=703
    num_ele(3)=823
    num_ele(4)=222
    num_ele(5)=287
    
    start_line(1)=102121
    start_line(2)=109800
    start_line(3)=111219
    start_line(4)=112878
    start_line(5)=113335    

    write(*,*)'请输入Group_NO' 
    read(*,*)Group_NO

    E_concrete=30000
    E_steel=200000
    
    allocate (dummy_read1(num_ele(Group_NO),2))
    allocate (dummy_read2(num_ele(Group_NO)))
    allocate (dummy_read3(num_ele(Group_NO),5))
    allocate (Elem_NO(num_ele(Group_NO)))
    allocate (Elem_len(num_ele(Group_NO)))
    allocate (DES_ele(num_ele(Group_NO)))
    allocate (Sec_B(num_ele(Group_NO)))
    allocate (Sec_H(num_ele(Group_NO)))
    allocate (Sec_W(num_ele(Group_NO)))
    allocate (Sec_F(num_ele(Group_NO)))
    allocate (Sec_tw(num_ele(Group_NO)))
    allocate (Sec_tf(num_ele(Group_NO)))
    allocate (Elem_A(num_ele(Group_NO)))
    allocate (Elem_I(num_ele(Group_NO)))
    allocate (Elem_EA(num_ele(Group_NO)))
    allocate (Mat(num_ele(Group_NO)))
    allocate (file_name(num_ele(Group_NO)))
    allocate (Mark(num_ele(Group_NO)))
    allocate (lable(40))
    
!***************** for concrete elements ***************    
    if(Group_NO==1.or.Group_NO==3)then    
!********************************************************
!读取数据
!********************************************************
    
    open(11, file='ECHO.txt')   ! 打开ECHO.txt文件
    do i=1,(start_line(Group_NO)-1)
        read(11,*)
    end do
    do i=1,num_ele(Group_NO)
        read(11,*) Elem_NO(i),dummy_read1(i,:),dummy_read2(i),dummy_read3(i,:),DES_ele(i)
        read(11,*) Elem_len(i)
        temp=DES_ele(i)
        lable(:)=0
        do j=1,35
            if(temp(j:j)=='-'.and.lable(1)==0)then
                m=j+1
                lable(1)=1
            end if
            if(temp(j:j)=='X'.and.lable(2)==0)then
                n=j-1
                p=j+1
                lable(2)=1
            end if
            if(temp(j:j)=='C'.and.lable(3)==0)then
                q=j-1
                r=j
                lable(3)=1
            end if
            if(temp(j:j)=='R')goto 100
        end do ! for j
        read(temp(m:n),*)Sec_B(i)
        read(temp(p:q),*)Sec_H(i)
        read(temp(r:r+2),*)Mat(i)
100        continue
        write(*,*)i
    end do ! for i
    close(11)

!********************************************************
!整理数据
!********************************************************

!材料Mat(i)
!跨度Elem_len(i)
!面积Elem_A(i),Elem_I(i),Elem_EA(i)
    do i=1,num_ele(Group_NO)
        Elem_A(i)=Sec_B(i)*Sec_H(i)
        Elem_I(i)=Sec_B(i)*Sec_H(i)*Sec_H(i)*Sec_H(i)/12
        Elem_EA(i)=E_concrete*Elem_A(i)
    end do ! for i
!********************************************************
!输出数据
!********************************************************

    write(NO,*)Group_NO
    file_name(Group_NO)='ECHO_out_'//trim(adjustl(NO))//'.txt'
    open(22,file=file_name(Group_NO),status='replace')
    write(22,'(A6,A10,3A14,2A10)')'Elem','Material','Span','A','I','B','H'
    do i=1,num_ele(Group_NO)
        write(22,'(I6,A10,3E14.6,2F10.2)')i,Mat(i),Elem_len(i)*1000,Elem_A(i),Elem_I(i),Sec_B(i),Sec_H(i)
    end do ! for i
    close(22)       
        
    deallocate (dummy_read1,dummy_read2,dummy_read3,Elem_NO,Elem_len,DES_ele,&
    &Sec_B,Sec_H,Sec_W,Sec_F,Sec_tw,Sec_tf,Elem_A,Elem_I,Elem_EA,Mat,start_line,Mark)     ! 解除动态数组
    
!***************** for concrete elements ***************

!****************** for steel elements *****************
    else
!********************************************************
!读取数据
!********************************************************
    
    open(11, file='ECHO.txt')   ! 打开ECHO.txt文件
    do i=1,(start_line(Group_NO)-1)
        read(11,*)
    end do
    do i=1,num_ele(Group_NO)
        read(11,*) Elem_NO(i),dummy_read1(i,:),dummy_read2(i),dummy_read3(i,:),DES_ele(i)
        read(11,*) Elem_len(i)
        temp=DES_ele(i)
        do j=1,35
            if(temp(j:j)=='H')then
                Mark(i)='H型'
                m=j+1
            else if(temp(j:j)=='B')then
                Mark(i)='方管'
                m=j+1 
            end if        
            if(temp(j:j)=='X')then
                n=j-8
                p=j-6
            end if
            if(temp(j:j)=='Q')then
                !q=j-1
                r=j
            end if
            if(temp(j:j+4)=='RIGID')goto 200
            !write(*,*)j
        end do ! for j
        read(temp(m:n),*)Sec_W(i)
        read(temp(p:(p+2)),*)Sec_F(i)
        read(temp((p+4):(p+5)),*)Sec_tw(i)
        read(temp((p+7):(p+8)),*)Sec_tf(i)
        read(temp(r:r+3),*)Mat(i)
200        continue
        write(*,*)i
    end do ! for i
    close(11)

!********************************************************
!整理数据
!********************************************************

!材料Mat(i)
!跨度Elem_len(i)
!面积Elem_A(i),Elem_I(i),Elem_EA(i)

    do i=1,num_ele(Group_NO)
        if(Mark(i)=='H型')then ! for H section
            Elem_A(i)=Sec_W(i)*Sec_F(i)-(Sec_F(i)-Sec_tw(i))*(Sec_W(i)-2*Sec_tf(i))
            Elem_I(i)=(Sec_F(i)*Sec_W(i)**3)/12-((Sec_F(i)-Sec_tw(i))*(Sec_W(i)-2*Sec_tf(i))**3)/12
            Elem_EA(i)=E_steel*Elem_A(i)
        else if(Mark(i)=='方管')then ! for Box section
            Elem_A(i)=Sec_W(i)*Sec_F(i)-(Sec_F(i)-2*Sec_tw(i))*(Sec_W(i)-2*Sec_tf(i))
            Elem_I(i)=(Sec_F(i)*Sec_W(i)**3)/12-((Sec_F(i)-2*Sec_tw(i))*(Sec_W(i)-2*Sec_tf(i))**3)/12
            Elem_EA(i)=E_steel*Elem_A(i)
        end if
    end do ! for i
    
!********************************************************
!输出数据
!********************************************************

    write(NO,*)Group_NO
    file_name(Group_NO)='ECHO_out_'//trim(adjustl(NO))//'.txt'
    open(22,file=file_name(Group_NO),status='replace')
    write(22,'(A6,A10,8A14)')'Elem','Material','Span','A','I','截面类型','翼缘宽度','翼缘厚度','截面高度','腹板厚度'
    do i=1,num_ele(Group_NO)
        write(22,'(I6,A10,3E14.6,A14,4E14.6)')i,Mat(i),Elem_len(i)*1000,Elem_A(i),Elem_I(i),Mark(i),Sec_F(i),Sec_tf(i),Sec_W(i),Sec_tw(i)
    end do ! for i
    close(22)       
        
    deallocate (dummy_read1,dummy_read2,dummy_read3,Elem_NO,Elem_len,DES_ele,&
    &Sec_B,Sec_H,Sec_W,Sec_F,Sec_tw,Sec_tf,Elem_A,Elem_I,Elem_EA,Mat,start_line,Mark)     ! 解除动态数组
        
!****************** for steel elements *****************       
    end if
end program echo_read