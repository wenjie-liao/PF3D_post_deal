! �����дʱ��20170321
! ���ܣ����ڶ�ȡ������·��ĵ���ԭʼ��¼
! Num_GMR  �������𶯵�������һ��Ϊ3���ֱ��Ƕ��������ϱ�������������𶯼�¼�����ø�
! Num_ULB  ���𶯼�¼��ʼ�У���ע���޸�
! Num_ULC  ���𶯼�¼��ֹ�У���ע���޸�
! Num_Col  ���𶯼�¼������һ��Ϊ8�����ø�
! dT       ���𶯼�¼�������ע���޸�
! �뽫3��ԭʼ���𶯼�¼�ļ�����Ϊ'0.EW.dat''0.SN.dat''0.UD.dat'
! ��ȡ�ĸ�ʽ��Num_Col�йأ�ע���޸�

program Main

implicit none 

integer(kind=4)                          :: i,j,k,m,n
integer(kind=4),parameter                :: Num_GMR=1                 ! ���𶯼�¼������
integer(kind=4),parameter                :: Num_ULB=5                ! useful line begin���𶯼�¼��ʼ��
integer(kind=4)                          :: Num_ULC            ! useful line close���𶯼�¼��ֹ��
integer(kind=4),parameter                :: Num_Col=5                 ! number of colomn ���𶯼�¼����
integer(kind=4)                          :: Num_Data                  ! ���𶯼�¼����
real(kind=8),parameter                   :: dT=0.01                  ! ���𶯼�¼���

real(kind=8),dimension(Num_GMR,Num_Col,(Num_ULC-Num_ULB+1))  :: Acce  ! ע�������¼����20000����Ҫ����
real(kind=8) :: nmax
character(len=50) :: EQname(Num_GMR)                                  ! ���𶯼�¼�ļ�����
character(len=50) :: EQname_R(Num_GMR)                                ! ���𶯼�¼����ļ�����

character(len=110)                       :: Buffer  

    EQname(1)='0.RSN.AT2';
    
    EQname_R(1)='1.RSN.txt';
    


!**********************************************************
!��ȡ����
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
        read(22,"(5E15.7)") (Acce(i,k,j), k=1, Num_Col)   !ע�������ʽ�����������й�
     end do
   
   close(22)
  end do

Num_Data=(Num_ULC-Num_ULB+1)*Num_Col
!**********************************************************
!��������
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