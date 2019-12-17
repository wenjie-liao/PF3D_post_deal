! FILE ZG. File type = binary sequential.
! 2018-9-26 VERSION V-1.0
! �ô������ڶ�ȡ�ļ�ZG
! ���а���MXPFL��������ܵȼ���
! ZyyyDxxx��ZyyyFxxx��ZyyyHxxx �ļ��з��������record length
! NOTE: PF3D�Ľ���ļ��������Intel visual Fortran(IVF)���б�̶�ȡ����Ϊ���а�����Form='binary'�ĸ�ʽ
! ��ϸ�����PF3D�����ȡ�ļ�7.1��Frame Element �C Detailed Component Results.
program zg_read
    implicit none
    
    integer*2 :: MXPFL              ! max. number of performance levels for which capacities have been specified.
    integer*2 :: group_recl(:,:)    ! ZyyyDxxx��ZyyyFxxx��ZyyyHxxx �ļ��з��������record length
    integer*4 :: num_group          ! ����
    
    integer*4 :: i,j,k,m
    
    allocatable group_recl          ! ����group_reclΪ�ɱ�����
    
    write(*,*)'please input element group number'   ! ��������
    read(*,*)num_group                              ! ��ȡ����
    
    allocate (group_recl(num_group,6))              ! ȷ�������С������6����ZyyyDxxx����������+������������ZyyyFxxx����������+������������ZyyyHxxx����������+��������������
    
    open(11, file='ZG',form='binary',access='sequential')   ! ��ZG�ļ�����ʽΪ binary��sequential��˳���ȡ��
        read(11) MXPFL                                      ! ��ȡ���ּܷ�
        do i=1,num_group
            read(11) group_recl(i,:)                        ! ��ȡ����Ԫ��record length����ZyyyDxxx�����ڣ�record length=0
        end do
    close(11)
    
    open(22,file='ZG_out.txt',status='replace')                              ! ��ZG_out�ļ������record length���
        write(22,'(A6)') 'MXPFL'
        write(22,'(I6)') MXPFL
        write(22,'(6A6)')'ZD1','ZD2','ZF1','ZF2','ZH1','ZH2'
        do i=1,num_group
            write(22,'(6I6)') group_recl(i,:)
        end do
    close(22)       
        
    deallocate (group_recl)     ! �����̬����
    
end program zg_read