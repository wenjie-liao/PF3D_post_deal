! FILE Zyyy.Hxxx. File type = binary direct access.
! 2018-9-26 VERSION V-1.0
! 该代码用于读取文件ZyyyHxxx
! 其中包含32种基本构件类型的结果，由于不同element group的构件类别不同，每个ZyyyHxxx文件中仅包含32种基本构件的部分结果
! 具体结果应根据文件ZG,ZG03来确定。其中ZG中包含record length的信息，而ZG03中包含record location的信息
! NOTE: PF3D的结果文件建议采用Intel visual Fortran(IVF)进行编程读取，因为其中包含有Form='binary'的格式
! 详细解读见PF3D结果读取文件7.1节Frame Element – Detailed Component Results.
    
    module lwjdata
    implicit none
    type :: LWJ_2_PMM_Hinge         ! 2 P-M2-M3Hinge, Steel Rotation or Curvature Type D/C ratios (2 x MXPFL bytes).
        integer*2 :: deform_DCratio_S     ! Deformation D/C ratios (MXPFL values, integer*2)        
    end type LWJ_2_PMM_Hinge  
        
    type :: LWJ_3_PMM_Hinge         ! 3 P-M2-M3 Hinge, Concrete Rotation or Curvature Type results (24 bytes).        
        real*4 :: bending_M2               !Bending moment about Axis 2
        real*4 :: bending_M3               !Bending moment about Axis 3
        real*4 :: axial_F                  !Axial force
        real*4 :: rot_curv_2               !Rotation or curvature about Axis 2
        real*4 :: rot_curv_3               !Rotation or curvature about Axis 3
    end type LWJ_3_PMM_Hinge
            
    type :: LWJ_4_PMM_Hinge         ! 4 P-M2-M3 Hinge, Concrete Rotation or Curvature Type D/C ratios (2 x MXPFL bytes).
        integer*2 :: deform_DCratio_C        !Deformation D/C ratios (MXPFL values, integer*2)
    end type LWJ_4_PMM_Hinge
    
    type :: LWJ_7_BC_Fiber          ! 7 Beam or Column Inelastic Fiber Segment results.(24 bytes).
        real*4 :: bending_M2            !Bending moment about Axis 2 at segment midpoint
        real*4 :: bending_M3            !Bending moment about Axis 3 at segment midpoint
        real*4 :: axial_F               !Axial force (real*4)
        real*4 :: bending_curv2         !Bending curvature about Axis 2 at segment midpoint
        real*4 :: bending_curv3         !Bending curvature about Axis 3 at segment midpoint
        real*4 :: axial_strain          !Axial strain
    end type LWJ_7_BC_Fiber
    
    type :: LWJ_8_BC_Fiber          ! 8 Beam or Column Inelastic Fiber Segment D/C ratios.(12 x MXPFL bytes).
        integer*2 :: steel_TS_DCratio       !Steel tension strain D/C ratios (MXPFL values, integer*2)
        integer*2 :: steel_CS_DCratio       !Steel compression strain D/C ratios (MXPFL values, integer*2)
        integer*2 :: concrete_TS_DCratio    !Concrete tension strain D/C ratios (MXPFL values, integer*2)
        integer*2 :: concrete_DS_DCratio    !Concrete compression D/C ratios (MXPFL values, integer*2)
        integer*2 :: P_rot_DCratio          !Positive rotation D/C ratios (MXPFL values, integer*2)
        integer*2 :: N_rot_DCratio          !Negative rotation D/C ratios (MXPFL values, integer*2)
    end type LWJ_8_BC_Fiber
    
    type :: LWJ_9_M_Hinge           ! 9 Moment Hinge, Rotation or Curvature Type results (8 bytes).
        real*4 :: bending_M         !Bending moment (real*4)
        real*4 :: rot_curv          !Rotation or curvature (real*4)
    end type LWJ_9_M_Hinge
    
    type :: LWJ_10_M_Hinge          ! 10 Moment Hinge, Rotation or Curvature Type D/C ratios.(4 x MXPFL bytes).
        integer*2 :: P_rot_DCratio      !Positive rotation or curvature D/C ratios (MXPFL values,integer*2)
        integer*2 :: N_rot_DCratio      !Negative rotation or curvature D/C ratios (MXPFL values,integer*2)
    end type LWJ_10_M_Hinge
            
    type :: LWJ_22_VV_Strength      ! 22 V2-V3 Strength Section D/C ratios (4 x MXPFL bytes).
        integer*2 :: cur_strength_DCratio       !Current strength D/C ratios (MXPFL values, integer*2)
        integer*2 :: max_strength_DCratio       !Maximum strength D/C ratios (MXPFL values, integer*2)
    end type LWJ_22_VV_Strength
    
    type :: LWJ_24_SF_Strength      ! 24 Shear Force Strength Section D/C ratios.(4 x MXPFL bytes).
        integer*2 :: cur_strength_DCratio       !Current strength D/C ratios (MXPFL values, integer*2)
        integer*2 :: max_strength_DCratio       !Maximum strength D/C ratios (MXPFL values, integer*2)
    end type LWJ_24_SF_Strength
    
    type :: LWJ_25_AF_Strength      ! 25 Axial Force Strength Section D/C ratios.(6 x MXPFL bytes).
        integer*2 :: cur_strength_DCratio       !Current strength D/C ratios (MXPFL values, integer*2)
        integer*2 :: max_Pstrength_DCratio      !Maximum positive strength D/C ratios (MXPFL values, integer*2)
        integer*2 :: max_Nstrength_DCratio      !Maximum negative strength D/C ratios (MXPFL values, integer*2)
    end type LWJ_25_AF_Strength
    
    end module lwjdata

program zhread
    use lwjdata
    implicit none
! 变量内容以及类型需要根据ZG03结果进行更改和调整,32类基本构件中编号2,3,4,7,8,9,10,22,24,25的构件类型
    
    type (LWJ_9_M_Hinge),allocatable :: BM(:,:,:)
    type (LWJ_9_M_Hinge),allocatable :: RC(:,:,:)
    type (LWJ_10_M_Hinge),allocatable :: PR_DC(:,:,:)
    type (LWJ_10_M_Hinge),allocatable :: NR_DC(:,:,:)
    type (LWJ_24_SF_Strength),allocatable :: CS_DC(:,:,:)
    type (LWJ_24_SF_Strength),allocatable :: MS_DC(:,:,:)
    
    integer*2,allocatable :: temp(:,:,:)
    integer*4 :: read_Egroup
    integer*4 :: Ntime
    integer*4 :: Nele
    integer*4 :: i,j,k,count

    character (len=50),allocatable :: ZxH2_raw(:)
    character (len=50),allocatable :: ZxH2_new(:)
    character (len=50) :: no
      
    write(*,*)'please input element group number'
    read(*,*)read_Egroup    
    write(*,*)'please input amount of time step'
    read (*,*)Ntime
    write(*,*)'please input amount of element'
    read (*,*)Nele
    
    k=read_Egroup
    
    allocate (BM(Ntime,Nele,2))
    allocate (RC(Ntime,Nele,2))
    allocate (PR_DC(Ntime,Nele,8))
    allocate (NR_DC(Ntime,Nele,8))    
    allocate (CS_DC(Ntime,Nele,8))
    allocate (MS_DC(Ntime,Nele,8))
    allocate (temp(Ntime,Nele,8))
    allocate (ZxH2_raw(15))
    allocate (ZxH2_new(15))
    
    write(no,*)k
    ZxH2_raw(k)='Z00'//trim(adjustl(no))//'H002' 
    ZxH2_new(k)='ZH00'//trim(adjustl(no))//'.txt'
    
    open (22,file=ZxH2_new(k),status='replace')
    close (22)
    
    do i=1,Ntime
        open (11, file=ZxH2_raw(k), Access = 'direct', Form = 'binary',recl=24)
        count=i
        do j=1,Nele
            count=j+Nele*(i-1)
            read (11,rec=count) BM(i,j,1)%bending_M,RC(i,j,1)%rot_curv,BM(i,j,2)%bending_M,RC(i,j,2)%rot_curv,&
            &PR_DC(i,j,1:4)%P_rot_DCratio,NR_DC(i,j,1:4)%N_rot_DCratio,PR_DC(i,j,5:8)%P_rot_DCratio,NR_DC(i,j,5:8)%N_rot_DCratio,&
            &CS_DC(i,j,1:4)%cur_strength_DCratio,MS_DC(i,j,1:4)%max_strength_DCratio,CS_DC(i,j,5:8)%cur_strength_DCratio,MS_DC(i,j,5:8)%max_strength_DCratio,temp(i,j,:)
        end do ! for j
        close (11)
        
        open (22,file=ZxH2_new(k),position='append')
        do j=1,Nele
            write (22,'(2I6,4E20.10)') i,j, BM(i,j,1)%bending_M, RC(i,j,1)%rot_curv, BM(i,j,2)%bending_M, RC(i,j,2)%rot_curv
        end do
        close (22)
    end do ! for i
        
    end program zhread
