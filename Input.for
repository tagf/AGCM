C     ****************
C     ****************
      SUBROUTINE INPUT(restfile,lun,restfile_ice,lun2)
C     ****************
C     ****************
C
C
C             READS INITIAL CONDITIONS
C
C
C************ BEGINNING OF COMMON ************
C
	include 'recom.fi'
 !     DIMENSION C(900)
 !     EQUIVALENCE (TAU,C(1))
C
C        NON STATE AND NON TEMPORARY VARIABLES IN MISC COMMON
C
      COMMON / MISC / PHIS(74,46),ISFTYP(74,46),TS(74,46)
     1  ,SD(74,46)
C
C        Q ARRAY - STATE VARIABLES
C
      COMMON / QARY / P(74,46),U(74,46,2),V(74,46,2),T(74,46,2)
     1  ,QW(74,46,2),GW(74,46),GT(74,46),SNOAMT(74,46)
C
C        MONTHLY SEA TEMPERATURES
C
      COMMON /SEA/ SS(74,46,12), SWIN(74,46,12)
C
	include 'dir.fi'

C***  FOR ICE MODELLING
      include 'paramz.fi'
       COMMON /M/ TSM(Niz,Njz),TMM(Niz,Njz),THM(Niz,Njz),TBM(Niz,Njz),
     2            HFM(Niz,Njz),HSM(Niz,Njz),HLM(Niz,Njz),DPM(Niz,Njz),
     3            QSM(Niz,Njz),QBM(Niz,Njz),VSM(Niz,Njz),TXM(Niz,Njz),
     5            TYM(Niz,Njz),W1M(Niz,Njz),W2M(Niz,Njz),QAM(Niz,Njz),
     6            UGM(Niz,Njz),VGM(Niz,Njz),UDM(Niz,Njz),VDM(Niz,Njz)
      include 'ice.fi'
c**************************
C
C************ END OF COMMON ******************
C
         REAL*4 SNGLSS(74,46,12)
	DIMENSION SWIN1(26,16,12)
	 character*40 restfile,restfile_ice,seatfile
      integer ttt(72,46)

      OPEN (lun,FILE=restfile,FORM='UNFORMATTED')
      rewind lun
      READ (lun) ((PHIS(I,J),I=2,73),J=1,46)
   !!!   READ (lun) C
      READ (lun)   
     1 TAU   ,TAUI  ,TAUO  ,TAUD  ,TAUE  ,TAUH  ,TAUC  ,TOFDAY,ROT   , 
     1 DT    ,DLAT  ,DLON  ,RAD   ,RSDIST,SIND  ,COSD  ,COSR  ,SINR  , 
     1 DAYPYR,ROTPER,SDEYR ,SOLTCE,APHEL ,DECMAX,ECCN  ,GUSTY ,        
     1 DAY   ,GRAV  ,RGAS  ,KAPA  ,PSF   ,PTROP ,PSL   ,CFG   , 
     1 FM    ,ED    ,PI    ,SIG1  ,SIG3  ,DSIG  ,PM    ,KAPEL ,RKAPA1, 
     1 STBO  ,GWM   ,DTC3  ,FRDAY ,CLH   ,COE1  ,HICE  ,CTYICE,CNDICE, 
     1 TICE  ,TCICE ,SNOWL ,COE   ,TSPD  ,PSTQ  ,QST   ,TST   ,PTRK  , 
     1PSFHO ,CALFA ,QC    ,PC    ,QCONST,EFVCON,TCT0  ,TCT2  ,TCT4  , 
     1TCST0 ,TCST2 ,TC01  ,TC02  ,TC03  ,TC04  ,TC23  ,TC24  ,TC34  , 
     1BLC   ,ALOGP0,FIM   ,HRGAS ,TCST4 ,FLR   ,PS4K  ,PS8K  ,ELOG  , 
     1S0, TOZONE    ,LAT   ,DXU   ,DXP   ,DYU,DYP, 
     1SFCALB,SINL  ,COSL  ,O3AMT ,TC12  ,TC14  , 
     1DXYP  ,F     ,SIG   ,FLEADN    ,FLEADS    ,ERROR  , 
     1COSLN,SINLN,DXYU1,DXYU2, GMT,GMR,GMKE, 
     1 JM,IM,ID,MNTHDY,SDEDY,NCYCLE,NC3,MONTH,MRCH,NSTEP,NAV

      READ (lun) ((ISFTYP(I,J),I=2,73),J=1,46)
      READ (lun) ((P(I,J),I=2,73),J=1,46)
      READ (lun) ((U(I,J,1),I=2,73),J=1,46)
      READ (lun) ((U(I,J,2),I=2,73),J=1,46)
      READ (lun) ((V(I,J,1),I=2,73),J=1,46)
      READ (lun) ((V(I,J,2),I=2,73),J=1,46)
      READ (lun) ((T(I,J,1),I=2,73),J=1,46)
      READ (lun) ((T(I,J,2),I=2,73),J=1,46)
      READ (lun) ((QW(I,J,1),I=2,73),J=1,46)
      READ (lun) ((QW(I,J,2),I=2,73),J=1,46)
      READ (lun) ((GW(I,J),I=2,73),J=1,46)
      READ (lun) ((GT(I,J),I=2,73),J=1,46)
      READ (lun) ((SNOAMT(I,J),I=2,73),J=1,46)
      READ (lun) ((TS(I,J),I=2,73),J=1,46)
      READ (lun) ((SD(I,J),I=2,73),J=1,46)
	close (lun)
c      SNOAMT=0.  !first time
C***  FOR ICE MODELLING
      OPEN (lun2,FILE=restfile_ice,FORM='UNFORMATTED')
      rewind lun2
      READ (lun2) TSM
      READ (lun2) TMM
      READ (lun2) TBM
      READ (lun2) HFM
      READ (lun2) HSM
      READ (lun2) UGM
      READ (lun2) VGM
      READ (lun2) UDM
      READ (lun2) VDM
      READ (lun2) W1M
      READ (lun2) W2M
      READ (lun2) GHIce
      READ (lun2) GHIce2
      READ (lun2) GIceComp
      READ (lun2) GIcePool
      READ (lun2) GIce2Pool
      READ (lun2) GIceHeatng
      READ (lun2) GIce2Heatng
      READ (lun2) GIceCND
      READ (lun2) GIce2CND
      READ (lun2) GIceUpFlow
      READ (lun2) GT1
      READ (lun2) GT2
      READ (lun2) SnoAmt1
      READ (lun2) SnoAmt2
c      READ (lun2) NewIce
	close (lun2)
c**************************
      ttt(1:72,:)=isftyp(2:73,:)
      ttt(6,4)=ttt(5,40)
      ttt(28,4)=ttt(31,4)
      ttt(29,4)=ttt(31,4)
      ttt(30,4)=ttt(31,4)
      ttt(18,5)=ttt(17,5)
      ttt(19,5)=ttt(17,5)
      ttt(20,5)=ttt(17,5)
      ttt(21,5)=ttt(17,5)
      ttt(25,5)=ttt(26,5)
      ttt(70,5)=ttt(71,5)
      ttt(23,10)=ttt(24,10)
      ttt(22,11)=ttt(23,11)
      ttt(22,12)=ttt(23,12)
      ttt(22,13)=ttt(23,13)
      ttt(22,13)=ttt(23,13)
      ttt(25,14)=ttt(26,14)
      ttt(66,14)=ttt(67,14)
      ttt(26,15)=ttt(27,15)
      ttt(26,15)=ttt(27,15)
      ttt(27,16)=ttt(28,16)
      ttt(27,16)=ttt(28,16)
      ttt(27,16)=ttt(28,16)
      ttt(43,17)=ttt(44,18)
      ttt(67,15)=ttt(68,15)
      ttt(67,16)=ttt(68,16)
      ttt(67,17)=ttt(68,17)
      ttt(23,18)=ttt(24,18)
      ttt(44,18)=ttt(45,18)
      ttt(46,18)=ttt(47,18)
      ttt(44,19)=ttt(45,19)
      ttt(29,19)=ttt(30,19)
      ttt(61,19)=ttt(62,19)
      ttt(29,20)=ttt(30,20)
      ttt(45,20)=ttt(46,20)
      ttt(62,20)=ttt(63,20)
      ttt(21,21)=ttt(22,21)
      ttt(29,21)=ttt(30,21)
      ttt(62,21)=ttt(63,21)
      ttt(64,21)=ttt(64,20)
      ttt(64,22)=ttt(64,20)
      ttt(66,22)=ttt(67,22)
      ttt(28,23)=ttt(29,23)
      ttt(45,23)=ttt(46,23)
      ttt(59,23)=ttt(60,23)
      ttt(64,23)=ttt(65,23)
      ttt(21,24)=ttt(22,24)
      ttt(45,24)=ttt(46,24)
      ttt(58,24)=ttt(57,24)
      ttt(59,24)=ttt(57,24)
      ttt(26,25)=ttt(25,25)
      ttt(57,25)=ttt(57,24)
      ttt(23,26)=ttt(25,26)
      ttt(24,26)=ttt(25,26)
      ttt(57,26)=ttt(57,27)
      ttt(20,27)=ttt(21,27)
      ttt(52,27)=ttt(51,27)
      ttt(53,27)=ttt(54,27)
      ttt(18,28)=ttt(17,28)
      ttt(19,28)=ttt(17,28)
      ttt(45,28)=ttt(46,28)
      ttt(47,28)=ttt(49,28)
      ttt(48,28)=ttt(49,28)
      ttt(53,28)=ttt(54,28)
      ttt(17,29)=ttt(18,29)
      ttt(48,29)=ttt(49,29)
      ttt(17,30)=ttt(18,30)
      ttt(34,30)=ttt(35,30)
      ttt(14,31)=ttt(15,31)
      ttt(18,31)=ttt(18,30)
      ttt(19,31)=ttt(19,30)
      ttt(20,31)=ttt(21,31)
      ttt(21,32)=ttt(22,32)
      ttt(44,32)=ttt(45,32)
      ttt(64,32)=ttt(63,32)
      ttt(36,33)=ttt(37,33)
      ttt(65,33)=ttt(66,33)
      ttt(12,34)=ttt(13,34)
      ttt(35,34)=ttt(36,34)
      ttt(63,34)=ttt(64,34)
      ttt(12,35)=ttt(13,35)
      ttt(24,35)=ttt(25,35)
      ttt(64,35)=ttt(65,35)
      ttt(25,36)=ttt(24,36)
      ttt(26,36)=ttt(27,36)
      ttt(37,36)=ttt(38,36)
      ttt(65,36)=ttt(66,36)
      ttt(11,37)=ttt(12,37)
      ttt(38,37)=ttt(39,37)
      ttt(65,37)=ttt(66,37)
      ttt(4,38)=ttt(5,38)
      ttt(36,38)=ttt(36,37)
      ttt(38,38)=ttt(39,38)
      ttt(66,38)=ttt(65,38)
      ttt(67,38)=ttt(65,38)
      ttt(68,38)=ttt(65,38)
      ttt(69,38)=ttt(70,38)
      ttt(3,39)=ttt(4,39)
      ttt(23,39)=ttt(22,39)
      ttt(28,39)=ttt(27,39)
      ttt(38,39)=ttt(39,39)
      ttt(70,39)=ttt(72,39)
      ttt(71,39)=ttt(72,39)
      ttt(2,40)=ttt(2,39)
      ttt(3,40)=ttt(4,40)
      ttt(23,40)=ttt(22,40)
      ttt(28,40)=ttt(27,40)
      ttt(38,40)=ttt(39,40)
      ttt(1,40)=ttt(72,40)
      ttt(3,41)=ttt(5,41)
      ttt(4,41)=ttt(5,41)
      ttt(6,41)=ttt(10,41)
      ttt(7,41)=ttt(10,41)
      ttt(8,41)=ttt(10,41)
      ttt(13,41)=ttt(14,41)
      ttt(23,41)=ttt(24,41)
      ttt(26,41)=ttt(27,41)
      ttt(32,41)=ttt(33,41)
      ttt(40,41)=ttt(41,41)
      ttt(41,41)=ttt(42,41)
      ttt(44,41)=ttt(43,41)
      ttt(45,41)=ttt(43,41)
      ttt(46,41)=ttt(43,41)
      ttt(47,41)=ttt(51,41)
      ttt(48,41)=ttt(51,41)
      ttt(49,41)=ttt(51,41)
      ttt(50,41)=ttt(51,41)
      ttt(14,42)=ttt(13,42)
      ttt(25,42)=ttt(26,42)
      ttt(33,42)=ttt(34,42)
      ttt(54,42)=ttt(53,42)
      ttt(58,42)=ttt(61,42)
      ttt(59,42)=ttt(61,42)
      ttt(60,42)=ttt(61,42)
      ttt(22,43)=ttt(21,43)
      ttt(23,43)=ttt(24,43)
      ttt(40,43)=ttt(39,43)
      ttt(41,43)=ttt(42,43)
      ttt(52,43)=ttt(51,43)
      ttt(53,43)=ttt(54,43)
      ttt(20,44)=ttt(19,44)
      ttt(21,44)=ttt(19,44)
      ttt(22,44)=ttt(19,44)
      ttt(23,44)=ttt(19,44)
      ttt(32,44)=ttt(35,44)
      ttt(33,44)=ttt(35,44)
      ttt(34,44)=ttt(35,44)
      isftyp(2:73,:)=ttt(1:72,:)
C
      DO 5 J=1,JM
      PHIS(1,J)=PHIS(73,J)
      PHIS(74,J)=PHIS(2,J)
      ISFTYP(1,J)=ISFTYP(73,J)
      ISFTYP(74,J)=ISFTYP(2,J)
      P(1,J)=P(73,J)
      P(74,J)=P(2,J)
      TS(1,J)=TS(73,J)
      TS(74,J)=TS(2,J)
      SD(74,J)=SD(2,J)
    5 SD(1,J)=SD(73,J)
      DO 6 L=1,2
      DO 6 J=1,JM
      U(1,J,L)=U(73,J,L)
      U(74,J,L)=U(2,J,L)
      V(1,J,L)=V(73,J,L)
      V(74,J,L)=V(2,J,L)
      T(1,J,L)=T(73,J,L)
      T(74,J,L)=T(2,J,L)
      QW(1,J,L)=QW(73,J,L)
      QW(74,J,L)=QW(2,J,L)
    6 CONTINUE
C
C        SET ACCUMULATIONS TO ZERO
C
c      DO 10 N=1,13
c      DO 10 I=1,3312
c   10 ZERO(I,N)=0.0
c      DO 15 N=63,74,2
c      DO 15 J=1,46
c      ZONAVG(J,N)=-1.E20
c   15 ZONAVG(J,N+1)=1.E20
         PRINT 111,TRIM(restfile),TAU
 111     FORMAT (' INPUT INITIAL DATA. FILE ',a,' TAU=',F9.2)
         write (7, 111) TRIM(restfile),TAU
C
C        READ 12 MONTHLY SEA TEMPERATURES
C
      seatfile=TRIM(BaseDir)//'SEAT\sst4x5'
      OPEN(8,FILE=seatfile,FORM='UNFORMATTED')
      DO 20 M=1,12
   20 READ (8) ((snglSS(I,J,M),I=2,73),J=1,46)
      close (8)
         DO 30 M=1,12
         DO 30 I=2,73
         DO 30 J=1,46
   30    SS(I,J,M)=SNGLSS(I,J,M)
         PRINT 112,TRIM(seatfile),TAU
	 write (7,112) TRIM(seatfile),tau
 112     FORMAT (' INPUT SEA DATA. FILE ',a,'  TAU=',F9.2)
C****************************
C     Surface Wind Reading
C
      OPEN(15, FILE=TRIM(BaseDir)//'SEAT\SWIN#F32',FORM='UNFORMATTED')
      READ (15) (((SWIN1(I,J,M),I=1,26),J=1,16),M=1,12)
      close (15)
c      READ(15) SWIN
	call OCEAN_STRETCH1(SWIN1,SWIN)
	do i=1,74
	 do j=1,46
	  do m=1,12
	   SWIN(I,J,M) = DMAX1(SWIN(I,J,M), GUSTY)
	  enddo
	 enddo
	enddo
C*****************************
      
      RETURN
      END
C**********************************************************************
C**********************************************************************
               SUBROUTINE OCEAN_STRETCH1(A,A1)
C**********************************************************************
C**********************************************************************
c раст€жка массивов в 3 раза. ¬ход:массив, размер по X 74
      IMPLICIT REAL*8(A-H,O-Z)
      include 'paramz.fi'
      DIMENSION A(26,16,12)
      DIMENSION A1(Niz+2,Njz,12)
	do k=1,12
	 do J=Njz,1,-1
	  do I=Niz+1,2,-1
	   A1(I,J,k) = A( (i+2)/3, j/3+1,k )
	  enddo
        A1(1,J,k)=A1(73,J,k)
        A1(74,J,k)=A1(2,J,k)
	 enddo
	enddo
      RETURN
      END
